use errors::{FileIoResult, ServiceError, ServiceResult, SessionError, SessionResult};
use path::{AbsPath, AbsPathBuf};

use futures::{future, task, Async, Future, Poll, Stream};
use itertools::Itertools as _Itertools;
use pbr::{MultiBar, Pipe, ProgressBar, Units};
use reqwest::header::{self, ContentLength};
use reqwest::unstable::async::Decoder;
use reqwest::{self, StatusCode};
use tokio_core::reactor::Core;
use url::Url;
use zip::ZipArchive;

use std::borrow::Cow;
use std::fs::File;
use std::io::{self, BufWriter, Write as _Write};
use std::time::{Duration, Instant};
use std::{fmt, panic, thread};

pub(super) struct ZipDownloader {
    client: reqwest::unstable::async::Client,
    core: Core,
}

impl ZipDownloader {
    pub(super) fn new(client: reqwest::unstable::async::Client, core: Core) -> Self {
        Self { client, core }
    }

    pub(super) fn download(
        &mut self,
        mut out: impl io::Write + Send + 'static,
        url_pref: Cow<str>,
        url_suf: &'static str,
        download_dir: AbsPath,
        names: &[impl AsRef<str>],
        cookie: Option<&header::Cookie>,
    ) -> ServiceResult<()> {
        let (client, core) = (&self.client, &mut self.core);
        let urls = Urls {
            pref: url_pref,
            names,
            suf: url_suf,
        };
        let paths = Paths {
            dir: download_dir,
            names,
        };
        writeln!(out, "URLS: {}\nTo:   {}", urls, download_dir.display(),)?;
        out.flush()?;
        let works = urls
            .try_to_urls()?
            .into_iter()
            .map(|url| receive_header(client, url, cookie.cloned()));
        let resps = core.run(future::join_all(works))?;
        if resps.len() == 1 {
            writeln!(out, "The endpoint returned 200.")?;
        } else {
            writeln!(out, "All endpoints returned 200.")?;
        }
        out.flush()?;
        let mut mb = MultiBar::on(out);
        let works = resps
            .into_iter()
            .zip(paths.iter())
            .map(|(resp, path)| DownloadBody::new(resp, &path, &mut mb))
            .collect::<FileIoResult<Vec<_>>>()?;
        let thread = thread::spawn(move || mb.listen());
        let result = core.run(future::join_all(works)).map(|_| ());
        thread.join().unwrap_or_else(|p| panic::resume_unwind(p));
        result
    }
}

struct Urls<'a, 'b, S: AsRef<str> + 'b> {
    pref: Cow<'a, str>,
    names: &'b [S],
    suf: &'static str,
}

impl<'a, 'b, S: AsRef<str>> Urls<'a, 'b, S> {
    fn try_to_urls(&self) -> SessionResult<Vec<Url>> {
        self.names
            .iter()
            .map(AsRef::as_ref)
            .map(|name| {
                let url = format!("{}{}{}", self.pref, name, self.suf);
                Url::parse(&url).map_err(|err| SessionError::ParseUrl(url, err))
            })
            .collect()
    }
}

impl<'a, 'b, S: AsRef<str>> fmt::Display for Urls<'a, 'b, S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.pref)?;
        if self.names.len() == 1 {
            write!(f, "{}", self.names[0].as_ref())
        } else {
            write!(
                f,
                "{{{}}}",
                self.names.iter().map(AsRef::as_ref).format(", ")
            )
        }?;
        write!(f, "{}", self.suf)
    }
}

struct Paths<'a, 'b, S: AsRef<str> + 'b> {
    dir: AbsPath<'a>,
    names: &'b [S],
}

impl<'a, 'b, S: AsRef<str>> Paths<'a, 'b, S> {
    fn iter<'c>(&'c self) -> impl Iterator<Item = AbsPathBuf> + 'c {
        self.names
            .iter()
            .map(move |s| self.dir.join(format!("{}.zip", s.as_ref())))
    }
}

fn receive_header(
    client: &reqwest::unstable::async::Client,
    url: Url,
    cookie: Option<header::Cookie>,
) -> impl Future<Item = reqwest::unstable::async::Response, Error = SessionError> {
    let mut req = client.get(url);
    if let Some(cookie) = cookie {
        req.header(cookie);
    }
    req.send()
        .map_err(SessionError::from)
        .and_then(|resp| match resp.status() {
            StatusCode::Ok => Ok(resp),
            s => Err(SessionError::UnexpectedStatusCode(vec![StatusCode::Ok], s)),
        })
}

struct DownloadBody {
    size_unknown: bool,
    path: AbsPathBuf,
    body: Decoder,
    file: BufWriter<File>,
    progress_bar: ProgressBar<Pipe>,
    pos: u64,
    updated: Instant,
}

impl DownloadBody {
    fn new(
        response: reqwest::unstable::async::Response,
        path: AbsPath,
        mb: &mut MultiBar<impl io::Write>,
    ) -> FileIoResult<Self> {
        const ALT_CAPACITY: usize = 30 * 1024 * 1024;
        let len = response.headers().get::<ContentLength>().map(|l| **l);
        let cap = len.map(|n| n as usize).unwrap_or(ALT_CAPACITY);
        let file = BufWriter::with_capacity(cap, ::fs::create_file_and_dirs(path)?);
        let mut progress_bar = mb.create_bar(len.unwrap_or(0));
        progress_bar.set_units(Units::Bytes);
        Ok(Self {
            size_unknown: len.is_none(),
            path: path.to_owned(),
            body: response.into_body(),
            file,
            progress_bar,
            pos: 0,
            updated: Instant::now(),
        })
    }

    fn update_progress_bar(&mut self) {
        if self.size_unknown {
            self.progress_bar.total = self.pos;
        }
        self.progress_bar.set(self.pos);
        self.updated = Instant::now();
    }
}

impl Future for DownloadBody {
    type Item = ();
    type Error = ServiceError;

    fn poll(&mut self) -> Poll<(), ServiceError> {
        match self.body.poll() {
            Err(err) => {
                self.progress_bar.finish_print("Failed");
                Err(err.into())
            }
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Ok(Async::Ready(None)) => {
                self.update_progress_bar();
                self.progress_bar.finish();
                self.file.flush()?;
                ZipArchive::new(File::open(&self.path)?)?;
                Ok(Async::Ready(()))
            }
            Ok(Async::Ready(Some(chunk))) => {
                const LEAST_INTERVAL: Duration = Duration::from_millis(200);
                self.file.write_all(&chunk)?;
                self.pos += chunk.len() as u64;
                if Instant::now() - self.updated > LEAST_INTERVAL {
                    self.update_progress_bar();
                }
                task::current().notify();
                Ok(Async::NotReady)
            }
        }
    }
}
