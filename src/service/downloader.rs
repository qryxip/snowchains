use errors::{FileIoResult, ServiceError, ServiceResult, SessionError, SessionResult};
use path::{AbsPath, AbsPathBuf};
use service;

use futures::sync::oneshot;
use futures::{self, future, task, Async, Future, Poll, Stream};
use itertools::Itertools as _Itertools;
use pbr::{MultiBar, Pipe, ProgressBar, Units};
use reqwest::header::{self, HeaderMap, HeaderValue};
use reqwest::unstable::async::Decoder;
use reqwest::{self, RedirectPolicy, StatusCode};
use tokio_core;
use url::Url;
use zip::ZipArchive;

use std::fs::File;
use std::io::{self, BufWriter, Write as _Write};
use std::time::{Duration, Instant};
use std::{fmt, panic, thread};

pub(super) struct ZipDownloader<'a, W: io::Write, S: AsRef<str> + 'a> {
    pub out: W,
    pub url_pref: &'a str,
    pub url_suf: &'static str,
    pub download_dir: AbsPath<'a>,
    pub names: &'a [S],
    pub timeout: Option<Duration>,
    pub cookie: Option<HeaderValue>,
}

impl<'a, W: io::Write, S: AsRef<str> + 'a> ZipDownloader<'a, W, S> {
    pub(super) fn download(mut self) -> ServiceResult<()> {
        let urls = Urls {
            pref: self.url_pref.to_owned(),
            names: self.names.iter().map(|s| s.as_ref().to_owned()).collect(),
            suf: self.url_suf,
        };
        let paths = Paths {
            dir: self.download_dir.to_owned(),
            names: self.names.iter().map(|s| s.as_ref().to_owned()).collect(),
        };
        let timeout = self.timeout;
        let cookie = self.cookie;
        writeln!(
            self.out,
            "URLS: {}\nTo:   {}",
            urls,
            self.download_dir.display(),
        )?;
        self.out.flush()?;
        let (header_result_tx, header_result_rx) = oneshot::channel();
        let (mut pb_tx, pb_rx) = futures::sync::mpsc::channel(self.names.len());
        let thread = thread::spawn(move || -> ServiceResult<()> {
            let mut builder = reqwest::unstable::async::Client::builder();
            if let Some(timeout) = timeout {
                builder.timeout(timeout);
            }
            let client = builder
                .redirect(RedirectPolicy::none())
                .referer(false)
                .default_headers({
                    let mut headers = HeaderMap::new();
                    headers.insert(header::USER_AGENT, service::USER_AGENT.parse().unwrap());
                    if let Some(cookie) = cookie {
                        headers.insert(header::COOKIE, cookie);
                    }
                    headers
                }).build()?;
            let works = urls
                .try_to_urls()?
                .into_iter()
                .map(|url| receive_header(&client, url));
            let mut core = tokio_core::reactor::Core::new()?;
            let resps = match core.run(future::join_all(works)) {
                Ok(resps) => {
                    header_result_tx.send(Ok(resps.len())).unwrap();
                    resps
                }
                Err(err) => {
                    header_result_tx.send(Err(())).unwrap();
                    return Err(err.into());
                }
            };
            let works = resps
                .into_iter()
                .zip(paths.iter())
                .zip(pb_rx.collect().wait().unwrap())
                .map(|((resp, path), pb)| DownloadBody::new(resp, &path, pb))
                .collect::<FileIoResult<Vec<_>>>()?;
            core.run(future::join_all(works).map(|_| ()))
        });
        match header_result_rx.wait() {
            Ok(Ok(1)) => {
                self.out.write_all(b"The endpoint returned 200.\n")?;
                self.out.flush()?;
            }
            Ok(Ok(_)) => {
                self.out.write_all(b"All endpoints returned 200.\n")?;
                self.out.flush()?;
            }
            _ => {}
        }
        let result = {
            let mut mb = MultiBar::on(&mut self.out);
            for _ in 0..self.names.len() {
                pb_tx.try_send(mb.create_bar(0)).unwrap();
            }
            drop(pb_tx);
            mb.listen();
            thread.join().unwrap_or_else(|p| panic::resume_unwind(p))
        };
        self.out.flush()?;
        result
    }
}

struct Urls {
    pref: String,
    names: Vec<String>,
    suf: &'static str,
}

impl Urls {
    fn try_to_urls(&self) -> SessionResult<Vec<Url>> {
        self.names
            .iter()
            .map(|name| {
                let url = format!("{}{}{}", self.pref, name, self.suf);
                Url::parse(&url).map_err(|err| SessionError::ParseUrl(url, err))
            }).collect()
    }
}

impl fmt::Display for Urls {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.pref)?;
        if self.names.len() == 1 {
            write!(f, "{}", self.names[0])
        } else {
            write!(f, "{{{}}}", self.names.iter().format(", "))
        }?;
        write!(f, "{}", self.suf)
    }
}

struct Paths {
    dir: AbsPathBuf,
    names: Vec<String>,
}

impl Paths {
    fn iter(&self) -> impl Iterator<Item = AbsPathBuf> {
        self.names
            .iter()
            .map(|s| self.dir.join(format!("{}.zip", s)))
            .collect::<Vec<_>>()
            .into_iter()
    }
}

fn receive_header(
    client: &reqwest::unstable::async::Client,
    url: Url,
) -> impl Future<Item = reqwest::unstable::async::Response, Error = SessionError> {
    let mut req = client.get(url);
    req.send()
        .map_err(SessionError::from)
        .and_then(|resp| match resp.status() {
            StatusCode::OK => Ok(resp),
            s => Err(SessionError::UnexpectedStatusCode(vec![StatusCode::OK], s)),
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
        mut progress_bar: ProgressBar<Pipe>,
    ) -> FileIoResult<Self> {
        const ALT_CAPACITY: usize = 30 * 1024 * 1024;
        let len = response
            .headers()
            .get(header::CONTENT_LENGTH)
            .and_then(|v| v.to_str().ok())
            .and_then(|s| s.parse::<usize>().ok());
        let cap = len.unwrap_or(ALT_CAPACITY);
        let file = BufWriter::with_capacity(cap, ::fs::create_file_and_dirs(path)?);
        progress_bar.total = len.map(|n| n as u64).unwrap_or(0);
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
                self.file.write_all(&chunk).map_err(|err| {
                    self.progress_bar.finish_print("Failed");
                    err
                })?;
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
