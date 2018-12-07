use crate::errors::{ExpandTemplateResult, ServiceError, ServiceErrorKind, ServiceResult};
use crate::path::{AbsPath, AbsPathBuf};
use crate::testsuite::DownloadDestinations;

use failure::Fail as _Fail;
use futures::sync::oneshot;
use futures::{future, task, Async, Future, Poll, Stream};
use itertools::Itertools as _Itertools;
use pbr::{MultiBar, Pipe, ProgressBar, Units};
use reqwest::header::{self, HeaderValue};
use reqwest::r#async::Decoder;
use reqwest::StatusCode;
use tokio::runtime::Runtime;
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
    pub destinations: &'a DownloadDestinations,
    pub names: &'a [S],
    pub client: reqwest::r#async::Client,
    pub cookie: Option<HeaderValue>,
}

impl<'a, W: io::Write, S: AsRef<str> + 'a> ZipDownloader<'a, W, S> {
    pub(super) fn download(mut self) -> ServiceResult<()> {
        let urls = Urls {
            pref: self.url_pref.to_owned(),
            names: self.names.iter().map(|s| s.as_ref().to_owned()).collect(),
            suf: self.url_suf,
        };
        let paths = self
            .names
            .iter()
            .map(|name| self.destinations.zip(name.as_ref()))
            .collect::<ExpandTemplateResult<Vec<_>>>()?;
        let client = self.client;
        let cookie = self.cookie;

        for path in &paths {
            if let Some(dir) = path.parent() {
                crate::fs::create_dir_all(&dir)?;
            }
        }

        writeln!(self.out, "{} to:", urls)?;
        for path in &paths {
            writeln!(self.out, "{}", path.display())?;
        }
        self.out.flush()?;

        let (header_result_tx, header_result_rx) = oneshot::channel();
        let (mut pb_tx, pb_rx) = futures::sync::mpsc::channel(self.names.len());
        let thread = thread::spawn(move || -> ServiceResult<()> {
            let (client, cookie) = (client, cookie);
            let works = {
                let urls = urls.try_to_urls()?;
                let mut works = Vec::with_capacity(urls.len());
                for url in urls {
                    works.push(receive_header(&client, url, cookie.as_ref()));
                }
                works
            };
            let mut runtime = Runtime::new()?;
            let fut = future::join_all(works).select(ctrl_c().map_err(Into::into));
            let resps = match runtime.block_on(fut) {
                Ok((resps, _)) => {
                    header_result_tx.send(Ok(resps.len())).unwrap();
                    resps
                }
                Err((err, _)) => {
                    header_result_tx.send(Err(())).unwrap();
                    return Err(err);
                }
            };
            let works = resps
                .into_iter()
                .zip(paths.iter())
                .zip(pb_rx.collect().wait().unwrap())
                .map(|((res, path), pb)| DownloadBody::try_new(res, &path, pb, ctrl_c::<()>()))
                .collect::<io::Result<Vec<_>>>()?;
            runtime.block_on(future::join_all(works)).map(|_| ())
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
    fn try_to_urls(&self) -> ServiceResult<Vec<Url>> {
        self.names
            .iter()
            .map(|name| {
                let url = format!("{}{}{}", self.pref, name, self.suf);
                Url::parse(&url).map_err(|e| e.context(ServiceErrorKind::ParseUrl(url)).into())
            })
            .collect()
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

fn receive_header(
    client: &reqwest::r#async::Client,
    url: Url,
    cookie: Option<&HeaderValue>,
) -> impl Future<Item = reqwest::r#async::Response, Error = ServiceError> {
    let req = client.get(url.clone());
    let req = match cookie {
        None => req,
        Some(cookie) => req.header(header::COOKIE, cookie),
    };
    req.send()
        .map_err(ServiceError::from)
        .and_then(|res| match res.status() {
            StatusCode::OK => Ok(res),
            s => Err(ServiceErrorKind::UnexpectedStatusCode(url, s, vec![StatusCode::OK]).into()),
        })
}

struct DownloadBody<F: Future<Error = io::Error>> {
    size_unknown: bool,
    path: AbsPathBuf,
    body: Decoder,
    file: BufWriter<File>,
    progress_bar: ProgressBar<Pipe>,
    pos: u64,
    updated: Instant,
    ctrlc: F,
}

impl<F: Future<Error = io::Error>> DownloadBody<F> {
    fn try_new(
        response: reqwest::r#async::Response,
        path: &AbsPath,
        mut progress_bar: ProgressBar<Pipe>,
        ctrlc: F,
    ) -> io::Result<Self> {
        const ALT_CAPACITY: usize = 30 * 1024 * 1024;
        let len = response
            .headers()
            .get(header::CONTENT_LENGTH)
            .and_then(|v| v.to_str().ok())
            .and_then(|s| s.parse::<usize>().ok());
        let cap = len.unwrap_or(ALT_CAPACITY);
        let file = BufWriter::with_capacity(
            cap,
            crate::fs::create_file_and_dirs(path).unwrap_or_else(|_| unimplemented!()),
        );
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
            ctrlc,
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

impl<F: Future<Error = io::Error>> Future for DownloadBody<F> {
    type Item = ();
    type Error = ServiceError;

    fn poll(&mut self) -> Poll<(), ServiceError> {
        if let Err(e) = self.ctrlc.poll() {
            self.progress_bar.finish_print("Interrupted");
            return Err(e.into());
        }
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

fn ctrl_c<T>() -> impl Future<Item = T, Error = io::Error> {
    tokio_signal::ctrl_c()
        .flatten_stream()
        .take(1)
        .into_future()
        .map_err(|(e, _)| e)
        .and_then::<_, io::Result<T>>(|_| {
            Err(io::Error::new(io::ErrorKind::Interrupted, "Interrupted"))
        })
}
