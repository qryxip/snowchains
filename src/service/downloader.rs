use errors::{ServiceError, ServiceResult, SessionError};

use futures_01::{future, task, Async, Future, Poll, Stream};
use itertools::Itertools as _Itertools;
use pbr::{MultiBar, Pipe, ProgressBar, Units};
use reqwest::header::{self, ContentLength};
use reqwest::unstable::async::Decoder;
use reqwest::{self, StatusCode};
use tokio_core::reactor::Core;
use zip::ZipArchive;

use std::io::{self, Cursor, Write as _Write};
use std::time::{Duration, Instant};
use std::{mem, panic, thread};

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
        mut out: impl 'static + io::Write + Send,
        url_pref: &str,
        url_sufs: &[impl AsRef<str>],
        cookie: Option<&header::Cookie>,
    ) -> ServiceResult<Vec<Vec<u8>>> {
        let (client, core) = (&self.client, &mut self.core);
        write!(out, "Downloading {}", url_pref)?;
        if url_sufs.len() == 1 {
            writeln!(out, "{}...", url_sufs[0].as_ref())?;
        } else {
            let sufs = url_sufs.iter().map(AsRef::as_ref).format(", ");
            writeln!(out, "{{{}}}...", sufs)?;
        }
        out.flush()?;
        let works = url_sufs
            .iter()
            .map(|url_suf| format!("{}{}", url_pref, url_suf.as_ref()))
            .map(|url| receive_header(client, &url, cookie.cloned()));
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
            .map(|resp| DownloadBody::new(resp, &mut mb))
            .collect::<Vec<_>>();
        let thread = thread::spawn(move || mb.listen());
        let zips = core
            .run(future::join_all(works))?
            .into_iter()
            .map(|zip| zip.into_inner().into_inner())
            .collect();
        thread.join().unwrap_or_else(|p| panic::resume_unwind(p));
        Ok(zips)
    }
}

fn receive_header(
    client: &reqwest::unstable::async::Client,
    url: &str,
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
    body: Decoder,
    buf: Cursor<Vec<u8>>,
    progress_bar: ProgressBar<Pipe>,
    updated: Instant,
}

impl DownloadBody {
    fn new(
        response: reqwest::unstable::async::Response,
        mb: &mut MultiBar<impl io::Write>,
    ) -> Self {
        const ALT_CAPACITY: usize = 30 * 1024 * 1024;
        let len = response.headers().get::<ContentLength>().map(|l| **l);
        let cap = len.map(|n| n as usize).unwrap_or(ALT_CAPACITY);
        let buf = Cursor::new(Vec::with_capacity(cap));
        let mut progress_bar = mb.create_bar(len.unwrap_or(0));
        progress_bar.set_units(Units::Bytes);
        Self {
            size_unknown: len.is_none(),
            body: response.into_body(),
            buf,
            progress_bar,
            updated: Instant::now(),
        }
    }

    fn update_progress_bar(&mut self) {
        let pos = self.buf.position();
        if self.size_unknown {
            self.progress_bar.total = pos;
        }
        self.progress_bar.set(pos);
        self.updated = Instant::now();
    }
}

impl Future for DownloadBody {
    type Item = ZipArchive<Cursor<Vec<u8>>>;
    type Error = ServiceError;

    fn poll(&mut self) -> Poll<ZipArchive<Cursor<Vec<u8>>>, ServiceError> {
        match self.body.poll() {
            Err(err) => {
                self.progress_bar.finish_print("Failed");
                Err(err.into())
            }
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Ok(Async::Ready(None)) => {
                self.update_progress_bar();
                self.progress_bar.finish();
                let buf = mem::replace(&mut self.buf, Cursor::new(vec![]));
                ZipArchive::new(buf).map(Async::Ready).map_err(Into::into)
            }
            Ok(Async::Ready(Some(chunk))) => {
                const LEAST_INTERVAL: Duration = Duration::from_millis(20);
                self.buf.write_all(&chunk).unwrap();
                if Instant::now() - self.updated > LEAST_INTERVAL {
                    self.update_progress_bar();
                }
                task::current().notify();
                Ok(Async::NotReady)
            }
        }
    }
}