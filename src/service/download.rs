use crate::errors::{ServiceError, ServiceErrorKind, ServiceResult};
use crate::service::session::HttpSession;
use crate::terminal::{TermOut, WriteAnsi as _};
use crate::util::lang_unstable::Never;

use failure::ResultExt as _;
use futures::{task, try_ready, Async, Future, Poll, Stream as _};
use reqwest::{header, StatusCode};
use tokio::io::AsyncWrite;
use tokio::runtime::Runtime;

use std::io::{self, Cursor, Write as _};
use std::time::{Duration, Instant};
use std::{char, mem};

pub(super) trait DownloadProgress {
    type Write: TermOut;

    fn requirements(&mut self) -> (&mut Self::Write, &HttpSession, &mut Runtime);

    /// # Panics
    ///
    /// Panics if the lengths are different.
    fn download_progress(
        &mut self,
        urls: &[impl AsRef<str>],
        alt_names: &[impl AsRef<str>],
        alt_reqs: Option<Vec<reqwest::r#async::RequestBuilder>>,
    ) -> ServiceResult<Vec<Vec<u8>>> {
        fn align(names: &[impl AsRef<str>], str_width: fn(&str) -> usize) -> (usize, Vec<String>) {
            let name_len = names
                .iter()
                .map(|s| str_width(s.as_ref()))
                .max()
                .unwrap_or(0);
            let names = names
                .iter()
                .map(|name| {
                    let mut name = name.as_ref().to_owned();
                    (0..name_len - str_width(&name)).for_each(|_| name.push(' '));
                    name
                })
                .collect();
            (name_len, names)
        }

        let (out, session, runtime) = self.requirements();
        let client = session.client();
        let cookie = session.cookies_to_header_value()?;
        const ALT_COLUMNS: usize = 100;
        let names = {
            let (url_len, urls) = align(urls, out.str_width_fn());
            if out.columns().unwrap_or(ALT_COLUMNS) < url_len + 63 {
                let (_, names) = align(alt_names, out.str_width_fn());
                names
            } else {
                urls
            }
        };
        let cjk = out.char_width_or_zero('\u{2588}') == 2;
        let progresses = match alt_reqs {
            None => names
                .into_iter()
                .zip(urls)
                .map(|(name, url)| {
                    let mut req = client.get(url.as_ref());
                    if let Some(cookie) = &cookie {
                        req = req.header(header::COOKIE, cookie.clone());
                    }
                    (name, Progress::Response(req.send()))
                })
                .collect(),
            Some(reqs) => names
                .into_iter()
                .zip(reqs)
                .map(|(s, r)| (s, Progress::Response(r.send())))
                .collect(),
        };
        let (contents, remaining) = if out.supports_color() {
            runtime.block_on(Downloading::new(
                crate::signal::ctrl_c(),
                Self::Write::async_wtr(),
                progresses,
                cjk,
            ))
        } else {
            runtime.block_on(Downloading::new(
                crate::signal::ctrl_c(),
                io::sink(),
                progresses,
                cjk,
            ))
        }?;
        out.write_all(&remaining)?;
        writeln!(out)?;
        out.flush()?;
        Ok(contents)
    }
}

struct Downloading<
    C: Future<Item = Never, Error = ServiceError> + Send + 'static,
    W: AsyncWrite,
    R: Future<Item = reqwest::r#async::Response, Error = reqwest::Error> + Send + 'static,
> {
    ctrlc: C,
    wtr: W,
    writing: Cursor<Vec<u8>>,
    needs_flush: bool,
    progresses: Vec<(String, Progress<R>)>,
    last_refreshed: Option<Instant>,
    min_refresh_interval: Duration,
    started: Instant,
    cjk: bool,
}

enum Progress<R: Future<Item = reqwest::r#async::Response, Error = reqwest::Error> + Send + 'static>
{
    Response(R),
    Body(Box<ProgressPending>),
    Finished { buf: Vec<u8>, time: Duration },
}

struct ProgressPending {
    decoder: reqwest::r#async::Decoder,
    buf: Vec<u8>,
    content_len: Option<usize>,
}

impl<
        C: Future<Item = Never, Error = ServiceError> + Send + 'static,
        W: AsyncWrite,
        R: Future<Item = reqwest::r#async::Response, Error = reqwest::Error> + Send + 'static,
    > Downloading<C, W, R>
{
    fn new(ctrlc: C, wtr: W, progresses: Vec<(String, Progress<R>)>, cjk: bool) -> Self {
        Self {
            ctrlc,
            wtr,
            writing: Cursor::new(Vec::with_capacity(1024)),
            needs_flush: false,
            progresses,
            last_refreshed: None,
            min_refresh_interval: Duration::from_millis(100),
            started: Instant::now(),
            cjk,
        }
    }

    fn render(&mut self, elapsed: Duration) {
        fn write_size(wtr: &mut Vec<u8>, n: usize) {
            if n >= 0x70_000_000 {
                write!(
                    wtr,
                    "{}.{} GiB",
                    n / 0x70_000_000,
                    (n % 0x70_000_000) / 0xb_333_334,
                )
            } else if n >= 0x100_000 {
                write!(
                    wtr,
                    "{:>4}.{} MiB",
                    n / 0x100_000,
                    (n % 0x100_000) / 0x20000
                )
            } else if n >= 0x400 {
                write!(wtr, "{:>4}.{} KiB", n / 0x400, (n % 0x400) / 0x67)
            } else {
                write!(wtr, "  {:>4} B  ", n)
            }
            .unwrap();
        }

        for (i, (name, progress)) in self.progresses.iter().enumerate() {
            if self.last_refreshed.is_some() {
                self.writing.get_mut().extend_from_slice(b"\x1b[2K");
            }
            match progress {
                Progress::Response(_) => {
                    write!(
                        self.writing.get_mut(),
                        "\x1b[1m{}\x1b[0m  Waiting response...",
                        name,
                    )
                    .unwrap();
                }
                Progress::Body(progress) => match (progress.buf.len(), progress.content_len) {
                    (n, None) => {
                        write!(self.writing.get_mut(), "\x1b[1m{}\x1b[0m  ???% ", name).unwrap();
                        self.writing.get_mut().extend_from_slice(if self.cjk {
                            b"[                                        ] "
                        } else {
                            b"[                    ] "
                        });
                        write_size(self.writing.get_mut(), n);
                        self.writing.get_mut().push(b' ');
                        write!(
                            self.writing,
                            "{:<02}:{:<02}",
                            elapsed.as_secs() / 60,
                            elapsed.as_secs() % 60,
                        )
                        .unwrap();
                    }
                    (n, Some(d)) => {
                        write!(
                            self.writing.get_mut(),
                            "\x1b[1m{}\x1b[0m  {:>3}% [",
                            name,
                            100 * n / d
                        )
                        .unwrap();
                        let p = (160 * n / d) as u32;
                        for i in 0..20 {
                            if p <= 8 * i && self.cjk {
                                self.writing.get_mut().extend_from_slice(b"  ");
                            } else if p <= 8 * i {
                                self.writing.get_mut().extend_from_slice(b" ");
                            } else if p >= 8 * (i + 1) {
                                self.writing
                                    .get_mut()
                                    .extend_from_slice("\u{2588}".as_bytes());
                            } else {
                                let c = char::from_u32(0x2590 - ((p - 8 * i) % 8)).unwrap();
                                write!(self.writing.get_mut(), "{}", c).unwrap()
                            }
                        }
                        self.writing.get_mut().extend_from_slice(b"] ");
                        write_size(self.writing.get_mut(), n);
                        self.writing.get_mut().push(b' ');
                        write!(
                            self.writing.get_mut(),
                            "{:<02}:{:<02}",
                            elapsed.as_secs() / 60,
                            elapsed.as_secs() % 60,
                        )
                        .unwrap();
                    }
                },
                Progress::Finished { buf, time } => {
                    let out = self.writing.get_mut();
                    write!(
                        out,
                        "\x1b[1m{}\x1b[0m  100% [\
                         \u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\
                         \u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\
                         \u{2588}\u{2588}\u{2588}\u{2588}] ",
                        name,
                    )
                    .unwrap();
                    write_size(out, buf.len());
                    out.push(b' ');
                    write!(
                        out,
                        "{:<02}:{:<02}",
                        time.as_secs() / 60,
                        time.as_secs() % 60
                    )
                    .unwrap();
                }
            }
            if i + 1 < self.progresses.len() {
                self.writing
                    .get_mut()
                    .extend_from_slice(if self.last_refreshed.is_some() {
                        b"\x1b[1E"
                    } else {
                        b"\n"
                    });
            }
        }
    }
}

impl<
        C: Future<Item = Never, Error = ServiceError> + Send + 'static,
        W: AsyncWrite,
        R: Future<Item = reqwest::r#async::Response, Error = reqwest::Error> + Send + 'static,
    > Future for Downloading<C, W, R>
{
    type Item = (Vec<Vec<u8>>, Vec<u8>);
    type Error = ServiceError;

    fn poll(&mut self) -> Poll<(Vec<Vec<u8>>, Vec<u8>), ServiceError> {
        #[derive(PartialEq, Clone, Copy)]
        enum Status {
            AllFinished,
            AnyPending,
            NeedsNotify,
        }

        self.ctrlc.poll()?;
        if self.needs_flush {
            try_ready!(self.wtr.poll_flush());
            self.needs_flush = false;
        }
        if self.writing.position() < self.writing.get_ref().len() as u64 {
            let n = try_ready!(self
                .wtr
                .poll_write(&self.writing.get_ref()[self.writing.position() as usize..]))
                as u64;
            self.writing.set_position(self.writing.position() + n);
            if self.writing.position() >= self.writing.get_ref().len() as u64 {
                self.writing.get_mut().clear();
                self.writing.set_position(0);
                match self.wtr.poll_flush()? {
                    Async::Ready(()) => {}
                    Async::NotReady => {
                        self.needs_flush = true;
                        return Ok(Async::NotReady);
                    }
                }
            }
        }
        let (mut any_not_ready, mut any_ready_some) = (false, false);
        for (_, progress) in &mut self.progresses {
            let next = match progress {
                Progress::Response(pending) => match pending.poll()? {
                    Async::NotReady => {
                        any_not_ready = true;
                        None
                    }
                    Async::Ready(res) => {
                        if res.status() != 200 {
                            return Err(ServiceErrorKind::UnexpectedStatusCode(
                                res.url().clone(),
                                res.status(),
                                vec![StatusCode::OK],
                            )
                            .into());
                        }
                        any_ready_some = true;
                        let (buf, content_len) = match res.headers().get(header::CONTENT_LENGTH) {
                            None => (vec![], None),
                            Some(s) => {
                                let l = s
                                    .to_str()
                                    .map_err(failure::Error::from)
                                    .and_then(|s| s.parse::<usize>().map_err(Into::into))
                                    .with_context(|_| {
                                        ServiceErrorKind::ReadHeader(header::CONTENT_LENGTH)
                                    })?;
                                (Vec::with_capacity(l), Some(l))
                            }
                        };
                        Some(Progress::Body(Box::new(ProgressPending {
                            decoder: res.into_body(),
                            buf,
                            content_len,
                        })))
                    }
                },
                Progress::Body(progress) => match progress.decoder.poll()? {
                    Async::NotReady => {
                        any_not_ready = true;
                        None
                    }
                    Async::Ready(Some(chunk)) => {
                        progress.buf.extend_from_slice(&chunk);
                        any_ready_some = true;
                        None
                    }
                    Async::Ready(None) => {
                        let buf = mem::replace(&mut progress.buf, vec![]);
                        let time = Instant::now() - self.started;
                        Some(Progress::Finished { buf, time })
                    }
                },
                Progress::Finished { .. } => None,
            };
            if let Some(next) = next {
                *progress = next;
            }
        }
        let status = if any_not_ready {
            Status::AnyPending
        } else if any_ready_some {
            Status::NeedsNotify
        } else {
            Status::AllFinished
        };
        let now = Instant::now();
        let start_refresh = status == Status::AllFinished
            || self.last_refreshed.map_or(true, |last_refreshed| {
                now > last_refreshed + self.min_refresh_interval
            });
        if start_refresh {
            if self.last_refreshed.is_some() {
                match self.progresses.len() - 1 {
                    0 => self.writing.get_mut().extend_from_slice(b"\x1b[0G"),
                    n => write!(self.writing.get_mut(), "\x1b[{}F", n).unwrap(),
                }
            }
            self.render(now - self.started);
            self.last_refreshed = Some(now);
        }
        match status {
            Status::AnyPending => Ok(Async::NotReady),
            Status::NeedsNotify => {
                task::current().notify();
                Ok(Async::NotReady)
            }
            Status::AllFinished => Ok(Async::Ready((
                self.progresses
                    .iter_mut()
                    .map(|(_, progress)| match progress {
                        Progress::Response(_) | Progress::Body(_) => unreachable!(),
                        Progress::Finished { buf, .. } => mem::replace(buf, vec![]),
                    })
                    .collect(),
                mem::replace(self.writing.get_mut(), vec![]),
            ))),
        }
    }
}
