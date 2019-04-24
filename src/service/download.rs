use crate::errors::{ServiceError, ServiceErrorKind, ServiceResult};
use crate::service::session::HttpSession;
use crate::terminal::HasTermProps;
use crate::util::io::AsyncBufferedWriter;

use failure::ResultExt as _;
use futures::{task, try_ready, Async, Future, Poll, Stream as _};
use reqwest::{header, StatusCode};
use termcolor::WriteColor;
use tokio::io::AsyncWrite;
use tokio::runtime::Runtime;

use std::convert::Infallible;
use std::time::{Duration, Instant};
use std::{char, io, mem};

pub(super) trait DownloadProgress {
    type Write: WriteColor + HasTermProps;

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
        let cjk = out.char_width('\u{2588}') == Some(2);
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
        if out.supports_color() && !out.is_synchronous() {
            runtime.block_on(Downloading::new(
                crate::signal::ctrl_c(),
                out.ansi_async_wtr(),
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
        }
    }
}

struct Downloading<
    C: Future<Item = Infallible, Error = ServiceError> + Send + 'static,
    W: AsyncWrite,
    R: Future<Item = reqwest::r#async::Response, Error = reqwest::Error> + Send + 'static,
> {
    ctrlc: C,
    bufwtr: AsyncBufferedWriter<W>,
    rendered_final_state: bool,
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
        C: Future<Item = Infallible, Error = ServiceError> + Send + 'static,
        W: AsyncWrite,
        R: Future<Item = reqwest::r#async::Response, Error = reqwest::Error> + Send + 'static,
    > Downloading<C, W, R>
{
    fn new(ctrlc: C, wtr: W, progresses: Vec<(String, Progress<R>)>, cjk: bool) -> Self {
        Self {
            ctrlc,
            bufwtr: AsyncBufferedWriter::new(wtr),
            rendered_final_state: false,
            progresses,
            last_refreshed: None,
            min_refresh_interval: Duration::from_millis(100),
            started: Instant::now(),
            cjk,
        }
    }

    fn render(&mut self, elapsed: Duration) {
        fn write_size(bufwtr: &mut AsyncBufferedWriter<impl AsyncWrite>, n: usize) {
            if n >= 0x70_000_000 {
                bufwtr.write_fmt_to_buf(format_args!(
                    "{}.{} GiB",
                    n / 0x70_000_000,
                    (n % 0x70_000_000) / 0xb_333_334,
                ));
            } else if n >= 0x100_000 {
                bufwtr.write_fmt_to_buf(format_args!(
                    "{:>4}.{} MiB",
                    n / 0x100_000,
                    (n % 0x100_000) / 0x20000,
                ));
            } else if n >= 0x400 {
                bufwtr.write_fmt_to_buf(format_args!(
                    "{:>4}.{} KiB",
                    n / 0x400,
                    (n % 0x400) / 0x67,
                ));
            } else {
                bufwtr.write_fmt_to_buf(format_args!("  {:>4} B  ", n));
            }
        }

        for (i, (name, progress)) in self.progresses.iter().enumerate() {
            if self.last_refreshed.is_some() {
                self.bufwtr.push_ansi_erase_in_line_entire();
            }
            match progress {
                Progress::Response(_) => {
                    self.bufwtr.push_ansi_bold();
                    self.bufwtr.push_str(name);
                    self.bufwtr.push_ansi_reset();
                    self.bufwtr.push_str("  Waiting response...");
                }
                Progress::Body(progress) => match (progress.buf.len(), progress.content_len) {
                    (n, None) => {
                        self.bufwtr.push_ansi_bold();
                        self.bufwtr.push_str(name);
                        self.bufwtr.push_ansi_reset();
                        self.bufwtr.push_str("  ???% ");
                        self.bufwtr.push_str(if self.cjk {
                            "[                                        ] "
                        } else {
                            "[                    ] "
                        });
                        write_size(&mut self.bufwtr, n);
                        self.bufwtr.push_char(' ');
                        self.bufwtr.write_fmt_to_buf(format_args!(
                            "{:<02}:{:<02}",
                            elapsed.as_secs() / 60,
                            elapsed.as_secs() % 60,
                        ));
                    }
                    (n, Some(d)) => {
                        self.bufwtr.write_fmt_to_buf(format_args!(
                            "\x1b[1m{}\x1b[0m  {:>3}% [",
                            name,
                            100 * n / d,
                        ));
                        let p = (160 * n / d) as u32;
                        for i in 0..20 {
                            if p <= 8 * i && self.cjk {
                                self.bufwtr.push_str("  ");
                            } else if p <= 8 * i {
                                self.bufwtr.push_char(' ');
                            } else if p >= 8 * (i + 1) {
                                self.bufwtr.push_char('\u{2588}');
                            } else {
                                let c = char::from_u32(0x2590 - ((p - 8 * i) % 8)).unwrap();
                                self.bufwtr.push_char(c);
                            }
                        }
                        self.bufwtr.push_str("] ");
                        write_size(&mut self.bufwtr, n);
                        self.bufwtr.push_char(' ');
                        self.bufwtr.write_fmt_to_buf(format_args!(
                            "{:<02}:{:<02}",
                            elapsed.as_secs() / 60,
                            elapsed.as_secs() % 60,
                        ));
                    }
                },
                Progress::Finished { buf, time } => {
                    self.bufwtr.push_ansi_bold();
                    self.bufwtr.push_str(name);
                    self.bufwtr.push_ansi_reset();
                    self.bufwtr.push_str(
                        "  100% [\
                         \u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\
                         \u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\
                         \u{2588}\u{2588}\u{2588}\u{2588}] ",
                    );
                    write_size(&mut self.bufwtr, buf.len());
                    self.bufwtr.push_char(' ');
                    self.bufwtr.write_fmt_to_buf(format_args!(
                        "{:<02}:{:<02}",
                        time.as_secs() / 60,
                        time.as_secs() % 60,
                    ));
                }
            }
            if i + 1 < self.progresses.len() {
                self.bufwtr.push_str(if self.last_refreshed.is_some() {
                    "\x1b[1E"
                } else {
                    "\n"
                });
            }
        }
    }
}

impl<
        C: Future<Item = Infallible, Error = ServiceError> + Send + 'static,
        W: AsyncWrite,
        R: Future<Item = reqwest::r#async::Response, Error = reqwest::Error> + Send + 'static,
    > Future for Downloading<C, W, R>
{
    type Item = Vec<Vec<u8>>;
    type Error = ServiceError;

    fn poll(&mut self) -> Poll<Vec<Vec<u8>>, ServiceError> {
        #[derive(PartialEq, Clone, Copy)]
        enum Status {
            AllFinished { rendered_final: bool },
            AnyPending { notify: bool },
        }

        self.ctrlc.poll()?;
        try_ready!(self.bufwtr.poll_flush_buf());

        let (mut any_not_ready, mut any_newly_ready_some) = (false, false);
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
                        any_newly_ready_some = true;
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
                        any_newly_ready_some = true;
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
            Status::AnyPending { notify: false }
        } else if any_newly_ready_some {
            Status::AnyPending { notify: true }
        } else {
            Status::AllFinished {
                rendered_final: self.rendered_final_state,
            }
        };

        match status {
            Status::AnyPending { notify } => {
                let now = Instant::now();
                let start_refresh = self.last_refreshed.map_or(true, |last_refreshed| {
                    now > last_refreshed + self.min_refresh_interval
                });
                if start_refresh {
                    if self.last_refreshed.is_some() {
                        match self.progresses.len() - 1 {
                            0 => self.bufwtr.push_ansi_cursor_horizontal_absolute(0),
                            n => self.bufwtr.push_ansi_cursor_previous_line(n),
                        }
                    }
                    self.render(now - self.started);
                    self.last_refreshed = Some(now);
                }
                if notify {
                    task::current().notify();
                }
                Ok(Async::NotReady)
            }
            Status::AllFinished {
                rendered_final: false,
            } => {
                let now = Instant::now();
                if self.last_refreshed.is_some() {
                    match self.progresses.len() - 1 {
                        0 => self.bufwtr.push_ansi_cursor_horizontal_absolute(0),
                        n => self.bufwtr.push_ansi_cursor_previous_line(n),
                    }
                }
                self.render(now - self.started);
                self.bufwtr.push_char('\n');
                self.rendered_final_state = true;
                task::current().notify();
                Ok(Async::NotReady)
            }
            Status::AllFinished {
                rendered_final: true,
            } => Ok(Async::Ready(
                self.progresses
                    .iter_mut()
                    .map(|(_, progress)| match progress {
                        Progress::Response(_) | Progress::Body(_) => unreachable!(),
                        Progress::Finished { buf, .. } => mem::replace(buf, vec![]),
                    })
                    .collect(),
            )),
        }
    }
}
