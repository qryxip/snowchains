use crate::errors::{ServiceError, ServiceErrorKind, ServiceResult};
use crate::service::session::HttpSession;
use crate::terminal::{TermOut, WriteAnsi};
use crate::util::lang_unstable::Never;

use failure::ResultExt;
use futures::sync::mpsc::UnboundedSender;
use futures::sync::oneshot;
use futures::{task, Async, Future, Poll, Stream};
use reqwest::{header, StatusCode};
use tokio::runtime::TaskExecutor;

use std::io::{self, Write};
use std::time::{Duration, Instant};
use std::{char, mem};

pub(super) trait DownloadProgress {
    type Write: TermOut;

    fn requirements(&mut self) -> (&mut Self::Write, &HttpSession, TaskExecutor);

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

        fn write_size(mut out: impl Write, n: usize) -> io::Result<()> {
            if n >= 0x70_000_000 {
                write!(
                    out,
                    "{}.{} GiB",
                    n / 0x70_000_000,
                    (n % 0x70_000_000) / 0xb_333_334,
                )
            } else if n >= 0x100_000 {
                write!(
                    out,
                    "{:>4}.{} MiB",
                    n / 0x100_000,
                    (n % 0x100_000) / 0x20000
                )
            } else if n >= 0x400 {
                write!(out, "{:>4}.{} KiB", n / 0x400, (n % 0x400) / 0x67)
            } else {
                write!(out, "  {:>4} B  ", n)
            }
        }

        let (mut out, session, executor) = self.requirements();
        let client = session.client();
        let cookie = session.cookies_to_header_value()?;
        let (write_order_tx, write_order_rx) = futures::sync::mpsc::unbounded();
        let oneshot_handle = oneshot::spawn(
            Downloading {
                ctrlc: crate::signal::ctrl_c(),
                write_order_tx,
                progresses: match alt_reqs {
                    None => urls
                        .iter()
                        .map(|url| {
                            let mut req = client.get(url.as_ref());
                            if let Some(cookie) = &cookie {
                                req = req.header(header::COOKIE, cookie.clone());
                            }
                            Progress::Response(req.send())
                        })
                        .collect(),
                    Some(reqs) => reqs
                        .into_iter()
                        .map(|r| Progress::Response(r.send()))
                        .collect(),
                },
                last_refreshed: None,
                min_refresh_interval: Duration::from_millis(100),
                started: Instant::now(),
            },
            &executor,
        );

        const ALT_COLUMNS: usize = 100;

        let (name_len, names) = {
            let (url_len, urls) = align(urls, out.str_width_fn());
            if out.columns().unwrap_or(ALT_COLUMNS) < url_len + 63 {
                align(alt_names, out.str_width_fn())
            } else {
                (url_len, urls)
            }
        };
        let cjk = out.char_width_or_zero('\u{2588}') == 2;
        let bar_size = if cjk { 42 } else { 22 };

        write_order_rx
            .then(|order| {
                // TODO: SIGWINCH
                if !out.supports_color() {
                    return Ok(());
                }
                let columns = out.columns().unwrap_or(ALT_COLUMNS);
                match &order.unwrap() {
                    WriteOrder::CursorUp(0) => write!(out, "\x1b[0G"),
                    WriteOrder::CursorUp(n) => write!(out, "\x1b[{}F", n),
                    WriteOrder::NextLine => write!(out, "\x1b[1E"),
                    WriteOrder::KillLine => write!(out, "\x1b[2K"),
                    WriteOrder::Lf => writeln!(out),
                    WriteOrder::WaitingResponse(i) if columns >= name_len + 21 => {
                        write!(out, "\x1b[1m{}\x1b[0m  Waiting response...", names[*i])
                    }
                    WriteOrder::SizeUnknown(i, t, n) if columns >= name_len + bar_size + 19 => {
                        write!(out, "\x1b[1m{}\x1b[0m  ???% ", names[*i])?;
                        out.write_str(if cjk {
                            "[                                        ] "
                        } else {
                            "[                    ] "
                        })?;
                        write_size(&mut out, *n)?;
                        out.write_str(" ")?;
                        write!(out, "{:<02}:{:<02}", t.as_secs() / 60, t.as_secs() % 60)
                    }
                    WriteOrder::SizeKnown(i, t, n, d) if columns >= name_len + bar_size + 19 => {
                        write!(out, "\x1b[1m{}\x1b[0m  {:>3}% [", names[*i], 100 * n / d)?;
                        let p = (160 * n / d) as u32;
                        for i in 0..20 {
                            if p <= 8 * i && cjk {
                                out.write_str("  ")
                            } else if p <= 8 * i {
                                out.write_str(" ")
                            } else if p >= 8 * (i + 1) {
                                out.write_str("\u{2588}")
                            } else {
                                let c = char::from_u32(0x2590 - ((p - 8 * i) % 8)).unwrap();
                                write!(out, "{}", c)
                            }?;
                        }
                        out.write_str("] ")?;
                        write_size(&mut out, *n)?;
                        out.write_str(" ")?;
                        write!(out, "{:<02}:{:<02}", t.as_secs() / 60, t.as_secs() % 60)
                    }
                    WriteOrder::Finished(i, t, l) if columns >= name_len + bar_size + 19 => {
                        write!(
                            out,
                            "\x1b[1m{}\x1b[0m  100% [\
                             \u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\
                             \u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\
                             \u{2588}\u{2588}\u{2588}\u{2588}] ",
                            names[*i],
                        )?;
                        write_size(&mut out, *l)?;
                        out.write_str(" ")?;
                        write!(out, "{:<02}:{:<02}", t.as_secs() / 60, t.as_secs() % 60)
                    }
                    WriteOrder::Flush => out.flush(),
                    _ if columns >= 3 => write!(out, "..."),
                    _ => Ok(()),
                }
            })
            .collect()
            .wait()?;
        writeln!(out)?;
        out.flush()?;

        oneshot_handle.wait()
    }
}

enum WriteOrder {
    CursorUp(usize),
    NextLine,
    KillLine,
    Lf,
    WaitingResponse(usize),
    SizeUnknown(usize, Duration, usize),
    SizeKnown(usize, Duration, usize, usize),
    Finished(usize, Duration, usize),
    Flush,
}

struct Downloading<
    C: Future<Item = Never, Error = ServiceError> + Send + 'static,
    R: Future<Item = reqwest::r#async::Response, Error = reqwest::Error> + Send + 'static,
> {
    ctrlc: C,
    write_order_tx: UnboundedSender<WriteOrder>,
    progresses: Vec<Progress<R>>,
    last_refreshed: Option<Instant>,
    min_refresh_interval: Duration,
    started: Instant,
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
        R: Future<Item = reqwest::r#async::Response, Error = reqwest::Error> + Send + 'static,
    > Future for Downloading<C, R>
{
    type Item = Vec<Vec<u8>>;
    type Error = ServiceError;

    fn poll(&mut self) -> Poll<Vec<Vec<u8>>, ServiceError> {
        #[derive(PartialEq, Clone, Copy)]
        enum Status {
            AllFinished,
            AnyPending,
            NeedsNotify,
        }

        self.ctrlc.poll()?;
        let (mut any_not_ready, mut any_ready_some) = (false, false);
        for progress in &mut self.progresses {
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
                let order = WriteOrder::CursorUp(self.progresses.len() - 1);
                self.write_order_tx.unbounded_send(order).unwrap();
            }
            let elapsed = now - self.started;
            for (i, progress) in self.progresses.iter().enumerate() {
                if self.last_refreshed.is_some() {
                    let order = WriteOrder::KillLine;
                    self.write_order_tx.unbounded_send(order).unwrap();
                }
                let order = match progress {
                    Progress::Response(_) => WriteOrder::WaitingResponse(i),
                    Progress::Body(progress) => match (progress.buf.len(), progress.content_len) {
                        (n, None) => WriteOrder::SizeUnknown(i, elapsed, n),
                        (n, Some(d)) => WriteOrder::SizeKnown(i, elapsed, n, d),
                    },
                    Progress::Finished { buf, time } => WriteOrder::Finished(i, *time, buf.len()),
                };
                self.write_order_tx.unbounded_send(order).unwrap();
                if i + 1 < self.progresses.len() {
                    let order = if self.last_refreshed.is_some() {
                        WriteOrder::NextLine
                    } else {
                        WriteOrder::Lf
                    };
                    self.write_order_tx.unbounded_send(order).unwrap();
                }
            }
            self.write_order_tx
                .unbounded_send(WriteOrder::Flush)
                .unwrap();
            self.last_refreshed = Some(now);
        }
        match status {
            Status::AnyPending => Ok(Async::NotReady),
            Status::NeedsNotify => {
                task::current().notify();
                Ok(Async::NotReady)
            }
            Status::AllFinished => Ok(Async::Ready(
                self.progresses
                    .iter_mut()
                    .map(|progress| match progress {
                        Progress::Response(_) | Progress::Body(_) => unreachable!(),
                        Progress::Finished { buf, .. } => mem::replace(buf, vec![]),
                    })
                    .collect(),
            )),
        }
    }
}
