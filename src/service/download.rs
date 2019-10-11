use crate::errors::{ServiceError, ServiceErrorKind, ServiceResult};
use crate::service::session::Session;
use crate::service::ResponseExt as _;
use crate::signal::Sigwinch;
use crate::terminal::HasTermProps;
use crate::util::io::AsyncBufferedWriter;

use futures01::{task, try_ready, Async, Future, Poll, Stream as _};
use maplit::btreeset;
use reqwest::StatusCode;
use termcolor::WriteColor;
use tokio::io::AsyncWrite;

use std::time::{Duration, Instant};
use std::{char, io, mem};

#[derive(Debug)]
pub(super) struct Name {
    short: String,
    long: String,
}

impl Name {
    pub(super) fn new(short: impl Into<String>, long: impl Into<String>) -> Self {
        let (short, long) = (short.into(), long.into());
        Self { short, long }
    }
}

pub(super) trait DownloadProgress: Session {
    fn download_progress(
        &mut self,
        reqs: Vec<(Name, reqwest::r#async::RequestBuilder)>,
    ) -> ServiceResult<Vec<Vec<u8>>> {
        let stderr = self.stderr();
        let columns = stderr.columns_fn();

        let short_name_width = reqs
            .iter()
            .map(|(n, _)| stderr.str_width(&n.short))
            .max()
            .unwrap_or(0);
        let long_name_width = reqs
            .iter()
            .map(|(n, _)| stderr.str_width(&n.long))
            .max()
            .unwrap_or(0);

        let progresses = reqs
            .into_iter()
            .map(|(name, req)| {
                let (mut short, mut long) = (name.short, name.long);
                (0..short_name_width - stderr.str_width(&short)).for_each(|_| short.push(' '));
                (0..long_name_width - stderr.str_width(&long)).for_each(|_| long.push(' '));
                (Name::new(short, long), Progress::Response(req.send()))
            })
            .collect::<Vec<_>>();

        let cjk = stderr.char_width('\u{2588}') == Some(2);

        if stderr.supports_color() && !stderr.is_synchronous() {
            let wtr = stderr.ansi_async_wtr();
            self.runtime().block_on(Downloading::try_new(
                wtr,
                progresses,
                short_name_width,
                long_name_width,
                columns,
                cjk,
            )?)
        } else {
            self.runtime().block_on(Downloading::try_new(
                io::sink(),
                progresses,
                short_name_width,
                long_name_width,
                columns,
                cjk,
            )?)
        }
    }
}

#[derive(Debug)]
struct Downloading<
    W: AsyncWrite,
    // `reqwest::async_impl::Pending` (private)
    R: Future<Item = reqwest::r#async::Response, Error = reqwest::Error> + Send + 'static,
> {
    sigwinch: Sigwinch,
    bufwtr: AsyncBufferedWriter<W>,
    rendered_final_state: bool,
    progresses: Vec<(Name, Progress<R>)>,
    last_refreshed: Option<Instant>,
    min_refresh_interval: Duration,
    current_columns: Option<usize>,
    columns: fn() -> Option<usize>,
    short_name_width: usize,
    long_name_width: usize,
    started: Instant,
    cjk: bool,
}

#[derive(Debug)]
enum Progress<R: Future<Item = reqwest::r#async::Response, Error = reqwest::Error> + Send + 'static>
{
    Response(R),
    Body(Box<ProgressPending>),
    Finished { buf: Vec<u8>, time: Duration },
}

#[derive(Debug)]
struct ProgressPending {
    decoder: reqwest::r#async::Decoder,
    buf: Vec<u8>,
    content_len: Option<usize>,
}

impl<
        W: AsyncWrite,
        R: Future<Item = reqwest::r#async::Response, Error = reqwest::Error> + Send + 'static,
    > Downloading<W, R>
{
    fn try_new(
        wtr: W,
        progresses: Vec<(Name, Progress<R>)>,
        short_name_width: usize,
        long_name_width: usize,
        columns: fn() -> Option<usize>,
        cjk: bool,
    ) -> io::Result<Self> {
        let sigwinch = Sigwinch::try_new()?;
        Ok(Self {
            sigwinch,
            bufwtr: AsyncBufferedWriter::new(wtr),
            rendered_final_state: false,
            progresses,
            last_refreshed: None,
            min_refresh_interval: Duration::from_millis(100),
            current_columns: (columns)(),
            columns,
            short_name_width,
            long_name_width,
            started: Instant::now(),
            cjk,
        })
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

        #[derive(Clone, Copy, PartialEq)]
        enum Mode {
            Short,
            Middle,
            Long,
        }

        const RIGHT_SIDE_WIDTH: usize = 63;

        let mode = match self.current_columns {
            Some(w) if self.short_name_width + RIGHT_SIDE_WIDTH > w => Mode::Short,
            Some(w) if self.long_name_width + RIGHT_SIDE_WIDTH > w => Mode::Middle,
            _ => Mode::Long,
        };

        for (i, (name, progress)) in self.progresses.iter().enumerate() {
            let name = match mode {
                Mode::Long => &name.long,
                Mode::Middle | Mode::Short => &name.short,
            };

            if self.last_refreshed.is_some() {
                self.bufwtr.push_ansi_erase_in_line_entire();
            }

            match progress {
                Progress::Response(_) => {
                    self.bufwtr.push_ansi_bold();
                    self.bufwtr.push_str(name);
                    self.bufwtr.push_ansi_reset();
                    if mode != Mode::Short {
                        self.bufwtr.push_str("  Waiting response...");
                    }
                }
                Progress::Body(progress) => match (progress.buf.len(), progress.content_len) {
                    (n, None) => {
                        self.bufwtr.push_ansi_bold();
                        self.bufwtr.push_str(name);
                        self.bufwtr.push_ansi_reset();
                        self.bufwtr.push_str("  ???% ");
                        if mode != Mode::Short {
                            self.bufwtr.push_str(if self.cjk {
                                "[                                        ] "
                            } else {
                                "[                    ] "
                            });
                        }
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
                            "\x1b[1m{}\x1b[0m  {:>3}% ",
                            name,
                            100 * n / d,
                        ));
                        if mode != Mode::Short {
                            self.bufwtr.push_str("[");
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
                        }
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
                    if mode != Mode::Short {
                        self.bufwtr.push_str(
                            "  100% [\
                             \u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\
                             \u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\u{2588}\
                             \u{2588}\u{2588}\u{2588}\u{2588}]",
                        );
                    }
                    self.bufwtr.push_char(' ');
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
        W: AsyncWrite,
        R: Future<Item = reqwest::r#async::Response, Error = reqwest::Error> + Send + 'static,
    > Future for Downloading<W, R>
{
    type Item = Vec<Vec<u8>>;
    type Error = ServiceError;

    fn poll(&mut self) -> Poll<Vec<Vec<u8>>, ServiceError> {
        #[derive(PartialEq, Clone, Copy)]
        enum Status {
            AllFinished { rendered_final: bool },
            AnyPending { notify: bool },
        }

        crate::signal::check_ctrl_c()?;

        if self.sigwinch.poll()?.is_ready() {
            self.current_columns = (self.columns)();
        }

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
                                res.url2(),
                                res.status(),
                                btreeset![StatusCode::OK],
                            )
                            .into());
                        }
                        any_newly_ready_some = true;
                        let content_len = res.content_length().map(|n| n as usize);
                        Some(Progress::Body(Box::new(ProgressPending {
                            decoder: res.into_body(),
                            buf: Vec::with_capacity(content_len.map(|n| n + 1).unwrap_or(1024)),
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
