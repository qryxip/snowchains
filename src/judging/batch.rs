use crate::command::JudgingCommand;
use crate::errors::JudgeResult;
use crate::judging::text::{self, Text, TextDiff};
use crate::judging::Verdict;
use crate::terminal::{HasTermProps, WriteExt as _};
use crate::testsuite::{BatchCase, ExpectedStdout};
use crate::time::MillisRoundedUp as _;
use crate::util;

use futures::{task, try_ready, Async, Future, Poll};
use serde::Serialize;
use termcolor::{Color, ColorSpec, WriteColor};
use tokio::io::{AsyncRead, AsyncWrite};

use std::process::ExitStatus;
use std::sync::Arc;
use std::time::{Duration, Instant};
use std::{fmt, io, mem};

pub(super) fn judge(
    case: &BatchCase,
    solver: &Arc<JudgingCommand>,
) -> JudgeResult<impl Future<Item = BatchVerdict, Error = io::Error>> {
    let crlf_to_lf = solver.crlf_to_lf();
    let (stdout_buf, stderr_buf) = (Vec::with_capacity(1024), Vec::with_capacity(1024));
    let mut solver = solver.spawn_async_piped()?;
    let start = Instant::now();
    let deadline = case.timelimit().map(|t| start + t);
    let stdin = solver.stdin().take().unwrap();
    let stdout = solver.stdout().take().unwrap();
    let stderr = solver.stderr().take().unwrap();
    Ok(Judge {
        input: case.input(),
        expected: case.expected().clone(),
        stdin: Writing::NotReady(stdin, 0),
        status: Waiting::NotReady(solver, start, deadline),
        stdout: Reading::NotReady(stdout, stdout_buf, crlf_to_lf),
        stderr: Reading::NotReady(stderr, stderr_buf, crlf_to_lf),
    })
}

#[derive(Debug)]
struct Judge {
    input: Arc<String>,
    expected: ExpectedStdout,
    stdin: Writing<tokio_process::ChildStdin>,
    status: Waiting<tokio_process::Child>,
    stdout: Reading<tokio_process::ChildStdout>,
    stderr: Reading<tokio_process::ChildStderr>,
}

impl Future for Judge {
    type Item = BatchVerdict;
    type Error = io::Error;

    fn poll(&mut self) -> Poll<BatchVerdict, io::Error> {
        match self.status.poll_wait()? {
            Async::NotReady => {
                try_ready!(self.stdin.poll_write(self.input.as_bytes()));
                Ok(Async::NotReady)
            }
            Async::Ready(Err(timelimit)) => Ok(Async::Ready(BatchVerdict(
                BatchVerdictInner::TimelimitExceeded {
                    timelimit,
                    stdin: Text::new(self.input.clone()),
                    expected: match &self.expected {
                        ExpectedStdout::Any { .. } => None,
                        ExpectedStdout::Exact(expected) => Some(Text::new(expected.clone())),
                        ExpectedStdout::Float { string, .. } => Some(Text::new(string.clone())),
                    },
                },
            ))),
            Async::Ready(Ok(())) => {
                try_ready!(self.stdout.poll_read());
                try_ready!(self.stderr.poll_read());
                let (status, elapsed) = self.status.unwrap();
                let outcome = CommandOutcome {
                    status,
                    elapsed,
                    stdin: self.input.clone(),
                    stdout: Arc::new(self.stdout.unwrap()),
                    stderr: Arc::new(self.stderr.unwrap()),
                };
                Ok(Async::Ready(outcome.compare(&self.expected)))
            }
        }
    }
}

#[derive(Debug)]
enum Writing<W: AsyncWrite> {
    NotReady(W, usize),
    Ready,
}

impl<W: AsyncWrite> Writing<W> {
    /// # Panics
    ///
    /// Panics if `num_wrote >= input.len()` where `self = Writing::NotReady(_, num_wrote)`
    fn poll_write(&mut self, input: &[u8]) -> Poll<(), io::Error> {
        let poll_status;
        match self {
            Writing::Ready => return Ok(Async::Ready(())),
            Writing::NotReady(stdin, num_wrote) => match stdin.poll_write(&input[*num_wrote..]) {
                Err(ref err) if err.kind() == io::ErrorKind::BrokenPipe => {
                    poll_status = Async::Ready(())
                }
                Err(err) => return Err(err),
                Ok(Async::NotReady) => poll_status = Async::NotReady,
                Ok(Async::Ready(n)) => {
                    *num_wrote += n;
                    poll_status = if *num_wrote == input.len() {
                        Async::Ready(())
                    } else {
                        Async::NotReady
                    };
                }
            },
        };
        match &poll_status {
            Async::Ready(()) => *self = Writing::Ready,
            Async::NotReady => task::current().notify(),
        }
        Ok(poll_status)
    }
}

#[derive(Debug)]
enum Waiting<F: Future<Item = ExitStatus>> {
    NotReady(F, Instant, Option<Instant>), // _1 <= _2
    Ready(ExitStatus, Duration),
}

impl<F: Future<Item = ExitStatus>> Waiting<F> {
    fn poll_wait(&mut self) -> Poll<std::result::Result<(), Duration>, F::Error> {
        let result;
        match self {
            Waiting::Ready(..) => return Ok(Async::Ready(Ok(()))),
            Waiting::NotReady(f, start, deadline) => {
                let now = Instant::now();
                if deadline.is_some() && now > (*deadline).unwrap() {
                    result = Some(Err((*deadline).unwrap() - *start));
                } else {
                    result = match f.poll()? {
                        Async::NotReady => None,
                        Async::Ready(s) => Some(Ok((s, now - *start))),
                    };
                }
            }
        }
        match result {
            None => {
                task::current().notify();
                Ok(Async::NotReady)
            }
            Some(Ok((s, t))) => {
                *self = Waiting::Ready(s, t);
                Ok(Async::Ready(Ok(())))
            }
            Some(Err(t)) => Ok(Async::Ready(Err(t))),
        }
    }

    /// # Panics
    ///
    /// Panics if `self` is `Waiting::NotReady(..)`.
    fn unwrap(&self) -> (ExitStatus, Duration) {
        match self {
            Waiting::NotReady(..) => panic!(),
            Waiting::Ready(s, t) => (*s, *t),
        }
    }
}

#[derive(Debug)]
enum Reading<R: AsyncRead> {
    NotReady(R, Vec<u8>, bool),
    Ready(String),
}

impl<R: AsyncRead> Reading<R> {
    fn poll_read(&mut self) -> Poll<(), io::Error> {
        fn string_from_utf8(s: Vec<u8>) -> io::Result<String> {
            String::from_utf8(s).map_err(|_| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "stream did not contain valid UTF-8",
                )
            })
        }

        let poll_status;
        match self {
            Reading::Ready(_) => return Ok(Async::Ready(())),
            Reading::NotReady(out, buf, _) => match try_ready!(out.read_buf(buf)) {
                0 => poll_status = Async::Ready(()),
                _ => {
                    if buf.len() + 512 > buf.capacity() {
                        buf.reserve(1024);
                    }
                    poll_status = Async::NotReady;
                }
            },
        }
        match &poll_status {
            Async::Ready(()) => {
                let s = match self {
                    Reading::Ready(_) => unreachable!(),
                    Reading::NotReady(_, buf, crlf_to_lf) => {
                        let s = string_from_utf8(mem::replace(buf, vec![]))?;
                        if *crlf_to_lf && s.contains("\r\n") {
                            s.replace("\r\n", "\n")
                        } else {
                            s
                        }
                    }
                };
                *self = Reading::Ready(s);
            }
            Async::NotReady => task::current().notify(),
        }
        Ok(poll_status)
    }

    /// # Panics
    ///
    /// Panics if `self` is `Reading::NotReady(..)`.
    fn unwrap(&mut self) -> String {
        match self {
            Reading::NotReady(..) => panic!(),
            Reading::Ready(s) => mem::replace(s, "".to_owned()),
        }
    }
}

#[derive(Debug)]
struct CommandOutcome {
    status: ExitStatus,
    elapsed: Duration,
    stdin: Arc<String>,
    stdout: Arc<String>,
    stderr: Arc<String>,
}

impl CommandOutcome {
    fn compare(&self, expected: &ExpectedStdout) -> BatchVerdict {
        let (status, elapsed) = (self.status, self.elapsed);
        let stdin = Text::new(self.stdin.clone());
        let stdout = self.stdout.clone();
        let stderr = Text::new(self.stderr.clone());
        let (expected, example, float_errors) = match expected {
            ExpectedStdout::Any { example } => {
                let example = example.as_ref().map(Clone::clone).map(Text::new);
                (None, example, None)
            }
            ExpectedStdout::Exact(expected) => (Some(expected.clone()), None, None),
            ExpectedStdout::Float { string, errors } => (Some(string.clone()), None, Some(*errors)),
        };
        BatchVerdict(if status.success() {
            if let Some(expected) = expected {
                match text::actual_or_diff(expected, stdout, float_errors) {
                    Ok(stdout) => BatchVerdictInner::Accepted {
                        elapsed,
                        stdin,
                        example,
                        stdout,
                        stderr,
                    },
                    Err(diff) => BatchVerdictInner::WrongAnswer {
                        elapsed,
                        stdin,
                        diff,
                        stderr,
                    },
                }
            } else {
                BatchVerdictInner::Accepted {
                    elapsed,
                    stdin,
                    example,
                    stdout: Text::new(stdout),
                    stderr,
                }
            }
        } else {
            BatchVerdictInner::RuntimeError {
                elapsed,
                stdin,
                expected: expected.map(Text::new),
                stdout: Text::new(stdout),
                stderr,
                status,
            }
        })
    }
}

#[derive(Debug, Serialize)]
#[serde(transparent)]
pub(crate) struct BatchVerdict(BatchVerdictInner);

#[derive(Debug, Serialize)]
#[serde(rename_all = "snake_case")]
enum BatchVerdictInner {
    Accepted {
        elapsed: Duration,
        stdin: Text,
        example: Option<Text>,
        stdout: Text,
        stderr: Text,
    },
    TimelimitExceeded {
        timelimit: Duration,
        stdin: Text,
        expected: Option<Text>,
    },
    WrongAnswer {
        elapsed: Duration,
        stdin: Text,
        diff: TextDiff,
        stderr: Text,
    },
    RuntimeError {
        elapsed: Duration,
        stdin: Text,
        expected: Option<Text>,
        stdout: Text,
        stderr: Text,
        #[serde(serialize_with = "util::serde::ser_exit_status")]
        status: ExitStatus,
    },
}

impl fmt::Display for BatchVerdict {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            BatchVerdictInner::Accepted { elapsed, .. } => {
                let elapsed = elapsed.millis_rounded_up();
                write!(fmt, "Accepted ({}ms)", elapsed)
            }
            BatchVerdictInner::TimelimitExceeded { timelimit, .. } => {
                let timelimit = timelimit.millis_rounded_up();
                write!(fmt, "Time Limit Exceeded ({}ms)", timelimit)
            }
            BatchVerdictInner::WrongAnswer { elapsed, .. } => {
                let elapsed = elapsed.millis_rounded_up();
                write!(fmt, "Wrong Answer ({}ms)", elapsed)
            }
            BatchVerdictInner::RuntimeError {
                elapsed, status, ..
            } => {
                let elapsed = elapsed.millis_rounded_up();
                write!(fmt, "Runtime Error ({}, {}ms)", status, elapsed)
            }
        }
    }
}

impl Verdict for BatchVerdict {
    fn is_success(&self) -> bool {
        match &self.0 {
            BatchVerdictInner::Accepted { .. } => true,
            _ => false,
        }
    }

    fn color_spec(&self) -> ColorSpec {
        let fg = match &self.0 {
            BatchVerdictInner::Accepted { .. } => Color::Green,
            BatchVerdictInner::TimelimitExceeded { .. } => Color::Red,
            _ => Color::Yellow,
        };
        let mut ret = ColorSpec::new();
        ret.set_fg(Some(fg)).set_intense(true);
        ret
    }

    fn print_details(
        &self,
        display_limit: Option<usize>,
        wtr: impl WriteColor + HasTermProps,
    ) -> io::Result<()> {
        struct Writer<W: WriteColor + HasTermProps> {
            wtr: W,
            display_limit: Option<usize>,
        }

        impl<W: WriteColor + HasTermProps> Writer<W> {
            fn print_section(&mut self, title: &str, text: &Text) -> io::Result<()> {
                self.wtr.set_color(color!(fg(Magenta), bold))?;
                writeln!(self.wtr, "{}", title)?;
                self.wtr.reset()?;
                if text.is_empty() {
                    self.wtr.set_color(color!(fg(Yellow), intense, bold))?;
                    self.wtr.write_str("EMPTY\n")?;
                    self.wtr.reset()
                } else if self.display_limit.map_or(false, |l| l < text.str_len()) {
                    self.println_size(text.str_len())
                } else {
                    text.print_pretty(&mut self.wtr)
                }
            }

            fn print_section_if_present(
                &mut self,
                title: &str,
                text: Option<&Text>,
            ) -> io::Result<()> {
                if let Some(text) = text {
                    self.print_section(title, text)?;
                }
                Ok(())
            }

            fn print_section_unless_empty(&mut self, title: &str, text: &Text) -> io::Result<()> {
                if !text.is_empty() {
                    self.wtr.set_color(color!(fg(Magenta), bold))?;
                    writeln!(self.wtr, "{}", title)?;
                    if self.display_limit.map_or(false, |l| l < text.str_len()) {
                        self.println_size(text.str_len())?;
                    } else {
                        self.wtr.reset()?;
                        text.print_pretty(&mut self.wtr)?;
                    }
                }
                Ok(())
            }

            fn print_diff(
                &mut self,
                left_title: &str,
                right_title: &str,
                diff: &TextDiff,
            ) -> io::Result<()> {
                self.wtr.set_color(color!(fg(Magenta), bold))?;
                self.wtr.write_str("diff:\n")?;
                self.wtr.reset()?;
                diff.print_pretty_with_title(left_title, right_title, &mut self.wtr)
            }

            fn println_size(&mut self, size: usize) -> io::Result<()> {
                let gib = size / 2usize.pow(30);
                let mib = (size / 2usize.pow(20)) & 0x3ff;
                let kib = (size / 2usize.pow(10)) & 0x3ff;
                let b = size & 0x3ff;
                self.wtr.set_color(color!(fg(Yellow), intense, bold))?;
                match (gib, mib, kib, b) {
                    (0, 0, 0, b) => writeln!(self.wtr, "{}B", b),
                    (0, 0, k, b) => writeln!(self.wtr, "{}.{}KiB", k, b / 0x67),
                    (0, m, k, _) => writeln!(self.wtr, "{}.{}MiB", m, k / 0x67),
                    (g, m, _, _) => writeln!(self.wtr, "{}.{}GiB", g, m / 0x67),
                }?;
                self.wtr.reset()
            }
        }

        let mut wtr = Writer { wtr, display_limit };
        match &self.0 {
            BatchVerdictInner::Accepted {
                stdin,
                example,
                stdout,
                stderr,
                ..
            } => {
                wtr.print_section("stdin:", stdin)?;
                wtr.print_section_if_present("example:", example.as_ref())?;
                wtr.print_section("stdout:", stdout)?;
                wtr.print_section_unless_empty("stderr:", stderr)
            }
            BatchVerdictInner::TimelimitExceeded {
                stdin, expected, ..
            } => {
                wtr.print_section("stdin:", stdin)?;
                wtr.print_section_if_present("expected:", expected.as_ref())
            }
            BatchVerdictInner::WrongAnswer {
                stdin,
                diff,
                stderr,
                ..
            } => {
                wtr.print_section("stdin:", stdin)?;
                wtr.print_diff("expected:", "actual:", diff)?;
                wtr.print_section_unless_empty("stderr:", stderr)
            }
            BatchVerdictInner::RuntimeError {
                stdin,
                expected,
                stdout,
                stderr,
                ..
            } => {
                wtr.print_section("stdin:", stdin)?;
                wtr.print_section_if_present("expected:", expected.as_ref())?;
                wtr.print_section("stdout:", stdout)?;
                wtr.print_section_unless_empty("stderr:", stderr)
            }
        }
    }
}
