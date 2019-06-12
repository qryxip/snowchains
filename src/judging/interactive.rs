use crate::errors::JudgeResult;
use crate::judging::text::Text;
use crate::judging::{JudgingCommand, Verdict};
use crate::terminal::HasTermProps;
use crate::testsuite::InteractiveCase;
use crate::time::MillisRoundedUp as _;
use crate::util;
use crate::util::collections::NonEmptyVec;

use derive_new::new;
use futures::{task, try_ready, Async, Future, Poll, Stream};
use serde::Serialize;
use termcolor::{Color, ColorSpec, WriteColor};
use tokio::io::{AsyncRead, AsyncWrite};

use std::process::ExitStatus;
use std::sync::Arc;
use std::time::{Duration, Instant};
use std::{fmt, io, mem};

pub(super) fn judge(
    case: &InteractiveCase,
    solver: &Arc<JudgingCommand>,
) -> JudgeResult<impl Future<Item = InteractiveVerdict, Error = io::Error>> {
    let t_trim_crlf = case.tester().crlf_to_lf();
    let s_trim_crlf = solver.crlf_to_lf();
    let mut tester = case.tester().spawn_async_piped()?;
    let mut solver = solver.spawn_async_piped()?;
    let start = Instant::now();
    let deadline = case.timelimit().map(|t| start + t);
    let t_stdin = tester.stdin().take().unwrap();
    let t_stdout = tester.stdout().take().unwrap();
    let t_stderr = tester.stderr().take().unwrap();
    let s_stdin = solver.stdin().take().unwrap();
    let s_stdout = solver.stdout().take().unwrap();
    let s_stderr = solver.stderr().take().unwrap();
    let stream = Interaction {
        tester_to_solver: Pipe::new(t_stdout, s_stdin, start, t_trim_crlf, Output::TesterStdout),
        solver_to_tester: Pipe::new(s_stdout, t_stdin, start, s_trim_crlf, Output::SolverStdout),
        tester_stderr: Reading::new(t_stderr, start, t_trim_crlf, Output::TesterStderr),
        solver_stderr: Reading::new(s_stderr, start, s_trim_crlf, Output::SolverStderr),
        tester_status: Waiting::new(tester, start, deadline, Output::TesterTerminated),
        solver_status: Waiting::new(solver, start, deadline, Output::SolverTerminated),
    }
    .collect();
    Ok(stream.map(|outputs| InteractiveVerdict {
        outputs: NonEmptyVec::try_new(outputs).unwrap(),
    }))
}

#[derive(Debug)]
struct Interaction {
    tester_to_solver: Pipe<tokio_process::ChildStdout, tokio_process::ChildStdin>,
    solver_to_tester: Pipe<tokio_process::ChildStdout, tokio_process::ChildStdin>,
    tester_stderr: Reading<tokio_process::ChildStderr>,
    solver_stderr: Reading<tokio_process::ChildStderr>,
    tester_status: Waiting,
    solver_status: Waiting,
}

impl Stream for Interaction {
    type Item = Output;
    type Error = io::Error;

    fn poll(&mut self) -> Poll<Option<Output>, io::Error> {
        let (mut tester_finished, mut solver_finished) = (true, true);
        macro_rules! poll {
            ($stream:expr, $finished_p:ident) => {
                match $stream.poll()? {
                    Async::NotReady => $finished_p = false,
                    Async::Ready(None) => {}
                    Async::Ready(Some(output)) => return Ok(Async::Ready(Some(output))),
                }
            };
        }
        poll!(self.tester_to_solver, tester_finished);
        poll!(self.solver_to_tester, solver_finished);
        poll!(self.tester_stderr, tester_finished);
        poll!(self.solver_stderr, solver_finished);
        if tester_finished {
            poll!(self.tester_status, tester_finished);
        }
        if solver_finished {
            poll!(self.solver_status, solver_finished);
        }
        Ok(if tester_finished && solver_finished {
            Async::Ready(None)
        } else {
            Async::NotReady
        })
    }
}

#[derive(Debug, new)]
struct Waiting {
    #[new(value = "false")]
    finished: bool,
    proc: tokio_process::Child,
    start: Instant,
    deadline: Option<Instant>,
    construct_output: fn(ExitStatus, Duration) -> Output,
}

impl Stream for Waiting {
    type Item = Output;
    type Error = io::Error;

    fn poll(&mut self) -> Poll<Option<Output>, io::Error> {
        let mut output = None;
        if !self.finished {
            let now = Instant::now();
            if self.deadline.is_some() && self.deadline.unwrap() < now {
                self.proc.kill()?;
                output = Some(Output::TimelimitExceeded(
                    self.deadline.unwrap() - self.start,
                ));
            } else {
                let status = try_ready!(self.proc.poll());
                output = Some((self.construct_output)(status, now - self.start));
            }
        }
        self.finished = true;
        Ok(Async::Ready(output))
    }
}

#[derive(Debug)]
enum Pipe<O: AsyncRead, I: AsyncWrite> {
    Ready,
    NotReady {
        stdout: O,
        stdin: I,
        buf: Vec<u8>,
        pos: usize,
        start: Instant,
        crlf_to_lf: bool,
        construct_output: fn(Text, Duration) -> Output,
    },
}

impl<O: AsyncRead, I: AsyncWrite> Pipe<O, I> {
    fn new(
        stdout: O,
        stdin: I,
        start: Instant,
        crlf_to_lf: bool,
        construct_output: fn(Text, Duration) -> Output,
    ) -> Self {
        Pipe::NotReady {
            stdout,
            stdin,
            buf: Vec::with_capacity(1024),
            pos: 0,
            start,
            crlf_to_lf,
            construct_output,
        }
    }
}

impl<O: AsyncRead, I: AsyncWrite> Stream for Pipe<O, I> {
    type Item = Output;
    type Error = io::Error;

    fn poll(&mut self) -> Poll<Option<Output>, io::Error> {
        if let Pipe::NotReady {
            stdout,
            stdin,
            buf,
            pos,
            start,
            crlf_to_lf,
            construct_output,
        } = self
        {
            match stdout.read_buf(buf)? {
                Async::Ready(0) if buf.is_empty() => {}
                Async::Ready(0) => {
                    while buf.len() > *pos {
                        match stdin.poll_write(&buf[*pos..]) {
                            Err(ref e) if e.kind() == io::ErrorKind::BrokenPipe => break,
                            Err(e) => return Err(e),
                            Ok(Async::NotReady) => return Ok(Async::NotReady),
                            Ok(Async::Ready(n)) => *pos += n,
                        }
                    }
                    let elapsed = Instant::now() - *start;
                    let output = string_from_utf8(mem::replace(buf, Vec::with_capacity(0)))?;
                    let output = if *crlf_to_lf && output.contains("\r\n") {
                        Text::new(Arc::new(output.replace("\r\n", "\n")))
                    } else {
                        Text::new(Arc::new(output))
                    };
                    let output = construct_output(output, elapsed);
                    return Ok(Async::Ready(Some(output)));
                }
                Async::Ready(_) => {
                    task::current().notify();
                    if buf.len() + 512 > buf.capacity() {
                        buf.reserve(1024);
                    }
                    return Ok(Async::NotReady);
                }
                Async::NotReady => {
                    task::current().notify();
                    if let Some(lf_pos) = buf.iter().rposition(|&c| c == b'\n') {
                        if *pos > lf_pos {
                            let elapsed = Instant::now() - *start;
                            let mut output = mem::replace(buf, Vec::with_capacity(1024));
                            *buf = output.split_off(*pos);
                            *pos = 0;
                            let output = string_from_utf8(output)?;
                            let output = if *crlf_to_lf && output.contains("\r\n") {
                                Text::new(Arc::new(output.replace("\r\n", "\n")))
                            } else {
                                Text::new(Arc::new(output))
                            };
                            let output = construct_output(output, elapsed);
                            return Ok(Async::Ready(Some(output)));
                        }
                        match stdin.poll_write(buf) {
                            Err(ref e) if e.kind() == io::ErrorKind::BrokenPipe => {
                                *pos = lf_pos + 1
                            }
                            Err(e) => return Err(e),
                            Ok(Async::NotReady) => {}
                            Ok(Async::Ready(n)) => *pos += n,
                        }
                    }
                    return Ok(Async::NotReady);
                }
            }
        }
        *self = Pipe::Ready;
        Ok(Async::Ready(None))
    }
}

#[derive(Debug, new)]
struct Reading<R: AsyncRead> {
    rdr: R,
    #[new(value = "Vec::with_capacity(1024)")]
    buf: Vec<u8>,
    start: Instant,
    crlf_to_lf: bool,
    construct_output: fn(Text, Duration) -> Output,
}

impl<R: AsyncRead> Stream for Reading<R> {
    type Item = Output;
    type Error = io::Error;

    fn poll(&mut self) -> Poll<Option<Output>, io::Error> {
        match self.rdr.read_buf(&mut self.buf)? {
            Async::Ready(0) if self.buf.is_empty() => Ok(Async::Ready(None)),
            Async::Ready(0) => {
                let elapsed = Instant::now() - self.start;
                let output = string_from_utf8(mem::replace(&mut self.buf, Vec::with_capacity(0)))?;
                let output = if self.crlf_to_lf && output.contains("\r\n") {
                    Text::new(Arc::new(output.replace("\r\n", "\n")))
                } else {
                    Text::new(Arc::new(output))
                };
                let output = (self.construct_output)(output, elapsed);
                Ok(Async::Ready(Some(output)))
            }
            Async::Ready(_) => {
                task::current().notify();
                if self.buf.len() + 512 > self.buf.capacity() {
                    self.buf.reserve(1024);
                }
                Ok(Async::NotReady)
            }
            Async::NotReady => {
                task::current().notify();
                if let Some(lf_pos) = self.buf.iter().rposition(|&c| c == b'\n') {
                    let elapsed = Instant::now() - self.start;
                    let mut output = mem::replace(&mut self.buf, Vec::with_capacity(1024));
                    self.buf = output.split_off(lf_pos + 1);
                    let output = string_from_utf8(output)?;
                    let output = if self.crlf_to_lf && output.contains("\r\n") {
                        Text::new(Arc::new(output.replace("\r\n", "\n")))
                    } else {
                        Text::new(Arc::new(output))
                    };
                    let output = (self.construct_output)(output, elapsed);
                    Ok(Async::Ready(Some(output)))
                } else {
                    Ok(Async::NotReady)
                }
            }
        }
    }
}

fn string_from_utf8(bytes: Vec<u8>) -> io::Result<String> {
    String::from_utf8(bytes).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "stream did not contain valid UTF-8",
        )
    })
}

#[derive(Debug, Serialize)]
pub(crate) struct InteractiveVerdict {
    outputs: NonEmptyVec<Output>,
}

impl Verdict for InteractiveVerdict {
    fn is_success(&self) -> bool {
        self.outputs.iter().all(|output| match output {
            Output::SolverStdout(..)
            | Output::SolverStderr(..)
            | Output::TesterStdout(..)
            | Output::TesterStderr(..) => true,
            Output::SolverTerminated(s, ..) | Output::TesterTerminated(s, ..) => s.success(),
            _ => false,
        })
    }

    fn color_spec(&self) -> ColorSpec {
        let fg = if self.is_success() {
            Color::Green
        } else {
            Color::Red
        };
        let mut ret = ColorSpec::new();
        ret.set_fg(Some(fg)).set_intense(true);
        ret
    }

    fn print_details(
        &self,
        _: Option<usize>,
        mut wtr: impl WriteColor + HasTermProps,
    ) -> io::Result<()> {
        // TODO
        for output in &self.outputs {
            writeln!(wtr, "{:#?}", output.partial_debug())?;
        }
        Ok(())
    }
}

impl fmt::Display for InteractiveVerdict {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let time = self.outputs.last().time_millis_rounded_up();
        if self.is_success() {
            write!(fmt, "Success ({}ms)", time)
        } else {
            write!(fmt, "Failure ({}ms)", time)
        }
    }
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "snake_case")]
enum Output {
    SolverStdout(Text, Duration),
    SolverStderr(Text, Duration),
    SolverTerminated(
        #[serde(serialize_with = "util::serde::ser_exit_status")] ExitStatus,
        Duration,
    ),
    TesterStdout(Text, Duration),
    TesterStderr(Text, Duration),
    TesterTerminated(
        #[serde(serialize_with = "util::serde::ser_exit_status")] ExitStatus,
        Duration,
    ),
    TimelimitExceeded(Duration),
}

impl Output {
    fn time_millis_rounded_up(&self) -> u128 {
        match self {
            Output::SolverStdout(_, t)
            | Output::SolverStderr(_, t)
            | Output::SolverTerminated(_, t)
            | Output::TesterStdout(_, t)
            | Output::TesterStderr(_, t)
            | Output::TesterTerminated(_, t)
            | Output::TimelimitExceeded(t) => *t,
        }
        .millis_rounded_up()
    }

    fn partial_debug<'a>(&'a self) -> impl fmt::Debug + 'a {
        struct Debug<'a>(&'a Output);

        impl fmt::Debug for Debug<'_> {
            fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                let mut partial_fmt = |name: &str, text: &Text, dur: Duration| -> _ {
                    fmt.debug_tuple(name)
                        .field(&text.partial_debug())
                        .field(&dur)
                        .finish()
                };

                match &self.0 {
                    Output::SolverStdout(t, d) => partial_fmt("SolverStdout", t, *d),
                    Output::SolverStderr(t, d) => partial_fmt("SolverStderr", t, *d),
                    Output::TesterStdout(t, d) => partial_fmt("TesterStdout", t, *d),
                    Output::TesterStderr(t, d) => partial_fmt("TesterStderr", t, *d),
                    o @ Output::SolverTerminated(..)
                    | o @ Output::TesterTerminated(..)
                    | o @ Output::TimelimitExceeded(_) => fmt::Debug::fmt(o, fmt),
                }
            }
        }

        Debug(self)
    }
}
