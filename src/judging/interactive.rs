use crate::errors::JudgeResult;
use crate::judging::text::{Line, Text, Width as _Width, Word};
use crate::judging::{JudgingCommand, Outcome};
use crate::terminal::{TermOut, WriteSpaces as _WriteSpaces};
use crate::testsuite::InteractiveCase;
use crate::time::MillisRoundedUp as _MillisRoundedUp;

use derive_new::new;
use futures::{task, try_ready, Async, Future, Poll, Stream};
use tokio::io::{AsyncRead, AsyncWrite};

use std::sync::Arc;
use std::time::{Duration, Instant};
use std::{cmp, fmt, io, mem, slice, str};

pub(super) fn judge(
    case: &InteractiveCase,
    solver: &Arc<JudgingCommand>,
) -> JudgeResult<impl Future<Item = InteractiveOutcome, Error = io::Error>> {
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
    }.collect();
    Ok(stream.map(|outputs| InteractiveOutcome {
        outputs: NonEmptyVec::new(outputs),
    }))
}

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

#[derive(new)]
struct Waiting {
    #[new(value = "false")]
    finished: bool,
    proc: tokio_process::Child,
    start: Instant,
    deadline: Option<Instant>,
    construct_output: fn(bool, Text, Duration) -> Output,
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
                output = Some((self.construct_output)(
                    status.success(),
                    Text::exact(&format!("{}\n", status)),
                    now - self.start,
                ));
            }
        }
        self.finished = true;
        Ok(Async::Ready(output))
    }
}

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
                    let output = mem::replace(buf, Vec::with_capacity(0));
                    let output = str_from_utf8(&output)?;
                    let output = if *crlf_to_lf && output.contains("\r\n") {
                        Text::exact(&output.replace("\r\n", "\n"))
                    } else {
                        Text::exact(output)
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
                            let mut output = mem::replace(buf, Vec::with_capacity(1024));
                            *buf = output.split_off(*pos);
                            *pos = 0;
                            let elapsed = Instant::now() - *start;
                            let output = str_from_utf8(&output)?;
                            let output = if *crlf_to_lf && output.contains("\r\n") {
                                Text::exact(&output.replace("\r\n", "\n"))
                            } else {
                                Text::exact(output)
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

#[derive(new)]
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
                let output = mem::replace(&mut self.buf, Vec::with_capacity(0));
                let output = str_from_utf8(&output)?;
                let output = if self.crlf_to_lf && output.contains("\r\n") {
                    Text::exact(&output.replace("\r\n", "\n"))
                } else {
                    Text::exact(output)
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
                    let mut output = mem::replace(&mut self.buf, Vec::with_capacity(1024));
                    self.buf = output.split_off(lf_pos + 1);
                    let elapsed = Instant::now() - self.start;
                    let output = str_from_utf8(&output)?;
                    let output = if self.crlf_to_lf && output.contains("\r\n") {
                        Text::exact(&output.replace("\r\n", "\n"))
                    } else {
                        Text::exact(output)
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

fn str_from_utf8(s: &[u8]) -> io::Result<&str> {
    str::from_utf8(s).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "stream did not contain valid UTF-8",
        )
    })
}

pub(super) struct InteractiveOutcome {
    outputs: NonEmptyVec<Output>,
}

impl InteractiveOutcome {
    fn success(&self) -> bool {
        self.outputs.iter().all(|o| o.success())
    }
}

impl fmt::Display for InteractiveOutcome {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let time = self.outputs.last().time().millis_rounded_up();
        if self.success() {
            write!(f, "Success ({}ms)", time)
        } else {
            write!(f, "Failure ({}ms)", time)
        }
    }
}

impl Outcome for InteractiveOutcome {
    fn failure(&self) -> bool {
        !self.success()
    }

    fn color(&self) -> u8 {
        if self.success() {
            10
        } else {
            9
        }
    }

    fn print_details(&self, mut out: impl TermOut) -> io::Result<()> {
        fn print_str_left_aligned(
            mut out: impl TermOut,
            color: Option<u8>,
            s: &str,
            width: usize,
        ) -> io::Result<()> {
            out.with_reset(|o| {
                if let Some(color) = color {
                    o.fg(color)?;
                }
                o.bold()?.write_str(s)
            })?;
            let str_width = out.str_width(s);
            out.write_spaces(cmp::max(width, str_width) - str_width)
        }

        fn print_line_left_aligned(
            mut out: impl TermOut,
            line: &Line<Word>,
            width: usize,
        ) -> io::Result<()> {
            for word in line.words() {
                word.print_as_common(&mut out)?;
            }
            let line_width = line.width(out.str_width_fn());
            out.write_spaces(cmp::max(width, line_width) - line_width)
        }

        let str_width = out.str_width_fn();

        let title_width = self.outputs.max(|output| match output {
            Output::TesterStdout(..)
            | Output::TesterStderr(..)
            | Output::SolverStdout(..)
            | Output::SolverStderr(..) => 13,
            Output::TesterTerminated(..) | Output::SolverTerminated(..) => 17,
            Output::TimelimitExceeded(_) => 18,
        });
        let left_width = self.outputs.max(|output| match output {
            Output::TesterStdout(text, _)
            | Output::TesterStderr(text, _)
            | Output::TesterTerminated(_, text, _) => text.width(str_width),
            _ => 0,
        });
        let right_width = self.outputs.max(|output| match output {
            Output::SolverStdout(text, _)
            | Output::SolverStderr(text, _)
            | Output::SolverTerminated(_, text, _) => text.width(str_width),
            _ => 0,
        });

        let dummy = Text::exact("\n");

        for output in &self.outputs {
            use self::Output::*;
            let (title, left, right, time, color) = match output {
                TesterStdout(s, t) => ("Tester Stdout", s, &dummy, t, None),
                SolverStdout(s, t) => ("Solver Stdout", &dummy, s, t, None),
                TesterStderr(s, t) => ("Tester Stderr", s, &dummy, t, Some(11)),
                SolverStderr(s, t) => ("Solver Stderr", &dummy, s, t, Some(11)),
                TesterTerminated(true, s, t) => ("Tester terminated", s, &dummy, t, Some(10)),
                SolverTerminated(true, s, t) => ("Solver terminated", &dummy, s, t, Some(10)),
                TesterTerminated(false, s, t) => ("Tester terminated", s, &dummy, t, Some(9)),
                SolverTerminated(false, s, t) => ("Solver terminated", &dummy, s, t, Some(9)),
                TimelimitExceeded(t) => ("Timelimit Exceeded", &dummy, &dummy, t, Some(9)),
            };
            let empty_line = Line::<Word>::default();
            let (left, right) = (left.lines(), right.lines());
            for i in 0..cmp::max(left.len(), right.len()) {
                let left = left.get(i).unwrap_or(&empty_line);
                let right = right.get(i).unwrap_or(&empty_line);
                write!(out, "│")?;
                match i {
                    0 => print_str_left_aligned(&mut out, color, title, title_width),
                    _ => out.write_spaces(title_width),
                }?;
                write!(out, "│")?;
                print_line_left_aligned(&mut out, left, left_width)?;
                write!(out, "│")?;
                print_line_left_aligned(&mut out, right, right_width)?;
                match i {
                    0 => writeln!(out, "│{}ms", time.millis_rounded_up()),
                    _ => writeln!(out, "│"),
                }?;
            }
        }
        Ok(())
    }
}

enum Output {
    SolverStdout(Text, Duration),
    SolverStderr(Text, Duration),
    SolverTerminated(bool, Text, Duration),
    TesterStdout(Text, Duration),
    TesterStderr(Text, Duration),
    TesterTerminated(bool, Text, Duration),
    TimelimitExceeded(Duration),
}

impl Output {
    fn success(&self) -> bool {
        match self {
            Output::SolverTerminated(success, ..) | Output::TesterTerminated(success, ..) => {
                *success
            }
            Output::TimelimitExceeded(_) => false,
            _ => true,
        }
    }

    fn time(&self) -> Duration {
        match self {
            Output::SolverStdout(_, time)
            | Output::SolverStderr(_, time)
            | Output::SolverTerminated(_, _, time)
            | Output::TesterStdout(_, time)
            | Output::TesterStderr(_, time)
            | Output::TesterTerminated(_, _, time)
            | Output::TimelimitExceeded(time) => *time,
        }
    }
}

struct NonEmptyVec<T>(Vec<T>);

impl<T> NonEmptyVec<T> {
    fn new(inner: Vec<T>) -> Self {
        debug_assert!(!inner.is_empty());
        NonEmptyVec(inner)
    }

    fn last(&self) -> &T {
        self.0.last().unwrap()
    }

    fn iter(&self) -> slice::Iter<T> {
        self.0.iter()
    }

    fn max<R: Ord>(&self, f: impl Fn(&T) -> R) -> R {
        self.0.iter().map(&f).max().unwrap()
    }
}

impl<'a, T> IntoIterator for &'a NonEmptyVec<T> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    fn into_iter(self) -> slice::Iter<'a, T> {
        self.0.iter()
    }
}
