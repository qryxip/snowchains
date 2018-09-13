use console::{ConsoleWrite, Palette};
use errors::JudgeResult;
use judging::text::{Line, Text, Width as _Width, Word};
use judging::{JudgingCommand, MillisRoundedUp, Outcome};
use testsuite::InteractiveCase;

use futures::{task, Async, Future, Poll, Stream};
use tokio::io::{AsyncRead, AsyncWrite as _AsyncWrite};
use tokio_process;

use std::io::{self, Cursor, Seek as _Seek, SeekFrom};
use std::sync::Arc;
use std::time::{Duration, Instant};
use std::{cmp, fmt, mem, slice, str};

/// Tests for `case` and `solver` and returns one `InteractiveOutcome`.
pub(super) fn judge(
    case: &InteractiveCase,
    solver: &Arc<JudgingCommand>,
) -> JudgeResult<InteractiveOutcome> {
    let (tester, timelimit) = (case.tester(), case.timelimit());
    let (tester, solver) = (tester.spawn_async_piped()?, solver.spawn_async_piped()?);
    let start = Instant::now();
    let stream = Interaction {
        tester,
        solver,
        tester_finished: false,
        solver_finished: false,
        tester_stdin_buf: Cursor::new(vec![]),
        solver_stdin_buf: Cursor::new(vec![]),
        tester_stdout_buf: Some(vec![]),
        solver_stdout_buf: Some(vec![]),
        tester_stderr_buf: Some(vec![]),
        solver_stderr_buf: Some(vec![]),
        exceeded: false,
        start,
        deadline: timelimit.map(|t| start + t),
    };
    let outputs = NonEmptyVec::new(stream.collect().wait()?);
    Ok(InteractiveOutcome { outputs })
}

struct Interaction {
    tester: tokio_process::Child,
    solver: tokio_process::Child,
    tester_finished: bool,
    solver_finished: bool,
    tester_stdin_buf: Cursor<Vec<u8>>,
    solver_stdin_buf: Cursor<Vec<u8>>,
    tester_stdout_buf: Option<Vec<u8>>,
    solver_stdout_buf: Option<Vec<u8>>,
    tester_stderr_buf: Option<Vec<u8>>,
    solver_stderr_buf: Option<Vec<u8>>,
    start: Instant,
    deadline: Option<Instant>,
    exceeded: bool,
}

impl Stream for Interaction {
    type Item = Output;
    type Error = io::Error;

    fn poll(&mut self) -> Poll<Option<Output>, io::Error> {
        if self.tester_finished && self.solver_finished {
            return Ok(Async::Ready(None));
        }

        task::current().notify();

        if let Some(deadline) = self.deadline {
            let now = Instant::now();
            if !self.exceeded && now > deadline {
                self.tester.kill()?;
                self.solver.kill()?;
                self.tester_finished = true;
                self.solver_finished = true;
                self.exceeded = true;
                let timelimit = deadline - self.start;
                return Ok(Async::Ready(Some(Output::TimelimitExceeded(timelimit))));
            }
        }

        if !self.tester_finished
            && self.tester_stdout_buf.is_none()
            && self.tester_stderr_buf.is_none()
        {
            return self
                .poll_status(
                    |this| &mut this.tester,
                    |this| this.tester_finished = true,
                    Output::TesterTerminated,
                ).map(|r| r.map(Some));
        }
        if !self.solver_finished
            && self.solver_stdout_buf.is_none()
            && self.solver_stderr_buf.is_none()
        {
            return self
                .poll_status(
                    |this| &mut this.solver,
                    |this| this.solver_finished = true,
                    Output::SolverTerminated,
                ).map(|r| r.map(Some));
        }

        if self.should_write_to_tester_stdin() {
            try_ready!(self.write_to_stdin(|this| (
                this.tester.stdin().as_mut().unwrap(),
                &mut this.tester_stdin_buf,
            )))
        }
        if self.should_write_to_solver_stdin() {
            try_ready!(self.write_to_stdin(|this| (
                this.solver.stdin().as_mut().unwrap(),
                &mut this.solver_stdin_buf,
            )))
        }

        if self.tester_stdout_buf.is_some() {
            if let Async::Ready(output) = self.read_output(
                |this| this.tester.stdout().as_mut().unwrap(),
                |this| this.tester_stdout_buf.as_mut().unwrap(),
                |this, input| this.solver_stdin_buf = input,
                |this| this.tester_stdout_buf = None,
                Output::TesterStdout,
            )? {
                return Ok(Async::Ready(Some(output)));
            }
        }
        if self.solver_stdout_buf.is_some() {
            if let Async::Ready(output) = self.read_output(
                |this| this.solver.stdout().as_mut().unwrap(),
                |this| this.solver_stdout_buf.as_mut().unwrap(),
                |this, input| this.tester_stdin_buf = input,
                |this| this.solver_stdout_buf = None,
                Output::SolverStdout,
            )? {
                return Ok(Async::Ready(Some(output)));
            }
        }

        if self.tester_stderr_buf.is_some() {
            if let Async::Ready(output) = self.read_output(
                |this| this.tester.stderr().as_mut().unwrap(),
                |this| this.tester_stderr_buf.as_mut().unwrap(),
                |_, _| (),
                |this| this.tester_stderr_buf = None,
                Output::TesterStderr,
            )? {
                return Ok(Async::Ready(Some(output)));
            }
        }
        if self.solver_stderr_buf.is_some() {
            if let Async::Ready(output) = self.read_output(
                |this| this.solver.stderr().as_mut().unwrap(),
                |this| this.solver_stderr_buf.as_mut().unwrap(),
                |_, _| (),
                |this| this.solver_stderr_buf = None,
                Output::SolverStderr,
            )? {
                return Ok(Async::Ready(Some(output)));
            }
        }

        Ok(Async::NotReady)
    }
}

impl Interaction {
    fn poll_status(
        &mut self,
        proc: fn(&mut Self) -> &mut tokio_process::Child,
        mark_finished: fn(&mut Self),
        construct_output: fn(bool, Text, Duration) -> Output,
    ) -> Poll<Output, io::Error> {
        let status = try_ready!(proc(self).poll());
        let success = status.success();
        let text = Text::exact(&format!("{}\n", status));
        let elapsed = Instant::now() - self.start;
        let output = construct_output(success, text, elapsed);
        mark_finished(self);
        Ok(Async::Ready(output))
    }

    fn should_write_to_tester_stdin(&self) -> bool {
        !self.tester_stdin_buf.get_ref().is_empty()
    }

    fn should_write_to_solver_stdin(&self) -> bool {
        !self.solver_stdin_buf.get_ref().is_empty()
    }

    fn write_to_stdin(
        &mut self,
        stdin_and_buf: fn(&mut Self) -> (&mut tokio_process::ChildStdin, &mut Cursor<Vec<u8>>),
    ) -> Poll<(), io::Error> {
        fn clear_buf(buf: &mut Cursor<Vec<u8>>) {
            buf.seek(SeekFrom::Start(0)).unwrap();
            buf.get_mut().clear();
        }

        let (stdin, buf) = stdin_and_buf(self);
        if buf.position() >= buf.get_ref().len() as u64 {
            clear_buf(buf);
            match stdin.poll_flush() {
                Err(err) => {
                    return if err.kind() == io::ErrorKind::BrokenPipe {
                        Ok(Async::Ready(()))
                    } else {
                        Err(err)
                    };
                }
                Ok(Async::Ready(())) => return Ok(Async::Ready(())),
                Ok(Async::NotReady) => {}
            }
        } else {
            match stdin.poll_write(&buf.get_ref()[buf.position() as usize..]) {
                Err(ref err) if err.kind() == io::ErrorKind::BrokenPipe => {
                    clear_buf(buf);
                    return Ok(Async::Ready(()));
                }
                Err(err) => return Err(err),
                Ok(Async::Ready(n)) => {
                    buf.seek(SeekFrom::Current(n as i64)).unwrap();
                }
                Ok(Async::NotReady) => {}
            }
        }
        Ok(Async::Ready(()))
    }

    fn read_output<R: AsyncRead>(
        &mut self,
        rdr: fn(&mut Self) -> &mut R,
        buf: fn(&mut Self) -> &mut Vec<u8>,
        on_read: fn(&mut Self, Cursor<Vec<u8>>),
        mark_finished: fn(&mut Self),
        construct_output: fn(Text, Duration) -> Output,
    ) -> Poll<Output, io::Error> {
        let capture_all = |this: &mut Self| -> io::Result<_> {
            let t = Instant::now() - this.start;
            let s = Text::exact(str_from_utf8(&buf(this)[..])?);
            let input = mem::replace(buf(this), vec![]);
            on_read(this, Cursor::new(input));
            Ok(construct_output(s, t))
        };

        let capture_line = |this: &mut Self| -> io::Result<_> {
            debug_assert!(buf(this).contains(&b'\n') && buf(this).last() != Some(&b'\n'));
            let t = Instant::now() - this.start;
            let i = buf(this).iter().position(|&c| c == b'\n').unwrap();
            let s = Text::exact(str_from_utf8(&buf(this)[0..=i])?);
            let trailing = buf(this).split_off(i + 1);
            let input = mem::replace(buf(this), trailing);
            on_read(this, Cursor::new(input));
            Ok(construct_output(s, t))
        };

        let buf_ends_with_lf = buf(self).last() == Some(&b'\n');
        let buf_contains_lf = buf(self).contains(&b'\n');
        let buf_empty = buf(self).is_empty();
        let mut temp_buf = unsafe { mem::uninitialized::<[u8; 1024]>() };
        match rdr(self).poll_read(&mut temp_buf)? {
            Async::NotReady if buf_ends_with_lf => capture_all(self).map(Async::Ready),
            Async::NotReady if buf_contains_lf => capture_line(self).map(Async::Ready),
            Async::NotReady => Ok(Async::NotReady),
            Async::Ready(0) if buf_empty => {
                mark_finished(self);
                Ok(Async::NotReady)
            }
            Async::Ready(0) => {
                let output = capture_all(self)?;
                mark_finished(self);
                Ok(Async::Ready(output))
            }
            Async::Ready(n) => {
                buf(self).extend_from_slice(&temp_buf[0..n]);
                Ok(Async::NotReady)
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

    fn palette(&self) -> Palette {
        if self.success() {
            Palette::Success
        } else {
            Palette::Fatal
        }
    }

    fn print_details(&self, mut out: impl ConsoleWrite) -> io::Result<()> {
        fn print_str_left_aligned(
            mut out: impl ConsoleWrite,
            s: &str,
            width: usize,
        ) -> io::Result<()> {
            write!(out, "{}", s)?;
            let str_width = out.width(s);
            out.write_spaces(cmp::max(width, str_width) - str_width)
        }

        fn print_line_left_aligned(
            mut out: impl ConsoleWrite,
            palette: Option<Palette>,
            line: &Line<Word>,
            width: usize,
        ) -> io::Result<()> {
            fn print(
                mut out: impl ConsoleWrite,
                line: &Line<Word>,
                width: usize,
            ) -> io::Result<()> {
                for word in line.words() {
                    word.print_as_common(&mut out)?;
                }
                let line_width = line.width(out.str_width_fn());
                out.write_spaces(cmp::max(width, line_width) - line_width)
            }

            match palette {
                None => print(out, line, width),
                Some(palette) => print(out.plain(palette), line, width),
            }
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
            use console::Palette::*;
            let (title, left, right, time, palette) = match output {
                TesterStdout(s, t) => ("Tester Stdout", s, &dummy, t, None),
                SolverStdout(s, t) => ("Solver Stdout", &dummy, s, t, None),
                TesterStderr(s, t) => ("Tester Stderr", s, &dummy, t, Some(Warning)),
                SolverStderr(s, t) => ("Solver Stderr", &dummy, s, t, Some(Warning)),
                TesterTerminated(true, s, t) => ("Tester terminated", s, &dummy, t, Some(Success)),
                SolverTerminated(true, s, t) => ("Solver terminated", &dummy, s, t, Some(Success)),
                TesterTerminated(false, s, t) => ("Tester terminated", s, &dummy, t, Some(Fatal)),
                SolverTerminated(false, s, t) => ("Solver terminated", &dummy, s, t, Some(Fatal)),
                TimelimitExceeded(t) => ("Timelimit Exceeded", &dummy, &dummy, t, Some(Fatal)),
            };
            let empty_line = Line::<Word>::default();
            let (left, right) = (left.lines(), right.lines());
            for i in 0..cmp::max(left.len(), right.len()) {
                let left = left.get(i).unwrap_or(&empty_line);
                let right = right.get(i).unwrap_or(&empty_line);
                write!(out, "│")?;
                match i {
                    0 => print_str_left_aligned(out.bold(palette), title, title_width),
                    _ => out.write_spaces(title_width),
                }?;
                write!(out, "│")?;
                print_line_left_aligned(&mut out, palette, left, left_width)?;
                write!(out, "│")?;
                print_line_left_aligned(&mut out, palette, right, right_width)?;
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
