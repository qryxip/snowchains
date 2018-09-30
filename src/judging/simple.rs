use command::JudgingCommand;
use console::{ConsoleWrite, Palette};
use errors::JudgeError;
use judging::text::{Line, PrintAligned, Text, Width, Word};
use judging::{MillisRoundedUp, Outcome};
use testsuite::{ExpectedStdout, SimpleCase};

use diff;
use futures::{task, Async, Future, Poll};
use tokio::io::{AsyncRead, AsyncWrite as _AsyncWrite};
use tokio_process;

use std::io::{self, Write as _Write};
use std::process::ExitStatus;
use std::sync::Arc;
use std::time::{Duration, Instant};
use std::{self, cmp, fmt, mem};

pub(super) fn judge(
    case: &SimpleCase,
    solver: &Arc<JudgingCommand>,
) -> impl Future<Item = SimpleOutcome, Error = JudgeError> {
    // https://github.com/rust-lang/rust/issues/54427
    Judge {
        input: case.input(),
        expected: case.expected(),
        timelimit: case.timelimit(),
        solver: solver.clone(),
        process: None,
    }
}

struct Judge {
    input: Arc<String>,
    expected: Arc<ExpectedStdout>,
    timelimit: Option<Duration>,
    solver: Arc<JudgingCommand>,
    process: Option<RunCommand>,
}

impl Future for Judge {
    type Item = SimpleOutcome;
    type Error = JudgeError;

    fn poll(&mut self) -> Poll<SimpleOutcome, JudgeError> {
        if self.process.is_none() {
            let solver = self.solver.spawn_async_piped()?;
            let start = Instant::now();
            self.process = Some(RunCommand {
                process: solver,
                input: self.input.clone(),
                state: State::Writing(0),
                stdout: Vec::with_capacity(2048),
                stderr: Vec::with_capacity(2048),
                start,
                deadline: self.timelimit.map(|t| start + t),
            });
            task::current().notify();
            Ok(Async::NotReady)
        } else {
            let result = try_ready!(self.process.as_mut().unwrap().poll());
            Ok(Async::Ready(match result {
                Err(timelimit) => SimpleOutcomeInner::TimelimitExceeded {
                    timelimit,
                    input: Text::exact(&self.input),
                    expected: match self.expected.as_ref() {
                        ExpectedStdout::AcceptAny { .. } => None,
                        ExpectedStdout::Exact(expected) => Some(Text::exact(&expected)),
                        ExpectedStdout::Float {
                            lines: expected, ..
                        } => Some(Text::float(expected, None)),
                    },
                }.into(),
                Ok(outcome) => outcome.compare(&self.expected),
            }))
        }
    }
}

struct RunCommand {
    process: tokio_process::Child,
    input: Arc<String>,
    state: State,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
    start: Instant,
    deadline: Option<Instant>,
}

impl Future for RunCommand {
    type Item = std::result::Result<CommandOutcome, Duration>;
    type Error = io::Error;

    fn poll(&mut self) -> Poll<std::result::Result<CommandOutcome, Duration>, io::Error> {
        fn string_from_utf8(s: Vec<u8>) -> io::Result<String> {
            String::from_utf8(s).map_err(|_| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "stream did not contain valid UTF-8",
                )
            })
        }

        match self.state {
            State::Writing(num_wrote) => {
                task::current().notify();
                let stdin = self.process.stdin().as_mut().unwrap();
                let input = &self.input.as_bytes()[num_wrote..];
                match stdin.poll_write(input) {
                    Err(ref err) if err.kind() == io::ErrorKind::BrokenPipe => {
                        self.state = State::PollingStatus;
                        Ok(Async::NotReady)
                    }
                    Err(err) => Err(err),
                    Ok(Async::NotReady) => Ok(Async::NotReady),
                    Ok(Async::Ready(n)) => {
                        let num_wrote = num_wrote + n;
                        self.state = if num_wrote == self.input.len() {
                            State::ShuttingDownStdin
                        } else {
                            State::Writing(num_wrote)
                        };
                        Ok(Async::NotReady)
                    }
                }
            }
            State::ShuttingDownStdin => {
                task::current().notify();
                try_ready!(self.process.stdin().as_mut().unwrap().shutdown());
                self.process.stdin().take(); // sends EOF by dropping `stdin`
                self.state = State::PollingStatus;
                Ok(Async::NotReady)
            }
            State::PollingStatus => {
                task::current().notify();
                let now = Instant::now();
                if self.deadline.is_some() && self.deadline.unwrap() < now {
                    self.process.kill()?;
                    return Ok(Async::Ready(Err(self.deadline.unwrap() - self.start)));
                }
                let (status, time) = (try_ready!(self.process.poll()), now - self.start);
                self.state = State::PollingStdout(status, time);
                Ok(Async::NotReady)
            }
            State::PollingStdout(status, time) => {
                task::current().notify();
                try_ready!(self.poll_output(
                    |this| this.process.stdout().as_mut().unwrap(),
                    |this| &mut this.stdout,
                ));
                self.state = State::PollingStderr(status, time);
                Ok(Async::NotReady)
            }
            State::PollingStderr(status, time) => {
                task::current().notify();
                try_ready!(self.poll_output(
                    |this| this.process.stderr().as_mut().unwrap(),
                    |this| &mut this.stderr,
                ));
                let stdout = string_from_utf8(mem::replace(&mut self.stdout, vec![]))?;
                let stderr = string_from_utf8(mem::replace(&mut self.stderr, vec![]))?;
                Ok(Async::Ready(Ok(CommandOutcome {
                    status,
                    elapsed: time,
                    input: self.input.clone(),
                    stdout: Arc::new(stdout),
                    stderr: Arc::new(stderr),
                })))
            }
        }
    }
}

impl RunCommand {
    fn poll_output<R: AsyncRead>(
        &mut self,
        pipe: fn(&mut Self) -> &mut R,
        buf: fn(&mut Self) -> &mut Vec<u8>,
    ) -> Poll<(), io::Error> {
        let mut temp_buf = unsafe { mem::uninitialized::<[u8; 1024]>() };
        match try_ready!(pipe(self).poll_read(&mut temp_buf)) {
            0 => Ok(Async::Ready(())),
            n => {
                buf(self).extend_from_slice(&temp_buf[..n]);
                task::current().notify();
                Ok(Async::NotReady)
            }
        }
    }
}

#[derive(Clone, Copy)]
enum State {
    Writing(usize),
    ShuttingDownStdin,
    PollingStatus,
    PollingStdout(ExitStatus, Duration),
    PollingStderr(ExitStatus, Duration),
}

struct CommandOutcome {
    status: ExitStatus,
    elapsed: Duration,
    input: Arc<String>,
    stdout: Arc<String>,
    stderr: Arc<String>,
}

impl CommandOutcome {
    fn compare(&self, expected: &ExpectedStdout) -> SimpleOutcome {
        let (status, elapsed) = (self.status, self.elapsed);
        let input = Text::exact(&self.input);
        let stderr = Text::exact(&self.stderr);
        let (stdout, expected) = match expected {
            ExpectedStdout::AcceptAny => (Text::exact(&self.stdout), None),
            ExpectedStdout::Exact(expected) => {
                (Text::exact(&self.stdout), Some(Text::exact(expected)))
            }
            ExpectedStdout::Float {
                lines,
                absolute_error,
                relative_error,
            } => {
                let errors = Some((*absolute_error, *relative_error));
                let expected = Text::float(lines, errors);
                let stdout = Text::float(&self.stdout, None);
                (stdout, Some(expected))
            }
        };
        if !status.success() {
            SimpleOutcomeInner::RuntimeError {
                elapsed,
                input,
                expected,
                stdout,
                stderr,
                status,
            }
        } else if expected.is_some() && *expected.as_ref().unwrap() != stdout {
            SimpleOutcomeInner::WrongAnswer {
                elapsed,
                input,
                diff: TextDiff::new(expected.as_ref().unwrap(), &stdout),
                stderr,
            }
        } else {
            SimpleOutcomeInner::Accepted {
                elapsed,
                input,
                stdout,
                stderr,
            }
        }.into()
    }
}

/// Test result.
#[cfg_attr(test, derive(Debug))]
pub(super) struct SimpleOutcome {
    inner: SimpleOutcomeInner,
}

impl fmt::Display for SimpleOutcome {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.inner {
            SimpleOutcomeInner::Accepted { elapsed, .. } => {
                write!(f, "Accepted ({}ms)", elapsed.millis_rounded_up())
            }
            SimpleOutcomeInner::TimelimitExceeded { timelimit, .. } => write!(
                f,
                "Time Limit Exceeded ({}ms)",
                timelimit.millis_rounded_up()
            ),
            SimpleOutcomeInner::WrongAnswer { elapsed, .. } => {
                write!(f, "Wrong Answer ({}ms)", elapsed.millis_rounded_up())
            }
            SimpleOutcomeInner::RuntimeError {
                elapsed, status, ..
            } => {
                let elapsed = elapsed.millis_rounded_up();
                write!(f, "Runtime Error ({}, {}ms)", status, elapsed)
            }
        }
    }
}

impl Outcome for SimpleOutcome {
    fn failure(&self) -> bool {
        match self.inner {
            SimpleOutcomeInner::Accepted { .. } => false,
            _ => true,
        }
    }

    fn palette(&self) -> Palette {
        match self.inner {
            SimpleOutcomeInner::Accepted { .. } => Palette::Success,
            SimpleOutcomeInner::TimelimitExceeded { .. } => Palette::Fatal,
            SimpleOutcomeInner::WrongAnswer { .. } | SimpleOutcomeInner::RuntimeError { .. } => {
                Palette::Warning
            }
        }
    }

    fn print_details(&self, mut out: impl ConsoleWrite) -> io::Result<()> {
        fn print_section(mut out: impl ConsoleWrite, title: &str, text: &Text) -> io::Result<()> {
            writeln!(out.bold(Palette::Title), "{}", title)?;
            if text.is_empty() {
                writeln!(out.bold(Palette::Warning), "EMPTY")
            } else {
                text.print_all(out)
            }
        }

        fn print_section_unless_empty<'a>(
            mut out: impl ConsoleWrite,
            title: &str,
            text: impl Into<Option<&'a Text>>,
        ) -> io::Result<()> {
            if let Some(text) = text.into() {
                if !text.is_empty() {
                    writeln!(out.bold(Palette::Title), "{}", title)?;
                    text.print_all(out)?;
                }
            }
            Ok(())
        }

        fn print_diff(mut out: impl ConsoleWrite, title: &str, diff: &TextDiff) -> io::Result<()> {
            writeln!(out.bold(Palette::Title), "{}", title)?;
            diff.print(out)
        }

        match &self.inner {
            SimpleOutcomeInner::Accepted {
                input,
                stdout,
                stderr,
                ..
            } => {
                print_section(&mut out, "input:", input)?;
                print_section(&mut out, "stdout:", stdout)?;
                print_section_unless_empty(&mut out, "stderr:", stderr)
            }
            SimpleOutcomeInner::TimelimitExceeded {
                input, expected, ..
            } => {
                print_section(&mut out, "input:", input)?;
                print_section_unless_empty(&mut out, "expected:", expected.as_ref())
            }
            SimpleOutcomeInner::WrongAnswer {
                input,
                diff,
                stderr,
                ..
            } => {
                print_section(&mut out, "input:", input)?;
                print_diff(&mut out, "diff:", diff)?;
                print_section_unless_empty(&mut out, "stderr:", stderr)
            }
            SimpleOutcomeInner::RuntimeError {
                input,
                expected,
                stdout,
                stderr,
                ..
            } => {
                print_section(&mut out, "input:", input)?;
                print_section_unless_empty(&mut out, "expected:", expected.as_ref())?;
                print_section_unless_empty(&mut out, "stdout:", stdout)?;
                print_section(&mut out, "stderr:", stderr)
            }
        }
    }
}

impl From<SimpleOutcomeInner> for SimpleOutcome {
    fn from(inner: SimpleOutcomeInner) -> Self {
        Self { inner }
    }
}

#[cfg_attr(test, derive(Debug))]
enum SimpleOutcomeInner {
    Accepted {
        elapsed: Duration,
        input: Text,
        stdout: Text,
        stderr: Text,
    },
    TimelimitExceeded {
        timelimit: Duration,
        expected: Option<Text>,
        input: Text,
    },
    WrongAnswer {
        elapsed: Duration,
        input: Text,
        diff: TextDiff,
        stderr: Text,
    },
    RuntimeError {
        elapsed: Duration,
        input: Text,
        expected: Option<Text>,
        stdout: Text,
        stderr: Text,
        status: ExitStatus,
    },
}

#[cfg_attr(test, derive(Debug))]
pub(super) enum TextDiff {
    SameNumLines {
        lines: Vec<(LineDiffDetialed, LineDiffDetialed)>,
    },
    Lines {
        lines: Vec<(LineDiff, LineDiff)>,
    },
}

impl TextDiff {
    fn new(left: &Text, right: &Text) -> Self {
        if left.lines().len() == right.lines().len() {
            let mut lines = vec![];
            for (left, right) in left.lines().iter().zip(right.lines()) {
                let (mut l_diffs, mut r_diffs) = (vec![], vec![]);
                for diff in diff::slice(left.words(), right.words()) {
                    match diff {
                        diff::Result::Left(l) => l_diffs.push(Diff::NotCommon(l.clone())),
                        diff::Result::Right(r) => r_diffs.push(Diff::NotCommon(r.clone())),
                        diff::Result::Both(l, r) => {
                            l_diffs.push(Diff::Common(l.clone()));
                            r_diffs.push(Diff::Common(r.clone()));
                        }
                    }
                }
                lines.push((Line::new(l_diffs), Line::new(r_diffs)));
            }
            TextDiff::SameNumLines { lines }
        } else {
            #[derive(Default)]
            struct St {
                lines: Vec<(LineDiff, LineDiff)>,
                l_diffs: Vec<Line<Word>>,
                r_diffs: Vec<Line<Word>>,
            }

            impl St {
                fn clean_up(&mut self) {
                    let (l_diffs_len, r_diffs_len) = (self.l_diffs.len(), self.r_diffs.len());
                    for i in 0..cmp::min(l_diffs_len, r_diffs_len) {
                        let left = Diff::NotCommon(self.l_diffs[i].clone());
                        let right = Diff::NotCommon(self.r_diffs[i].clone());
                        self.lines.push((left, right));
                    }
                    let empty = Diff::NotCommon(Line::default());
                    if l_diffs_len < r_diffs_len {
                        for i in l_diffs_len..r_diffs_len {
                            let right = Diff::NotCommon(self.r_diffs[i].clone());
                            self.lines.push((empty.clone(), right));
                        }
                    } else {
                        for i in r_diffs_len..l_diffs_len {
                            let left = Diff::NotCommon(self.l_diffs[i].clone());
                            self.lines.push((left, empty.clone()));
                        }
                    }
                    self.l_diffs.clear();
                    self.r_diffs.clear();
                }
            }

            let mut st = St::default();
            for diff in diff::slice(left.lines(), right.lines()) {
                match diff {
                    diff::Result::Left(l) => st.l_diffs.push(l.clone()),
                    diff::Result::Right(r) => st.r_diffs.push(r.clone()),
                    diff::Result::Both(l, r) => {
                        st.clean_up();
                        st.lines
                            .push((Diff::Common(l.clone()), Diff::Common(r.clone())));
                    }
                }
            }
            st.clean_up();
            TextDiff::Lines { lines: st.lines }
        }
    }

    fn print(&self, out: impl ConsoleWrite) -> io::Result<()> {
        fn print(
            lines: &[(impl PrintAligned, impl PrintAligned)],
            mut out: impl ConsoleWrite,
        ) -> io::Result<()> {
            let (l_max_width, r_max_width) = {
                let (mut l_max_width, mut r_max_width) = (0, 0);
                for (l, r) in lines {
                    l_max_width = cmp::max(l_max_width, l.width(out.str_width_fn()));
                    r_max_width = cmp::max(r_max_width, r.width(out.str_width_fn()));
                }
                (l_max_width, r_max_width)
            };
            let (wl, wr) = (cmp::max(l_max_width, 8), cmp::max(r_max_width, 6));
            out.write_all("│".as_bytes())?;
            out.bold(Palette::Title).write_all(b"expected")?;
            out.write_spaces(wl - 8)?;
            out.write_all("│".as_bytes())?;
            out.bold(Palette::Title).write_all(b"stdout")?;
            out.write_spaces(wr - 6)?;
            out.write_all("│\n".as_bytes())?;
            for (l, r) in lines {
                out.write_all("│".as_bytes())?;
                l.print_aligned(&mut out, wl)?;
                out.write_all("│".as_bytes())?;
                r.print_aligned(&mut out, wr)?;
                out.write_all("│\n".as_bytes())?;
            }
            Ok(())
        }

        match self {
            TextDiff::SameNumLines { lines } => print(lines, out),
            TextDiff::Lines { lines } => print(lines, out),
        }
    }
}

#[cfg_attr(test, derive(Debug))]
#[derive(Clone)]
pub(super) enum Diff<T> {
    Common(T),
    NotCommon(T),
}

impl<T: Width> Width for Diff<T> {
    fn width(&self, f: fn(&str) -> usize) -> usize {
        match self {
            Diff::Common(x) => x.width(f),
            Diff::NotCommon(x) => x.width(f),
        }
    }
}

type LineDiffDetialed = Line<Diff<Word>>;
type LineDiff = Diff<Line<Word>>;

impl PrintAligned for LineDiffDetialed {
    fn print_aligned<W: ConsoleWrite>(&self, mut out: W, min_width: usize) -> io::Result<()> {
        for word_diff in self.words() {
            match word_diff {
                Diff::Common(w) => w.print_as_common(&mut out),
                Diff::NotCommon(w) => w.print_as_difference(&mut out),
            }?;
        }
        let width = self.width(out.str_width_fn());
        out.write_spaces(cmp::max(width, min_width) - width)
    }
}

impl PrintAligned for LineDiff {
    fn print_aligned<W: ConsoleWrite>(&self, mut out: W, min_width: usize) -> io::Result<()> {
        let (l, f): (_, fn(&Word, &mut W) -> io::Result<()>) = match self {
            Diff::Common(l) => (l, |w, out| w.print_as_common(out)),
            Diff::NotCommon(l) => (l, |w, out| w.print_as_difference(out)),
        };
        l.words().iter().try_for_each(|w| f(w, &mut out))?;
        let width = l.width(out.str_width_fn());
        out.write_spaces(cmp::max(width, min_width) - width)
    }
}

impl Text {
    fn float(s: &str, errors: Option<(f64, f64)>) -> Self {
        if let Some((absolute_error, relative_error)) = errors {
            let on_plain = |string: String| match string.parse::<f64>() {
                Ok(value) => Word::FloatLeft {
                    value,
                    string: Arc::new(string),
                    absolute_error,
                    relative_error,
                },
                Err(_) => Word::Plain(Arc::new(string)),
            };
            Self::new(s, on_plain)
        } else {
            fn on_plain(string: String) -> Word {
                let string = Arc::new(string);
                match string.parse::<f64>() {
                    Ok(value) => Word::FloatRight { value, string },
                    Err(_) => Word::Plain(string),
                }
            }
            Self::new(s, on_plain)
        }
    }

    fn print_all(&self, mut out: impl ConsoleWrite) -> io::Result<()> {
        for line in self.lines() {
            for word in line.words() {
                word.print_as_common(&mut out)?;
            }
            writeln!(out)?;
        }
        Ok(())
    }
}
