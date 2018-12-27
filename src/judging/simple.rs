use crate::errors::JudgeResult;
use crate::judging::command::JudgingCommand;
use crate::judging::text::{Line, PrintAligned, Text, Width, Word};
use crate::judging::Outcome;
use crate::terminal::{TermOut, WriteAnsi, WriteSpaces};
use crate::testsuite::{ExpectedStdout, SimpleCase};
use crate::time::MillisRoundedUp;

use derive_more::From;
use futures::{task, try_ready, Async, Future, Poll};
use itertools::Itertools;
use tokio::io::{AsyncRead, AsyncWrite};

use std::process::ExitStatus;
use std::sync::Arc;
use std::time::{Duration, Instant};
use std::{cmp, fmt, io, mem};

pub(super) fn accepts(case: &SimpleCase, stdout: &str) -> SimpleOutcome {
    let input = Text::exact(&case.input());
    let (stdout, expected, example) = match case.expected().as_ref() {
        ExpectedStdout::Any { example } => (
            Text::exact(stdout),
            None,
            example.as_ref().map(|s| Text::exact(s)),
        ),
        ExpectedStdout::Exact(expected) => {
            (Text::exact(&stdout), Some(Text::exact(expected)), None)
        }
        ExpectedStdout::Float {
            string,
            absolute_error,
            relative_error,
        } => {
            let errors = Some((*absolute_error, *relative_error));
            let expected = Text::float(string, errors);
            let stdout = Text::float(&stdout, None);
            (stdout, Some(expected), None)
        }
    };
    if let Some(expected) = &expected {
        if stdout != *expected {
            return SimpleOutcomeInner::WrongAnswer {
                elapsed: Duration::new(0, 0),
                input,
                diff: TextDiff::new(expected, &stdout),
                stderr: Text::exact(""),
            }
            .into();
        }
    }
    SimpleOutcomeInner::Accepted {
        elapsed: Duration::new(0, 0),
        input,
        example,
        stdout,
        stderr: Text::exact(""),
    }
    .into()
}

pub(super) fn judge(
    case: &SimpleCase,
    solver: &Arc<JudgingCommand>,
) -> JudgeResult<impl Future<Item = SimpleOutcome, Error = io::Error>> {
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
        expected: case.expected(),
        stdin: Writing::NotReady(stdin, 0),
        status: Waiting::NotReady(solver, start, deadline),
        stdout: Reading::NotReady(stdout, stdout_buf, crlf_to_lf),
        stderr: Reading::NotReady(stderr, stderr_buf, crlf_to_lf),
    })
}

struct Judge {
    input: Arc<String>,
    expected: Arc<ExpectedStdout>,
    stdin: Writing<tokio_process::ChildStdin>,
    status: Waiting<tokio_process::Child>,
    stdout: Reading<tokio_process::ChildStdout>,
    stderr: Reading<tokio_process::ChildStderr>,
}

impl Future for Judge {
    type Item = SimpleOutcome;
    type Error = io::Error;

    fn poll(&mut self) -> Poll<SimpleOutcome, io::Error> {
        match self.status.poll_wait()? {
            Async::NotReady => {
                try_ready!(self.stdin.poll_write(self.input.as_bytes()));
                Ok(Async::NotReady)
            }
            Async::Ready(Err(timelimit)) => Ok(Async::Ready(
                SimpleOutcomeInner::TimelimitExceeded {
                    timelimit,
                    input: Text::exact(&self.input),
                    expected: match self.expected.as_ref() {
                        ExpectedStdout::Any { .. } => None,
                        ExpectedStdout::Exact(expected) => Some(Text::exact(&expected)),
                        ExpectedStdout::Float { string, .. } => Some(Text::float(string, None)),
                    },
                }
                .into(),
            )),
            Async::Ready(Ok(())) => {
                try_ready!(self.stdout.poll_read());
                try_ready!(self.stderr.poll_read());
                let (status, elapsed) = self.status.unwrap();
                let outcome = CommandOutcome {
                    status,
                    elapsed,
                    input: self.input.clone(),
                    stdout: Arc::new(self.stdout.unwrap()),
                    stderr: Arc::new(self.stderr.unwrap()),
                };
                Ok(Async::Ready(outcome.compare(&self.expected)))
            }
        }
    }
}

enum Writing<W: AsyncWrite> {
    NotReady(W, usize),
    Ready,
}

impl<W: AsyncWrite> Writing<W> {
    /// # Panics
    ///
    /// Panics if `num_wrote` >= `input.len()` where `self == Writing::NotReady(_, num_wrote)`
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
        let (stdout, expected, example) = match expected {
            ExpectedStdout::Any { example } => (
                Text::exact(&self.stdout),
                None,
                example.as_ref().map(|s| Text::exact(s)),
            ),
            ExpectedStdout::Exact(expected) => {
                (Text::exact(&self.stdout), Some(Text::exact(expected)), None)
            }
            ExpectedStdout::Float {
                string,
                absolute_error,
                relative_error,
            } => {
                let errors = Some((*absolute_error, *relative_error));
                let expected = Text::float(string, errors);
                let stdout = Text::float(&self.stdout, None);
                (stdout, Some(expected), None)
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
                example,
                stdout,
                stderr,
            }
        }
        .into()
    }
}

/// Test result.
#[cfg_attr(test, derive(Debug))]
#[derive(From)]
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

    fn color(&self) -> u8 {
        match self.inner {
            SimpleOutcomeInner::Accepted { .. } => 10,
            SimpleOutcomeInner::TimelimitExceeded { .. } => 9,
            SimpleOutcomeInner::WrongAnswer { .. } | SimpleOutcomeInner::RuntimeError { .. } => 11,
        }
    }

    fn print_details(&self, display_limit: Option<usize>, out: impl TermOut) -> io::Result<()> {
        struct Writer<W: WriteAnsi> {
            out: W,
            display_limit: Option<usize>,
        }

        impl<W: TermOut> Writer<W> {
            fn write_section(&mut self, title: &str, text: &Text) -> io::Result<()> {
                let (out, display_limit) = (&mut self.out, self.display_limit);
                out.with_reset(|o| o.fg(13)?.bold()?.write_str(title))?;
                out.write_str("\n")?;
                if text.is_empty() {
                    out.with_reset(|o| o.fg(11)?.bold()?.write_str("EMPTY\n"))
                } else if display_limit.map_or(false, |l| text.size() > l) {
                    super::writeln_size(out, text.size())
                } else {
                    text.print_all(out)
                }
            }

            fn write_section_unless_empty<'a>(
                &mut self,
                title: &str,
                text: impl Into<Option<&'a Text>>,
            ) -> io::Result<()> {
                if let Some(text) = text.into() {
                    let (out, display_limit) = (&mut self.out, self.display_limit);
                    out.with_reset(|o| o.fg(13)?.bold()?.write_str(title))?;
                    out.write_str("\n")?;
                    if display_limit.map_or(false, |l| text.size() > l) {
                        super::writeln_size(out, text.size())?;
                    } else if !text.is_empty() {
                        text.print_all(out)?;
                    }
                }
                Ok(())
            }

            fn write_diff(&mut self, title: &str, diff: &TextDiff) -> io::Result<()> {
                let (out, display_limit) = (&mut self.out, self.display_limit);
                out.with_reset(|o| o.fg(13)?.bold()?.write_str(title))?;
                out.write_str("\n")?;
                if display_limit.map_or(false, |l| diff.size() > l) {
                    super::writeln_size(out, diff.size())
                } else {
                    diff.print(out)
                }
            }
        }

        let mut wtr = Writer { out, display_limit };
        match &self.inner {
            SimpleOutcomeInner::Accepted {
                input,
                example,
                stdout,
                stderr,
                ..
            } => {
                wtr.write_section("input:", input)?;
                wtr.write_section_unless_empty("example:", example.as_ref())?;
                wtr.write_section("stdout:", stdout)?;
                wtr.write_section_unless_empty("stderr:", stderr)
            }
            SimpleOutcomeInner::TimelimitExceeded {
                input, expected, ..
            } => {
                wtr.write_section("input:", input)?;
                wtr.write_section_unless_empty("expected:", expected.as_ref())
            }
            SimpleOutcomeInner::WrongAnswer {
                input,
                diff,
                stderr,
                ..
            } => {
                wtr.write_section("input:", input)?;
                wtr.write_diff("diff:", diff)?;
                wtr.write_section_unless_empty("stderr:", stderr)
            }
            SimpleOutcomeInner::RuntimeError {
                input,
                expected,
                stdout,
                stderr,
                ..
            } => {
                wtr.write_section("input:", input)?;
                wtr.write_section_unless_empty("expected:", expected.as_ref())?;
                wtr.write_section_unless_empty("stdout:", stdout)?;
                wtr.write_section("stderr:", stderr)
            }
        }
    }
}

#[cfg_attr(test, derive(Debug))]
enum SimpleOutcomeInner {
    Accepted {
        elapsed: Duration,
        input: Text,
        example: Option<Text>,
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
        size: usize,
        lines: Vec<(LineDiffDetialed, LineDiffDetialed)>,
    },
    Lines {
        size: usize,
        lines: Vec<(LineDiff, LineDiff)>,
    },
}

impl TextDiff {
    fn new(left: &Text, right: &Text) -> Self {
        let size = cmp::max(left.size(), right.size());
        if left.lines().len() == right.lines().len() {
            let mut lines = vec![];
            for (left, right) in left.lines().iter().zip_eq(right.lines()) {
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
            TextDiff::SameNumLines { size, lines }
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
            TextDiff::Lines {
                size,
                lines: st.lines,
            }
        }
    }

    fn size(&self) -> usize {
        match self {
            TextDiff::SameNumLines { size, .. } => *size,
            TextDiff::Lines { size, .. } => *size,
        }
    }

    fn print(&self, out: impl TermOut) -> io::Result<()> {
        fn print(
            lines: &[(impl PrintAligned, impl PrintAligned)],
            mut out: impl TermOut,
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
            out.write_str("│")?;
            out.with_reset(|o| o.fg(13)?.bold()?.write_str("expected"))?;
            out.write_spaces(wl - 8)?;
            out.write_str("│")?;
            out.with_reset(|o| o.fg(13)?.bold()?.write_str("stdout"))?;
            out.write_spaces(wr - 6)?;
            out.write_str("│\n")?;
            for (l, r) in lines {
                out.write_str("│")?;
                l.print_aligned(&mut out, wl)?;
                out.write_str("│")?;
                r.print_aligned(&mut out, wr)?;
                out.write_str("│\n")?;
            }
            Ok(())
        }

        match self {
            TextDiff::SameNumLines { lines, .. } => print(lines, out),
            TextDiff::Lines { lines, .. } => print(lines, out),
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
    fn print_aligned<W: TermOut>(&self, mut out: W, min_width: usize) -> io::Result<()> {
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
    fn print_aligned<W: TermOut>(&self, mut out: W, min_width: usize) -> io::Result<()> {
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

    fn print_all(&self, mut out: impl TermOut) -> io::Result<()> {
        for line in self.lines() {
            for word in line.words() {
                word.print_as_common(&mut out)?;
            }
            writeln!(out)?;
        }
        Ok(())
    }
}
