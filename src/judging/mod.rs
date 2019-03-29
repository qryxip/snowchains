mod batch;
mod interactive;
mod text;

use crate::command::JudgingCommand;
use crate::config::{self, Config};
use crate::errors::{JudgeError, JudgeErrorKind, JudgeResult};
use crate::terminal::{TermOut, WriteAnsi, WriteSpaces as _};
use crate::testsuite::{TestCase, TestCases};
use crate::util::collections::NonEmptyVec;
use crate::util::lang_unstable::Never;

use futures::{task, try_ready, Async, Future, Poll};
use tokio::io::AsyncWrite;
use tokio::runtime::Runtime;

use std::fmt::{self, Write as _};
use std::io::{self, Cursor, Write as _};
use std::num::NonZeroUsize;
use std::sync::Arc;
use std::{cmp, mem, vec};

pub(crate) fn only_transpile<O: TermOut, E: TermOut>(
    stderr: E,
    config: &Config,
    mode: config::Mode,
    problem: &str,
    force: bool,
) -> JudgeResult<bool> {
    match config.solver_transpilation(mode)? {
        None => Ok(false),
        Some(transpilation) => {
            let transpilation = transpilation.expand(problem)?;
            transpilation.run::<O, _>(stderr, force)?;
            Ok(true)
        }
    }
}

/// Executes the tests.
///
/// # Errors
///
/// Returns `Err` if compilation or execution command fails, or any test fails.
pub(crate) fn judge<O: TermOut, E: TermOut>(params: JudgeParams<E>) -> JudgeResult<()> {
    struct Progress<W, S, C, F: Future> {
        jobs: NonZeroUsize,
        color: bool,
        wtr: W,
        ctrlc: S,
        content: Cursor<Vec<u8>>,
        needs_flush: bool,
        processes: Vec<(Title, Process<C, F>)>, // non-empty until `Finished`
    }

    impl<W, S, C, F: Future> Progress<W, S, C, F> {
        fn new(
            color: bool,
            wtr: W,
            ctrlc: S,
            processes: NonEmptyVec<(Title, Process<C, F>)>,
            jobs: NonZeroUsize,
        ) -> Self {
            Self {
                jobs,
                color,
                wtr,
                ctrlc,
                needs_flush: false,
                content: Cursor::new({
                    let mut inner = "".to_owned();
                    if color {
                        for (i, (title, _)) in processes.iter().enumerate() {
                            write!(inner, "\x1b[1m{}\x1b[0m Running...", title.with_padding)
                                .unwrap();
                            if i + 1 < processes.len().get() {
                                inner.push('\n');
                            }
                        }
                    } else {
                        inner = format!("0/{} test finished (0 failure)\n", processes.len());
                    }
                    inner.into()
                }),
                processes: processes.into(),
            }
        }
    }

    impl<
            W: AsyncWrite,
            S: Future<Item = Never, Error = JudgeError>,
            C,
            F: Future<Error = io::Error>,
        > Future for Progress<W, S, C, F>
    where
        F::Item: Outcome,
    {
        type Item = Vec<(Title, F::Item)>;
        type Error = JudgeError;

        fn poll(&mut self) -> Poll<Vec<(Title, F::Item)>, JudgeError> {
            self.ctrlc.poll()?;

            let mut needs_notify = true;
            let mut newly_finished = false;

            let mut jobs = self.jobs.get();
            for (_, process) in &mut self.processes {
                let mut next = None;
                if jobs > 0 {
                    match process {
                        Process::NotRunning(f, case, cmd) => {
                            next = Some(Process::Running(f(&case, &cmd)?));
                            jobs -= 1;
                        }
                        Process::Running(fut) => {
                            if let Async::Ready(outcome) = fut.poll()? {
                                next = Some(Process::Finished(outcome));
                                newly_finished = true;
                            } else {
                                jobs -= 1;
                                needs_notify = false;
                            }
                        }
                        Process::Finished(_) => {}
                    }
                }
                if let Some(next) = next {
                    *process = next;
                }
            }

            if self.needs_flush {
                try_ready!(self.wtr.poll_flush());
                self.needs_flush = false;
                if self.processes.iter().all(|(_, p)| p.is_finished()) {
                    let outcomes = mem::replace(&mut self.processes, vec![])
                        .into_iter()
                        .map(|(t, o)| (t, o.finished().unwrap()))
                        .collect();
                    return Ok(Async::Ready(outcomes));
                }
            }
            if self.content.position() < self.content.get_ref().len() as u64 {
                let n = try_ready!(self
                    .wtr
                    .poll_write(&self.content.get_ref()[self.content.position() as usize..]));
                self.content
                    .set_position(self.content.position() + n as u64);
                if self.content.position() >= self.content.get_ref().len() as u64 {
                    self.content.set_position(0);
                    self.content.get_mut().clear();
                    self.needs_flush = true;
                }
            }

            if newly_finished && self.content.get_ref().is_empty() {
                let out = self.content.get_mut();
                let num_cases = self.processes.len();
                let (mut num_finished, mut num_failures) = (0, 0);
                for (_, process) in &self.processes {
                    if let Process::Finished(outcome) = process {
                        num_finished += 1;
                        if outcome.failure() {
                            num_failures += 1;
                        }
                    }
                }
                if self.color {
                    match num_cases - 1 {
                        0 => out.extend_from_slice(b"\x1b[0G"),
                        n => write!(out, "\x1b[{}F", n).unwrap(),
                    }
                    for (i, (title, process)) in self.processes.iter().enumerate() {
                        write!(out, "\x1b[2K\x1b[1m{}\x1b[0m ", title.with_padding).unwrap();
                        match process {
                            Process::NotRunning(..) => out.extend_from_slice(b"Waiting..."),
                            Process::Running(_) => out.extend_from_slice(b"Running..."),
                            Process::Finished(outcome) => {
                                out.extend_from_slice(b"\x1b[38;5;");
                                write!(out, "{}m{}\x1b[0m", outcome.color(), outcome).unwrap();
                            }
                        }
                        if i + 1 < num_cases {
                            out.extend_from_slice(b"\x1b[1E");
                        } else if num_finished == num_cases {
                            out.push(b'\n');
                        }
                    }
                } else {
                    writeln!(
                        out,
                        "{}/{} {} finished ({})",
                        num_finished,
                        num_cases,
                        if num_finished > 1 { "tests" } else { "test" },
                        plural!(num_failures, "failure", "failures"),
                    )
                    .unwrap();
                }
            }
            if needs_notify {
                task::current().notify();
            }
            Ok(Async::NotReady)
        }
    }

    enum Process<C, F: Future> {
        NotRunning(
            fn(&C, &Arc<JudgingCommand>) -> JudgeResult<F>,
            C,
            Arc<JudgingCommand>,
        ),
        Running(F),
        Finished(F::Item),
    }

    impl<C, F: Future> Process<C, F>
    where
        F::Item: Outcome,
    {
        fn is_finished(&self) -> bool {
            match self {
                Process::Finished(_) => true,
                _ => false,
            }
        }

        fn finished(self) -> Option<F::Item> {
            match self {
                Process::Finished(item) => Some(item),
                _ => None,
            }
        }
    }

    struct Title {
        with_padding: String,
        without_padding: String,
    }

    impl Title {
        fn new(
            index: NonZeroUsize,
            index_max: NonZeroUsize,
            name: &str,
            name_max_width: usize,
        ) -> Self {
            let (mut with_padding, mut without_padding) = ("".to_owned(), "".to_owned());
            for _ in index.to_string().len()..index_max.to_string().len() {
                with_padding.push(' ');
            }
            for s in &mut [&mut with_padding, &mut without_padding] {
                write!(s, "{}/{} ({})", index, index_max, name).unwrap();
            }
            (name.len()..name_max_width).for_each(|_| with_padding.push(' '));
            Self {
                with_padding,
                without_padding,
            }
        }
    }

    fn judge_all<
        E: TermOut,
        C: TestCase + Send + 'static,
        F: Future<Error = io::Error> + Send + 'static,
    >(
        mut stderr: E,
        jobs: NonZeroUsize,
        display_limit: Option<usize>,
        cases: NonEmptyVec<C>,
        solver: &Arc<JudgingCommand>,
        judge: fn(&C, &Arc<JudgingCommand>) -> JudgeResult<F>,
    ) -> JudgeResult<()>
    where
        F::Item: Outcome + Send + 'static,
    {
        let num_cases = cases.len();
        let names = cases.ref_map(|c| c.name());
        let name_max_width = names.iter().map(|s| stderr.str_width(s)).max().unwrap_or(0);

        let mut runtime = Runtime::new()?;
        let progress = Progress::new(
            stderr.supports_color(),
            E::async_wtr(),
            crate::signal::ctrl_c(),
            {
                names.zip_eq(cases).enumerate_map(|idx, (name, case)| {
                    let title = Title::new(
                        NonZeroUsize::new(idx + 1).unwrap(),
                        num_cases,
                        name.as_ref(),
                        name_max_width,
                    );
                    (title, Process::NotRunning(judge, case, solver.clone()))
                })
            },
            jobs,
        );
        let outcomes = runtime.block_on(progress)?;
        let _ = runtime.shutdown_now().wait();

        let num_failures = outcomes.iter().filter(|(_, o)| o.failure()).count();
        if let Some(num_failures) = NonZeroUsize::new(num_failures) {
            for (title, outcome) in &outcomes {
                stderr.with_reset(|w| write!(w.bold()?, "\n{}", title.without_padding))?;
                stderr.write_str(" ")?;
                stderr.with_reset(|w| writeln!(w.fg(outcome.color())?, "{}", outcome))?;
                outcome.print_details(display_limit, &mut stderr)?;
            }
            stderr.flush()?;
            Err(JudgeErrorKind::TestFailed(num_failures, num_cases).into())
        } else {
            writeln!(
                stderr,
                "All of the {} passed.",
                plural!(num_cases.get(), "test", "tests")
            )?;
            stderr.flush()?;
            Ok(())
        }
    }

    let JudgeParams {
        mut stderr,
        config,
        mode,
        problem,
        force_compile,
        jobs,
    } = params;

    let (cases, paths_formatted) = config.testcase_loader(mode)?.load_merging(problem)?;
    let jobs = jobs
        .or_else(|| config.judge_jobs())
        .unwrap_or_else(|| NonZeroUsize::new(1).unwrap());
    let display_limit = config.judge_display_limit();
    let tester_transpilations = cases.interactive_tester_transpilations();
    let tester_compilations = cases.interactive_tester_compilations();
    let solver = config.solver(mode)?.expand(&problem)?;
    let solver_transpilation = match config.solver_transpilation(mode)? {
        Some(transpilation) => Some(transpilation.expand(&problem)?),
        None => None,
    };
    let solver_compilation = match config.solver_compilation(mode)? {
        Some(compilation) => Some(compilation.expand(&problem)?),
        None => None,
    };

    for tester_transpilation in tester_transpilations {
        tester_transpilation.run::<O, _>(&mut stderr, force_compile)?;
        writeln!(stderr)?;
    }
    for tester_compilation in tester_compilations {
        tester_compilation.run::<O, _>(&mut stderr, force_compile)?;
        writeln!(stderr)?;
    }
    if let Some(solver_transpilation) = solver_transpilation {
        solver_transpilation.run::<O, _>(&mut stderr, force_compile)?;
        writeln!(stderr)?;
    }
    if let Some(solver_compilation) = solver_compilation {
        solver_compilation.run::<O, _>(&mut stderr, force_compile)?;
        writeln!(stderr)?;
    }

    solver.write_info(&mut stderr, &paths_formatted)?;
    stderr.flush()?;

    let solver = Arc::new(solver);
    match cases {
        TestCases::Batch(cases) => {
            let f = batch::judge;
            judge_all(stderr, jobs, display_limit, cases, &solver, f)
        }
        TestCases::Interactive(cases) => {
            let f = interactive::judge;
            judge_all(stderr, jobs, display_limit, cases, &solver, f)
        }
    }
}

pub(crate) struct JudgeParams<'a, E: TermOut> {
    pub stderr: E,
    pub config: &'a Config,
    pub mode: config::Mode,
    pub problem: &'a str,
    pub force_compile: bool,
    pub jobs: Option<NonZeroUsize>,
}

pub(self) trait Outcome: fmt::Display {
    fn failure(&self) -> bool;
    fn color(&self) -> u8;
    fn print_details(&self, display_limit: Option<usize>, out: impl TermOut) -> io::Result<()>;

    fn print_title(
        &self,
        mut out: impl TermOut,
        i: impl DisplayableNum,
        n: impl DisplayableNum,
        name: &str,
        name_width: Option<usize>,
    ) -> io::Result<()> {
        if name_width.is_some() {
            out.write_spaces(n.num_digits() - i.num_digits())?;
        }
        out.with_reset(|o| write!(o.bold()?, "{}/{} ({})", i, n, name))?;
        let l = out.str_width(name);
        let name_width = name_width.unwrap_or(0);
        out.write_spaces(cmp::max(name_width, l) - l + 1)?;
        out.with_reset(|o| writeln!(o.fg(self.color())?, "{}", self))
    }
}

trait DisplayableNum: fmt::Display + Copy {
    fn num_digits(self) -> usize;
}

impl DisplayableNum for usize {
    fn num_digits(mut self) -> usize {
        let mut r = 1;
        while self > 9 {
            self /= 10;
            r += 1;
        }
        r
    }
}

impl DisplayableNum for NonZeroUsize {
    fn num_digits(self) -> usize {
        self.get().num_digits()
    }
}

pub(self) fn writeln_size(mut out: impl WriteAnsi, size: usize) -> io::Result<()> {
    let gib = size / 2usize.pow(30);
    let mib = (size / 2usize.pow(20)) & 0x3ff;
    let kib = (size / 2usize.pow(10)) & 0x3ff;
    let b = size & 0x3ff;
    out.with_reset(|out| {
        out.fg(11)?.bold()?;
        match (gib, mib, kib, b) {
            (0, 0, 0, b) => writeln!(out, "{}B", b),
            (0, 0, k, b) => writeln!(out, "{}.{}KiB", k, b / 0x67),
            (0, m, k, _) => writeln!(out, "{}.{}MiB", m, k / 0x67),
            (g, m, _, _) => writeln!(out, "{}.{}GiB", g, m / 0x67),
        }
    })
}
