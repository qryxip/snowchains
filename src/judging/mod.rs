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

use futures::{try_ready, Async, Future, Poll};
use itertools::Itertools as _;
use tokio::io::AsyncWrite;
use tokio::runtime::Runtime;

use std::io::{self, Cursor, Write as _};
use std::num::NonZeroUsize;
use std::sync::Arc;
use std::{cmp, fmt, mem, vec};

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
pub(crate) fn judge<O: TermOut, E: TermOut>(params: JudgeParams<O, E>) -> JudgeResult<()> {
    struct Progress<W, S, C, F: Future> {
        jobs: NonZeroUsize,
        color: bool,
        wtr: W,
        ctrlc: S,
        content: Cursor<Vec<u8>>,
        needs_flush: bool,
        processes: Vec<(String, Process<C, F>)>,
    }

    impl<W, S, C, F: Future> Progress<W, S, C, F> {
        fn new(
            color: bool,
            wtr: W,
            ctrlc: S,
            processes: Vec<(String, Process<C, F>)>,
            jobs: NonZeroUsize,
        ) -> Self {
            Self {
                jobs,
                color,
                wtr,
                ctrlc,
                needs_flush: false,
                content: Cursor::new({
                    let mut inner = format!("0/{} test finished (0 failure)", processes.len());
                    if !color {
                        inner += "\n";
                    }
                    inner.into()
                }),
                processes,
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
        type Item = Vec<(String, F::Item)>;
        type Error = JudgeError;

        fn poll(&mut self) -> Poll<Vec<(String, F::Item)>, JudgeError> {
            self.ctrlc.poll()?;
            if self.needs_flush {
                try_ready!(self.wtr.poll_flush());
                self.needs_flush = false;
                if self.processes.iter().all(|(_, p)| p.is_finished()) {
                    let outcomes = mem::replace(&mut self.processes, vec![])
                        .into_iter()
                        .map(|(s, o)| (s, o.finished().unwrap()))
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
            let num_cases = self.processes.len();
            let mut num_finished = 0;
            let mut num_failures = 0;
            let mut jobs = self.jobs.get();
            let mut newly_finished = false;
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
                            }
                        }
                        Process::Finished(_) => {}
                    }
                }
                if let Some(next) = next {
                    *process = next;
                }
                if let Process::Finished(outcome) = process {
                    num_finished += 1;
                    if outcome.failure() {
                        num_failures += 1;
                    }
                }
            }
            if newly_finished {
                if self.color {
                    self.content
                        .get_mut()
                        .extend_from_slice(match num_failures {
                            0 => b"\x1b[0G\x1b[2K\x1b[38;5;10m",
                            _ => b"\x1b[0G\x1b[2K\x1b[38;5;9m",
                        });
                }
                write!(
                    self.content.get_mut(),
                    "{}/{} {} finished ({})",
                    num_finished,
                    num_cases,
                    if num_finished > 1 { "tests" } else { "test" },
                    plural!(num_failures, "failure", "failures"),
                )
                .unwrap();
                self.content.get_mut().extend_from_slice(
                    if self.color && num_cases == num_finished {
                        b"\x1b[0m\n"
                    } else if self.color {
                        b"\x1b[0m"
                    } else {
                        b"\n"
                    },
                );
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

    impl<C, F: Future> Process<C, F> {
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

    fn judge_all<
        O: TermOut,
        E: TermOut,
        C: TestCase + Send + 'static,
        F: Future<Error = io::Error> + Send + 'static,
    >(
        mut stdout: O,
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
            names
                .into_iter()
                .zip_eq(cases)
                .map(|(name, case)| {
                    (
                        String::clone(&name),
                        Process::NotRunning(judge, case, solver.clone()),
                    )
                })
                .collect(),
            jobs,
        );
        let outcomes = runtime.block_on(progress)?;
        let _ = runtime.shutdown_now().wait();

        let num_failures = outcomes.iter().filter(|(_, o)| o.failure()).count();
        if let Some(num_failures) = NonZeroUsize::new(num_failures) {
            for (i, (name, outcome)) in outcomes.into_iter().enumerate() {
                writeln!(stdout)?;
                outcome.print_title(&mut stdout, i + 1, num_cases, &name, None)?;
                outcome.print_details(display_limit, &mut stdout)?;
            }
            stdout.flush()?;
            Err(JudgeErrorKind::TestFailed(num_failures, num_cases).into())
        } else {
            for (i, (name, outcome)) in outcomes.into_iter().enumerate() {
                outcome.print_title(&mut stdout, i + 1, num_cases, &name, Some(name_max_width))?;
            }
            stdout.flush()?;
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
        stdout,
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
        TestCases::Batch(cases) => judge_all(
            stdout,
            stderr,
            jobs,
            display_limit,
            cases,
            &solver,
            batch::judge,
        ),
        TestCases::Interactive(cases) => judge_all(
            stdout,
            stderr,
            jobs,
            display_limit,
            cases,
            &solver,
            interactive::judge,
        ),
    }
}

pub(crate) struct JudgeParams<'a, O: TermOut, E: TermOut> {
    pub stdout: O,
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
