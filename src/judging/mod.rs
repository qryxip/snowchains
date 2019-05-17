mod batch;
mod interactive;
mod text;

use crate::command::JudgingCommand;
use crate::config::{self, Config};
use crate::errors::{JudgeError, JudgeResult};
use crate::judging::batch::BatchVerdict;
use crate::judging::interactive::InteractiveVerdict;
use crate::outcome::Outcome;
use crate::terminal::HasTermProps;
use crate::testsuite::{BatchCase, InteractiveCase, TestCase, TestCases};
use crate::util::collections::NonEmptyVec;
use crate::util::io::AsyncBufferedWriter;

use futures::{task, try_ready, Async, Future, Poll};
use serde::{Serialize, Serializer};
use serde_derive::Serialize;
use termcolor::{ColorSpec, WriteColor};
use tokio::io::AsyncWrite;
use tokio::runtime::Runtime;

use std::convert::Infallible;
use std::fmt::{self, Write as _};
use std::num::NonZeroUsize;
use std::sync::Arc;
use std::{io, mem, str, vec};

pub(crate) fn only_transpile(
    stdout: impl HasTermProps,
    stderr: impl WriteColor + HasTermProps,
    config: &Config,
    mode: config::Mode,
    problem: &str,
    force: bool,
) -> JudgeResult<bool> {
    match config.solver_transpilation(mode)? {
        None => Ok(false),
        Some(transpilation) => {
            let transpilation = transpilation.expand(problem)?;
            transpilation.run(stdout, stderr, force)?;
            Ok(true)
        }
    }
}

/// Executes the tests.
///
/// # Errors
///
/// Returns `Err` if compilation or execution command fails, or any test fails.
pub(crate) fn judge(
    params: JudgeParams<impl HasTermProps, impl WriteColor + HasTermProps>,
) -> JudgeResult<JudgeOutcome> {
    struct Progress<W: AsyncWrite, S, C, F: Future> {
        jobs: NonZeroUsize,
        color: bool,
        bufwtr: AsyncBufferedWriter<W>,
        ctrlc: S,
        processes: Vec<(String, Process<C, F>)>, // non-empty until `Finished`
    }

    impl<W: AsyncWrite, S, C, F: Future> Progress<W, S, C, F> {
        fn new(
            color: bool,
            wtr: W,
            ctrlc: S,
            processes: NonEmptyVec<(String, Process<C, F>)>,
            jobs: NonZeroUsize,
        ) -> Self {
            Self {
                jobs,
                color,
                bufwtr: {
                    let mut bufwtr = AsyncBufferedWriter::new(wtr);
                    if color {
                        for (i, (title, _)) in processes.iter().enumerate() {
                            bufwtr.push_ansi_bold();
                            bufwtr.push_str(title);
                            bufwtr.push_ansi_reset();
                            bufwtr.push_str(" Running...");
                            if i + 1 < processes.len().get() {
                                bufwtr.push_char('\n');
                            }
                        }
                    } else {
                        bufwtr.write_fmt_to_buf(format_args!(
                            "0/{} test finished (0 failure)\n",
                            processes.len(),
                        ));
                    }
                    bufwtr
                },
                ctrlc,
                processes: processes.into(),
            }
        }
    }

    impl<
            W: AsyncWrite,
            S: Future<Item = Infallible, Error = JudgeError>,
            C,
            F: Future<Error = io::Error>,
        > Future for Progress<W, S, C, F>
    where
        F::Item: Verdict,
    {
        type Item = Vec<F::Item>;
        type Error = JudgeError;

        fn poll(&mut self) -> Poll<Vec<F::Item>, JudgeError> {
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
                            if let Async::Ready(verdict) = fut.poll()? {
                                next = Some(Process::Finished(verdict));
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

            try_ready!(self.bufwtr.poll_flush_buf());

            if self.processes.iter().all(|(_, p)| p.is_finished()) && !newly_finished {
                let verdicts = mem::replace(&mut self.processes, vec![])
                    .into_iter()
                    .map(|(_, v)| v.finished().unwrap())
                    .collect();
                return Ok(Async::Ready(verdicts));
            }

            if newly_finished {
                let num_cases = self.processes.len();
                let (mut num_finished, mut num_failures) = (0, 0);
                for (_, process) in &self.processes {
                    if let Process::Finished(verdict) = process {
                        num_finished += 1;
                        if !verdict.is_success() {
                            num_failures += 1;
                        }
                    }
                }
                if self.color {
                    match num_cases - 1 {
                        0 => self.bufwtr.push_ansi_cursor_horizontal_absolute(0),
                        n => self.bufwtr.push_ansi_cursor_previous_line(n),
                    }
                    for (i, (title, process)) in self.processes.iter().enumerate() {
                        self.bufwtr.push_ansi_erase_in_line_entire();
                        self.bufwtr.push_ansi_bold();
                        self.bufwtr.push_str(title);
                        self.bufwtr.push_ansi_reset();
                        self.bufwtr.push_char(' ');
                        match process {
                            Process::NotRunning(..) => self.bufwtr.push_str("Waiting..."),
                            Process::Running(_) => self.bufwtr.push_str("Running..."),
                            Process::Finished(verdict) => {
                                self.bufwtr.push_ansi_color(&verdict.color_spec());
                                self.bufwtr.write_fmt_to_buf(format_args!("{}", verdict));
                                self.bufwtr.push_ansi_reset();
                            }
                        }
                        if i + 1 < num_cases {
                            self.bufwtr.push_ansi_cursor_next_line(1);
                        } else if num_finished == num_cases {
                            self.bufwtr.push_char('\n');
                        }
                    }
                } else {
                    self.bufwtr.write_fmt_to_buf(format_args!(
                        "{}/{} {} finished ({})",
                        num_finished,
                        num_cases,
                        if num_finished > 1 { "tests" } else { "test" },
                        plural!(num_failures, "failure", "failures"),
                    ));
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
        F::Item: Verdict,
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

    fn judge_all<
        E: WriteColor + HasTermProps,
        C: TestCase + Send + 'static,
        F: Future<Error = io::Error> + Send + 'static,
    >(
        stderr: &E,
        jobs: NonZeroUsize,
        display_limit: Option<usize>,
        cases: NonEmptyVec<C>,
        solver: &Arc<JudgingCommand>,
        judge: fn(&C, &Arc<JudgingCommand>) -> JudgeResult<F>,
    ) -> JudgeResult<JudgeOutcomeRepr<C, F::Item>>
    where
        F::Item: Verdict,
    {
        let num_cases = cases.len();
        let names = cases.ref_map(TestCase::name);
        let name_max_width = names.iter().map(|s| stderr.str_width(s)).max().unwrap_or(0);

        let mut runtime = Runtime::new()?;
        let progress = Progress::new(
            stderr.supports_color() && !stderr.is_synchronous(),
            stderr.ansi_async_wtr(),
            crate::signal::ctrl_c(),
            {
                names
                    .zip_eq(cases.clone())
                    .enumerate_map(|idx, (name, case)| {
                        let idx = idx + 1;
                        let str_width = stderr.str_width_fn();
                        let n = num_cases.to_string().len() - idx.to_string().len();
                        let mut title = " ".repeat(n);
                        write!(title, "{}/{} ({})", idx, num_cases, name).unwrap();
                        (str_width(&*name)..name_max_width).for_each(|_| title.push(' '));
                        (title, Process::NotRunning(judge, case, solver.clone()))
                    })
            },
            jobs,
        );
        let verdicts = runtime.block_on(progress)?;
        let _ = runtime.shutdown_now().wait();

        Ok(JudgeOutcomeRepr {
            display_limit,
            verdicts: cases
                .zip_eq(verdicts)
                .map(|(test_case, verdict)| JudgeOutcomeReprVerdict {
                    is_success: verdict.is_success(),
                    test_case,
                    verdict,
                }),
        })
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
        .or_else(|| NonZeroUsize::new(num_cpus::get()))
        .unwrap_or_else(|| NonZeroUsize::new(1).unwrap());
    let display_limit = config.judge_display_limit();

    for tester_transpilation in cases.interactive_tester_transpilations() {
        tester_transpilation.run(&stdout, &mut stderr, force_compile)?;
        writeln!(stderr)?;
    }
    for tester_compilation in cases.interactive_tester_compilations() {
        tester_compilation.run(&stdout, &mut stderr, force_compile)?;
        writeln!(stderr)?;
    }
    if let Some(solver_transpilation) = config.solver_transpilation(mode)? {
        solver_transpilation
            .expand(&problem)?
            .run(&stdout, &mut stderr, force_compile)?;
        writeln!(stderr)?;
    }
    if let Some(solver_compilation) = config.solver_compilation(mode)? {
        solver_compilation
            .expand(&problem)?
            .run(&stdout, &mut stderr, force_compile)?;
        writeln!(stderr)?;
    }

    let solver = config.solver(mode)?.expand(&problem)?;

    solver.write_info(&mut stderr, &paths_formatted)?;
    stderr.flush()?;

    let (stderr, solver) = (&stderr, Arc::new(solver));
    match cases {
        TestCases::Batch(cases) => {
            let f = batch::judge;
            judge_all(stderr, jobs, display_limit, cases, &solver, f).map(JudgeOutcome::Batch)
        }
        TestCases::Interactive(cases) => {
            let f = interactive::judge;
            judge_all(stderr, jobs, display_limit, cases, &solver, f).map(JudgeOutcome::Interactive)
        }
    }
}

#[derive(Debug)]
pub(crate) struct JudgeParams<'a, O: HasTermProps, E: WriteColor + HasTermProps> {
    pub(crate) stdout: O,
    pub(crate) stderr: E,
    pub(crate) config: &'a Config,
    pub(crate) mode: config::Mode,
    pub(crate) problem: &'a str,
    pub(crate) force_compile: bool,
    pub(crate) jobs: Option<NonZeroUsize>,
}

#[derive(Debug)]
pub(crate) enum JudgeOutcome {
    Batch(JudgeOutcomeRepr<BatchCase, BatchVerdict>),
    Interactive(JudgeOutcomeRepr<InteractiveCase, InteractiveVerdict>),
}

impl Outcome for JudgeOutcome {
    fn is_success(&self) -> bool {
        match self {
            JudgeOutcome::Batch(r) => r.verdicts.iter().all(|v| v.is_success),
            JudgeOutcome::Interactive(r) => r.verdicts.iter().all(|v| v.is_success),
        }
    }

    fn print_pretty(
        &self,
        verbose: bool,
        stdout: impl WriteColor + HasTermProps,
    ) -> io::Result<()> {
        fn print_verdicts(
            repr: &JudgeOutcomeRepr<impl TestCase, impl Verdict>,
            verbose: bool,
            mut stdout: impl WriteColor + HasTermProps,
        ) -> io::Result<()> {
            let JudgeOutcomeRepr {
                display_limit,
                verdicts,
            } = repr;

            let num_failures = verdicts.iter().filter(|v| !v.is_success).count();

            if verbose || num_failures > 0 {
                for (i, verdict) in verdicts.iter().enumerate() {
                    let (test_case, verdict) = (&verdict.test_case, &verdict.verdict);
                    if i > 0 {
                        writeln!(stdout)?;
                    }
                    stdout.set_color(color!(bold))?;
                    let (i, len, name) = (i + 1, verdicts.len(), test_case.name());
                    write!(stdout, "{}/{} ({}) ", i, len, name)?;
                    stdout.set_color(&verdict.color_spec())?;
                    writeln!(stdout, "{}", verdict)?;
                    verdict.print_details(*display_limit, &mut stdout)?;
                    stdout.reset()?;
                }
                writeln!(stdout)?;
            }

            if num_failures == 0 {
                stdout.set_color(color!(fg(Green), intense))?;
                write!(
                    stdout,
                    "All of the {} passed.",
                    plural!(verdicts.len().get(), "test", "tests"),
                )?;
            } else {
                stdout.set_color(color!(fg(Red), intense))?;
                write!(
                    stdout,
                    "{}/{} test{} failed.",
                    num_failures,
                    verdicts.len(),
                    if num_failures > 1 { "s" } else { "" },
                )?;
            }
            stdout.reset()?;
            writeln!(stdout)?;
            stdout.flush()
        }

        match self {
            JudgeOutcome::Batch(r) => print_verdicts(&r, verbose, stdout),
            JudgeOutcome::Interactive(r) => print_verdicts(&r, verbose, stdout),
        }
    }
}

impl Serialize for JudgeOutcome {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        match self {
            JudgeOutcome::Batch(r) => r.serialize(serializer),
            JudgeOutcome::Interactive(r) => r.serialize(serializer),
        }
    }
}

#[derive(Debug, Serialize)]
pub(crate) struct JudgeOutcomeRepr<C: TestCase, V: Verdict> {
    display_limit: Option<usize>,
    verdicts: NonEmptyVec<JudgeOutcomeReprVerdict<C, V>>,
}

#[derive(Debug, Serialize)]
struct JudgeOutcomeReprVerdict<C: TestCase, V: Verdict> {
    is_success: bool,
    test_case: C,
    verdict: V,
}

pub(crate) trait Verdict: fmt::Display + fmt::Debug + Serialize + Send + 'static {
    fn is_success(&self) -> bool;
    fn color_spec(&self) -> ColorSpec;
    fn print_details(
        &self,
        display_limit: Option<usize>,
        wtr: impl WriteColor + HasTermProps,
    ) -> io::Result<()>;
}
