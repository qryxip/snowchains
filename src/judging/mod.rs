mod batch;
mod interactive;
mod text;

use crate::command::JudgingCommand;
use crate::config::{self, Config};
use crate::errors::{JudgeError, JudgeErrorKind, JudgeResult};
use crate::terminal::{HasTermProps, WriteColorExt as _, WriteExt as _};
use crate::testsuite::{TestCase, TestCases};
use crate::util::collections::NonEmptyVec;
use crate::util::io::AsyncBufferedWriter;

use futures::{task, try_ready, Async, Future, Poll};
use serde_derive::Serialize;
use termcolor::WriteColor;
use tokio::io::AsyncWrite;
use tokio::runtime::Runtime;

use std::convert::Infallible;
use std::fmt::{self, Write as _};
use std::io::{self, Write as _};
use std::num::NonZeroUsize;
use std::sync::Arc;
use std::{mem, vec};

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
        processes: Vec<(Title, Process<C, F>)>, // non-empty until `Finished`
    }

    impl<W: AsyncWrite, S, C, F: Future> Progress<W, S, C, F> {
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
                bufwtr: {
                    let mut bufwtr = AsyncBufferedWriter::new(wtr);
                    if color {
                        for (i, (title, _)) in processes.iter().enumerate() {
                            bufwtr.push_ansi_bold();
                            bufwtr.push_str(&title.with_padding);
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

            try_ready!(self.bufwtr.poll_flush_buf());

            if self.processes.iter().all(|(_, p)| p.is_finished()) && !newly_finished {
                let outcomes = mem::replace(&mut self.processes, vec![])
                    .into_iter()
                    .map(|(t, o)| (t, o.finished().unwrap()))
                    .collect();
                return Ok(Async::Ready(outcomes));
            }

            if newly_finished {
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
                        0 => self.bufwtr.push_ansi_cursor_horizontal_absolute(0),
                        n => self.bufwtr.push_ansi_cursor_previous_line(n),
                    }
                    for (i, (title, process)) in self.processes.iter().enumerate() {
                        self.bufwtr.push_ansi_erase_in_line_entire();
                        self.bufwtr.push_ansi_bold();
                        self.bufwtr.push_str(&title.with_padding);
                        self.bufwtr.push_ansi_reset();
                        self.bufwtr.push_char(' ');
                        match process {
                            Process::NotRunning(..) => self.bufwtr.push_str("Waiting..."),
                            Process::Running(_) => self.bufwtr.push_str("Running..."),
                            Process::Finished(outcome) => {
                                self.bufwtr.push_ansi_fg_256(outcome.color());
                                self.bufwtr.write_fmt_to_buf(format_args!("{}", outcome));
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
        E: WriteColor + HasTermProps,
        C: TestCase + Send + 'static,
        F: Future<Error = io::Error> + Send + 'static,
    >(
        mut stderr: E,
        jobs: NonZeroUsize,
        display_limit: Option<usize>,
        cases: NonEmptyVec<C>,
        solver: &Arc<JudgingCommand>,
        judge: fn(&C, &Arc<JudgingCommand>) -> JudgeResult<F>,
    ) -> JudgeResult<JudgeOutcome>
    where
        F::Item: Outcome + Send + 'static,
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
                stderr.with_reset(|w| write!(w.bold().set()?, "\n{}", title.without_padding))?;
                stderr.write_str(" ")?;
                stderr.with_reset(|w| writeln!(w.fg(outcome.color()).set()?, "{}", outcome))?;
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
            Ok(JudgeOutcome {})
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
        .or_else(|| NonZeroUsize::new(num_cpus::get()))
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
        tester_transpilation.run(&stdout, &mut stderr, force_compile)?;
        writeln!(stderr)?;
    }
    for tester_compilation in tester_compilations {
        tester_compilation.run(&stdout, &mut stderr, force_compile)?;
        writeln!(stderr)?;
    }
    if let Some(solver_transpilation) = solver_transpilation {
        solver_transpilation.run(&stdout, &mut stderr, force_compile)?;
        writeln!(stderr)?;
    }
    if let Some(solver_compilation) = solver_compilation {
        solver_compilation.run(&stdout, &mut stderr, force_compile)?;
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

#[derive(Debug)]
pub(crate) struct JudgeParams<'a, O: HasTermProps, E: WriteColor + HasTermProps> {
    pub stdout: O,
    pub stderr: E,
    pub config: &'a Config,
    pub mode: config::Mode,
    pub problem: &'a str,
    pub force_compile: bool,
    pub jobs: Option<NonZeroUsize>,
}

#[derive(Debug, Serialize)]
pub(crate) struct JudgeOutcome {}

pub(self) trait Outcome: fmt::Display {
    fn failure(&self) -> bool;
    fn color(&self) -> u8;
    fn print_details(
        &self,
        display_limit: Option<usize>,
        out: impl WriteColor + HasTermProps,
    ) -> io::Result<()>;
}

pub(self) fn writeln_size(mut out: impl WriteColor, size: usize) -> io::Result<()> {
    let gib = size / 2usize.pow(30);
    let mib = (size / 2usize.pow(20)) & 0x3ff;
    let kib = (size / 2usize.pow(10)) & 0x3ff;
    let b = size & 0x3ff;
    out.with_reset(|out| {
        let out = out.fg(11).bold().set()?;
        match (gib, mib, kib, b) {
            (0, 0, 0, b) => writeln!(out, "{}B", b),
            (0, 0, k, b) => writeln!(out, "{}.{}KiB", k, b / 0x67),
            (0, m, k, _) => writeln!(out, "{}.{}MiB", m, k / 0x67),
            (g, m, _, _) => writeln!(out, "{}.{}GiB", g, m / 0x67),
        }
    })
}
