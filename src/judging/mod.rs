mod interactive;
mod simple;
mod text;

use command::{CompilationCommand, JudgingCommand};
use config::Config;
use console::{ConsoleWrite, Palette};
use errors::{JudgeError, JudgeResult};
use testsuite::{TestCase, TestCases};

use futures::{self, Future, Sink as _Sink, Stream as _Stream};
use tokio::runtime::Runtime;

use std::collections::HashSet;
use std::fmt;
use std::io::{self, Write};
use std::sync::Arc;
use std::time::Duration;

/// Executes the tests.
///
/// # Errors
///
/// Returns `Err` if compilation or execution command fails, or any test fails.
pub(crate) fn judge(prop: JudgeProp<impl ConsoleWrite, impl ConsoleWrite>) -> JudgeResult<()> {
    fn judge_all<
        C: TestCase,
        O: Outcome + Send + 'static,
        F: Future<Item = O, Error = JudgeError> + Send + 'static,
    >(
        (mut stdout, mut stderr): (impl ConsoleWrite, impl ConsoleWrite),
        cases: Vec<C>,
        solver: &Arc<JudgingCommand>,
        judge: fn(&C, &Arc<JudgingCommand>) -> F,
    ) -> JudgeResult<()> {
        let num_cases = cases.len();
        writeln!(stdout, "Running {}...", plural!(num_cases, "test", "tests"))?;
        stdout.flush()?;

        let names = cases.iter().map(|c| c.name()).collect::<Vec<_>>();
        let name_max_width = names.iter().map(|s| stdout.width(s)).max().unwrap_or(0);

        let (tx, rx) = futures::sync::mpsc::channel(num_cases);
        let mut runtime = Runtime::new()?;
        for (i, (case, name)) in cases.into_iter().zip(names).enumerate() {
            let tx = tx.clone();
            runtime.spawn(judge(&case, solver).then(move |r| {
                tx.send((i, name, r)).wait().unwrap();
                Ok(())
            }));
        }
        write!(stderr, "0/{} test finished (0 failure)", num_cases)?;
        if !stderr.supports_color() {
            writeln!(stderr)?;
        }
        stderr.flush()?;
        let (mut num_finished, mut num_failures) = (0, 0);
        let mut outcomes = rx
            .take(num_cases as u64)
            .then::<_, JudgeResult<_>>(|r| {
                let (i, name, r) = r.unwrap();
                let outcome = r?;
                num_finished += 1;
                if outcome.failure() {
                    num_failures += 1;
                }
                if stderr.supports_color() {
                    stderr.inner_writer().write_all(b"\x1b[0G\x1b[2K")?;
                }
                let palette = match num_failures {
                    0 => Palette::Success,
                    _ => Palette::Fatal,
                };
                write!(
                    stderr.plain(palette),
                    "{}/{} {} finished ({})",
                    num_finished,
                    num_cases,
                    if num_finished > 1 { "tests" } else { "test" },
                    plural!(num_failures, "failure", "failures"),
                )?;
                if !stderr.supports_color() {
                    writeln!(stderr)?;
                }
                stderr.flush()?;
                Ok((i, name, outcome))
            }).collect()
            .wait()?;
        if stderr.supports_color() {
            writeln!(stderr)?;
            stderr.flush()?;
        }
        outcomes.sort_by_key(|(i, _, _)| *i);
        let _ = runtime.shutdown_on_idle().wait();

        if num_failures == 0 {
            for (i, name, outcome) in outcomes {
                outcome.print_title(&mut stdout, i, num_cases, &name, name_max_width)?;
            }
            writeln!(
                stdout,
                "All of the {} passed.",
                plural!(num_cases, "test", "tests")
            )?;
            stdout.flush()?;
            Ok(())
        } else {
            for (i, name, outcome) in outcomes {
                writeln!(stdout)?;
                outcome.print_title(&mut stdout, i, num_cases, &name, name_max_width)?;
                outcome.print_details(&mut stdout)?;
            }
            stdout.flush()?;
            Err(JudgeError::TestFailed(num_failures, num_cases))
        }
    }

    let JudgeProp {
        mut stdout,
        mut stderr,
        cases,
        case_paths_formatted,
        solver,
        solver_compilation,
        tester_compilations,
        force_compile,
    } = prop;
    if let Some(solver_compilation) = solver_compilation {
        solver_compilation.run((&mut stdout, &mut stderr), force_compile)?;
        writeln!(stdout)?;
        stdout.flush()?;
    }
    for tester_compilation in tester_compilations {
        tester_compilation.run((&mut stdout, &mut stderr), force_compile)?;
        writeln!(stdout)?;
        stdout.flush()?;
    }
    solver.write_info(&mut stdout, &case_paths_formatted)?;
    let (out, solver) = ((stdout, stderr), Arc::new(solver));
    match cases {
        TestCases::Simple(cases) => judge_all(out, cases, &solver, simple::judge),
        TestCases::Interactive(cases) => judge_all(out, cases, &solver, interactive::judge),
    }
}

pub(crate) struct JudgeProp<O: ConsoleWrite, E: ConsoleWrite> {
    stdout: O,
    stderr: E,
    cases: TestCases,
    case_paths_formatted: String,
    solver: JudgingCommand,
    solver_compilation: Option<CompilationCommand>,
    tester_compilations: HashSet<Arc<CompilationCommand>>,
    force_compile: bool,
}

impl<O: ConsoleWrite, E: ConsoleWrite> JudgeProp<O, E> {
    pub fn new(
        (stdout, stderr): (O, E),
        config: &Config,
        problem: &str,
        language: Option<&str>,
        force_compile: bool,
    ) -> ::Result<Self> {
        let (cases, paths_formatted) = config.testcase_loader().load_merging(problem)?;
        let solver = config.solver(language)?.expand(&problem)?;
        let solver_compilation = match config.solver_compilation(language)? {
            Some(compilation) => Some(compilation.expand(&problem)?),
            None => None,
        };

        Ok(Self {
            stdout,
            stderr,
            tester_compilations: cases.interactive_tester_compilations(),
            cases,
            case_paths_formatted: paths_formatted,
            solver,
            solver_compilation,
            force_compile,
        })
    }
}

pub(self) trait Outcome: fmt::Display {
    fn failure(&self) -> bool;
    fn palette(&self) -> Palette;
    fn print_details(&self, printer: impl ConsoleWrite) -> io::Result<()>;

    fn print_title(
        &self,
        mut out: impl ConsoleWrite,
        i: usize,
        n: usize,
        name: &str,
        name_width: usize,
    ) -> io::Result<()> {
        (0..format!("{}", n).len() - format!("{}", i + 1).len())
            .try_for_each(|_| write!(out, " "))?;
        write!(out.bold(None), "{}/{} ({})", i + 1, n, name)?;
        (0..=name_width - out.width(name)).try_for_each(|_| write!(out, " "))?;
        writeln!(out.bold(self.palette()), "{}", self)
    }
}

pub(self) trait MillisRoundedUp {
    /// As milliseconds rounded up.
    fn millis_rounded_up(self) -> u64;
}

impl MillisRoundedUp for Duration {
    fn millis_rounded_up(self) -> u64 {
        (1_000_000_000 * self.as_secs() + u64::from(self.subsec_nanos()) + 999_999) / 1_000_000
    }
}
