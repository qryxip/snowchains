mod interactive;
mod simple;

use command::{CompilationCommand, JudgingCommand};
use errors::{JudgeError, JudgeErrorKind, JudgeResult};
use judging::interactive::InteractiveOutput;
use judging::simple::SimpleOutput;
use testsuite::{SuiteFilePaths, TestCases};

use std::fmt;
use std::io;
use std::iter::FromIterator;
use std::sync::Arc;
use std::time::Duration;

/// Tests for given test case file paths, executaion command, and compilation command.
///
/// # Errors
///
/// Returns `Err` if compilation or execution command fails, or any test fails.
pub fn judge(
    suite_paths: &SuiteFilePaths,
    solver: JudgingCommand,
    compilation: Option<CompilationCommand>,
) -> JudgeResult<()> {
    fn judge_all(cases: TestCases, solver: &Arc<JudgingCommand>) -> JudgeResult<()> {
        let num_cases = cases.len();
        solver.print_args_and_working_dir();
        let suf = if num_cases > 1 { "s" } else { "" };
        println!("Running {} test{}...", num_cases, suf);

        let mut last_path = None;

        macro_rules! judge_all { ($cases: expr, $method: path) => {
            $cases.into_iter().enumerate().map(|(i, case)| {
                let path = case.get_path();
                if Some(&path) != last_path.as_ref() {
                    println!("Running test cases in {}", path.display());
                    last_path = Some(case.get_path());
                }
                let output = $method(case, solver)?;
                output.print_title(i, num_cases);
                Ok(output)
            }).collect::<JudgeResult<JudgingOutputs>>()?
        } }

        match cases {
            TestCases::Simple(cases) => judge_all!(cases, simple::judge),
            TestCases::Interactive(cases) => judge_all!(cases, interactive::judge),
        }.show_result(num_cases)
    }

    enum JudgingOutputs {
        Simple(Vec<SimpleOutput>),
        Interactive(Vec<InteractiveOutput>),
    }

    impl FromIterator<SimpleOutput> for JudgingOutputs {
        fn from_iter<T: IntoIterator<Item = SimpleOutput>>(iter: T) -> Self {
            JudgingOutputs::Simple(iter.into_iter().collect())
        }
    }

    impl FromIterator<InteractiveOutput> for JudgingOutputs {
        fn from_iter<T: IntoIterator<Item = InteractiveOutput>>(iter: T) -> Self {
            JudgingOutputs::Interactive(iter.into_iter().collect())
        }
    }

    impl JudgingOutputs {
        fn show_result(self, num_cases: usize) -> JudgeResult<()> {
            fn count_num_failures<O: JudgingOutput>(outputs: &[O]) -> usize {
                outputs.iter().filter(|o| o.failure()).count()
            }

            fn eprint_failure_details<O: JudgingOutput>(outputs: &[O], num_cases: usize) {
                outputs.iter().enumerate().for_each(|(i, o)| {
                    eprintln!("");
                    o.eprint_title(i, num_cases);
                    o.eprint_details();
                })
            }

            let suf = if num_cases > 1 { "s" } else { "" };
            let num_failures = match self {
                JudgingOutputs::Simple(ref xs) => count_num_failures(xs),
                JudgingOutputs::Interactive(ref xs) => count_num_failures(xs),
            };
            if num_failures == 0 {
                println!("All of the {} test{} passed.", num_cases, suf);
                Ok(())
            } else {
                match self {
                    JudgingOutputs::Simple(ref xs) => eprint_failure_details(xs, num_cases),
                    JudgingOutputs::Interactive(ref xs) => eprint_failure_details(xs, num_cases),
                }
                bail!(JudgeErrorKind::TestFailure(num_failures, num_cases))
            }
        }
    }

    if let Some(compilation) = compilation {
        compilation.execute()?;
        println!();
    }
    judge_all(suite_paths.load_and_merge_all()?, &Arc::new(solver))
}

trait JudgingOutput
where
    Self: fmt::Display,
{
    /// Whether `self` is a failure.
    fn failure(&self) -> bool;
    /// A color of `self`.
    fn color(&self) -> u32;
    /// Prints details to stderr.
    fn eprint_details(&self);

    fn print_title(&self, i: usize, n: usize) {
        (0..format!("{}", n).len() - format!("{}", i + 1).len()).for_each(|_| print!(" "));
        print_bold!(None, "{}/{} ", i + 1, n);
        println_bold!(Some(self.color()), "{}", self);
    }

    fn eprint_title(&self, i: usize, n: usize) {
        (0..format!("{}", n).len() - format!("{}", i + 1).len()).for_each(|_| eprint!(" "));
        eprint_bold!(None, "{}/{} ", i + 1, n);
        eprintln_bold!(Some(self.color()), "{}", self);
    }
}

trait WrapNotFoundErrorMessage {
    type Item;
    /// Maps `io::Error` to `JudgingError`.
    fn wrap_not_found_error_message<F: FnOnce() -> String>(
        self,
        arg0: F,
    ) -> JudgeResult<Self::Item>;
}

impl<T> WrapNotFoundErrorMessage for io::Result<T> {
    type Item = T;

    fn wrap_not_found_error_message<F: FnOnce() -> String>(self, arg0: F) -> JudgeResult<T> {
        self.map_err(|e| -> JudgeError {
            match e.kind() {
                io::ErrorKind::NotFound => JudgeErrorKind::CommandNotFound(arg0()).into(),
                _ => e.into(),
            }
        })
    }
}

trait MillisRoundedUp {
    /// As milliseconds rounded up.
    fn millis_rounded_up(self) -> u64;
}

impl MillisRoundedUp for Duration {
    fn millis_rounded_up(self) -> u64 {
        (1_000_000_000 * self.as_secs() + u64::from(self.subsec_nanos()) + 999_999) / 1_000_000
    }
}
