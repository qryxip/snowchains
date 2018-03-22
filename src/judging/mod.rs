mod interactive;
mod simple;

use command::{CompilationCommand, JudgingCommand};
use errors::{JudgeError, JudgeErrorKind, JudgeResult};
use terminal::Color;
use testsuite::{TestCase, TestCases};

use std::{fmt, io};
use std::sync::Arc;
use std::time::Duration;

/// Executes the tests.
///
/// # Errors
///
/// Returns `Err` if compilation or execution command fails, or any test fails.
pub fn judge(
    cases: TestCases,
    solver: JudgingCommand,
    compilation: Option<CompilationCommand>,
) -> JudgeResult<()> {
    fn judge_all<C: TestCase, O: JudgingOutput>(
        cases: Vec<C>,
        solver: &Arc<JudgingCommand>,
        judge: fn(&C, &Arc<JudgingCommand>) -> JudgeResult<O>,
    ) -> JudgeResult<()> {
        let num_cases = cases.len();
        solver.print_args_and_working_dir();
        println_plural!("Running {}...", num_cases, "test", "tests");

        let mut last_path = None;
        let outputs = cases
            .into_iter()
            .enumerate()
            .map(|(i, case)| {
                let path = case.path();
                if Some(&path) != last_path.as_ref() {
                    println!("Running test cases in {}", path.display());
                    last_path = Some(case.path());
                }
                let output = judge(&case, solver)?;
                output.print_title(i, num_cases);
                Ok(output)
            })
            .collect::<JudgeResult<Vec<O>>>()?;

        let num_failures = outputs.iter().filter(|o| o.failure()).count();
        if num_failures == 0 {
            println_plural!("All of the {} passed.", num_cases, "test", "tests");
            Ok(())
        } else {
            outputs.iter().enumerate().for_each(|(i, o)| {
                eprintln!("");
                o.eprint_title(i, num_cases);
                o.eprint_details();
            });
            bail!(JudgeErrorKind::TestFailure(num_failures, num_cases))
        }
    }

    if let Some(compilation) = compilation {
        compilation.execute()?;
        println!();
    }
    match cases {
        TestCases::Simple(cases) => judge_all(cases, &Arc::new(solver), simple::judge),
        TestCases::Interactive(cases) => judge_all(cases, &Arc::new(solver), interactive::judge),
    }
}

pub(self) trait JudgingOutput
where
    Self: fmt::Display,
{
    /// Whether `self` is a failure.
    fn failure(&self) -> bool;
    /// A color of `self`.
    fn color(&self) -> Color;
    /// Prints details to stderr.
    fn eprint_details(&self);

    fn print_title(&self, i: usize, n: usize) {
        (0..format!("{}", n).len() - format!("{}", i + 1).len()).for_each(|_| print!(" "));
        print_bold!(None, "{}/{} ", i + 1, n);
        println_bold!(self.color(), "{}", self);
    }

    fn eprint_title(&self, i: usize, n: usize) {
        (0..format!("{}", n).len() - format!("{}", i + 1).len()).for_each(|_| eprint!(" "));
        eprint_bold!(None, "{}/{} ", i + 1, n);
        eprintln_bold!(self.color(), "{}", self);
    }
}

pub(self) trait WrapNotFoundErrorMessage {
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

pub(self) trait MillisRoundedUp {
    /// As milliseconds rounded up.
    fn millis_rounded_up(self) -> u64;
}

impl MillisRoundedUp for Duration {
    fn millis_rounded_up(self) -> u64 {
        (1_000_000_000 * self.as_secs() + u64::from(self.subsec_nanos()) + 999_999) / 1_000_000
    }
}
