mod interactive;
mod simple;

use command::{CompilationCommand, JudgingCommand};
use config::Config;
use errors::{JudgeError, JudgeErrorKind, JudgeResult};
use terminal::Color;
use testsuite::{TestCase, TestCases};

use unicode_width::UnicodeWidthStr as _UnicodeWidthStr;

use std::{fmt, io};
use std::sync::Arc;
use std::time::Duration;

/// Executes the tests.
///
/// # Errors
///
/// Returns `Err` if compilation or execution command fails, or any test fails.
pub fn judge(prop: JudgeProp) -> JudgeResult<()> {
    fn judge_all<C: TestCase, O: JudgingOutput>(
        cases: Vec<C>,
        solver: &Arc<JudgingCommand>,
        judge: fn(&C, &Arc<JudgingCommand>) -> JudgeResult<O>,
    ) -> JudgeResult<()> {
        let num_cases = cases.len();
        println_plural!("Running {}...", num_cases, "test", "tests");

        let filenames = cases
            .iter()
            .map(|case| {
                case.path()
                    .file_name()
                    .map(|s| s.to_string_lossy().into_owned())
                    .unwrap_or_default()
            })
            .collect::<Vec<_>>();
        let filename_max_width = filenames.iter().map(|s| s.width_cjk()).max().unwrap_or(0);
        let outputs = cases
            .into_iter()
            .zip(filenames.iter())
            .enumerate()
            .map(|(i, (case, filename))| {
                let output = judge(&case, solver)?;
                output.print_title(i, num_cases, &filename, filename_max_width);
                Ok(output)
            })
            .collect::<JudgeResult<Vec<O>>>()?;

        let num_failures = outputs.iter().filter(|o| o.failure()).count();
        if num_failures == 0 {
            println_plural!("All of the {} passed.", num_cases, "test", "tests");
            Ok(())
        } else {
            outputs
                .iter()
                .zip(filenames.iter())
                .enumerate()
                .for_each(|(i, (output, filename))| {
                    eprintln!("");
                    output.eprint_title(i, num_cases, filename, filename_max_width);
                    output.eprint_details();
                });
            bail!(JudgeErrorKind::TestFailure(num_failures, num_cases))
        }
    }

    let (cases, case_paths, solver, compilation) =
        (prop.cases, prop.case_paths, prop.solver, prop.compilation);
    if let Some(compilation) = compilation {
        compilation.execute()?;
        println!();
    }
    solver.print_info(&case_paths);
    match cases {
        TestCases::Simple(cases) => judge_all(cases, &Arc::new(solver), simple::judge),
        TestCases::Interactive(cases) => judge_all(cases, &Arc::new(solver), interactive::judge),
    }
}

pub struct JudgeProp {
    cases: TestCases,
    case_paths: String,
    solver: JudgingCommand,
    compilation: Option<CompilationCommand>,
}

impl JudgeProp {
    pub fn from_config(config: &Config, target: &str, language: Option<&str>) -> ::Result<Self> {
        let paths = config.suite_paths(&target)?;
        let cases = paths.load_merging(false)?;
        let text = paths.existing_paths_as_text()?;
        let solver = config.construct_solver(&target, language)?;
        let compilation = config.construct_compilation_command(&target, language)?;
        Ok(Self {
            cases,
            case_paths: text,
            solver,
            compilation,
        })
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

    fn print_title(&self, i: usize, n: usize, name: &str, name_width: usize) {
        (0..format!("{}", n).len() - format!("{}", i + 1).len()).for_each(|_| print!(" "));
        print_bold!(None, "{}/{} ({})", i + 1, n, name);
        (0..name_width - name.width_cjk() + 1).for_each(|_| print!(" "));
        println_bold!(self.color(), "{}", self);
    }

    fn eprint_title(&self, i: usize, n: usize, name: &str, name_width: usize) {
        (0..format!("{}", n).len() - format!("{}", i + 1).len()).for_each(|_| eprint!(" "));
        eprint_bold!(None, "{}/{} ({})", i + 1, n, name);
        (0..name_width - name.width_cjk() + 1).for_each(|_| eprint!(" "));
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
