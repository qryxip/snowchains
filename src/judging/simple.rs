use command::JudgingCommand;
use errors::JudgeResult;
use judging::{JudgingOutput, MillisRoundedUp};
use terminal::Color;
use testsuite::SimpleCase;
use util;

use std::{self, fmt, thread};
use std::io::{self, Write};
use std::process::ExitStatus;
use std::sync::Arc;
use std::time::{Duration, Instant};

/// Tests for `case` and `solver` and returns one `SimpleOutput`.
pub fn judge(case: &SimpleCase, solver: &Arc<JudgingCommand>) -> JudgeResult<SimpleOutput> {
    let (tx, rx) = std::sync::mpsc::channel();
    {
        let (case, solver) = (case.clone(), solver.clone());
        thread::spawn(move || tx.send(run(&case, &solver)).unwrap());
    }
    if let (input, expected, Some(timelimit)) = case.values() {
        rx.recv_timeout(timelimit + Duration::from_millis(50))
            .unwrap_or_else(|_| {
                Ok(SimpleOutput::TimelimitExceeded {
                    timelimit,
                    input,
                    expected,
                })
            })
    } else {
        rx.recv().unwrap()
    }.map_err(Into::into)
}

fn run(case: &SimpleCase, solver: &JudgingCommand) -> io::Result<SimpleOutput> {
    let (input, expected, timelimit) = case.values();
    let mut solver = solver.spawn_piped()?;
    let start = Instant::now();
    solver.stdin.as_mut().unwrap().write_all(input.as_bytes())?;

    let status = solver.wait()?;
    let elapsed = start.elapsed();
    let stdout = Arc::new(util::string_from_read(solver.stdout.unwrap(), 1024)?);
    let stderr = Arc::new(util::string_from_read(solver.stderr.unwrap(), 1024)?);

    // `expected` is empty IFF omitted.
    if timelimit.is_some() && elapsed > timelimit.unwrap() {
        Ok(SimpleOutput::TimelimitExceeded {
            timelimit: timelimit.unwrap(),
            input,
            expected,
        })
    } else if status.success() && (expected.is_empty() || expected == stdout) {
        Ok(SimpleOutput::Accepted {
            elapsed,
            input,
            stdout,
            stderr,
        })
    } else if status.success() {
        Ok(SimpleOutput::WrongAnswer {
            elapsed,
            input,
            expected,
            stdout,
            stderr,
        })
    } else {
        Ok(SimpleOutput::RuntimeError {
            elapsed,
            input,
            expected,
            stdout,
            stderr,
            status,
        })
    }
}

/// Test result.
pub enum SimpleOutput {
    // Each string may be empty.
    Accepted {
        elapsed: Duration,
        input: Arc<String>,
        stdout: Arc<String>,
        stderr: Arc<String>,
    },
    TimelimitExceeded {
        timelimit: Duration,
        input: Arc<String>,
        expected: Arc<String>,
    },
    WrongAnswer {
        elapsed: Duration,
        input: Arc<String>,
        expected: Arc<String>,
        stdout: Arc<String>,
        stderr: Arc<String>,
    },
    RuntimeError {
        elapsed: Duration,
        input: Arc<String>,
        expected: Arc<String>,
        stdout: Arc<String>,
        stderr: Arc<String>,
        status: ExitStatus,
    },
}

impl fmt::Display for SimpleOutput {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SimpleOutput::Accepted { elapsed, .. } => {
                write!(f, "Accepted ({}ms)", elapsed.millis_rounded_up())
            }
            SimpleOutput::TimelimitExceeded { timelimit, .. } => write!(
                f,
                "Time Limit Exceeded ({}ms)",
                timelimit.millis_rounded_up()
            ),
            SimpleOutput::WrongAnswer { elapsed, .. } => {
                write!(f, "Wrong Answer ({}ms)", elapsed.millis_rounded_up())
            }
            SimpleOutput::RuntimeError {
                elapsed, status, ..
            } => {
                let elapsed = elapsed.millis_rounded_up();
                write!(f, "Runtime Error ({}, {}ms)", status, elapsed)
            }
        }
    }
}

impl JudgingOutput for SimpleOutput {
    fn failure(&self) -> bool {
        match *self {
            SimpleOutput::Accepted { .. } => false,
            _ => true,
        }
    }

    fn color(&self) -> Color {
        match *self {
            SimpleOutput::Accepted { .. } => Color::Success,
            SimpleOutput::TimelimitExceeded { .. } => Color::Fatal,
            SimpleOutput::WrongAnswer { .. } | SimpleOutput::RuntimeError { .. } => Color::Warning,
        }
    }

    fn eprint_details(&self) {
        const THRESHOLD_TO_OMIT: usize = 1024;

        fn eprint_size(num_bytes: usize) {
            if num_bytes > 10 * 1024 * 1024 {
                let mb = num_bytes / (1024 * 1024);
                eprintln_bold!(Color::Warning, "OMITTED ({}MB)", mb);
            } else if num_bytes > 10 * 1024 {
                let kb = num_bytes / 1024;
                eprintln_bold!(Color::Warning, "OMITTED ({}KB)", kb);
            } else {
                eprintln_bold!(Color::Warning, "OMITTED ({}B)", num_bytes);
            }
        }

        fn eprint_section(head: &'static str, content: &str) {
            let num_bytes = content.as_bytes().len();
            eprintln_bold!(Color::Title, "{}:", head);
            if num_bytes == 0 {
                eprintln_bold!(Color::Warning, "EMPTY");
            } else if num_bytes > THRESHOLD_TO_OMIT {
                eprint_size(num_bytes);
            } else {
                util::eprintln_trimming_trailing_newline(content);
            }
        }

        fn eprint_section_unless_empty(head: &'static str, content: &str) {
            let num_bytes = content.as_bytes().len();
            if num_bytes > THRESHOLD_TO_OMIT {
                eprint_size(num_bytes);
            } else if num_bytes > 0 {
                eprintln_bold!(Color::Title, "{}:", head);
                util::eprintln_trimming_trailing_newline(content);
            }
        }

        match *self {
            SimpleOutput::Accepted {
                ref input,
                ref stdout,
                ref stderr,
                ..
            } => {
                eprint_section("input", input);
                eprint_section("stdout", stdout);
                eprint_section_unless_empty("stderr", stderr);
            }
            SimpleOutput::TimelimitExceeded {
                ref input,
                ref expected,
                ..
            } => {
                eprint_section("input", input);
                eprint_section_unless_empty("expected", expected);
            }
            SimpleOutput::WrongAnswer {
                ref input,
                ref expected,
                ref stdout,
                ref stderr,
                ..
            } => {
                eprint_section("input", input);
                eprint_section("expected", expected);
                eprint_section("stdout", stdout);
                eprint_section_unless_empty("stderr", stderr);
            }
            SimpleOutput::RuntimeError {
                ref input,
                ref expected,
                ref stdout,
                ref stderr,
                ..
            } => {
                eprint_section("input", input);
                eprint_section_unless_empty("expected", expected);
                eprint_section_unless_empty("stdout", stdout);
                eprint_section("stderr", stderr);
            }
        }
    }
}

#[cfg(all(test, unix))]
mod tests {
    use command::JudgingCommand;
    use judging::simple::SimpleOutput;
    use testsuite::SimpleCase;

    use std::sync::Arc;

    #[test]
    #[ignore]
    fn it_judges() {
        static CODE: &str = r#"read a;read b c;read s;printf "%d %s\n" $(expr $a + $b + $c) $s"#;
        let command = Arc::new(JudgingCommand::from_args("bash", &["-c", CODE]).unwrap());
        let wa_command = Arc::new(JudgingCommand::from_args("bash", &["-c", "echo 0"]).unwrap());
        let re_command = Arc::new(JudgingCommand::from_args("bash", &["-c", "exit 1"]).unwrap());
        let case1 = SimpleCase::new("1\n2 3\ntest\n", "6 test\n", 100);
        let case2 = SimpleCase::new("72\n128 256\nmyonmyon\n", "456 myonmyon\n", 100);
        for case in vec![case1, case2] {
            match super::judge(&case, &command).unwrap() {
                SimpleOutput::Accepted { .. } => (),
                o => panic!("{}", o),
            }
            match super::judge(&case, &wa_command).unwrap() {
                SimpleOutput::WrongAnswer { .. } => (),
                o => panic!("{}", o),
            }
            match super::judge(&case, &re_command).unwrap() {
                SimpleOutput::RuntimeError { .. } => (),
                o => panic!("{}", o),
            }
        }
    }
}
