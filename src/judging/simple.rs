use command::JudgingCommand;
use errors::JudgeResult;
use judging::{JudgingOutput, MillisRoundedUp};
use terminal::Color;
use testsuite::{ExpectedStdout, SimpleCase};
use util;

use std::{self, fmt, thread};
use std::io::Write as _Write;
use std::process::ExitStatus;
use std::sync::Arc;
use std::time::{Duration, Instant};

/// Tests for `case` and `solver` and returns one `SimpleOutput`.
pub(super) fn judge(case: &SimpleCase, solver: &Arc<JudgingCommand>) -> JudgeResult<SimpleOutput> {
    let (tx, rx) = std::sync::mpsc::channel();
    {
        let (case, solver) = (case.clone(), solver.clone());
        thread::spawn(move || {
            let _ = tx.send(run(&case, &solver));
        });
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
    }
}

fn run(case: &SimpleCase, solver: &JudgingCommand) -> JudgeResult<SimpleOutput> {
    let (input, expected, timelimit) = case.values();

    let mut solver = solver.spawn_piped()?;
    let start = Instant::now();
    solver.stdin.as_mut().unwrap().write_all(input.as_bytes())?;

    let status = solver.wait()?;
    let elapsed = start.elapsed();
    let stdout = Arc::new(util::string_from_read(solver.stdout.unwrap(), 1024)?);
    let stderr = Arc::new(util::string_from_read(solver.stderr.unwrap(), 1024)?);

    if timelimit.is_some() && elapsed > timelimit.unwrap() {
        Ok(SimpleOutput::TimelimitExceeded {
            timelimit: timelimit.unwrap(),
            input,
            expected,
        })
    } else if status.success() && is_match(&expected, &stdout) {
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

fn is_match(expected: &ExpectedStdout, stdout: &str) -> bool {
    fn check<F: FnMut(f64, f64) -> bool>(expected: &str, actual: &str, mut on_float: F) -> bool {
        expected.split_whitespace().count() == actual.split_whitespace().count()
            && expected
                .split_whitespace()
                .zip(actual.split_whitespace())
                .all(|(e, a)| {
                    if let (Ok(e), Ok(a)) = (e.parse::<f64>(), a.parse::<f64>()) {
                        on_float(e, a)
                    } else {
                        e == a
                    }
                })
    }

    match *expected {
        ExpectedStdout::AcceptAny => true,
        ExpectedStdout::Exact(ref s) => s == stdout,
        ExpectedStdout::Lines(ref ls) => {
            let stdout = stdout.lines().collect::<Vec<_>>();
            ls.lines().count() == stdout.len()
                && ls.lines().zip(stdout.iter()).all(|(l, &r)| l == r)
        }
        ExpectedStdout::Float {
            ref lines,
            absolute_error,
            relative_error,
        } => {
            let stdout = stdout.lines().collect::<Vec<_>>();
            lines.lines().count() == stdout.len()
                && lines.lines().zip(stdout.iter()).all(|(e, a)| {
                    check(e, a, |e, a| {
                        let (d, r) = (absolute_error, relative_error);
                        (a - e).abs() <= d || ((a - e) / e).abs() <= r // Doesn't care NaN
                    })
                })
        }
    }
}

/// Test result.
#[cfg_attr(test, derive(Debug))]
pub(super) enum SimpleOutput {
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
        expected: Arc<ExpectedStdout>,
    },
    WrongAnswer {
        elapsed: Duration,
        input: Arc<String>,
        expected: Arc<ExpectedStdout>,
        stdout: Arc<String>,
        stderr: Arc<String>,
    },
    RuntimeError {
        elapsed: Duration,
        input: Arc<String>,
        expected: Arc<ExpectedStdout>,
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
            eprintln_bold!(Color::Title, "{}:", head);
            let num_bytes = content.as_bytes().len();
            if num_bytes > THRESHOLD_TO_OMIT {
                eprint_size(num_bytes);
            } else if num_bytes > 0 {
                util::eprintln_trimming_trailing_newline(content);
            }
        }

        fn eprint_expected_sectioon_unless_empty(content: &ExpectedStdout) {
            match *content {
                ExpectedStdout::AcceptAny => {}
                ExpectedStdout::Exact(ref content) => {
                    eprint_section("expected", content);
                }
                ExpectedStdout::Lines(ref lines) => {
                    eprintln_bold!(Color::Title, r"expected:");
                    for l in lines.lines() {
                        eprintln!("{}", l);
                    }
                }
                ExpectedStdout::Float {
                    ref lines,
                    absolute_error,
                    relative_error,
                } => {
                    eprintln_bold!(
                        Color::Title,
                        "expected (absolute: {}, relative: {}):",
                        absolute_error,
                        relative_error
                    );
                    for l in lines.lines() {
                        if l.split_whitespace().any(|t| t.parse::<f64>().is_ok()) {
                            for (i, t) in l.split_whitespace().enumerate() {
                                match t.parse::<f64>() {
                                    Ok(v) if i == 0 => eprint_bold!(Color::CommandInfo, "{}", v),
                                    Ok(v) => eprint_bold!(Color::CommandInfo, " {}", v),
                                    Err(_) if i == 0 => eprint!("{}", t),
                                    Err(_) => eprint!(" {}", t),
                                }
                            }
                            eprintln!();
                        } else {
                            eprintln!("{}", l);
                        }
                    }
                }
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
                eprint_expected_sectioon_unless_empty(expected);
            }
            SimpleOutput::WrongAnswer {
                ref input,
                ref expected,
                ref stdout,
                ref stderr,
                ..
            } => {
                eprint_section("input", input);
                eprint_expected_sectioon_unless_empty(expected);
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
                eprint_expected_sectioon_unless_empty(expected);
                eprint_section_unless_empty("stdout", stdout);
                eprint_section("stderr", stderr);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use command::JudgingCommand;
    use errors::JudgeErrorKind;
    use judging::simple::SimpleOutput;
    use testsuite::SimpleCase;

    use env_logger;

    use std::io;
    use std::sync::Arc;

    #[test]
    #[ignore]
    fn it_judges_for_atcoder_practice_a() {
        static CODE: &str =
            r"(a, (b, c), s) = (int(input()), map(int, input().split()), input()); \
              print(f'{a + b + c} {s}')";
        static IN1: &str = "1\n2 3\ntest\n";
        static OUT1: &str = "6 test\n";
        static IN2: &str = "72\n128 256\nmyonmyon\n";
        static OUT2: &str = "456 myonmyon\n";
        let _ = env_logger::try_init();
        let correct_command = python3_command(CODE).unwrap();
        let wrong_command = python3_command("").unwrap();
        let error_command = python3_command("import sys; sys.exit(1)").unwrap();
        let case1 = SimpleCase::default_matching(IN1, OUT1, None);
        let case2 = SimpleCase::default_matching(IN2, OUT2, None);
        for case in vec![case1, case2] {
            match super::judge(&case, &correct_command).unwrap() {
                SimpleOutput::Accepted { .. } => (),
                o => panic!("{:?}", o),
            }
            match super::judge(&case, &wrong_command).unwrap() {
                SimpleOutput::WrongAnswer { .. } => (),
                o => panic!("{:?}", o),
            }
            match super::judge(&case, &error_command).unwrap() {
                SimpleOutput::RuntimeError { .. } => (),
                o => panic!("{:?}", o),
            }
        }
    }

    #[test]
    #[ignore]
    fn it_judges_for_atcoder_tricky_b() {
        static CODE: &str = r#"import math


def main():
    r = ''
    for _ in range(0, int(input())):
        a, b, c = map(int, input().split())
        if a == 0 and b == 0 and c == 0:
            r += '3\n'
        elif a == 0 and b == 0:
            r += '0\n'
        elif a == 0:
            r += '1 {}\n'.format(-c / b)
        else:
            d = b ** 2.0 - 4.0 * c * a
            if d < 0.0:
                r += '0\n'
            elif d == 0.0:
                r += '1 {}\n'.format(-b / (2.0 * a))
            else:
                d_sqrt = math.sqrt(d)
                x1 = (-b - d_sqrt) / (2.0 * a) if b > 0 else \
                     (-b + d_sqrt) / (2.0 * a)
                x2 = c / (a * x1)
                if x1 < x2:
                    r += '2 {} {}\n'.format(x1, x2)
                else:
                    r += '2 {} {}\n'.format(x2, x1)
    print(r, end='', flush=True)


if __name__ == '__main__':
    main()
"#;
        static IN: &str = "3\n1 -3 2\n-10 30 -20\n100 -300 200\n";
        static OUT: &str = "2 1.000 2.000\n2 1.000 2.000\n2 1.000 2.000\n";
        let _ = env_logger::try_init();
        let command = python3_command(CODE).unwrap();
        let error = 1e-9f64;
        // It may take more than 1 second on Windows
        let case = SimpleCase::float_matching(IN, OUT, None, error, error);
        match super::judge(&case, &command).unwrap() {
            SimpleOutput::Accepted { .. } => {}
            o => panic!("{:?}", o),
        }
    }

    #[test]
    #[ignore]
    fn it_timeouts() {
        static CODE: &str = r"import time; time.sleep(1)";
        let _ = env_logger::try_init();
        let command = python3_command(CODE).unwrap();
        let case = SimpleCase::default_matching("", "", 200);
        match super::judge(&case, &command).unwrap() {
            SimpleOutput::TimelimitExceeded { .. } => {}
            o => panic!("{:?}", o),
        }
    }

    #[test]
    #[ignore]
    fn it_denies_non_utf8_answers() {
        static CODE: &str = r"import sys; sys.stdout.buffer.write(b'\xc3\x28')";
        let _ = env_logger::try_init();
        let command = python3_command(CODE).unwrap();
        let case = SimpleCase::default_matching("", "", None);
        let e = super::judge(&case, &command).unwrap_err();
        if let &JudgeErrorKind::Io(ref e) = e.kind() {
            if let io::ErrorKind::InvalidData = e.kind() {
                return;
            }
        }
        panic!("{:?}", e);
    }

    #[test]
    #[ignore]
    fn it_denies_nonexisting_commands() {
        let _ = env_logger::try_init();
        let command = Arc::new(JudgingCommand::from_args("nonexisting", &[]).unwrap());
        let case = SimpleCase::default_matching("", "", None);
        let e = super::judge(&case, &command).unwrap_err();
        if let &JudgeErrorKind::Command(_) = e.kind() {
            return;
        }
        panic!("{:?}", e);
    }

    fn python3_command(code: &str) -> io::Result<Arc<JudgingCommand>> {
        #[cfg(windows)]
        static PYTHON3: &str = "python";
        #[cfg(not(windows))]
        static PYTHON3: &str = "python3";
        let command = JudgingCommand::from_args(PYTHON3, &["-c", code])?;
        Ok(Arc::new(command))
    }
}
