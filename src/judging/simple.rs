use command::JudgingCommand;
use errors::JudgeResult;
use judging::{JudgingOutput, MillisRoundedUp};
use palette::Palette;
use testsuite::{ExpectedStdout, SimpleCase};
use util;

use std::io::Write as _Write;
use std::process::ExitStatus;
use std::sync::Arc;
use std::time::{Duration, Instant};
use std::{self, fmt, thread};

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

    match expected {
        ExpectedStdout::AcceptAny => true,
        ExpectedStdout::Exact(s) => s == stdout,
        ExpectedStdout::Lines(ls) => {
            let stdout = stdout.lines().collect::<Vec<_>>();
            ls.lines().count() == stdout.len()
                && ls.lines().zip(stdout.iter()).all(|(l, &r)| l == r)
        }
        ExpectedStdout::Float {
            lines,
            absolute_error,
            relative_error,
        } => {
            let stdout = stdout.lines().collect::<Vec<_>>();
            lines.lines().count() == stdout.len()
                && lines.lines().zip(stdout.iter()).all(|(e, a)| {
                    check(e, a, |e, a| {
                        let (d, r) = (*absolute_error, *relative_error);
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
        match self {
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
        match self {
            SimpleOutput::Accepted { .. } => false,
            _ => true,
        }
    }

    fn palette(&self) -> Palette {
        match self {
            SimpleOutput::Accepted { .. } => Palette::Success,
            SimpleOutput::TimelimitExceeded { .. } => Palette::Fatal,
            SimpleOutput::WrongAnswer { .. } | SimpleOutput::RuntimeError { .. } => {
                Palette::Warning
            }
        }
    }

    fn eprint_details(&self) {
        const THRESHOLD_TO_OMIT: usize = 1024;

        fn eprint_size(num_bytes: usize) {
            if num_bytes > 10 * 1024 * 1024 {
                let msg = format!("OMITTED ({}MB)", num_bytes / (1024 * 1024));
                eprintln!("{}", Palette::Warning.bold().paint(msg));
            } else if num_bytes > 10 * 1024 {
                let msg = format!("OMITTED ({}KB)", num_bytes / 1024);
                eprintln!("{}", Palette::Warning.bold().paint(msg));
            } else {
                let msg = format!("OMITTED ({}B)", num_bytes);
                eprintln!("{}", Palette::Warning.bold().paint(msg));
            }
        }

        fn eprint_section(head: &'static str, content: &str) {
            eprintln!("{}", Palette::Title.bold().paint(format!("{}:", head)));
            let num_bytes = content.as_bytes().len();
            if num_bytes == 0 {
                eprintln!("{}", Palette::Warning.bold().paint("EMPTY"));
            } else if num_bytes > THRESHOLD_TO_OMIT {
                eprint_size(num_bytes);
            } else {
                util::eprintln_trimming_trailing_newline(content);
            }
        }

        fn eprint_section_unless_empty(head: &'static str, content: &str) {
            eprintln!("{}", Palette::Title.bold().paint(format!("{}:", head)));
            let num_bytes = content.as_bytes().len();
            if num_bytes > THRESHOLD_TO_OMIT {
                eprint_size(num_bytes);
            } else if num_bytes > 0 {
                util::eprintln_trimming_trailing_newline(content);
            }
        }

        fn eprint_expected_sectioon_unless_empty(content: &ExpectedStdout) {
            match content {
                ExpectedStdout::AcceptAny => {}
                ExpectedStdout::Exact(content) => {
                    eprint_section("expected", content);
                }
                ExpectedStdout::Lines(lines) => {
                    eprintln!("{}", Palette::Title.bold().paint("expected:"));
                    for l in lines.lines() {
                        eprintln!("{}", l);
                    }
                }
                ExpectedStdout::Float {
                    lines,
                    absolute_error,
                    relative_error,
                } => {
                    let msg = format!(
                        "expected (absolute: {}, relative: {}):",
                        absolute_error, relative_error
                    );
                    eprintln!("{}", Palette::Title.bold().paint(msg));
                    for l in lines.lines() {
                        if l.split_whitespace().any(|t| t.parse::<f64>().is_ok()) {
                            for (i, t) in l.split_whitespace().enumerate() {
                                match t.parse::<f64>() {
                                    Ok(v) if i == 0 => {
                                        eprint!("{}", Palette::CommandInfo.paint(v.to_string()))
                                    }
                                    Ok(v) => {
                                        eprint!("{}", Palette::CommandInfo.paint(v.to_string()))
                                    }
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

        match self {
            SimpleOutput::Accepted {
                input,
                stdout,
                stderr,
                ..
            } => {
                eprint_section("input", input);
                eprint_section("stdout", stdout);
                eprint_section_unless_empty("stderr", stderr);
            }
            SimpleOutput::TimelimitExceeded {
                input, expected, ..
            } => {
                eprint_section("input", input);
                eprint_expected_sectioon_unless_empty(expected);
            }
            SimpleOutput::WrongAnswer {
                input,
                expected,
                stdout,
                stderr,
                ..
            } => {
                eprint_section("input", input);
                eprint_expected_sectioon_unless_empty(expected);
                eprint_section("stdout", stdout);
                eprint_section_unless_empty("stderr", stderr);
            }
            SimpleOutput::RuntimeError {
                input,
                expected,
                stdout,
                stderr,
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
    use errors::{JudgeError, JudgeResult};
    use judging::simple::SimpleOutput;
    use path::AbsPathBuf;
    use testsuite::SimpleCase;

    use env_logger;
    use tempdir::TempDir;

    use std::process::{Command, Stdio};
    use std::sync::{mpsc, Arc};
    use std::time::Duration;
    use std::{env, io, thread};

    #[test]
    #[ignore]
    fn it_judges_for_atcoder_practice_a() {
        static IN1: &str = "1\n2 3\ntest\n";
        static OUT1: &str = "6 test\n";
        static IN2: &str = "72\n128 256\nmyonmyon\n";
        static OUT2: &str = "456 myonmyon\n";
        let _ = env_logger::try_init();
        let correct_command = bash("read a; read b c; read s; echo `expr $a + $b + $c` $s");
        let wrong_command = bash("echo yee");
        let error_command = bash("echo error message 1>&2 && exit 1");
        for (case_in, case_out) in vec![(IN1, OUT1), (IN2, OUT2)] {
            match judge_default_matching(case_in, case_out, 500, &correct_command).unwrap() {
                SimpleOutput::Accepted {
                    input,
                    stdout,
                    stderr,
                    ..
                } => {
                    assert_eq!(case_in, input.as_str());
                    assert_eq!(case_out, stdout.as_str());
                    assert_eq!("", stderr.as_str());
                }
                o => panic!("{:?}", o),
            }
            match judge_default_matching(case_in, case_out, 500, &wrong_command).unwrap() {
                SimpleOutput::WrongAnswer {
                    input,
                    stdout,
                    stderr,
                    ..
                } => {
                    assert_eq!(case_in, input.as_str());
                    assert_eq!("yee\n", stdout.as_str());
                    assert_eq!("", stderr.as_str());
                }
                o => panic!("{:?}", o),
            }
            match judge_default_matching(case_in, case_out, 500, &error_command).unwrap() {
                SimpleOutput::RuntimeError {
                    input,
                    stdout,
                    stderr,
                    status,
                    ..
                } => {
                    assert_eq!(case_in, input.as_str());
                    assert_eq!("", stdout.as_str());
                    assert_eq!("error message\n", stderr.as_str());
                    assert_eq!(Some(1), status.code());
                }
                o => panic!("{:?}", o),
            }
        }
    }

    #[cfg(not(windows))]
    #[test]
    #[ignore]
    fn it_judges_for_atcoder_tricky_b() {
        // Fastest code!
        // https://beta.atcoder.jp/contests/tricky/submissions?f.Language=&f.Status=AC&f.Task=tricky_2&orderBy=time_consumption
        // https://beta.atcoder.jp/contests/language-test-201603/submissions?f.Language=&f.Status=AC&f.Task=tricky_2&orderBy=time_consumption
        static CODE: &str = r#"
#![cfg_attr(feature = "cargo-clippy", allow(redundant_field_names, many_single_char_names))]

use std::fmt;
use std::io::{self, BufWriter, Read, Write as _Write};
use std::str::{self, FromStr};

fn main() {
    let abcs = {
        let mut sc = InputScanOnce::new(io::stdin(), 1024 * 1024);
        let n = sc.next();
        sc.trios::<i64, i64, i64>(n)
    };
    let mut out = BufWriter::new(io::stdout());
    for (a, b, c) in abcs {
        if a == 0 && b == 0 && c == 0 {
            writeln!(out, "3").unwrap();
        } else if a == 0 && b == 0 {
            writeln!(out, "0").unwrap();
        } else if a == 0 {
            writeln!(out, "1 {}", (-c as f64) / (b as f64)).unwrap();
        } else {
            let (a, b, c) = (a as f64, b as f64, c as f64);
            let d = b.powi(2) - 4.0 * c * a;
            if d < 0.0 {
                writeln!(out, "0").unwrap();
            } else if d == 0.0 {
                let x = -b / (2.0 * a);
                writeln!(out, "1 {}", x).unwrap();
            } else {
                let d_sqrt = d.sqrt();
                let x1 = if b >= 0.0 {
                    (-b - d_sqrt) / (2.0 * a)
                } else {
                    (-b + d_sqrt) / (2.0 * a)
                };
                let x2 = c / (a * x1);
                if x1 < x2 {
                    writeln!(out, "2 {} {}", x1, x2).unwrap();
                } else {
                    writeln!(out, "2 {} {}", x2, x1).unwrap();
                }
            }
        }
    }
    out.flush().unwrap();
}

struct InputScanOnce {
    buf: Vec<u8>,
    pos: usize,
}

impl InputScanOnce {
    fn new<R: Read>(mut reader: R, estimated: usize) -> Self {
        let mut buf = Vec::with_capacity(estimated);
        io::copy(&mut reader, &mut buf).unwrap();
        InputScanOnce { buf: buf, pos: 0 }
    }

    #[inline]
    fn next<T: FromStr>(&mut self) -> T
    where
        T::Err: fmt::Debug,
    {
        let mut start = None;
        loop {
            match (self.buf[self.pos], start.is_some()) {
                (b' ', true) | (b'\n', true) => break,
                (_, true) | (b' ', false) | (b'\n', false) => self.pos += 1,
                (_, false) => start = Some(self.pos),
            }
        }
        let target = &self.buf[start.unwrap()..self.pos];
        unsafe { str::from_utf8_unchecked(target) }.parse().unwrap()
    }

    fn trios<T1: FromStr, T2: FromStr, T3: FromStr>(&mut self, n: usize) -> Vec<(T1, T2, T3)>
    where
        T1::Err: fmt::Debug,
        T2::Err: fmt::Debug,
        T3::Err: fmt::Debug,
    {
        (0..n)
            .map(|_| (self.next(), self.next(), self.next()))
            .collect()
    }
}
"#;
        static IN: &str = "3\n1 -3 2\n-10 30 -20\n100 -300 200\n";
        static OUT: &str = "2 1.000 2.000\n2 1.000 2.000\n2 1.000 2.000\n";
        let _ = env_logger::try_init();
        let tempdir = TempDir::new("it_judges_for_atcoder_tricky_b").unwrap();
        let wd = AbsPathBuf::new_or_panic(tempdir.path());
        let src = wd.join("a.rs");
        let bin = wd.join("a");
        ::fs::write(&src, CODE.as_bytes()).unwrap();
        let status = Command::new("rustc")
            .arg(src.as_ref())
            .arg("-o")
            .arg(bin.as_ref())
            .current_dir(&wd)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .unwrap();
        assert!(status.success());
        let command = Arc::new(JudgingCommand::from_args(bin.as_ref(), &[], wd));
        match judge_float_matching(IN, OUT, 500, 1e-9f64, &command).unwrap() {
            SimpleOutput::Accepted { .. } => tempdir.close().unwrap(),
            o => panic!("{:?}", o),
        }
    }

    #[test]
    #[ignore]
    fn it_timeouts() {
        let _ = env_logger::try_init();
        let command = bash("sleep 1");
        match judge_default_matching("input\n", "", 200, &command).unwrap() {
            SimpleOutput::TimelimitExceeded {
                timelimit, input, ..
            } => {
                assert_eq!(Duration::from_millis(200), timelimit);
                assert_eq!("input\n", input.as_str());
            }
            o => panic!("{:?}", o),
        }
    }

    #[test]
    #[ignore]
    fn it_denies_non_utf8_answers() {
        let _ = env_logger::try_init();
        let command = bash(r"echo $'\xc3\x28'");
        match judge_default_matching("", "", 500, &command).unwrap_err() {
            JudgeError::Io(ref e) if e.kind() == io::ErrorKind::InvalidData => {}
            e => panic!("{:?}", e),
        }
    }

    #[test]
    #[ignore]
    fn it_denies_nonexisting_commands() {
        let _ = env_logger::try_init();
        let cwd = env::current_dir().map(AbsPathBuf::new_or_panic).unwrap();
        let command = Arc::new(JudgingCommand::from_args("nonexisting", &[], cwd));
        match judge_default_matching("", "", 500, &command).unwrap_err() {
            JudgeError::Command(..) => {}
            e => panic!("{:?}", e),
        }
    }

    fn judge_default_matching(
        input: &str,
        output: &str,
        timelimit: u64,
        command: &Arc<JudgingCommand>,
    ) -> JudgeResult<SimpleOutput> {
        let timelimit = Duration::from_millis(timelimit);
        let case = SimpleCase::default_matching(input, output, timelimit);
        judge(&case, timelimit * 2, command)
    }

    fn judge_float_matching(
        input: &str,
        output: &str,
        timelimit: u64,
        error: f64,
        command: &Arc<JudgingCommand>,
    ) -> JudgeResult<SimpleOutput> {
        let timelimit = Duration::from_millis(timelimit);
        let case = SimpleCase::float_matching(input, output, timelimit, error, error);
        judge(&case, timelimit * 2, command)
    }

    fn judge(
        case: &SimpleCase,
        timeout: Duration,
        command: &Arc<JudgingCommand>,
    ) -> JudgeResult<SimpleOutput> {
        let (case, command) = (case.clone(), command.clone());
        let (tx, rx) = mpsc::channel();
        thread::spawn(move || loop {
            match super::judge(&case, &command) {
                Err(JudgeError::Io(ref e)) if e.kind() == io::ErrorKind::BrokenPipe => {}
                Err(err) => {
                    tx.send(Err(err)).unwrap();
                    break;
                }
                Ok(output) => {
                    tx.send(Ok(output)).unwrap();
                    break;
                }
            }
        });
        rx.recv_timeout(timeout).unwrap()
    }

    fn bash(code: &str) -> Arc<JudgingCommand> {
        #[cfg(windows)]
        static BASH: &str = r"C:\msys64\usr\bin\bash.exe";
        #[cfg(not(windows))]
        static BASH: &str = "/bin/bash";
        let cwd = env::current_dir().map(AbsPathBuf::new_or_panic).unwrap();
        Arc::new(JudgingCommand::from_args(BASH, &["-c", code], cwd))
    }
}
