use super::error::{JudgeErrorKind, JudgeResult};
use super::testcase::{Case, Cases};
use super::util::UnwrapAsRefMut;
use std::convert::From;
use std::fmt::{self, Debug, Formatter};
use std::io::{self, Read, Write};
use std::path::Path;
use std::process::{Command, ExitStatus, Stdio};
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};
use term::{Attr, color};


pub fn run_judge(cases: &str, target: &str, args: &[&str]) -> JudgeResult<()> {
    let (cases, target) = (Cases::load(Path::new(cases))?, Path::new(target));
    judge_all(cases, target, args)
}


pub fn judge_all(cases: Cases, target: &Path, args: &[&str]) -> JudgeResult<()> {
    fn judge(case: Case, program: String, args: Vec<String>, timeout: u64) -> JudgeOutput {
        fn run_program(case: Case, program: String, args: Vec<String>) -> JudgeOutput {
            fn go(case: Case, program: String, args: Vec<String>) -> io::Result<JudgeOutput> {
                let (expected, input) = case.into();
                let mut child = Command::new(program)
                    .args(args)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .stderr(Stdio::piped())
                    .spawn()?;
                let start = Instant::now();
                child.stdin.unwrap_as_ref_mut().write_all(input.as_bytes())?;

                let status = child.wait()?;
                let t = {
                    let t = start.elapsed();
                    (1000000000 * t.as_secs() + t.subsec_nanos() as u64) / 1000000
                };
                let (stdout, stderr) = {
                    let (mut stdout, mut stderr) = (String::new(), String::new());
                    child.stdout.unwrap().read_to_string(&mut stdout)?;
                    child.stderr.unwrap().read_to_string(&mut stderr)?;
                    (stdout, stderr)
                };

                if status.success() && expected == stdout {
                    Ok(JudgeOutput::Ac(t))
                } else if status.success() {
                    Ok(JudgeOutput::Wa(t, expected, stdout))
                } else {
                    Ok(JudgeOutput::Re(status, stderr))
                }
            }

            go(case, program, args).into()
        }

        let (tx, rx) = mpsc::channel();
        thread::spawn(move || {
            let _ = tx.send(run_program(case, program, args));
        });
        match rx.recv_timeout(Duration::from_millis(timeout + 50)) {
            Ok(JudgeOutput::Ac(t)) if t > timeout => JudgeOutput::Tle(timeout),
            Ok(output) => output,
            Err(_) => JudgeOutput::Tle(timeout),
        }
    }

    let (timeout, num_tests) = (cases.timeout(), cases.num_cases());
    let suf = if num_tests > 1 { "s" } else { "" };
    let mut failures = vec![];

    println!("\nRunning {} test{}...", num_tests, suf);
    for (i, case) in cases.into_cases().into_iter().enumerate() {
        let target = match target.to_str() {
            Some(s) => s.to_owned(),
            None => bail!(io::Error::from(io::ErrorKind::InvalidInput)),
        };
        let args = args.iter().map(|&arg| arg.into()).collect();
        let output = judge(case, target, args, timeout);
        output.print_title(i, num_tests);
        match output {
            JudgeOutput::Ac(_) => {}
            failure => failures.push((i, failure)),
        }
    }

    if failures.is_empty() {
        Ok(println!("All of the {} test{} passed.", num_tests, suf))
    } else {
        for &(i, ref failure) in &failures {
            println!("");
            failure.print_title(i, num_tests);
            failure.print_failure_detail();
        }
        bail!(JudgeErrorKind::TestFailed(failures.len()))
    }
}


enum JudgeOutput {
    Ac(u64),
    Tle(u64),
    Wa(u64, String, String),
    Re(ExitStatus, String),
    UnexpectedIoError(io::Error),
}

impl From<io::Result<JudgeOutput>> for JudgeOutput {
    fn from(from: io::Result<JudgeOutput>) -> Self {
        from.unwrap_or_else(|e| JudgeOutput::UnexpectedIoError(e))
    }
}

impl Debug for JudgeOutput {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            JudgeOutput::Ac(t) => write!(f, "AC ({}ms)", t),
            JudgeOutput::Tle(t) => write!(f, "TLE ({}ms)", t),
            JudgeOutput::Wa(t, _, _) => write!(f, "WA ({}ms)", t),
            JudgeOutput::Re(status, _) => write!(f, "RE ({})", status),
            JudgeOutput::UnexpectedIoError(ref e) => {
                write!(f, "UNEXPECTED IO ERROR ({:?})", e.kind())
            }
        }
    }
}

impl JudgeOutput {
    fn print_title(&self, i: usize, n: usize) {
        for _ in 0..format!("{}", n).len() - format!("{}", i + 1).len() {
            print!(" ");
        }
        print_decorated!(Attr::Bold, None, "{}/{} ", i + 1, n);

        let color = match *self {
            JudgeOutput::Ac(_) => color::GREEN,
            JudgeOutput::Tle(_) => color::RED,
            JudgeOutput::Wa(_, _, _) => color::YELLOW,
            JudgeOutput::Re(_, _) => color::YELLOW,
            JudgeOutput::UnexpectedIoError(_) => color::RED,
        };
        println_decorated!(Attr::Bold, Some(color), "{:?}", self);
    }

    fn print_failure_detail(&self) {
        fn writeln_error_trimming_last_newline(s: &str) {
            if s.chars().last() == Some('\n') {
                write!(io::stderr(), "{}", s).unwrap();
                io::stderr().flush().unwrap();
            } else {
                writeln!(io::stderr(), "{}", s).unwrap();
            }
        }

        match *self {
            JudgeOutput::Ac(_) => {}
            JudgeOutput::Tle(t) => {
                eprintln_decorated!(Attr::Bold, Some(color::RED), "Timelimit exceeded ({}ms)", t);
            }
            JudgeOutput::Wa(_, ref expected, ref actual) => {
                eprintln_decorated!(Attr::Bold, Some(color::MAGENTA), "expected:");
                writeln_error_trimming_last_newline(expected);
                eprintln_decorated!(Attr::Bold, Some(color::MAGENTA), "actual:");
                writeln_error_trimming_last_newline(actual);
            }
            JudgeOutput::Re(_, ref message) => {
                writeln_error_trimming_last_newline(message);
            }
            JudgeOutput::UnexpectedIoError(ref e) => {
                writeln_error_trimming_last_newline(&format!("{}", e));
            }
        }
    }
}
