use super::error::{JudgeError, JudgeResult};
use super::util::UnwrapAsRefMut;
use std::convert::From;
use std::fmt::Display;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::process::{Command, ExitStatus, Stdio};
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};
use term::{Attr, color};

pub enum JudgeOutput {
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

impl JudgeOutput {
    pub fn print_title(&self, i: usize, n: usize) {
        for _ in 0..format!("{}", n).len() - format!("{}", i + 1).len() {
            print!(" ");
        }
        write_decorated!(Attr::Bold, None, "{}/{} ", i + 1, n);

        match *self {
            JudgeOutput::Ac(t) => {
                writeln_decorated!(Attr::Bold, Some(color::GREEN), "AC ({}ms)", t);
            }
            JudgeOutput::Tle(t) => {
                writeln_decorated!(Attr::Bold, Some(color::RED), "TLE ({}ms)", t);
            }
            JudgeOutput::Wa(t, _, _) => {
                writeln_decorated!(Attr::Bold, Some(color::YELLOW), "WA ({}ms)", t);
            }
            JudgeOutput::Re(status, _) => {
                writeln_decorated!(Attr::Bold, Some(color::YELLOW), "RE ({})", status);
            }
            JudgeOutput::UnexpectedIoError(ref e) => {
                writeln_decorated!(Attr::Bold,
                                   Some(color::RED),
                                   "UNEXPECTED IO ERROR ({:?})",
                                   e.kind());
            }
        }
    }

    pub fn print_failure_detail(&self) {
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
                writeln_error_decorated!(Attr::Bold,
                                         Some(color::RED),
                                         "Timelimit exceeded ({}ms)",
                                         t);
            }
            JudgeOutput::Wa(_, ref expected, ref actual) => {
                writeln_error_decorated!(Attr::Bold, Some(color::MAGENTA), "expected:");
                writeln_error_trimming_last_newline(expected);
                writeln_error_decorated!(Attr::Bold, Some(color::MAGENTA), "actual:");
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


#[derive(Deserialize)]
pub struct Cases {
    timeout: u64,
    cases: Vec<Case>,
}

impl Cases {
    pub fn judge_all(self, target: PathBuf) -> JudgeResult<()> {
        let timeout = self.timeout;
        let num_tests = self.cases.len();
        let suf = if num_tests > 1 { "s" } else { "" };
        let mut outputs = Vec::new();
        let mut failed = false;

        println!("\nRunning {} test{}...", num_tests, suf);
        for (i, case) in self.cases.into_iter().enumerate() {
            let target = target
                .to_str()
                .map(|s| s.to_string())
                .ok_or(JudgeError::Io(io::ErrorKind::InvalidInput.into()))?;
            let output = case.judge(target, vec![], timeout);
            match output {
                JudgeOutput::Ac(_) => {}
                _ => failed = true,
            }
            output.print_title(i, num_tests);
            outputs.push(output);
        }

        if failed {
            Err(JudgeError::TestFailed(outputs))
        } else {
            println!("All of the {} test{} passed.", num_tests, suf);
            Ok(())
        }
    }
}


#[derive(Deserialize)]
struct Case {
    expected: NonNestedTomlValue,
    input: NonNestedTomlValue,
}

impl Case {
    fn judge(self, program: String, args: Vec<String>, timeout: u64) -> JudgeOutput {
        fn run_program(case: Case, program: String, args: Vec<String>) -> JudgeOutput {
            fn go(case: Case, program: String, args: Vec<String>) -> io::Result<JudgeOutput> {
                let input: String = case.input.into();
                let expected: String = case.expected.into();
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
                    let elapsed = start.elapsed();
                    (1000000000 * elapsed.as_secs() + elapsed.subsec_nanos() as u64) / 1000000
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
        thread::spawn(move || { let _ = tx.send(run_program(self, program, args)); });
        match rx.recv_timeout(Duration::from_millis(timeout + 50)) {
            Ok(JudgeOutput::Ac(t)) if t > timeout => JudgeOutput::Tle(timeout),
            Ok(output) => output,
            Err(_) => JudgeOutput::Tle(timeout),
        }
    }
}


#[derive(Deserialize)]
#[serde(untagged)]
enum NonNestedTomlValue {
    IntegerArray(Vec<i64>),
    FloatArray(Vec<f64>),
    StringArray(Vec<String>),
    Integer(i64),
    Float(f64),
    String(String),
}

impl Into<String> for NonNestedTomlValue {
    fn into(self) -> String {
        use std::fmt::Write;

        fn concat_all<T: Display>(a: Vec<T>) -> String {
            let mut result = String::new();
            for x in a {
                writeln!(result, "{}", x).unwrap();
            }
            result
        }

        match self {
            NonNestedTomlValue::IntegerArray(a) => concat_all(a),
            NonNestedTomlValue::FloatArray(a) => concat_all(a),
            NonNestedTomlValue::StringArray(a) => concat_all(a),
            NonNestedTomlValue::Integer(n) => format!("{}\n", n),
            NonNestedTomlValue::Float(v) => format!("{}\n", v),
            NonNestedTomlValue::String(mut s) => {
                if s.chars().last() != Some('\n') {
                    s.push('\n');
                }
                s
            }
        }
    }
}
