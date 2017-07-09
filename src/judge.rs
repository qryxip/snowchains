use super::error::{JudgeError, JudgeResult};
use super::util::UnwrapAsRefMut;
use serde_json;
use std::convert::From;
use std::fmt::{self, Display, Formatter};
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::path::Path;
use std::process::{Command, ExitStatus, Stdio};
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};
use term::{Attr, color};
use toml;

pub fn judge(cases: &str, target: &str, args: &[&str]) -> JudgeResult<()> {
    Cases::load(Path::new(cases))?
        .judge_all(Path::new(target), args)
}


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
        print_decorated!(Attr::Bold, None, "{}/{} ", i + 1, n);

        match *self {
            JudgeOutput::Ac(t) => {
                println_decorated!(Attr::Bold, Some(color::GREEN), "AC ({}ms)", t);
            }
            JudgeOutput::Tle(t) => {
                println_decorated!(Attr::Bold, Some(color::RED), "TLE ({}ms)", t);
            }
            JudgeOutput::Wa(t, _, _) => {
                println_decorated!(Attr::Bold, Some(color::YELLOW), "WA ({}ms)", t);
            }
            JudgeOutput::Re(status, _) => {
                println_decorated!(Attr::Bold, Some(color::YELLOW), "RE ({})", status);
            }
            JudgeOutput::UnexpectedIoError(ref e) => {
                println_decorated!(Attr::Bold,
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


#[derive(Serialize, Deserialize)]
pub struct Cases {
    timeout: u64,
    cases: Vec<Case>,
}

impl Cases {
    pub fn from_text(timeout: u64, cases: Vec<(String, String)>) -> Self {
        Self {
            timeout: timeout,
            cases: cases
                .into_iter()
                .map(|(expected, input)| {
                         Case {
                             expected: NonNestedValue::NonArray(NonArrayValue::String(expected)),
                             input: NonNestedValue::NonArray(NonArrayValue::String(input)),
                         }
                     })
                .collect(),
        }
    }

    pub fn load(path: &Path) -> JudgeResult<Self> {
        let mut file = File::open(path)?;
        let mut buf = String::new();
        file.read_to_string(&mut buf)?;
        match path.extension() {
            Some(ref ext) if *ext == "json" => Ok(serde_json::from_str(&buf)?),
            Some(ref ext) if *ext == "toml" => Ok(toml::from_str(&buf)?),
            Some(ref ext) => Err(JudgeError::UnsupportedExtension(format!("{:?}", ext))),
            _ => Err(JudgeError::UnsupportedExtension(format!("no extension"))),
        }
    }

    pub fn save(&self, path: &Path) -> JudgeResult<()> {
        fs::create_dir_all(path.parent().unwrap())?;
        let mut file = File::create(path)?;
        let serialized = match path.extension() {
            Some(ref ext) if *ext == "json" => serde_json::to_string(self).unwrap(),
            Some(ref ext) if *ext == "toml" => toml::to_string(self).unwrap(),
            Some(ref ext) => return Err(JudgeError::UnsupportedExtension(format!("{:?}", ext))),
            _ => return Err(JudgeError::UnsupportedExtension(format!("no extension"))),
        };
        Ok(file.write_all(serialized.as_bytes())?)
    }

    pub fn judge_all(self, target: &Path, args: &[&str]) -> JudgeResult<()> {
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
            let args = args.iter().map(|&arg| arg.into()).collect();
            let output = case.judge(target, args, timeout);
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


#[derive(Serialize, Deserialize)]
struct Case {
    expected: NonNestedValue,
    input: NonNestedValue,
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


#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum NonNestedValue {
    Array(Vec<NonArrayValue>),
    NonArray(NonArrayValue),
}

impl Into<String> for NonNestedValue {
    fn into(self) -> String {
        use std::fmt::Write;

        fn as_lines<T: Display>(a: &[T]) -> String {
            let mut result = String::new();
            for x in a {
                writeln!(result, "{}", x).unwrap();
            }
            result
        }

        match self {
            NonNestedValue::Array(a) => as_lines(&a),
            NonNestedValue::NonArray(NonArrayValue::Integer(n)) => format!("{}\n", n),
            NonNestedValue::NonArray(NonArrayValue::Float(v)) => format!("{}\n", v),
            NonNestedValue::NonArray(NonArrayValue::String(mut s)) => {
                if s.chars().last() != Some('\n') {
                    s.push('\n');
                }
                s
            }
        }
    }
}


#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum NonArrayValue {
    Integer(i64),
    Float(f64),
    String(String),
}

impl Display for NonArrayValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            NonArrayValue::Integer(n) => write!(f, "{}", n),
            NonArrayValue::Float(v) => write!(f, "{}", v),
            NonArrayValue::String(ref s) => write!(f, "{}", s),
        }
    }
}
