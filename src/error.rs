use super::judge::JudgeOutput;
use std::env;
use std::io;
use std::process;
use term::{Attr, color};
use toml;

pub trait OrExit1 {
    /// If `self` is `Err`, prints the error messages and exit with code 1.
    fn or_exit1(self);
}

impl OrExit1 for JudgeResult<()> {
    fn or_exit1(self) {
        if let Err(e) = self {
            e.print_error_details();
            process::exit(1);
        }
    }
}


pub type JudgeResult<T> = Result<T, JudgeError>;


pub enum JudgeError {
    NotInCrate,
    BuildFailed,
    Io(io::Error),
    DeserializationFailed(String),
    TestFailed(Vec<JudgeOutput>),
}

impl JudgeError {
    fn print_error_details(&self) {
        use std::io::Write;

        let (attr, color) = (Attr::Bold, Some(color::RED));
        match *self {
            JudgeError::TestFailed(_) => {}
            _ => write_error_decorated!(attr, color, "error: "),
        }
        match *self {
            JudgeError::NotInCrate => {
                writeln!(io::stderr(),
                         "could not find `Cargo.toml` in `{}` or any parent directory",
                         env::current_dir()
                             .unwrap_or_default()
                             .to_str()
                             .unwrap_or_default())
                    .unwrap();
            }
            JudgeError::BuildFailed => {
                write_error_decorated!(attr, color, "error: ");
                writeln!(io::stderr(), "aborted because the build failed").unwrap();
            }
            JudgeError::Io(ref e) => {
                write_error_decorated!(attr, color, "io error: ");
                writeln!(io::stderr(), "{}", e).unwrap();
            }
            JudgeError::DeserializationFailed(ref s) => {
                writeln_error_decorated!(attr, color, "deserialization faild:");
                writeln!(io::stderr(), "{}", s).unwrap();
            }
            JudgeError::TestFailed(ref outputs) => {
                writeln!(io::stderr(), "").unwrap();
                let n = outputs.len();
                let mut first = true;
                for (i, output) in outputs.iter().enumerate() {
                    match *output {
                        JudgeOutput::Ac(_) => {}
                        ref output => {
                            if first {
                                first = false;
                            } else {
                                writeln!(io::stderr(), "").unwrap();
                            }
                            output.print_title(i, n);
                            output.print_failure_detail();
                        }
                    }
                }
            }
        }
    }
}

impl From<io::Error> for JudgeError {
    fn from(from: io::Error) -> Self {
        JudgeError::Io(from)
    }
}

impl From<toml::de::Error> for JudgeError {
    fn from(from: toml::de::Error) -> Self {
        JudgeError::DeserializationFailed(format!("{}", from))
    }
}
