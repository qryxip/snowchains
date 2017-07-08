use super::judge::JudgeOutput;
use cookie;
use reqwest::{self, StatusCode, UrlError};
use serde_json;
use std::env;
use std::io::{self, Write};
use std::process;
use term::{Attr, color};
use toml;

pub trait OrExit1 {
    type Item;
    /// If `self` is `Err`, prints the error messages and exit with code 1.
    fn or_exit1(self) -> Self::Item;
}

impl<T, E: PrintErrorDetails> OrExit1 for Result<T, E> {
    type Item = T;
    fn or_exit1(self) -> T {
        match self {
            Ok(x) => x,
            Err(e) => {
                e.print_error_details();
                process::exit(1);
            }
        }
    }
}


pub trait PrintErrorDetails {
    fn print_error_details(&self);
}


pub type ServiceResult<T> = Result<T, ServiceError>;


#[derive(Debug)]
pub enum ServiceError {
    ScrapingFailed,
    UnexpectedHttpCode(StatusCode),
    Reqwest(reqwest::Error),
    Url(UrlError),
    Io(io::Error),
    SerdeJson(serde_json::Error),
    CookieParse(cookie::ParseError),
}

impl PrintErrorDetails for ServiceError {
    fn print_error_details(&self) {

        write_error_decorated!(Attr::Bold, Some(color::RED), "error:");
        writeln!(io::stderr(), " {:?}", self).unwrap();
        unimplemented!();
    }
}

impl From<reqwest::Error> for ServiceError {
    fn from(from: reqwest::Error) -> Self {
        ServiceError::Reqwest(from)
    }
}

impl From<UrlError> for ServiceError {
    fn from(from: UrlError) -> Self {
        ServiceError::Url(from)
    }
}

impl From<io::Error> for ServiceError {
    fn from(from: io::Error) -> Self {
        ServiceError::Io(from)
    }
}

impl From<serde_json::Error> for ServiceError {
    fn from(from: serde_json::Error) -> Self {
        ServiceError::SerdeJson(from)
    }
}

impl From<cookie::ParseError> for ServiceError {
    fn from(from: cookie::ParseError) -> Self {
        ServiceError::CookieParse(from)
    }
}


pub type JudgeResult<T> = Result<T, JudgeError>;


pub enum JudgeError {
    ProjectNotFound,
    BuildFailed,
    UnsupportedExtension(String),
    DeserializationFailed(String),
    Io(io::Error),
    TestFailed(Vec<JudgeOutput>),
}

impl PrintErrorDetails for JudgeError {
    fn print_error_details(&self) {
        use std::io::Write;

        let (attr, color) = (Attr::Bold, Some(color::RED));
        match *self {
            JudgeError::TestFailed(_) => {}
            _ => write_error_decorated!(attr, color, "error: "),
        }
        match *self {
            JudgeError::ProjectNotFound => {
                writeln!(io::stderr(),
                         "could not find `Cargo.toml` in `{}` or any parent directory",
                         env::current_dir()
                             .unwrap_or_default()
                             .to_str()
                             .unwrap_or_default())
                    .unwrap();
            }
            JudgeError::BuildFailed => {
                writeln!(io::stderr(), "aborted because the build failed").unwrap();
            }
            JudgeError::UnsupportedExtension(ref extension) => {
                writeln!(io::stderr(), "unsupported format: {}", extension).unwrap();
            }
            JudgeError::DeserializationFailed(ref s) => {
                writeln_error_decorated!(attr, color, "deserialization faild:");
                writeln!(io::stderr(), "{}", s).unwrap();
            }
            JudgeError::Io(ref e) => {
                writeln!(io::stderr(), "{}", e).unwrap();
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

impl From<serde_json::Error> for JudgeError {
    fn from(from: serde_json::Error) -> Self {
        JudgeError::DeserializationFailed(format!("{}", from))
    }
}

impl From<toml::de::Error> for JudgeError {
    fn from(from: toml::de::Error) -> Self {
        JudgeError::DeserializationFailed(format!("{}", from))
    }
}
