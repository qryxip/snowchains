use crate::path::AbsPathBuf;
use crate::template::Tokens;

use snowchains_proc_macros::{DoubleFrom, FailPair, PartialFailPair};

use chrono::{DateTime, Local};
use derive_new::new;
use failure::{Backtrace, Fail};
use itertools::Itertools;
use reqwest::header::{HeaderName, HeaderValue};
use reqwest::StatusCode;
use url::Url;
use zip::result::ZipError;

use std::ffi::OsString;
use std::process::ExitStatus;
use std::{fmt, io};

pub type Result<T> = std::result::Result<T, self::Error>;

#[derive(DoubleFrom, Debug, PartialFailPair)]
pub enum Error {
    Context(failure::Context<self::ErrorKind>),
    Service(ServiceError),
    Judge(JudgeError),
    TestSuite(TestSuiteError),
    Config(ConfigError),
    ExpandTemplate(ExpandTemplateError),
    File(FileError),
    #[double_from = "io::Error"]
    Io(StdError<io::Error>),
}

#[derive(Debug, derive_more::Display)]
pub enum ErrorKind {
    #[display(fmt = "Sorry, not yet implemented")]
    Unimplemented,
}

pub(crate) type ServiceResult<T> = std::result::Result<T, ServiceError>;

#[derive(DoubleFrom, Debug, PartialFailPair)]
pub enum ServiceError {
    Context(failure::Context<ServiceErrorKind>),
    Scrape(ScrapeError),
    TestSuite(TestSuiteError),
    ExpandTemplate(ExpandTemplateError),
    File(FileError),
    #[double_from = "reqwest::Error"]
    Reqwest(StdError<reqwest::Error>),
    #[double_from = "serde_urlencoded::ser::Error"]
    SerdeUrlencodedSer(StdError<serde_urlencoded::ser::Error>),
    #[double_from = "ZipError"]
    Zip(StdError<ZipError>),
    #[double_from = "io::Error"]
    Io(StdError<io::Error>),
}

#[derive(Debug, derive_more::Display)]
pub enum ServiceErrorKind {
    #[display(
        fmt = "Received non UTF-8 content (encoding = {:?})",
        r#"_0.as_ref().map(String::as_str).unwrap_or("<none>")"#
    )]
    NonUtf8Content(Option<String>),
    #[display(fmt = "Failed to parse a URL: {:?}", _0)]
    ParseUrl(String),
    #[display(fmt = "Failed to parse a cookie in {}: {:?}", "_0.display()", _1)]
    ParseCookieFromPath(AbsPathBuf, String),
    #[display(fmt = "Failed to parse a cookie from {}: {:?}", _0, _1)]
    ParseCookieFromUrl(Url, HeaderValue),
    #[display(fmt = "The response does not contain {:?} header", _0)]
    HeaderMissing(HeaderName),
    #[display(fmt = "Failed to read {:?} header", _0)]
    ReadHeader(HeaderName),
    #[display(fmt = "Forbidden by the \"robots.txt\"")]
    ForbiddenByRobotsTxt,
    #[display(
        fmt = "{}: Unexpected HTTP status code {} (expected [{}])",
        _0,
        _1,
        "_2.iter().format(\", \")"
    )]
    UnexpectedStatusCode(Url, StatusCode, Vec<StatusCode>),
    #[display(
        fmt = "The default browser terminated abnormally {}",
        r#"match _0.code() {
               Some(c) => format!("with code {}", c),
               None => "without code".to_owned(),
           }"#
    )]
    Webbrowser(ExitStatus),
    #[display(fmt = r#"Found an accepted submission. Add "--skip-checking-duplication" ("-d")"#)]
    AlreadyAccepted,
    #[display(fmt = "{} will begin at {}", _0, _1)]
    ContestNotBegun(String, DateTime<Local>),
    #[display(fmt = "{} not found", _0)]
    ContestNotFound(String),
    #[display(fmt = "Please specify problem")]
    PleaseSpecifyProblems,
    #[display(fmt = "No such problem: {:?}", _0)]
    NoSuchProblem(String),
    #[display(fmt = "Failed to recognize language by {:?}", _0)]
    RecognizeByExtension(String),
    #[display(
        fmt = "Submission rejected: language={:?}, size={}, status={}, location={}",
        _0,
        _1,
        "_2.as_u16()",
        r#"_3.as_ref().map(|s| format!("{:?}", s)).unwrap_or_else(|| "<none>".to_owned())"#
    )]
    SubmissionRejected(String, usize, StatusCode, Option<String>),
    #[display(fmt = "Failed to login")]
    LoginOnTest,
}

pub(crate) type ScrapeResult<T> = std::result::Result<T, ScrapeError>;

#[derive(Default, Debug, derive_more::Display, Fail, new)]
#[display(fmt = "Failed to scrape")]
pub struct ScrapeError {
    #[new(default)]
    #[fail(backtrace)]
    backtrace: Backtrace,
}

pub(crate) type JudgeResult<T> = std::result::Result<T, JudgeError>;

#[derive(DoubleFrom, Debug, PartialFailPair)]
pub enum JudgeError {
    Context(failure::Context<JudgeErrorKind>),
    TestSuite(TestSuiteError),
    Config(ConfigError),
    ExpandTemplate(ExpandTemplateError),
    File(FileError),
    #[double_from = "io::Error"]
    Io(StdError<io::Error>),
}

#[derive(Debug, derive_more::Display)]
pub enum JudgeErrorKind {
    #[display(fmt = "The length is {} but the index is {}", _0, _1)]
    IndexOutOfBounds(usize, usize),
    #[display(fmt = "Expected \"simple\" case")]
    ExpectedSimple,
    #[display(fmt = "Failed to execute {:?}", _0)]
    Command(OsString),
    #[display(
        fmt = "The {} command terminated abnormally {}",
        noun,
        r#"match status.code() {
             Some(c) => format!("with code {}", c),
             None => "without code".to_owned(),
           }"#
    )]
    Build {
        noun: &'static str,
        status: ExitStatus,
    },
    #[display(
        fmt = "{}/{} Test{} failed",
        _0,
        _1,
        r#"if *_0 > 0 { "s" } else { "" }"#
    )]
    TestFailed(usize, usize),
}

pub(crate) type TestSuiteResult<T> = std::result::Result<T, TestSuiteError>;

#[derive(DoubleFrom, Debug, PartialFailPair)]
pub enum TestSuiteError {
    Context(failure::Context<TestSuiteErrorKind>),
    Config(ConfigError),
    ExpandTemplate(ExpandTemplateError),
    File(FileError),
    #[double_from = "io::Error"]
    Io(StdError<io::Error>),
}

#[derive(Debug, derive_more::Display)]
pub enum TestSuiteErrorKind {
    #[display(fmt = "Failed to serialize the data")]
    Serialize,
    #[display(fmt = "None of {} exists. Execute \"download\" command first", _0)]
    NoFile(String),
    #[display(fmt = "Different types of suites")]
    DifferentTypesOfSuites,
    #[display(fmt = "Target suite is not \"simple\" type")]
    SuiteIsNotSimple,
    #[display(fmt = "{:?} is unsubmittable", _0)]
    Unsubmittable(AbsPathBuf),
    #[display(fmt = "Unsupported extension: {:?}", _0)]
    UnsupportedExtension(String),
}

pub(crate) type ConfigResult<T> = std::result::Result<T, ConfigError>;

#[derive(Debug, derive_more::Display, Fail)]
#[display(fmt = "{}", kind)]
pub struct ConfigError {
    kind: ConfigErrorKind,
    #[fail(backtrace)]
    backtrace: Backtrace,
}

impl From<ConfigErrorKind> for ConfigError {
    fn from(kind: ConfigErrorKind) -> Self {
        Self {
            kind,
            backtrace: Backtrace::new(),
        }
    }
}

#[derive(Debug, derive_more::Display)]
pub enum ConfigErrorKind {
    #[display(fmt = "Language not specified")]
    LanguageNotSpecified,
    #[display(fmt = "No such language: {:?}", _0)]
    NoSuchLanguage(String),
    #[display(fmt = "Property not set: {:?}", _0)]
    PropertyNotSet(&'static str),
}

pub(crate) type ExpandTemplateResult<T> = std::result::Result<T, ExpandTemplateError>;

#[derive(Debug, FailPair)]
pub struct ExpandTemplateError(failure::Context<ExpandTemplateErrorKind>);

#[derive(Debug, derive_more::Display, Fail)]
pub enum ExpandTemplateErrorKind {
    #[display(
        fmt = "Failed to expand ({:?} % {:?}) as a non UTF-8 string",
        tokens,
        problem
    )]
    OsStr { tokens: Tokens, problem: String },
    #[display(
        fmt = "Failed to expand ({} </> ({:?} % {:?})) as a non UTF-8 string",
        "base_dir.display()",
        tokens,
        problem
    )]
    Path {
        tokens: Tokens,
        problem: String,
        base_dir: AbsPathBuf,
    },
    #[display(fmt = "{:?} not found in the config", _0)]
    NoSuchShell(String),
    #[display(
        fmt = "Unknown specifier {:?}: expected \"\", \"lower\", \"upper\", \"kebab\", \
               \"snake\", \"screaming\", \"mixed\", \"pascal\" or \"title\"",
        _0
    )]
    UnknownSpecifier(String),
    #[display(fmt = "The environment variable {} is not present", _0)]
    EnvVarNotPresent(String),
    #[display(fmt = "The environment variable {} is not valid unicode", _0)]
    EnvVarNotUnicode(String),
}

pub(crate) type FileResult<T> = std::result::Result<T, FileError>;

#[derive(DoubleFrom, Debug, PartialFailPair)]
pub enum FileError {
    Context(failure::Context<FileErrorKind>),
    #[double_from = "io::Error"]
    Io(StdError<io::Error>),
}

#[derive(Debug, derive_more::Display)]
pub enum FileErrorKind {
    #[display(fmt = "Failed to read {}", "_0.display()")]
    Read(AbsPathBuf),
    #[display(fmt = "Failed to write to {}", "_0.display()")]
    Write(AbsPathBuf),
    #[display(fmt = "Failed to create {}", "_0.display()")]
    CreateDir(AbsPathBuf),
    #[display(fmt = "Failed to open {} in read-only mode", "_0.display()")]
    OpenRo(AbsPathBuf),
    #[display(fmt = "Failed to open/create {} in write-only mode", "_0.display()")]
    OpenWo(AbsPathBuf),
    #[display(fmt = "Failed to open/create {} in read/write mode", "_0.display()")]
    OpenRw(AbsPathBuf),
    #[display(fmt = "Failed to lock {}", "_0.display()")]
    Lock(AbsPathBuf),
    #[display(fmt = "Failed to read a zip file")]
    ReadZip,
    #[display(
        fmt = "Could not find {:?} in {} or any parent directory",
        filename,
        "start.display()"
    )]
    Find {
        filename: &'static str,
        start: AbsPathBuf,
    },
}

#[derive(Debug, derive_more::Display)]
#[display(fmt = "{}", messages)]
pub struct StdError<E: std::error::Error + Send + Sync + 'static> {
    inner: E,
    messages: Messages,
    backtrace: Backtrace,
}

impl<E: std::error::Error + Send + Sync + 'static> From<E> for StdError<E> {
    fn from(from: E) -> Self {
        let messages_rev = {
            let mut messages = vec![from.to_string()];
            let mut cause = std::error::Error::cause(&from);
            while let Some(next) = cause {
                messages.push(next.to_string());
                cause = next.cause();
            }
            for i in (0..messages.len()).rev() {
                let duplicated = format!(": {}", messages[i]);
                if messages[0..i].iter().all(|m| m.ends_with(&duplicated)) {
                    for message in &mut messages[0..i] {
                        let at = message.len() - duplicated.len();
                        message.split_off(at);
                    }
                }
            }
            messages.into_iter().rev()
        };
        let mut messages = None;
        for message in messages_rev {
            messages = Some(Messages {
                message,
                next: messages.map(Box::new),
            });
        }
        Self {
            inner: from,
            messages: messages.unwrap(),
            backtrace: Backtrace::new(),
        }
    }
}

impl<E: std::error::Error + Send + Sync + 'static> Fail for StdError<E> {
    fn cause(&self) -> Option<&dyn Fail> {
        self.messages.cause()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        Some(&self.backtrace)
    }
}

#[derive(derive_more::Display)]
#[display(fmt = "{}", message)]
struct Messages {
    message: String,
    next: Option<Box<Messages>>,
}

impl fmt::Debug for Messages {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{:?}", self.message)?;
        let mut next = self.next.as_ref();
        loop {
            if next.is_none() {
                break write!(f, "]");
            } else {
                write!(f, ", {:?}", next.unwrap().message)?;
                next = next.unwrap().next.as_ref();
            }
        }
    }
}

impl Fail for Messages {
    fn cause(&self) -> Option<&dyn Fail> {
        self.next.as_ref().map(|n| n.as_ref() as &dyn Fail)
    }
}

#[cfg(test)]
mod tests {
    use super::StdError;

    use derive_new::new;
    use failure::Fail;

    use std::fmt;

    #[test]
    fn test_std_error() {
        #[derive(Debug, new)]
        struct E {
            value: &'static str,
            #[new(default)]
            source: Option<Box<E>>,
        }

        impl E {
            fn chain(self, value: &'static str) -> Self {
                Self {
                    value,
                    source: Some(Box::new(self)),
                }
            }
        }

        impl fmt::Display for E {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str(self.value)
            }
        }

        impl std::error::Error for E {
            fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
                self.source
                    .as_ref()
                    .map(|s| s as &(dyn std::error::Error + 'static))
            }
        }

        let err = E::new("foo").chain("bar").chain("baz").chain("qux");
        let err = StdError::from(err);
        assert_eq!(
            Fail::iter_chain(&err)
                .map(ToString::to_string)
                .collect::<Vec<_>>(),
            vec![
                "qux".to_owned(),
                "baz".to_owned(),
                "bar".to_owned(),
                "foo".to_owned(),
            ],
        );

        let err = E::new("failed to lookup address information: Name or service not known");
        let err = err.chain(
            "http://example.com/: an error occurred trying to connect: \
             failed to lookup address information: Name or service not known",
        );
        let err = StdError::from(err);
        assert_eq!(
            Fail::iter_chain(&err)
                .map(ToString::to_string)
                .collect::<Vec<_>>(),
            vec![
                "http://example.com/: an error occurred trying to connect".to_owned(),
                "failed to lookup address information: Name or service not known".to_owned(),
            ],
        );
    }
}
