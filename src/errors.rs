use chrono::{DateTime, Local};
use crate::path::AbsPathBuf;
use crate::template::Tokens;
use failure::{Context, Fail};
use itertools::Itertools as _Itertools;
use reqwest::header::{HeaderValue, InvalidHeaderValue};
use reqwest::StatusCode;
use url::Url;
use zip::result::ZipError;

use std::ffi::OsString;
use std::process::ExitStatus;
use std::string::FromUtf8Error;
use std::{fmt, io};

pub type Result<T> = std::result::Result<T, self::Error>;

#[derive(Debug)]
pub enum Error {
    Service(ServiceError),
    Judge(JudgeError),
    SuiteFile(SuiteFileError),
    LoadConfig(LoadConfigError),
    ExpandTemplate(ExpandTemplateError),
    Io(StdErrorWithDisplayChain<io::Error>),
    Unimplemented,
}

impl fmt::Display for self::Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            self::Error::Service(e) => write!(f, "{}", e),
            self::Error::Judge(e) => write!(f, "{}", e),
            self::Error::SuiteFile(e) => write!(f, "{}", e),
            self::Error::LoadConfig(e) => write!(f, "{}", e),
            self::Error::ExpandTemplate(e) => write!(f, "{}", e),
            self::Error::Io(e) => write!(f, "{}", e),
            self::Error::Unimplemented => write!(f, "Sorry, not yet implemented"),
        }
    }
}

impl Fail for self::Error {
    fn cause(&self) -> Option<&dyn Fail> {
        match self {
            crate::Error::Service(e) => e.cause(),
            crate::Error::Judge(e) => e.cause(),
            crate::Error::SuiteFile(e) => e.cause(),
            crate::Error::LoadConfig(e) => e.cause(),
            crate::Error::ExpandTemplate(e) => e.cause(),
            crate::Error::Io(e) => e.cause(),
            _ => None,
        }
    }
}

derive_from!(
    Error::Service <- ServiceError,
    Error::Judge <- JudgeError,
    Error::SuiteFile <- SuiteFileError,
    Error::LoadConfig <- LoadConfigError,
    Error::ExpandTemplate <- ExpandTemplateError,
    Error::Io <- io::Error,
);

pub(crate) type ServiceResult<T> = std::result::Result<T, ServiceError>;

#[derive(Debug)]
pub enum ServiceError {
    Session(SessionError),
    CodeReplace(CodeReplaceError),
    SuiteFile(SuiteFileError),
    ExpandTemplate(ExpandTemplateError),
    Submit(SubmitError),
    ChronoParse(StdErrorWithDisplayChain<chrono::ParseError>),
    Reqwest(StdErrorWithDisplayChain<reqwest::Error>),
    ToStr(StdErrorWithDisplayChain<reqwest::header::ToStrError>),
    SerdeUrlencodedSer(StdErrorWithDisplayChain<serde_urlencoded::ser::Error>),
    Zip(StdErrorWithDisplayChain<ZipError>),
    Io(StdErrorWithDisplayChain<io::Error>),
    AlreadyAccepted,
    ContestNotBegun(String, DateTime<Local>),
    ContestNotFound(String),
    PleaseSpecifyProblems,
    Scrape,
    UnexpectedRedirection(String),
    LoginOnTest,
}

impl fmt::Display for ServiceError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ServiceError::Session(e) => write!(f, "{}", e),
            ServiceError::CodeReplace(e) => write!(f, "{}", e),
            ServiceError::SuiteFile(e) => write!(f, "{}", e),
            ServiceError::ExpandTemplate(e) => write!(f, "{}", e),
            ServiceError::Submit(e) => write!(f, "{}", e),
            ServiceError::ChronoParse(e) => write!(f, "{}", e),
            ServiceError::Reqwest(e) => write!(f, "{}", e),
            ServiceError::ToStr(e) => write!(f, "{}", e),
            ServiceError::SerdeUrlencodedSer(e) => write!(f, "{}", e),
            ServiceError::Zip(e) => write!(f, "{}", e),
            ServiceError::Io(e) => write!(f, "{}", e),
            ServiceError::AlreadyAccepted => write!(
                f,
                "Found an accepted submission. Add \"--skip-checking-duplication\" (\"-d\")"
            ),
            ServiceError::ContestNotBegun(s, t) => write!(f, "{} will begin at {}", s, t),
            ServiceError::ContestNotFound(s) => write!(f, "{} not found", s),
            ServiceError::PleaseSpecifyProblems => write!(f, "Please specify problems"),
            ServiceError::Scrape => write!(f, "Failed to scrape"),
            ServiceError::UnexpectedRedirection(u) => write!(f, "Unexpected redirection to {}", u),
            ServiceError::LoginOnTest => write!(f, "Failed to login"),
        }
    }
}

derive_from!(
    ServiceError::Session <- SessionError,
    ServiceError::CodeReplace <- CodeReplaceError,
    ServiceError::SuiteFile <- SuiteFileError,
    ServiceError::ExpandTemplate <- ExpandTemplateError,
    ServiceError::Submit <- SubmitError,
    ServiceError::ChronoParse <- chrono::ParseError,
    ServiceError::Reqwest <- reqwest::Error,
    ServiceError::ToStr <- reqwest::header::ToStrError,
    ServiceError::SerdeUrlencodedSer <- serde_urlencoded::ser::Error,
    ServiceError::Zip <- ZipError,
    ServiceError::Io <- io::Error,
);

impl Fail for ServiceError {
    fn cause(&self) -> Option<&dyn Fail> {
        match self {
            ServiceError::Session(e) => e.cause(),
            ServiceError::CodeReplace(e) => e.cause(),
            ServiceError::SuiteFile(e) => e.cause(),
            ServiceError::ExpandTemplate(e) => e.cause(),
            ServiceError::ChronoParse(e) => e.cause(),
            ServiceError::Reqwest(e) => e.cause(),
            ServiceError::ToStr(e) => e.cause(),
            ServiceError::SerdeUrlencodedSer(e) => e.cause(),
            ServiceError::Zip(e) => e.cause(),
            ServiceError::Io(e) => e.cause(),
            _ => None,
        }
    }
}

#[derive(Debug, Fail)]
pub enum SubmitError {
    NoSuchProblem(String),
    Rejected(String, usize, Option<Url>),
}

impl fmt::Display for SubmitError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SubmitError::NoSuchProblem(problem) => write!(f, "No such problem: {:?}", problem),
            SubmitError::Rejected(lang_id, len, None) => write!(
                f,
                "Submission rejected: language={:?}, size={}, location=<none>",
                lang_id, len
            ),
            SubmitError::Rejected(lang_id, len, Some(location)) => write!(
                f,
                "Submission rejected: language={:?}, size={}, location={}",
                lang_id, len, location
            ),
        }
    }
}

pub(crate) type SessionResult<T> = std::result::Result<T, SessionError>;

#[derive(Debug)]
pub enum SessionError {
    Bincode(StdErrorWithDisplayChain<bincode::Error>),
    Reqwest(StdErrorWithDisplayChain<reqwest::Error>),
    InvalidHeaderValue(StdErrorWithDisplayChain<InvalidHeaderValue>),
    ToStr(StdErrorWithDisplayChain<reqwest::header::ToStrError>),
    Io(StdErrorWithDisplayChain<io::Error>),
    Start(Context<StartSessionError>),
    ParseUrl(String, url::ParseError),
    ParseCookieFromPath(String, AbsPathBuf, cookie::ParseError),
    ParseCookieFromUrl(HeaderValue, Url),
    HeaderMissing(&'static str),
    ForbiddenByRobotsTxt,
    UnexpectedStatusCode(Vec<StatusCode>, StatusCode),
    Webbrowser(ExitStatus),
}

derive_from!(
    SessionError::Bincode <- bincode::Error,
    SessionError::Reqwest <- reqwest::Error,
    SessionError::InvalidHeaderValue <- InvalidHeaderValue,
    SessionError::ToStr <- reqwest::header::ToStrError,
    SessionError::Io <- io::Error,
    SessionError::Start <- Context<StartSessionError>,
);

impl fmt::Display for SessionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SessionError::Bincode(e) => write!(f, "{}", e),
            SessionError::Reqwest(e) => write!(f, "{}", e),
            SessionError::InvalidHeaderValue(e) => write!(f, "{}", e),
            SessionError::ToStr(e) => write!(f, "{}", e),
            SessionError::Io(e) => write!(f, "{}", e),
            SessionError::Start(e) => write!(f, "{}", e),
            SessionError::ParseUrl(s, _) => write!(f, "Failed to parse {:?}", s),
            SessionError::ParseCookieFromPath(s, p, _) => {
                write!(f, "Failed to parse {:?} in {}", s, p.display())
            }
            SessionError::ParseCookieFromUrl(s, u) => {
                write!(f, "Failed to parse {:?} from {}", s, u)
            }
            SessionError::HeaderMissing(s) => {
                write!(f, "The response does not contain {:?} header", s)
            }
            SessionError::ForbiddenByRobotsTxt => write!(f, "Forbidden by robots.txt"),
            SessionError::UnexpectedStatusCode(ss, s) => write!(
                f,
                "Unexpected HTTP status code {} (expected [{}])",
                s,
                ss.iter().format(", "),
            ),
            SessionError::Webbrowser(s) => match s.code() {
                Some(c) => write!(
                    f,
                    "The default browser terminated abnormally with code {}",
                    c
                ),
                None => write!(
                    f,
                    "The default browser terminated abnormally without code (possibly killed)"
                ),
            },
        }
    }
}

impl Fail for SessionError {
    fn cause(&self) -> Option<&dyn Fail> {
        match self {
            SessionError::Bincode(e) => e.cause(),
            SessionError::Reqwest(e) => e.cause(),
            SessionError::InvalidHeaderValue(e) => e.cause(),
            SessionError::ToStr(e) => e.cause(),
            SessionError::Io(e) => e.cause(),
            SessionError::Start(e) => e.cause(),
            SessionError::ParseUrl(_, e) => Some(e),
            SessionError::ParseCookieFromPath(_, _, e) => Some(e),
            _ => None,
        }
    }
}

#[derive(Debug, Fail)]
#[fail(display = "Failed to start a session")]
pub struct StartSessionError;

pub(crate) type JudgeResult<T> = std::result::Result<T, JudgeError>;

#[derive(Debug)]
pub enum JudgeError {
    SuiteFile(SuiteFileError),
    LoadConfig(LoadConfigError),
    ExpandTemplate(ExpandTemplateError),
    Io(io::Error),
    IndexOutOfBounds(usize, usize),
    ExpectedSimple,
    Command(OsString, io::Error),
    Compile(ExitStatus),
    TestFailed(usize, usize),
}

derive_from!(
    JudgeError::SuiteFile <- SuiteFileError,
    JudgeError::LoadConfig <- LoadConfigError,
    JudgeError::ExpandTemplate <- ExpandTemplateError,
    JudgeError::Io <- io::Error,
);

impl fmt::Display for JudgeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            JudgeError::SuiteFile(e) => write!(f, "{}", e),
            JudgeError::LoadConfig(e) => write!(f, "{}", e),
            JudgeError::ExpandTemplate(e) => write!(f, "{}", e),
            JudgeError::Io(_) => write!(f, "An IO error occurred"),
            JudgeError::IndexOutOfBounds(l, i) => {
                write!(f, "The length is {} but the index is {}", l, i)
            }
            JudgeError::ExpectedSimple => write!(f, "Expected \"simple\" case"),
            JudgeError::Command(c, _) => write!(f, "Failed to execute: {:?}", c),
            JudgeError::Compile(s) => write!(
                f,
                "The compilation command terminated abnormally {}",
                if let Some(code) = s.code() {
                    format!("with code {}", code)
                } else {
                    "without code".to_owned()
                }
            ),
            JudgeError::TestFailed(n, d) => write!(
                f,
                "{}/{} Test{} failed",
                n,
                d,
                if *n > 0 { "s" } else { "" }
            ),
        }
    }
}

impl Fail for JudgeError {
    fn cause(&self) -> Option<&dyn Fail> {
        match self {
            JudgeError::SuiteFile(e) => e.cause(),
            JudgeError::LoadConfig(e) => e.cause(),
            JudgeError::ExpandTemplate(e) => e.cause(),
            JudgeError::Io(e) => Some(e),
            _ => None,
        }
    }
}

pub(crate) type SuiteFileResult<T> = std::result::Result<T, SuiteFileError>;

#[derive(Debug)]
pub enum SuiteFileError {
    LoadConfig(LoadConfigError),
    ExpandTemplate(ExpandTemplateError),
    SerdeJson(StdErrorWithDisplayChain<serde_json::Error>),
    SerdeYaml(StdErrorWithDisplayChain<serde_yaml::Error>),
    TomlSer(StdErrorWithDisplayChain<toml::ser::Error>),
    Io(io::Error),
    NoFile(String),
    DifferentTypesOfSuites,
    SuiteIsNotSimple,
    Unsubmittable(String),
    RegexGroupOutOfBounds(usize),
    UnsupportedExtension(String),
}

impl fmt::Display for SuiteFileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SuiteFileError::LoadConfig(e) => write!(f, "{}", e),
            SuiteFileError::ExpandTemplate(e) => write!(f, "{}", e),
            SuiteFileError::Io(_) => write!(f, "An IO error occurred"),
            SuiteFileError::SerdeJson(_)
            | SuiteFileError::SerdeYaml(_)
            | SuiteFileError::TomlSer(_) => write!(f, "Failed to serialize"),
            SuiteFileError::NoFile(s) => write!(
                f,
                "None of {} exists. Execute \"download\" command first",
                s
            ),
            SuiteFileError::DifferentTypesOfSuites => write!(f, "Different types of suites"),
            SuiteFileError::SuiteIsNotSimple => write!(f, "Target suite is not \"simple\" type"),
            SuiteFileError::Unsubmittable(p) => write!(f, "{:?} is unsubmittable", p),
            SuiteFileError::RegexGroupOutOfBounds(i) => {
                write!(f, "Regex group out of bounds: {}", i)
            }
            SuiteFileError::UnsupportedExtension(e) => write!(f, "Unsupported extension; {:?}", e),
        }
    }
}

impl Fail for SuiteFileError {
    fn cause(&self) -> Option<&dyn Fail> {
        match self {
            SuiteFileError::LoadConfig(e) => e.cause(),
            SuiteFileError::ExpandTemplate(e) => e.cause(),
            SuiteFileError::SerdeJson(e) => Some(e),
            SuiteFileError::SerdeYaml(e) => Some(e),
            SuiteFileError::TomlSer(e) => Some(e),
            SuiteFileError::Io(e) => Some(e),
            _ => None,
        }
    }
}

derive_from!(
    SuiteFileError::LoadConfig <- LoadConfigError,
    SuiteFileError::ExpandTemplate <- ExpandTemplateError,
    SuiteFileError::SerdeJson <- serde_json::Error,
    SuiteFileError::SerdeYaml <- serde_yaml::Error,
    SuiteFileError::TomlSer <- toml::ser::Error,
    SuiteFileError::Io <- io::Error,
);

pub(crate) type LoadConfigResult<T> = std::result::Result<T, LoadConfigError>;

#[derive(Debug, Fail)]
pub enum LoadConfigError {
    #[fail(display = "Language not specified")]
    LanguageNotSpecified,
    #[fail(display = "No such language: {:?}", _0)]
    NoSuchLanguage(String),
    #[fail(display = "Property not set: {:?}", _0)]
    PropertyNotSet(&'static str),
}

pub(crate) type CodeReplaceResult<T> = std::result::Result<T, CodeReplaceError>;

#[derive(Debug)]
pub enum CodeReplaceError {
    ExpandTemplate(ExpandTemplateError),
    NonUtf8(FromUtf8Error),
    RegexGroupOutOfBounds(usize),
    NoMatch(String),
}

derive_from!(
    CodeReplaceError::ExpandTemplate <- ExpandTemplateError,
    CodeReplaceError::NonUtf8 <- FromUtf8Error,
);

impl fmt::Display for CodeReplaceError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CodeReplaceError::ExpandTemplate(e) => write!(f, "{}", e),
            CodeReplaceError::NonUtf8(_) => write!(f, "The source code is not valid UTF-8"),
            CodeReplaceError::RegexGroupOutOfBounds(i) => {
                write!(f, "Regex group out of bounds: {}", i)
            }
            CodeReplaceError::NoMatch(s) => write!(f, "No match: {:?}", s),
        }
    }
}

impl Fail for CodeReplaceError {
    fn cause(&self) -> Option<&dyn Fail> {
        match self {
            CodeReplaceError::ExpandTemplate(e) => e.cause(),
            CodeReplaceError::NonUtf8(e) => Some(e),
            _ => None,
        }
    }
}

pub(crate) type ExpandTemplateResult<T> = std::result::Result<T, ExpandTemplateError>;

#[derive(Debug)]
pub enum ExpandTemplateError {
    Context(Context<ExpandTemplateErrorContext>),
    Io(StdErrorWithDisplayChain<io::Error>),
    UnknownSpecifier(String),
    EnvVarNotPresent(String),
    EnvVarNotUnicode(String, OsString),
}

derive_from!(
    ExpandTemplateError::Context <- Context<ExpandTemplateErrorContext>,
    ExpandTemplateError::Io <- io::Error,
);

impl fmt::Display for ExpandTemplateError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExpandTemplateError::Context(c) => write!(f, "{}", c),
            ExpandTemplateError::Io(e) => write!(f, "{}", e),
            ExpandTemplateError::UnknownSpecifier(s) => write!(
                f,
                "Unknown specifier {:?}: expected \"\", \"lower\", \"upper\", \"kebab\", \
                 \"snake\", \"screaming\", \"mixed\", \"pascal\" or \"title\"",
                s
            ),
            ExpandTemplateError::EnvVarNotPresent(k) => {
                write!(f, "Environment variable {:?} is not present", k)
            }
            ExpandTemplateError::EnvVarNotUnicode(k, v) => write!(
                f,
                "Environment variable {:?} is not valid unicode: {:?}",
                k, v
            ),
        }
    }
}

impl Fail for ExpandTemplateError {
    fn cause(&self) -> Option<&dyn Fail> {
        match self {
            ExpandTemplateError::Context(c) => c.cause(),
            ExpandTemplateError::Io(e) => e.cause(),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum ExpandTemplateErrorContext {
    Str {
        tokens: Tokens,
        problem: String,
    },
    OsStr {
        tokens: Tokens,
        problem: String,
    },
    Path {
        tokens: Tokens,
        problem: String,
        base_dir: AbsPathBuf,
    },
}

impl fmt::Display for ExpandTemplateErrorContext {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExpandTemplateErrorContext::Str { tokens, problem } => write!(
                f,
                "Failed to expand ({:?} % {:?}) as a UTF-8 string",
                tokens, problem
            ),
            ExpandTemplateErrorContext::OsStr { tokens, problem } => write!(
                f,
                "Failed to expand ({:?} % {:?}) as a non UTF-8 string",
                tokens, problem
            ),
            ExpandTemplateErrorContext::Path {
                tokens,
                problem,
                base_dir,
            } => write!(
                f,
                "Failed to expand ({} </> ({:?} % {:?})) as an absolute path",
                base_dir.display(),
                tokens,
                problem,
            ),
        }
    }
}

#[derive(Debug)]
pub struct StdErrorWithDisplayChain<E: 'static + std::error::Error + Send + Sync> {
    inner: E,
    chain: Option<Box<DisplayChain>>,
}

impl<E: 'static + std::error::Error + Send + Sync> From<E> for StdErrorWithDisplayChain<E> {
    fn from(from: E) -> Self {
        let mut causes_rev = {
            let mut causes = vec![];
            let mut cause: Option<&dyn std::error::Error> = from.cause();
            while let Some(next_cause) = cause {
                causes.push(next_cause.to_string());
                cause = (next_cause as &dyn std::error::Error).cause();
            }
            causes.into_iter().rev()
        };
        let chain = match causes_rev.next() {
            None => None,
            Some(cause) => {
                let mut chain = Box::new(DisplayChain {
                    display: cause.to_string(),
                    next: None,
                });
                for cause in causes_rev {
                    chain = Box::new(DisplayChain {
                        display: cause.to_string(),
                        next: Some(chain),
                    });
                }
                Some(chain)
            }
        };
        Self { inner: from, chain }
    }
}

impl<E: 'static + std::error::Error + Send + Sync> fmt::Display for StdErrorWithDisplayChain<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl<E: 'static + std::error::Error + Send + Sync> Fail for StdErrorWithDisplayChain<E> {
    fn cause(&self) -> Option<&dyn Fail> {
        match self.chain.as_ref() {
            None => None,
            Some(chain) => Some(chain.as_ref()),
        }
    }
}

#[derive(Debug)]
struct DisplayChain {
    display: String,
    next: Option<Box<DisplayChain>>,
}

impl fmt::Display for DisplayChain {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.display)
    }
}

impl Fail for DisplayChain {
    fn cause(&self) -> Option<&dyn Fail> {
        match self.next.as_ref() {
            None => None,
            Some(cause) => Some(cause.as_ref()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::errors::StdErrorWithDisplayChain;

    use failure::Fail;

    use std::fmt;

    #[test]
    fn std_error_with_display_chain_works() {
        #[derive(Debug)]
        struct E {
            value: &'static str,
            cause: Option<Box<E>>,
        }

        impl E {
            fn new(value: &'static str) -> Self {
                Self { value, cause: None }
            }

            fn chain(self, value: &'static str) -> Self {
                Self {
                    value,
                    cause: Some(Box::new(self)),
                }
            }
        }

        impl fmt::Display for E {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "E({:?})", self.value)
            }
        }

        impl std::error::Error for E {
            fn cause(&self) -> Option<&dyn std::error::Error> {
                self.cause.as_ref().map(|c| c as &dyn std::error::Error)
            }
        }

        let e = E::new("foo").chain("bar").chain("baz").chain("qux");
        let e = StdErrorWithDisplayChain::from(e);
        assert_eq!(
            (&e as &Fail)
                .iter_chain()
                .map(ToString::to_string)
                .collect::<Vec<_>>(),
            vec![
                "E(\"qux\")".to_owned(),
                "E(\"baz\")".to_owned(),
                "E(\"bar\")".to_owned(),
                "E(\"foo\")".to_owned(),
            ]
        );
    }
}
