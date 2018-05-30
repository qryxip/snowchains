use chrono::{self, DateTime, Local};
use cookie;
use failure::{Context, Fail};
use itertools::Itertools as _Itertools;
use reqwest::{self, StatusCode};
use url::{self, Url};
use zip::result::ZipError;

use std::ffi::OsString;
use std::path::PathBuf;
use std::process::ExitStatus;
use std::string::FromUtf8Error;
use std::sync::mpsc::RecvError;
use std::{self, fmt, io};

pub type Result<T> = std::result::Result<T, self::Error>;

#[derive(Debug)]
pub enum Error {
    Service(ServiceError),
    Judge(JudgeError),
    SuiteFile(SuiteFileError),
    Config(ConfigError),
    TemplateExpand(TemplateExpandError),
    FileIo(FileIoError),
    Std(StdErrorChain),
    Unimplemented,
    HomeDirNotFound,
}

impl fmt::Display for self::Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            self::Error::Service(e) => write!(f, "{}", e),
            self::Error::Judge(e) => write!(f, "{}", e),
            self::Error::SuiteFile(e) => write!(f, "{}", e),
            self::Error::Config(e) => write!(f, "{}", e),
            self::Error::TemplateExpand(e) => write!(f, "{}", e),
            self::Error::FileIo(e) => write!(f, "{}", e),
            self::Error::Std(e) => write!(f, "{}", e),
            self::Error::Unimplemented => write!(f, "Sorry, not yet implemented"),
            self::Error::HomeDirNotFound => write!(f, "Home directory not found"),
        }
    }
}

impl Fail for self::Error {
    fn cause(&self) -> Option<&Fail> {
        match self {
            ::Error::Service(e) => e.cause(),
            ::Error::Judge(e) => e.cause(),
            ::Error::SuiteFile(e) => e.cause(),
            ::Error::Config(e) => e.cause(),
            ::Error::TemplateExpand(e) => e.cause(),
            ::Error::FileIo(e) => e.cause(),
            ::Error::Std(e) => e.cause(),
            _ => None,
        }
    }
}

derive_from!(Error::Service        <- ServiceError);
derive_from!(Error::Judge          <- JudgeError);
derive_from!(Error::SuiteFile      <- SuiteFileError);
derive_from!(Error::Config         <- ConfigError);
derive_from!(Error::TemplateExpand <- TemplateExpandError);
derive_from!(Error::FileIo         <- FileIoError);
derive_from!(Error::Std            <- io::Error);

pub(crate) type ServiceResult<T> = std::result::Result<T, ServiceError>;

#[derive(Debug)]
pub enum ServiceError {
    Session(SessionError),
    CodeReplace(CodeReplaceError),
    SuiteFile(SuiteFileError),
    TemplateExpand(TemplateExpandError),
    FileIo(FileIoError),
    Std(StdErrorChain),
    AlreadyAccepted,
    ContestNotBegun(String, DateTime<Local>),
    ContestNotFound(String),
    NoSuchProblem(String),
    Scrape,
    Webbrowser(ExitStatus),
    WrongCredentialsOnTest,
}

impl fmt::Display for ServiceError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ServiceError::Session(e) => write!(f, "{}", e),
            ServiceError::CodeReplace(e) => write!(f, "{}", e),
            ServiceError::SuiteFile(e) => write!(f, "{}", e),
            ServiceError::TemplateExpand(e) => write!(f, "{}", e),
            ServiceError::FileIo(e) => write!(f, "{}", e),
            ServiceError::Std(e) => write!(f, "{}", e),
            ServiceError::AlreadyAccepted => write!(
                f,
                "Found an accepted submission. Add \"--skip-checking-duplication\" (\"-d\")"
            ),
            ServiceError::ContestNotBegun(s, t) => write!(f, "{} will begin at {}", s, t),
            ServiceError::ContestNotFound(s) => write!(f, "{} not found", s),
            ServiceError::NoSuchProblem(s) => write!(f, "No such problem: {:?}", s),
            ServiceError::Scrape => write!(f, "Failed to scrape"),
            ServiceError::Webbrowser(s) => match s.code() {
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
            ServiceError::WrongCredentialsOnTest => write!(f, "Wrong credentials"),
        }
    }
}

impl Fail for ServiceError {
    fn cause(&self) -> Option<&Fail> {
        match self {
            ServiceError::Session(e) => e.cause(),
            ServiceError::CodeReplace(e) => e.cause(),
            ServiceError::SuiteFile(e) => e.cause(),
            ServiceError::TemplateExpand(e) => e.cause(),
            ServiceError::FileIo(e) => e.cause(),
            ServiceError::Std(e) => e.cause(),
            _ => None,
        }
    }
}

derive_from!(ServiceError::Session        <- SessionError);
derive_from!(ServiceError::CodeReplace    <- CodeReplaceError);
derive_from!(ServiceError::SuiteFile      <- SuiteFileError);
derive_from!(ServiceError::TemplateExpand <- TemplateExpandError);
derive_from!(ServiceError::FileIo         <- FileIoError);
derive_from!(ServiceError::Std            <- chrono::ParseError);
derive_from!(ServiceError::Std            <- reqwest::Error);
derive_from!(ServiceError::Std            <- ZipError);
derive_from!(ServiceError::Std            <- io::Error);

pub(crate) type SessionResult<T> = std::result::Result<T, SessionError>;

#[derive(Debug)]
pub enum SessionError {
    Serialize(SerializeError),
    FileIo(FileIoError),
    Std(StdErrorChain),
    Start(Context<StartSessionError>),
    ParseUrl(String, url::ParseError),
    ParseCookieFromPath(String, PathBuf, cookie::ParseError),
    ParseCookieFromUrl(String, Url, cookie::ParseError),
    ForbiddenByRobotsTxt,
    UnexpectedStatusCode(Vec<StatusCode>, StatusCode),
}

derive_from!(SessionError::Serialize <- SerializeError);
derive_from!(SessionError::FileIo    <- FileIoError);
derive_from!(SessionError::Std       <- reqwest::Error);
derive_from!(SessionError::Start     <- Context<StartSessionError>);

impl fmt::Display for SessionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SessionError::Serialize(e) => write!(f, "{}", e),
            SessionError::FileIo(e) => write!(f, "{}", e),
            SessionError::Std(e) => write!(f, "{}", e),
            SessionError::Start(e) => write!(f, "{}", e),
            SessionError::ParseUrl(s, _) => write!(f, "Failed to parse {:?}", s),
            SessionError::ParseCookieFromPath(s, p, _) => {
                write!(f, "Failed to parse {:?} in {}", s, p.display())
            }
            SessionError::ParseCookieFromUrl(s, u, _) => {
                write!(f, "Failed to parse {:?} from {}", s, u)
            }
            SessionError::ForbiddenByRobotsTxt => write!(f, "Forbidden by robots.txt"),
            SessionError::UnexpectedStatusCode(ss, s) => write!(
                f,
                "Unexpected HTTP status code {} (expected [{}])",
                ss.iter().format(", "),
                s,
            ),
        }
    }
}

impl Fail for SessionError {
    fn cause(&self) -> Option<&Fail> {
        match self {
            SessionError::Serialize(e) => e.cause(),
            SessionError::FileIo(e) => e.cause(),
            SessionError::Std(e) => e.cause(),
            SessionError::Start(e) => e.cause(),
            SessionError::ParseUrl(_, e) => Some(e),
            SessionError::ParseCookieFromPath(_, _, e)
            | SessionError::ParseCookieFromUrl(_, _, e) => Some(e),
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
    FileIo(FileIoError),
    Io(io::Error),
    Recv(RecvError),
    Command(OsString, io::Error),
    Compile(ExitStatus),
    TestFailure(usize, usize),
}

derive_from!(JudgeError::SuiteFile <- SuiteFileError);
derive_from!(JudgeError::FileIo    <- FileIoError);
derive_from!(JudgeError::Io        <- io::Error);
derive_from!(JudgeError::Recv      <- RecvError);

impl fmt::Display for JudgeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            JudgeError::SuiteFile(e) => write!(f, "{}", e),
            JudgeError::FileIo(e) => write!(f, "{}", e),
            JudgeError::Io(_) => write!(f, "An IO error occurred"),
            JudgeError::Recv(e) => write!(f, "{}", e),
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
            JudgeError::TestFailure(n, d) => write!(
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
    fn cause(&self) -> Option<&Fail> {
        match self {
            JudgeError::SuiteFile(e) => e.cause(),
            JudgeError::FileIo(e) => e.cause(),
            JudgeError::Io(e) => Some(e),
            _ => None,
        }
    }
}

pub(crate) type SuiteFileResult<T> = std::result::Result<T, SuiteFileError>;

#[derive(Debug)]
pub enum SuiteFileError {
    Config(ConfigError),
    TemplateExpand(TemplateExpandError),
    Serialize(SerializeError),
    FileIo(FileIoError),
    Io(io::Error),
    DirNotExist(PathBuf),
    NoFile(PathBuf),
    DifferentTypesOfSuites,
    SuiteIsNotSimple,
    Unsubmittable(String),
    RegexGroupOutOfBounds(usize),
    UnsupportedExtension(String),
}

impl fmt::Display for SuiteFileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SuiteFileError::Config(e) => write!(f, "{}", e),
            SuiteFileError::TemplateExpand(e) => write!(f, "{}", e),
            SuiteFileError::Serialize(e) => write!(f, "{}", e),
            SuiteFileError::FileIo(e) => write!(f, "{}", e),
            SuiteFileError::Io(_) => write!(f, "An IO error occurred"),
            SuiteFileError::DirNotExist(d) => write!(
                f,
                "{:?} does not exist. Execute \"download\" command first",
                d
            ),
            SuiteFileError::NoFile(d) => write!(
                f,
                "No test suite file in {:?}. Execute \"download\" command first",
                d
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
    fn cause(&self) -> Option<&Fail> {
        match self {
            SuiteFileError::Config(e) => e.cause(),
            SuiteFileError::TemplateExpand(e) => e.cause(),
            SuiteFileError::Serialize(e) => e.cause(),
            SuiteFileError::FileIo(e) => e.cause(),
            SuiteFileError::Io(e) => Some(e),
            _ => None,
        }
    }
}

derive_from!(SuiteFileError::Config         <- ConfigError);
derive_from!(SuiteFileError::TemplateExpand <- TemplateExpandError);
derive_from!(SuiteFileError::Serialize      <- SerializeError);
derive_from!(SuiteFileError::FileIo         <- FileIoError);
derive_from!(SuiteFileError::Io             <- io::Error);

pub(crate) type ConfigResult<T> = std::result::Result<T, ConfigError>;

#[derive(Debug, Fail)]
pub enum ConfigError {
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
    TemplateExpand(TemplateExpandError),
    NonUtf8(FromUtf8Error),
    RegexGroupOutOfBounds(usize),
    NoMatch(String),
}

derive_from!(CodeReplaceError::TemplateExpand <- TemplateExpandError);
derive_from!(CodeReplaceError::NonUtf8        <- FromUtf8Error);

impl fmt::Display for CodeReplaceError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CodeReplaceError::TemplateExpand(e) => write!(f, "{}", e),
            CodeReplaceError::NonUtf8(_) => write!(f, "The source code is not valid UTF-8"),
            CodeReplaceError::RegexGroupOutOfBounds(i) => {
                write!(f, "Regex group out of bounds: {}", i)
            }
            CodeReplaceError::NoMatch(s) => write!(f, "No match: {:?}", s),
        }
    }
}

impl Fail for CodeReplaceError {
    fn cause(&self) -> Option<&Fail> {
        match self {
            CodeReplaceError::TemplateExpand(e) => e.cause(),
            CodeReplaceError::NonUtf8(e) => Some(e),
            _ => None,
        }
    }
}

pub(crate) type TemplateExpandResult<T> = std::result::Result<T, TemplateExpandError>;

#[derive(Debug)]
pub enum TemplateExpandError {
    Context(Context<TemplateExpandErrorContext>),
    UnknownSpecifier(String),
    EnvVarNotPresent(String),
    EnvVarNotUnicode(String, OsString),
    HomeDirNotFound,
    UnsupportedUseOfTilde,
}

derive_from!(TemplateExpandError::Context <- Context<TemplateExpandErrorContext>);

impl fmt::Display for TemplateExpandError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TemplateExpandError::Context(c) => write!(f, "{}", c),
            TemplateExpandError::UnknownSpecifier(s) => write!(
                f,
                "Unknown specifier {:?}: expected \"\", \"lower\", \"upper\", \"kebab\", \
                 \"snake\", \"screaming\", \"mixed\", \"pascal\" or \"title\"",
                s
            ),
            TemplateExpandError::EnvVarNotPresent(k) => {
                write!(f, "Environment variable {:?} is not present", k)
            }
            TemplateExpandError::EnvVarNotUnicode(k, v) => write!(
                f,
                "Environment variable {:?} is not valid unicode: {:?}",
                k, v
            ),
            TemplateExpandError::HomeDirNotFound => write!(f, "Home directory not found"),
            TemplateExpandError::UnsupportedUseOfTilde => write!(f, "Unsupported use of \"~\""),
        }
    }
}

impl Fail for TemplateExpandError {
    fn cause(&self) -> Option<&Fail> {
        match self {
            TemplateExpandError::Context(c) => c.cause(),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct TemplateExpandErrorContext {
    debug: String,
    target: String,
    ty: &'static str,
}

impl TemplateExpandErrorContext {
    pub(crate) fn new(
        debug: &impl fmt::Debug,
        target: impl Into<String>,
        ty: &'static str,
    ) -> Self {
        Self {
            debug: format!("{:?}", debug),
            target: target.into(),
            ty,
        }
    }
}

impl fmt::Display for TemplateExpandErrorContext {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Failed to expand {} % {:?} as {}",
            self.debug, self.target, self.ty
        )
    }
}

#[derive(Debug, Fail)]
#[fail(display = "Failed to serialize {}", content)]
pub struct SerializeError {
    content: String,
    #[cause]
    cause: StdErrorChain,
}

impl SerializeError {
    pub(crate) fn new(content: impl fmt::Debug, cause: impl std::error::Error) -> Self {
        Self {
            content: format!("{:?}", content),
            cause: cause.into(),
        }
    }
}

pub(crate) type FileIoResult<T> = std::result::Result<T, FileIoError>;

#[derive(Debug)]
pub struct FileIoError {
    kind: FileIoErrorKind,
    path: PathBuf,
    cause: Option<StdErrorChain>,
}

impl FileIoError {
    pub(crate) fn new(kind: FileIoErrorKind, path: impl Into<PathBuf>) -> Self {
        Self {
            kind,
            path: path.into(),
            cause: None,
        }
    }

    pub(crate) fn chaining(
        kind: FileIoErrorKind,
        path: impl Into<PathBuf>,
        cause: impl std::error::Error,
    ) -> Self {
        Self {
            kind,
            path: path.into(),
            cause: Some(cause.into()),
        }
    }

    pub(crate) fn read_zip(path: impl Into<PathBuf>, e: ZipError) -> Self {
        match e {
            ZipError::Io(e) => Self::chaining(FileIoErrorKind::Read, path, e),
            ZipError::InvalidArchive(m) => Self::new(FileIoErrorKind::InvalidZipArchive(m), path),
            ZipError::UnsupportedArchive(m) => {
                Self::new(FileIoErrorKind::UnsupportedZipArchive(m), path)
            }
            ZipError::FileNotFound => Self::chaining(
                FileIoErrorKind::OpenInReadOnly,
                path,
                io::Error::from(io::ErrorKind::NotFound),
            ),
        }
    }
}

impl fmt::Display for FileIoError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let path = self.path.display();
        match self.kind {
            FileIoErrorKind::Search(name) => write!(
                f,
                "Could not find {:?} in {} or any parent directory",
                name, path,
            ),
            FileIoErrorKind::OpenInReadOnly => write!(
                f,
                "An IO error occurred while opening {} in read-only mode",
                path,
            ),
            FileIoErrorKind::OpenInWriteOnly => write!(
                f,
                "An IO error occurred while opening {} in write-only mode",
                path,
            ),
            FileIoErrorKind::OpenInReadWrite => write!(
                f,
                "An IO error occurred while opening {} in read/write mode",
                path,
            ),
            FileIoErrorKind::Lock => write!(f, "Failed to lock {}", path),
            FileIoErrorKind::CreateDirAll => write!(f, "Failed to create {}", path),
            FileIoErrorKind::ReadDir | FileIoErrorKind::Read => {
                write!(f, "Failed to read {}", path)
            }
            FileIoErrorKind::Write => write!(f, "Failed to write to {}", path),
            FileIoErrorKind::Deserialize => write!(f, "Failed to deserialize data from {}", path),
            FileIoErrorKind::InvalidZipArchive(m) => {
                write!(f, "{} is invalid Zip archive: {}", path, m)
            }
            FileIoErrorKind::UnsupportedZipArchive(m) => {
                write!(f, "{} is unsupported Zip archive: {}", path, m)
            }
        }
    }
}

impl Fail for FileIoError {
    fn cause(&self) -> Option<&Fail> {
        match self.cause.as_ref() {
            Some(cause) => Some(cause),
            None => None,
        }
    }
}

#[derive(Debug)]
pub(crate) enum FileIoErrorKind {
    Search(&'static str),
    OpenInReadOnly,
    OpenInWriteOnly,
    OpenInReadWrite,
    Lock,
    CreateDirAll,
    ReadDir,
    Read,
    Write,
    Deserialize,
    InvalidZipArchive(&'static str),
    UnsupportedZipArchive(&'static str),
}

#[derive(Debug)]
pub struct StdErrorChain {
    display: String,
    debug: String,
    next: Option<Box<StdErrorChain>>,
}

impl fmt::Display for StdErrorChain {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.display)
    }
}

impl Fail for StdErrorChain {
    fn cause(&self) -> Option<&Fail> {
        match self.next.as_ref() {
            Some(next) => Some(next.as_ref()),
            None => None,
        }
    }
}

impl<E: std::error::Error> From<E> for StdErrorChain {
    fn from(from: E) -> Self {
        let mut errors = vec![(format!("{}", from), format!("{:?}", from))];
        let mut from: &std::error::Error = &from;
        while let Some(cause) = from.cause() {
            errors.push((format!("{}", cause), format!("{:?}", cause)));
            from = cause;
        }
        let (display, debug) = errors.pop().unwrap();
        let mut chain = Self {
            display,
            debug,
            next: None,
        };
        for (display, debug) in errors.into_iter().rev() {
            chain = Self {
                display,
                debug,
                next: Some(Box::new(chain)),
            }
        }
        chain
    }
}
