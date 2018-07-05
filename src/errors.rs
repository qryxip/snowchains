#![cfg_attr(rustfmt, rustfmt_skip)] // https://github.com/rust-lang-nursery/rustfmt/issues/2743

use chrono::{self, DateTime, Local};
use failure::{Context, Fail};
use itertools::Itertools as _Itertools;
use reqwest::{self, StatusCode};
use url::{self, Url};
use zip::result::ZipError;
use {cookie, serde_urlencoded};
use path::AbsPathBuf;

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
    LoadConfig(LoadConfigError),
    TemplateExpand(TemplateExpandError),
    FileIo(FileIoError),
    Getcwd(io::Error),
    Unimplemented,
}

impl fmt::Display for self::Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            self::Error::Service(e) => write!(f, "{}", e),
            self::Error::Judge(e) => write!(f, "{}", e),
            self::Error::SuiteFile(e) => write!(f, "{}", e),
            self::Error::LoadConfig(e) => write!(f, "{}", e),
            self::Error::TemplateExpand(e) => write!(f, "{}", e),
            self::Error::FileIo(e) => write!(f, "{}", e),
            self::Error::Getcwd(_) => write!(f, "Failed to get the current directory"),
            self::Error::Unimplemented => write!(f, "Sorry, not yet implemented"),
        }
    }
}

impl Fail for self::Error {
    fn cause(&self) -> Option<&dyn Fail> {
        match self {
            ::Error::Service(e) => e.cause(),
            ::Error::Judge(e) => e.cause(),
            ::Error::SuiteFile(e) => e.cause(),
            ::Error::LoadConfig(e) => e.cause(),
            ::Error::TemplateExpand(e) => e.cause(),
            ::Error::FileIo(e) => e.cause(),
            ::Error::Getcwd(e) => Some(e),
            _ => None,
        }
    }
}

derive_from!(Error::Service        <- ServiceError);
derive_from!(Error::Judge          <- JudgeError);
derive_from!(Error::SuiteFile      <- SuiteFileError);
derive_from!(Error::LoadConfig     <- LoadConfigError);
derive_from!(Error::TemplateExpand <- TemplateExpandError);
derive_from!(Error::FileIo         <- FileIoError);

pub(crate) type ServiceResult<T> = std::result::Result<T, ServiceError>;

#[derive(Debug)]
pub enum ServiceError {
    Session(SessionError),
    CodeReplace(CodeReplaceError),
    SuiteFile(SuiteFileError),
    TemplateExpand(TemplateExpandError),
    Serialize(SerializeError),
    FileIo(FileIoError),
    Submit(SubmitError),
    Std(StdErrorChain),
    AlreadyAccepted,
    ContestNotBegun(String, DateTime<Local>),
    ContestNotFound(String),
    PleaseSpecifyProblems,
    Scrape,
    UnexpectedRedirection(String),
    WrongCredentialsOnTest,
}

impl fmt::Display for ServiceError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ServiceError::Session(e) => write!(f, "{}", e),
            ServiceError::CodeReplace(e) => write!(f, "{}", e),
            ServiceError::SuiteFile(e) => write!(f, "{}", e),
            ServiceError::TemplateExpand(e) => write!(f, "{}", e),
            ServiceError::Serialize(e) => write!(f, "{}", e),
            ServiceError::FileIo(e) => write!(f, "{}", e),
            ServiceError::Submit(e) => write!(f, "{}", e),
            ServiceError::Std(e) => write!(f, "{}", e),
            ServiceError::AlreadyAccepted => write!(
                f,
                "Found an accepted submission. Add \"--skip-checking-duplication\" (\"-d\")"
            ),
            ServiceError::ContestNotBegun(s, t) => write!(f, "{} will begin at {}", s, t),
            ServiceError::ContestNotFound(s) => write!(f, "{} not found", s),
            ServiceError::PleaseSpecifyProblems => write!(f, "Please specify problems"),
            ServiceError::Scrape => write!(f, "Failed to scrape"),
            ServiceError::UnexpectedRedirection(u) => write!(f, "Unexpected redirection to {}", u),
            ServiceError::WrongCredentialsOnTest => write!(f, "Wrong credentials"),
        }
    }
}

derive_from!(ServiceError::Session        <- SessionError);
derive_from!(ServiceError::CodeReplace    <- CodeReplaceError);
derive_from!(ServiceError::SuiteFile      <- SuiteFileError);
derive_from!(ServiceError::TemplateExpand <- TemplateExpandError);
derive_from!(ServiceError::Serialize      <- SerializeError);
derive_from!(ServiceError::Submit         <- SubmitError);
derive_from!(ServiceError::FileIo         <- FileIoError);
derive_from!(ServiceError::Std            <- chrono::ParseError);
derive_from!(ServiceError::Std            <- reqwest::Error);
derive_from!(ServiceError::Std            <- serde_urlencoded::ser::Error);
derive_from!(ServiceError::Std            <- ZipError);
derive_from!(ServiceError::Std            <- io::Error);

impl Fail for ServiceError {
    fn cause(&self) -> Option<&dyn Fail> {
        match self {
            ServiceError::Session(e) => e.cause(),
            ServiceError::CodeReplace(e) => e.cause(),
            ServiceError::SuiteFile(e) => e.cause(),
            ServiceError::TemplateExpand(e) => e.cause(),
            ServiceError::Serialize(e) => e.cause(),
            ServiceError::FileIo(e) => e.cause(),
            ServiceError::Std(e) => e.cause(),
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
    Serialize(SerializeError),
    FileIo(FileIoError),
    Std(StdErrorChain),
    Start(Context<StartSessionError>),
    ParseUrl(String, url::ParseError),
    ParseCookieFromPath(String, AbsPathBuf, cookie::ParseError),
    ParseCookieFromUrl(String, Url, cookie::ParseError),
    HeaderMissing(&'static str),
    ForbiddenByRobotsTxt,
    UnexpectedStatusCode(Vec<StatusCode>, StatusCode),
    Webbrowser(ExitStatus),
}

derive_from!(SessionError::Serialize <- SerializeError);
derive_from!(SessionError::FileIo    <- FileIoError);
derive_from!(SessionError::Std       <- reqwest::Error);
derive_from!(SessionError::Std       <- io::Error);
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
    fn cause(&self) -> Option<&dyn Fail> {
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
    LoadConfig(LoadConfigError),
    TemplateExpand(TemplateExpandError),
    Serialize(SerializeError),
    FileIo(FileIoError),
    Io(io::Error),
    DirNotExist(AbsPathBuf),
    NoFile(AbsPathBuf),
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
    fn cause(&self) -> Option<&dyn Fail> {
        match self {
            SuiteFileError::LoadConfig(e) => e.cause(),
            SuiteFileError::TemplateExpand(e) => e.cause(),
            SuiteFileError::Serialize(e) => e.cause(),
            SuiteFileError::FileIo(e) => e.cause(),
            SuiteFileError::Io(e) => Some(e),
            _ => None,
        }
    }
}

derive_from!(SuiteFileError::LoadConfig     <- LoadConfigError);
derive_from!(SuiteFileError::TemplateExpand <- TemplateExpandError);
derive_from!(SuiteFileError::Serialize      <- SerializeError);
derive_from!(SuiteFileError::FileIo         <- FileIoError);
derive_from!(SuiteFileError::Io             <- io::Error);

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
    fn cause(&self) -> Option<&dyn Fail> {
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
    FileIo(FileIoError),
    UnknownSpecifier(String),
    EnvVarNotPresent(String),
    EnvVarNotUnicode(String, OsString),
}

derive_from!(TemplateExpandError::Context <- Context<TemplateExpandErrorContext>);
derive_from!(TemplateExpandError::FileIo  <- FileIoError);

impl fmt::Display for TemplateExpandError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TemplateExpandError::Context(c) => write!(f, "{}", c),
            TemplateExpandError::FileIo(e) => write!(f, "{}", e),
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
        }
    }
}

impl Fail for TemplateExpandError {
    fn cause(&self) -> Option<&dyn Fail> {
        match self {
            TemplateExpandError::Context(c) => c.cause(),
            TemplateExpandError::FileIo(e) => e.cause(),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct TemplateExpandErrorContext {
    debug: String,
    problem: String,
    ty: &'static str,
}

impl TemplateExpandErrorContext {
    pub(crate) fn new(
        debug: &impl fmt::Debug,
        problem: impl Into<String>,
        ty: &'static str,
    ) -> Self {
        Self {
            debug: format!("{:?}", debug),
            problem: problem.into(),
            ty,
        }
    }
}

impl fmt::Display for TemplateExpandErrorContext {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Failed to expand {} % {:?} as {}",
            self.debug, self.problem, self.ty
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
            FileIoErrorKind::HomeDirNotFound => write!(f, "Home directory not found"),
            FileIoErrorKind::UnsupportedUseOfTilde => {
                write!(f, "Unsupported use of \"~\": {}", path)
            }
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
    fn cause(&self) -> Option<&dyn Fail> {
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
    HomeDirNotFound,
    UnsupportedUseOfTilde,
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
    fn cause(&self) -> Option<&dyn Fail> {
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
