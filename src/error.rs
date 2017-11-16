use bincode;
use chrono::{self, DateTime, Local};
use cookie;
use error_chain::ChainedError;
use log::SetLoggerError;
use regex;
use reqwest::{self, StatusCode, UrlError};
use serde_json;
use serde_urlencoded;
use serde_yaml;
use std::error::Error;
use std::fmt;
use std::io;
use std::path::PathBuf;
use std::process::ExitStatus;
use std::sync::mpsc::RecvError;
use term::color;
use toml;
use zip::result::ZipError;


pub trait PrintChainColored {
    /// Prints the chained errors of `self` like `quick_main!`, coloring with `term`.
    fn print_chain_colored(&self);
}

impl<E: ChainedError> PrintChainColored for E {
    fn print_chain_colored(&self) {
        eprint_bold!(Some(color::RED), "Error: ");
        eprintln!("{}", self);
        for e_kind in self.iter().skip(1) {
            eprint_bold!(None, "Caused by: ");
            eprintln!("{}", e_kind);
        }
        if let Some(backtrace) = self.backtrace() {
            eprintln!("{:?}", backtrace);
        }
    }
}


error_chain!{
    types{
        SnowchainsError, SnowchainsErrorKind, SnowchainsResultExt, SnowchainsResult;
    }

    links {
        Service(ServiceError, ServiceErrorKind);
        Judging(JudgingError, JudgingErrorKind);
        SuiteFile(SuiteFileError, SuiteFileErrorKind);
        Config(ConfigError, ConfigErrorKind);
    }

    foreign_links {
        SetLogger(SetLoggerError);
    }
}


error_chain! {
    types {
        ServiceError, ServiceErrorKind, ServiceResultExt, ServiceResult;
    }

    links {
        SuiteFile(SuiteFileError, SuiteFileErrorKind);
    }

    foreign_links {
        Bincode(bincode::Error);
        ChronoParse(chrono::ParseError);
        CookieParse(cookie::ParseError);
        Io(io::Error);
        Recv(RecvError);
        Reqwest(reqwest::Error);
        SerdeJson(serde_json::Error);
        SerdeUrlencodedSer(serde_urlencoded::ser::Error);
        Url(UrlError);
        Zip(ZipError);
    }

    errors {
        AlreadyAccepted {
            description("Found an accepted submission")
            display("Found an accepted submission. Add \"--force\" to submit")
        }

        ContestNotBegun(contest_name: String, begins_at: DateTime<Local>) {
            description("Contest has not begun yet")
            display("{} will begin at {}", contest_name, begins_at)
        }

        ContestNotFound(contest_name: String) {
            description("Contest not found")
            display("{} not found", contest_name)
        }

        InvalidStatusCode(code: u16) {
            description("Invalid status code (should be unreachable as long as `code` is constant)")
            display("Invalid status code: {}", code)
        }

        NoSuchProblem(name: String) {
            description("No such problem")
            display("No such problem: {:?}", name)
        }

        ReplacingClassNameFailure(path: PathBuf) {
            description("Replacing the class name fails")
            display("Failed to replace the class name in {}", path.display())
        }

        ScrapingFailed {
            description("Scraping failed")
            display("Scraping failed")
        }

        Thread {
            description("Thread error")
            display("Thread error")
        }

        UnexpectedHttpCode(expected: Vec<StatusCode>, actual: StatusCode) {
            description("Unexpected HTTP response code")
            display("The response code is {}, expected {}",
                    actual,
                    expected.iter().map(StatusCode::to_string).collect::<Vec<_>>().join(" or "))
        }

        Webbrowser(status: ExitStatus) {
            description("Failed to open a URL in the default browser")
            display("The default browser terminated abnormally {}",
                    if let Some(code) = status.code() { format!("with code {}", code) }
                    else { "without code".to_owned() })
        }
    }
}


error_chain! {
    types {
        JudgingError, JudgingErrorKind, JudgingResultExt, JudgingResult;
    }

    links {
        SuiteFile(SuiteFileError, SuiteFileErrorKind);
    }

    foreign_links {
        Io(io::Error);
        Recv(RecvError);
    }

    errors {
        CommandNotFound(command: String) {
            description("Command not found")
            display("No such command: {:?}", command)
        }

        CompilationFailure(status: ExitStatus) {
            description("Compilation command failed")
            display("The compilation command terminated abnormally {}",
                    if let Some(code) = status.code() { format!("with code {}", code) }
                    else { "without code".to_owned() })
        }

        TestFailure(n: usize, d: usize) {
            description("Test faild")
            display("{}/{} Test{} failed", n, d, if *n > 0 { "s" } else { "" })
        }
    }
}


error_chain! {
    types {
        SuiteFileError, SuiteFileErrorKind, SuiteFileResultExt, SuiteFileResult;
    }

    foreign_links {
        Io(io::Error);
        SerdeJson(serde_json::Error);
        SerdeYaml(serde_yaml::Error);
        TomlDe(toml::de::Error);
        TomlSer(toml::ser::Error);
    }

    errors {
        DifferentTypesOfSuites {
            description("Different types of suites")
            display("Different types of suites")
        }

        SuiteIsInteractive {
            description("Target suite is interactive")
            display("Target suite is interactive")
        }
    }
}


error_chain! {
    types {
        ConfigError, ConfigErrorKind, ConfigResultExt, ConfigResult;
    }

    foreign_links {
        PathFormat(PathFormatError);
        Io(io::Error);
        Regex(regex::Error);
        SerdeYaml(serde_yaml::Error);
    }

    errors {
        ConfigFileNotFound {
            description("\"snowchains.yml\" not found")
            display("\"snowchains.yml\" not found")
        }

        NoSuchLanguage(name: String) {
            description("Language not found")
            display("No such language: \"{}\"", name)
        }

        PropertyNotSet(property: &'static str) {
            description("Property not set")
            display("Property not set: \"{}\"", property)
        }

        UnsupportedExtension(extension: String) {
            description("Unsupported extension")
            display("Unsupported extension: \"{}\"", extension)
        }
    }
}


pub type PathFormatResult<T> = Result<T, PathFormatError>;


#[derive(Debug)]
pub enum PathFormatError {
    Syntax(String),
    NoSuchSpecifier(String, String, &'static [&'static str]),
    NoSuchKeyword(String, String, Vec<&'static str>),
}

impl fmt::Display for PathFormatError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PathFormatError::Syntax(ref s) => write!(f, "Syntax error: {:?}", s),
            PathFormatError::NoSuchSpecifier(ref s, ref sp, sps) => {
                write!(f, "No such format specifier {:?}", sp)?;
                write!(f, " (expected {:?}): {:?}", sps, s)
            }
            PathFormatError::NoSuchKeyword(ref s, ref kw, ref kws) => {
                write!(f, "No such keyword {:?} (expected {:?}): {:?}", kw, kws, s)
            }
        }
    }
}

impl Error for PathFormatError {
    fn description(&self) -> &str {
        "Error about format string in config file"
    }
}
