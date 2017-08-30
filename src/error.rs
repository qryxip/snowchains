use cookie;
use error_chain::ChainedError;
use reqwest::{self, StatusCode, UrlError};
use serde_json;
use serde_urlencoded;
use serde_yaml;
use std::io;
use std::process;
use std::sync::mpsc::RecvError;
use term::{Attr, color};
use toml;


pub trait OrExit1 {
    type Target;
    /// if `self` is `Err`, prints the error details and exit with code 1.
    fn or_exit1(self) -> Self::Target;
}

impl<T, E: ChainedError> OrExit1 for Result<T, E> {
    type Target = T;

    fn or_exit1(self) -> T {
        match self {
            Ok(x) => x,
            Err(e) => {
                eprint_decorated!(Attr::Bold, Some(color::RED), "\nerror: ");
                eprintln!("{}", e);
                for e_kind in e.iter().skip(1) {
                    eprint_decorated!(Attr::Bold, Some(color::RED), "caused by: ");
                    eprintln!("{}", e_kind);
                }
                if let Some(backtrace) = e.backtrace() {
                    eprintln!("{:?}", backtrace);
                }
                process::exit(1);
            }
        }
    }
}


error_chain! {
    types {
        ServiceError, ServiceErrorKind, ServiceResultExt, ServiceResult;
    }

    links {
        TestCase(TestCaseError, TestCaseErrorKind);
    }

    foreign_links {
        Reqwest(reqwest::Error);
        Url(UrlError);
        Io(io::Error);
        SerdeJson(serde_json::Error);
        SerdeUrlEncodedSerialization(serde_urlencoded::ser::Error);
        CookieParse(cookie::ParseError);
    }

    errors {
        ScrapingFailed {
            description("Scraping failed")
                display("Scraping faild")
        }

        UnexpectedHttpCode(expected: StatusCode, actual: StatusCode) {
            description("Unexpected HTTP response code")
                display("The response code is {}, expected {}", actual, expected)
        }
    }
}


error_chain! {
    types {
        JudgeError, JudgeErrorKind, JudgeResultExt, JudgeResult;
    }

    links {
        TestCase(TestCaseError, TestCaseErrorKind);
    }

    foreign_links {
        Io(io::Error);
        Recv(RecvError);
    }

    errors {
        TestFailed(n: usize) {
            description("Test faild")
                display("{} Test{} failed", n, if *n > 0 { "s" } else { "" })
        }
    }
}


error_chain! {
    types {
        TestCaseError, TestCaseErrorKind, TestCaseResultExt, TestCaseResult;
    }

    foreign_links {
        Io(io::Error);
        SerdeJson(serde_json::Error);
        SerdeYaml(serde_yaml::Error);
        TomlSerialization(toml::ser::Error);
        TomlDeserialization(toml::de::Error);
    }

    errors {
        UnsupportedExtension(extension: String) {
            description("Unsupported extension")
                display("Unsupported extension: \"{}\"", extension)
        }
    }
}


error_chain! {
    types {
        ConfigError, ConfigErrorKind, ConfigResultExt, ConfigResult;
    }

    foreign_links {
        Io(io::Error);
        SerdeYaml(serde_yaml::Error);
    }

    errors {
        ConfigFileNotFound {
            description("\"snowchains.yml\" not found")
                display("\"snowchains.yml\" not found")
        }

        NoSuchTarget(name: String) {
            description("Target not found")
                display("No such target: \"{}\"", name)
        }

        PropertyNotSet(property: &'static str) {
            description("Property not set")
                display("Property not set: \"{}\"", property)
        }
    }
}
