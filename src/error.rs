use cookie;
use error_chain::ChainedError;
use reqwest::{self, StatusCode, UrlError};
use serde_json;
use serde_urlencoded;
use std::io;
use std::process;
use term::{Attr, color};
use toml;


pub trait OrExit1 {
    /// if `self` is `Err`, prints the error details and exit with code 1.
    fn or_exit1(self);
}

impl<E: ChainedError> OrExit1 for Result<(), E> {
    fn or_exit1(self) {
        if let Err(e) = self {
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


error_chain! {
    types {
        ServiceError, ServiceErrorKind, ServiceResultExt, ServiceResult;
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

    foreign_links {
        Io(io::Error);
        SerdeJson(serde_json::Error);
        TomlSerialization(toml::ser::Error);
        TomlDeserialization(toml::de::Error);
    }

    errors {
        ProjectNotFound {
            description("Project not found")
                display("Project not found")
        }

        BuildFailed {
            description("Build failed")
                display("Build failed")
        }

        UnsupportedExtension(extension: String) {
            description("Unsupported extension")
                display("Unsupported extension: \"{}\"", extension)
        }

        TestFailed(n: usize) {
            description("Test faild")
                display("{} Test{} failed", n, if *n > 0 { "s" } else { "" })
        }
    }
}
