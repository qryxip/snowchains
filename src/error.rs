use cookie;
use error_chain::ChainedError;
use regex;
use reqwest::{self, StatusCode, UrlError};
use serde_json;
use serde_urlencoded;
use serde_yaml;
use std::io;
use std::path::PathBuf;
use std::process::ExitStatus;
use std::sync::mpsc::RecvError;
use term::{Attr, color};
use toml;


pub trait PrintChainColored {
    /// Prints the chained errors of `self` like `quick_main!`, coloring with `term`.
    fn print_chain_colored(&self);
}

impl<E: ChainedError> PrintChainColored for E {
    fn print_chain_colored(&self) {
        eprint_decorated!(Attr::Bold, Some(color::RED), "Error: ");
        eprintln!("{}", self);
        for e_kind in self.iter().skip(1) {
            eprint_decorated!(Attr::Bold, None, "Caused by: ");
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
        Judge(JudgeError, JudgeErrorKind);
        Config(ConfigError, ConfigErrorKind);
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
        UnsupportedContest(name: String) {
            description("Unsupported contest name")
                display("Unsupported contest name \"{}\"", name)
        }

        NoSuchProblem(name: String) {
            description("No such problem")
                display("No such problem: \"{}\"", name)
        }

        ScrapingFailed {
            description("Scraping failed")
                display("Scraping faild")
        }

        ReplacingClassNameFailure(path: PathBuf) {
            description("Replacing the class name fails")
                display("Failed to replace the class name in {}", path.display())
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
        BuildFailure(status: ExitStatus) {
            description("Build failed")
                display("Build failed{}",
                        if let Some(code) = status.code() {
                            format!(" with code {}", code)
                        } else {
                            "".to_owned()
                        })
        }

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
}


error_chain! {
    types {
        ConfigError, ConfigErrorKind, ConfigResultExt, ConfigResult;
    }

    foreign_links {
        Io(io::Error);
        Regex(regex::Error);
        SerdeYaml(serde_yaml::Error);
    }

    errors {
        ConfigFileNotFound {
            description("\"snowchains.yml\" not found")
                display("\"snowchains.yml\" not found")
        }

        UnsupportedExtension(extension: String) {
            description("Unsupported extension")
                display("Unsupported extension: \"{}\"", extension)
        }

        NoSuchLanguage(name: String) {
            description("Language not found")
                display("No such language: \"{}\"", name)
        }

        PropertyNotSet(property: &'static str) {
            description("Property not set")
                display("Property not set: \"{}\"", property)
        }
    }
}
