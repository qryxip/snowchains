use chrono::{self, DateTime, Local};
use httpsession::UrlError;
use zip::result::ZipError;
use {httpsession, serde_json, serde_urlencoded, serde_yaml, toml};

use std::ffi::OsString;
use std::io;
use std::path::PathBuf;
use std::process::ExitStatus;
use std::string::FromUtf8Error;
use std::sync::mpsc::RecvError;

error_chain!{
    foreign_links {
        Service(ServiceError/*, ServiceErrorKind*/);
        Judge(JudgeError/*, JudgeErrorKind*/);
        SuiteFile(SuiteFileError/*, SuiteFileErrorKind*/);
        Config(ConfigError/*, ConfigErrorKind*/);
        TemplateExpand(TemplateExpandError/*, TemplateExpandErrorKind*/);
        FileIo(FileIoError/*, FileIoErrorKind*/);
        Io(io::Error);
    }

    errors {
        Unimplemented {
            description("Unimplemented")
            display("Sorry, not yet implemented")
        }

        HomeDirNotFound {
            description("Home directory not found")
            display("Home directory not found")
        }
    }
}

error_chain! {
    types {
        ServiceError, ServiceErrorKind, ServiceResultExt, ServiceResult;
    }

    foreign_links {
        FileIo(FileIoError/*, FileIoErrorKind*/);
        CodeReplace(CodeReplaceError/*, CodeReplaceErrorKind*/);
        SuiteFile(SuiteFileError/*, SuiteFileErrorKind*/);
        TemplateExpand(TemplateExpandError/*, TemplateExpandErrorKind*/);
        ChronoParse(chrono::ParseError);
        HttpSession(httpsession::Error);
        Io(io::Error);
        Recv(RecvError);
        SerdeJson(serde_json::Error);
        SerdeUrlencodedSer(serde_urlencoded::ser::Error);
        Url(UrlError);
        Zip(ZipError);
    }

    errors {
        AlreadyAccepted {
            description("Found an accepted submission")
            display("Found an accepted submission. Add \"--skip-checking-duplication\" (\"-d\")")
        }

        ContestNotBegun(contest_name: String, begins_at: DateTime<Local>) {
            description("Contest has not begun yet")
            display("{} will begin at {}", contest_name, begins_at)
        }

        ContestNotFound(contest_name: String) {
            description("Contest not found")
            display("{} not found", contest_name)
        }

        HttpSessionStart {
            description("Failed to start a HTTP session")
            display("Failed to start the HTTP session")
        }

        NoSuchProblem(name: String) {
            description("No such problem")
            display("No such problem: {:?}", name)
        }

        Scrape {
            description("Scraping failed")
            display("Scraping failed")
        }

        Webbrowser(status: ExitStatus) {
            description("Failed to open a URL in the default browser")
            display("{}",
                    if let Some(code) = status.code() {
                        format!("The default browser terminated abnormally with code {}", code)
                    } else {
                        "The default browser terminated abnormally without code (possibly killed)"
                            .to_owned()
                    })
        }

        WrongCredentialsOnTest {
            description("Wrong username or password")
            display("Wrong username or password")
        }
    }
}

error_chain! {
    types {
        JudgeError, JudgeErrorKind, JudgeResultExt, JudgeResult;
    }

    foreign_links {
        SuiteFile(SuiteFileError/*, SuiteFileErrorKind*/);
        FileIo(FileIoError/*, FileIoErrorKind*/);
        Io(io::Error);
        Recv(RecvError);
    }

    errors {
        Command(command: OsString) {
            description("Failed to execute a command")
            display("Failed to execute: {:?}", command)
        }

        Compile(status: ExitStatus) {
            description("Compilation failed")
            display("The compilation command terminated abnormally {}",
                    if let Some(code) = status.code() { format!("with code {}", code) }
                    else { "without code".to_owned() })
        }

        TestFailure(n: usize, d: usize) {
            description("Test failed")
            display("{}/{} Test{} failed", n, d, if *n > 0 { "s" } else { "" })
        }
    }
}

error_chain! {
    types {
        SuiteFileError, SuiteFileErrorKind, SuiteFileResultExt, SuiteFileResult;
    }

    foreign_links {
        Config(ConfigError/*, ConfigErrorKind*/);
        TemplateExpand(TemplateExpandError/*, TemplateExpandErrorKind*/);
        FileIo(FileIoError/*, FileIoErrorKind*/);
        Io(io::Error);
        SerdeJson(serde_json::Error);
        SerdeYaml(serde_yaml::Error);
        TomlDe(toml::de::Error);
        TomlSer(toml::ser::Error);
        Zip(ZipError);
    }

    errors {
        DirNotExist(directory: PathBuf) {
            description("Directory does not exist")
            display("{:?} does not exist. Execute \"download\" command first", directory)
        }

        NoFile(directory: PathBuf) {
            description("No test suite file")
            display("No test suite file in {:?}. Execute \"download\" command first", directory)
        }

        DifferentTypesOfSuites {
            description("Different types of suites")
            display("Different types of suites")
        }

        SuiteIsNotSimple {
            description("Target suite is not \"simple\" type")
            display("Target suite is not \"simple\" type")
        }

        Unsubmittable(problem: String) {
            description("The problem is unsubmittable")
            display("{:?} is unsubmittable", problem)
        }

        RegexGroupOutOfBounds(group: usize) {
            description("Regex group out of bounds")
            display("Regex group out of bounds: {}", group)
        }

        UnsupportedExtension(extension: String) {
            description("Unsupported extension")
            display("Unsupported extension; {:?}", extension)
        }
    }
}

error_chain! {
    types {
        ConfigError, ConfigErrorKind, ConfigResultExt, ConfigResult;
    }

    errors {
        LanguageNotSpecified {
            description("Language not specified")
            display("Language not specified")
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

error_chain! {
    types {
        CodeReplaceError, CodeReplaceErrorKind, CodeReplaceResultExt, CodeReplaceResult;
    }

    foreign_links {
        TemplateExpand(TemplateExpandError/*, TemplateExpandErrorKind*/);
        FromUtf8(FromUtf8Error);
    }

    errors {
        RegexGroupOutOfBounds(group: usize) {
            description("Regex group out of bounds")
            display("Regex group out of bounds: {}", group)
        }

        NoMatch(regex: String) {
            description("No match")
            display("No match: {:?}", regex)
        }
    }
}

error_chain! {
    types {
        TemplateExpandError, TemplateExpandErrorKind, TemplateExpandResultExt, TemplateExpandResult;
    }

    foreign_links {
        Io(io::Error);
    }

    errors {
        TemplateExpand(debug: String, target: String, ty: &'static str) {
            description("Failed to expand a template")
            display("Failed to expand {} % {:?} as {}", debug, target, ty)
        }

        UnknownSpecifier(specifier: String) {
            description("Unknown specifier")
            display("Unknown specifier {:?}: expected \"\", \"lower\", \"upper\", \"kebab\", \
                     \"snake\", \"screaming\", \"mixed\", \"pascal\" or \"title\"", specifier)
        }

        EnvVarNotPresent(name: String) {
            description("An environment variable is not present")
            display("Environment variable {:?} is not present", name)
        }

        EnvVarNotUnicode(name: String, value: OsString) {
            description("An environment variable is not valid unicode")
            display("Environment variable {:?} is not valid unicode: {:?}", name, value)
        }

        HomeDirNotFound {
            description("Home directory not found")
            display("Home directory not found")
        }

        UnsupportedUseOfTilde {
            description("Unsupported use of \"~\"")
            display("Unsupported use of \"~\"")
        }
    }
}

error_chain! {
    types {
        FileIoError, FileIoErrorKind, FileIoResultExt, FileIoResult;
    }

    foreign_links {
        Io(io::Error);
        SerdeYaml(serde_yaml::Error);
    }

    errors {
        Search(name: &'static str, start: PathBuf) {
            description("Failed to search")
            display("Could not find {:?} in {} or any parent directory", name, start.display())
        }

        OpenInReadOnly(path: PathBuf) {
            description("Failed to open a file in read-only mode")
            display("An IO error occurred while opening {} in read-only mode", path.display())
        }

        OpenInWriteOnly(path: PathBuf) {
            description("Failed to open a file in write-only mode")
            display("An IO error occurred while opening {} in write-only mode", path.display())
        }

        CreateDirAll(dir: PathBuf) {
            description("Failed to create a directory")
            display("Failed to create {}", dir.display())
        }

        ReadDir(dir: PathBuf) {
            description("Failed to read a directory")
            display("Failed to read {}", dir.display())
        }

        Write(path: PathBuf) {
            description("Failed to write data to a file")
            display("Failed to write to {}", path.display())
        }
    }
}
