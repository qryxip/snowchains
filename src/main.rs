#[macro_use]
extern crate snowchains;

#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate log;
#[macro_use]
extern crate structopt;

extern crate env_logger;
extern crate httpsession;

use snowchains::{util, ServiceName};
use snowchains::config::{self, Config};
use snowchains::errors::FileIoResult;
use snowchains::judging::{self, JudgeProp};
use snowchains::service::{atcoder, atcoder_beta, hackerrank, DownloadProp, InitProp, RestoreProp,
                          SubmitProp};
use snowchains::terminal::Color;
use snowchains::testsuite::{self, SuiteFileExtension, SuiteFilePath};

use error_chain::{ChainedError, ExitCode};
use httpsession::ColorMode;
use structopt::StructOpt as _StructOpt;

use std::{env, process};
use std::path::PathBuf;
use std::time::Duration;

macro_rules! quick_main_colored {
    ($main: expr) => {
        fn main() {
            quick_main_colored($main)
        }
    };
}

quick_main_colored!(|| -> snowchains::Result<()> {
    env_logger::init();
    match Opt::from_args() {
        Opt::Init { directory } => {
            info!("Running \"init\" command");
            config::init(&directory, None, None)?;
        }
        Opt::Switch { service, contest } => {
            info!("Running \"switch\" command");
            config::switch(service, &contest, &env::current_dir()?)?;
        }
        Opt::Login { service } => {
            info!("Running \"login\" command");
            let init_prop = init_prop(service)?;
            match service {
                ServiceName::AtCoder => atcoder::login(),
                ServiceName::AtCoderBeta => atcoder_beta::login(&init_prop),
                ServiceName::HackerRank => hackerrank::login(),
                ServiceName::Other => unreachable!(),
            }?;
        }
        Opt::Participate { service, contest } => {
            info!("Running \"participate\" command");
            let init_prop = init_prop(service)?;
            match service {
                ServiceName::AtCoder => atcoder::participate(&contest),
                ServiceName::AtCoderBeta => atcoder_beta::participate(&contest, &init_prop),
                _ => unreachable!(),
            }?;
        }
        Opt::Download {
            service,
            contest,
            open_browser,
        } => {
            info!("Running \"download\" command");
            let config = Config::load_from_file(service, contest, &env::current_dir()?)?;
            let init_prop = init_prop(config.service())?;
            let download_prop = DownloadProp::new(&config, open_browser)?;
            match config.service() {
                ServiceName::AtCoder => atcoder::download(&download_prop),
                ServiceName::AtCoderBeta => atcoder_beta::download(&init_prop, download_prop),
                ServiceName::HackerRank => hackerrank::download(&download_prop),
                ServiceName::Other => bail!(snowchains::ErrorKind::Unimplemented),
            }?;
        }
        Opt::Restore { service, contest } => {
            info!("Running \"restore\" command");
            let config = Config::load_from_file(service, contest, &env::current_dir()?)?;
            let init_prop = init_prop(config.service())?;
            let restore_prop = RestoreProp::new(&config)?;
            match config.service() {
                ServiceName::AtCoderBeta => atcoder_beta::restore(&init_prop, restore_prop)?,
                _ => bail!(snowchains::ErrorKind::Unimplemented),
            };
        }
        Opt::Append {
            target,
            extension,
            input,
            output,
            service,
            contest,
        } => {
            info!("Running \"append\" command");
            let config = Config::load_from_file(service, contest, &env::current_dir()?)?;
            let dir = config.testfiles_dir().expand("")?;
            let path = SuiteFilePath::new(&dir, &target, extension);
            testsuite::append(&path, &input, output.as_ref().map(String::as_str))?;
        }
        Opt::Judge {
            target,
            language,
            service,
            contest,
        } => {
            let language = language.as_ref().map(String::as_str);
            info!("Running \"judge\" command");
            let config = Config::load_from_file(service, contest, &env::current_dir()?)?;
            let prop = JudgeProp::new(&config, &target, language)?;
            judging::judge(prop)?;
        }
        Opt::Submit {
            target,
            language,
            service,
            contest,
            open_browser,
            skip_judging,
            skip_checking_duplication,
        } => {
            let language = language.as_ref().map(String::as_str);
            info!("Running \"submit\" command");
            let config = Config::load_from_file(service, contest, &env::current_dir()?)?;
            let init_prop = init_prop(config.service())?;
            let judge_prop = JudgeProp::new(&config, &target, language)?;
            let submit_prop = SubmitProp::new(
                &config,
                target,
                language,
                open_browser,
                skip_checking_duplication,
            )?;
            if !skip_judging {
                judging::judge(judge_prop)?;
                println!();
            }
            match config.service() {
                ServiceName::AtCoderBeta => atcoder_beta::submit(&init_prop, submit_prop)?,
                _ => bail!(snowchains::ErrorKind::Unimplemented),
            };
        }
    }
    Ok(())
});

#[derive(StructOpt)]
#[structopt(usage = "snowchains <i|init> [directory]\n    \
                     snowchains <w|switch> <service> <contest>\n    \
                     snowchains <l|login> <service>\n    \
                     snowchains <p|participate> <service> <contest>\n    \
                     snowchains <d|download> [-s|--service <service>]\
                     \n                            [-c|--contest <contest>]\
                     \n                            [-b|--open-browser]\n    \
                     snowchains <r|restore> [-s|--service <service>]\
                     \n                           [-c|--contest <contest>]\n    \
                     snowchains <a|append> <target> <extension> <input> [output]\
                     \n                                                       \
                     [-s|--service <service>]\
                     \n                                                       \
                     [-c|--contest <contest>]\n    \
                     snowchains {j|judge} <target> [-l|--language <language>]\
                     \n                                  [-s|--service <service>]\
                     \n                                  [-c|--contest <contest>]\n    \
                     snowchains {s|submit} <target> [-l|--language <language>]\
                     \n                                   [-s|--service <service>]\
                     \n                                   [-c|--contest <contest>]\
                     \n                                   [-b|--open-browser]\
                     \n                                   [-j|--skip-judging]\
                     \n                                   [-d|--skip-checking-duplication]")]
enum Opt {
    #[structopt(name = "init", about = "Creates a \"snowchains.yaml\"",
                usage = "snowchains <i|init> [directory]",
                raw(display_order = "1", aliases = r#"&["i"]"#))]
    Init {
        #[structopt(name = "directory", help = "Directory to create a \"snowchains.yaml\"",
                    parse(from_os_str), default_value = ".")]
        directory: PathBuf,
    },

    #[structopt(name = "switch",
                about = "Changes <service> and <contest> of the \"snowchains.yaml\"",
                usage = "snowchains <w|switch> <service> <contest>",
                raw(display_order = "2", aliases = r#"&["w"]"#))]
    Switch {
        #[structopt(name = "service", help = "Service name",
                    raw(possible_values = r#"&["atcoder", "atcoderbeta", "hackerrank", "other"]"#))]
        service: ServiceName,
        #[structopt(name = "contest", help = "Contest name")]
        contest: String,
    },

    #[structopt(name = "login", about = "Logges in to a service",
                usage = "snowchains <l|login> <service>",
                raw(display_order = "3", aliases = r#"&["l"]"#))]
    Login {
        #[structopt(name = "service", help = "Service name",
                    raw(possible_values = r#"&["atcoder", "atcoderbeta", "hackerrank"]"#))]
        service: ServiceName,
    },

    #[structopt(name = "participate", about = "Participates in a contest",
                usage = "snowchains <p|participate> <service> <contest>",
                raw(display_order = "4", aliases = r#"&["p"]"#))]
    Participate {
        #[structopt(name = "service", help = "Service name",
                    raw(possible_values = r#"&["atcoder", "atcoderbeta"]"#))]
        service: ServiceName,
        #[structopt(name = "contest", help = "Contest name")]
        contest: String,
    },

    #[structopt(name = "download", about = "Downloads test cases",
                usage = "snowchains <d|download> [-s|--service <service>]\
                         \n                            [-c|--contest <contest>]\
                         \n                            [-b|--open-browser]",
                raw(display_order = "5", aliases = r#"&["d"]"#))]
    Download {
        #[structopt(short = "s", long = "service", help = "Service name",
                    raw(display_order = "1",
                        possible_values = r#"&["atcoder", "atcoderbeta", "hackerrank"]"#))]
        service: Option<ServiceName>,
        #[structopt(short = "c", long = "contest", help = "Contest name",
                    raw(display_order = "2"))]
        contest: Option<String>,
        #[structopt(short = "b", long = "open-browser",
                    help = "Opens the pages with your default browser", raw(display_order = "1"))]
        open_browser: bool,
    },

    #[structopt(name = "restore", about = "Downloads the source codes you've submitted",
                usage = "snowchains <r|restore> [-s|--service <service>]\
                         \n                           [-c|--contest <contest>]",
                raw(display_order = "6", aliases = r#"&["r"]"#))]
    Restore {
        #[structopt(short = "s", long = "service", help = "Service name",
                    raw(display_order = "1", possible_values = "&[\"atcoderbeta\"]"))]
        service: Option<ServiceName>,
        #[structopt(short = "c", long = "contest", help = "Contest name",
                    raw(display_order = "2"))]
        contest: Option<String>,
    },

    #[structopt(name = "append", about = "Appends a test case to a test suite file",
                usage = "snowchains <a|append> <target> <extension> <input> [output]\
                         \n                                                       \
                         [-s|--service <service>]\
                         \n                                                       \
                         [-c|--contest <contest>]",
                raw(display_order = "7", aliases = r#"&["a"]"#))]
    Append {
        #[structopt(name = "target", help = "Target name")]
        target: String,
        #[structopt(name = "extension", help = "Extension",
                    raw(possible_values = r#"&["json", "toml", "yaml", "yml"]"#))]
        extension: SuiteFileExtension,
        #[structopt(name = "input", help = "\"input\" value to append")]
        input: String,
        #[structopt(name = "output", help = "\"expected\" value to append")]
        output: Option<String>,
        #[structopt(short = "s", long = "service", help = "Service name",
                    raw(display_order = "1",
                        possible_values = r#"&["atcoder", "atcoderbeta", "hackerrank", "other"]"#))]
        service: Option<ServiceName>,
        #[structopt(short = "c", long = "contest", help = "Contest name",
                    raw(display_order = "2"))]
        contest: Option<String>,
    },

    #[structopt(name = "judge", about = "Tests a binary or script",
                usage = "snowchains <j|judge> <target> [-l|--language <language>]\
                         \n                                  [-s|--service <service>]\
                         \n                                  [-c|--contest <contest>]",
                raw(display_order = "8", aliases = r#"&["j"]"#))]
    Judge {
        #[structopt(name = "target", help = "Target name")]
        target: String,
        #[structopt(short = "l", long = "language", help = "Language name",
                    raw(display_order = "1"))]
        language: Option<String>,
        #[structopt(short = "s", long = "service", help = "Service name",
                    raw(display_order = "2",
                        possible_values = r#"&["atcoder", "atcoderbeta", "hackerrank", "other"]"#))]
        service: Option<ServiceName>,
        #[structopt(short = "c", long = "contest", help = "Contest name",
                    raw(display_order = "3"))]
        contest: Option<String>,
    },

    #[structopt(name = "submit", about = "Submits a source code",
                usage = "snowchains <s|submit> <target> [-l|--language <language>]\
                         \n                                   [-s|--service <service>]\
                         \n                                   [-c|--contest <contest>]\
                         \n                                   [-b|--open-browser]\
                         \n                                   [-j|--skip-judging]\
                         \n                                   [-d|--skip-checking-duplication]",
                raw(display_order = "9", aliases = r#"&["s"]"#))]
    Submit {
        #[structopt(name = "target", help = "Target name")]
        target: String,
        #[structopt(short = "l", long = "language", help = "Language name",
                    raw(display_order = "1"))]
        language: Option<String>,
        #[structopt(short = "s", long = "service", help = "Service name",
                    raw(display_order = "2", possible_values = "&[\"atcoderbeta\"]"))]
        service: Option<ServiceName>,
        #[structopt(short = "c", long = "contest", help = "Contest name",
                    raw(display_order = "3"))]
        contest: Option<String>,
        #[structopt(short = "b", long = "open-browser",
                    help = "Opens the pages with your default browser", raw(display_order = "1"))]
        open_browser: bool,
        #[structopt(short = "j", long = "skip-judging", help = "Skips judging",
                    raw(display_order = "2"))]
        skip_judging: bool,
        #[structopt(short = "d", long = "skip-checking-duplication",
                    help = "Submits even if the contest is active and your code is already \
                            accepted",
                    raw(display_order = "3"))]
        skip_checking_duplication: bool,
    },
}

fn init_prop(service: ServiceName) -> FileIoResult<InitProp> {
    let cookie_path =
        util::fs::join_from_home(&[".local", "share", "snowchains", service.as_str()])?;
    Ok(InitProp::new(
        cookie_path,
        ColorMode::Prefer256,
        Duration::from_secs(20),
        None,
    ))
}

pub fn quick_main_colored<C: ExitCode, E: ChainedError>(
    main: fn() -> std::result::Result<C, E>,
) -> ! {
    match main() {
        Ok(code) => process::exit(ExitCode::code(code)),
        Err(e) => {
            eprintln!();
            if e.iter().count() > 1 {
                eprint!("    ");
            }
            for (i, e) in e.iter().enumerate() {
                let head = if i == 0 { "Error: " } else { "Caused by: " };
                eprint_bold!(Color::Fatal, "{}", head);
                eprintln_bold!(None, "{}", e);
            }
            if let Some(backtrace) = e.backtrace() {
                eprintln!("{:?}", backtrace);
            }
            process::exit(1)
        }
    }
}
