#![recursion_limit = "1024"]
#![cfg_attr(feature = "cargo-clippy", allow(explicit_write, match_ref_pats))]

#[macro_use]
extern crate custom_derive;
#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
#[macro_use]
extern crate newtype_derive;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate structopt;

extern crate bincode;
extern crate chrono;
extern crate cookie;
extern crate env_logger;
extern crate httpsession;
extern crate pbr;
extern crate regex;
extern crate reqwest;
extern crate robots_txt;
extern crate rpassword;
extern crate rprompt;
extern crate select;
extern crate serde;
extern crate serde_json;
extern crate serde_urlencoded;
extern crate serde_yaml;
extern crate term;
extern crate toml;
extern crate webbrowser;
extern crate zip;

#[macro_use]
mod macros;

mod command;
mod config;
mod errors;
mod judging;
mod service;
mod testsuite;
mod util;

use config::{Config, ServiceName};
use errors::Result;
use service::{atcoder, atcoder_beta, hackerrank};
use testsuite::{SuiteFileExtension, SuiteFilePath};

use structopt::StructOpt;

use std::path::PathBuf;

quick_main_colored!(|| -> ::Result<()> {
    env_logger::init();
    match Opt::from_args() {
        Opt::Init {
            default_language,
            directory,
        } => {
            info!("Running command \"init\"");
            config::init(&default_language, directory)?;
        }
        Opt::Switch { service, contest } => {
            info!("Running command \"switch\"");
            config::switch(service, &contest)?;
        }
        Opt::Login { service } => {
            info!("Running command \"login\"");
            match service {
                ServiceName::AtCoder => atcoder::login(),
                ServiceName::AtCoderBeta => atcoder_beta::login(),
                ServiceName::HackerRank => hackerrank::login(),
            }?;
        }
        Opt::Participate { service, contest } => {
            info!("Running command \"participate\"");
            match service {
                ServiceName::AtCoder => atcoder::participate(&contest),
                ServiceName::AtCoderBeta => atcoder_beta::participate(&contest),
                ServiceName::HackerRank => unreachable!(),
            }?;
        }
        Opt::Download { open_browser } => {
            info!("Running command \"download\"");
            let config = Config::load_from_file()?;
            let service = config.service_name()?;
            let contest = config.contest_name()?;
            let dir_to_save = config.suite_dir()?;
            let extension = config.get_extension_on_downloading();
            match service {
                ServiceName::AtCoder => atcoder::download(&contest, &dir_to_save, extension),
                ServiceName::AtCoderBeta => {
                    atcoder_beta::download(&contest, &dir_to_save, extension, open_browser)
                }
                ServiceName::HackerRank => {
                    hackerrank::download(&contest, &dir_to_save, extension, open_browser)
                }
            }?;
        }
        Opt::Append {
            target,
            extension,
            input,
            output,
        } => {
            info!("Running command \"append\"");
            let config = Config::load_from_file()?;
            let dir = config.suite_dir()?;
            let path = SuiteFilePath::new(&dir, target, extension);
            testsuite::append(&path, &input, output.as_ref().map(String::as_str))?;
        }
        Opt::Judge { target, language } => {
            info!("Running command \"judge\"");
            let language = language.as_ref().map(String::as_str);
            let config = Config::load_from_file()?;
            let paths = config.suite_paths(&target)?;
            let solver = config.construct_solver(&target, language)?;
            let compilation = config.construct_compilation_command(&target, language)?;
            judging::judge(&paths, solver, compilation)?;
        }
        Opt::Submit {
            target,
            language,
            open_browser,
            skip_judging,
            no_check,
        } => {
            info!("Running command \"submit\"");
            let language = language.as_ref().map(String::as_str);
            let config = Config::load_from_file()?;
            let service = config.service_name()?;
            let contest = config.contest_name()?;
            let src_path = config.src_path(&target, language)?;
            if !skip_judging {
                let paths = config.suite_paths(&target)?;
                let solver = config.construct_solver(&target, language)?;
                let compilation = config.construct_compilation_command(&target, language)?;
                judging::judge(&paths, solver, compilation)?;
                println!();
            }
            match service {
                ServiceName::AtCoder => unimplemented!(),
                ServiceName::AtCoderBeta => {
                    let lang_id = config.atcoder_lang_id(language)?;
                    atcoder_beta::submit(
                        &contest,
                        &target,
                        lang_id,
                        &src_path,
                        open_browser,
                        no_check,
                    )
                }
                ServiceName::HackerRank => unimplemented!(),
            }?;
        }
    }
    Ok(())
});

#[derive(StructOpt)]
#[structopt(usage = "snowchains init <default-language> <directory>\n    \
                     snowchains switch <service> <contest>\n    \
                     snowchains login <service>\n    \
                     snowchains participate <service> <contest>\n    \
                     snowchains download [--open-browser]\n    \
                     snowchains append <target> <extension> <input> [output]\n    \
                     snowchains judge <target> [language]\n    \
                     snowchains submit <target> [language] [--open-browser] [--skip-judging] \
                     [--no-check]")]
enum Opt {
    #[structopt(name = "init", about = "Creates \"snowchains.yaml\"", raw(display_order = "1"))]
    Init {
        #[structopt(name = "default-language", help = "Default language")]
        default_language: String,
        #[structopt(name = "directory", help = "Directory to create \"snowchains.yaml\"",
                    parse(from_os_str))]
        directory: PathBuf,
    },

    #[structopt(name = "switch",
                about = "Changes <service> and <contest> of \"snowchains.yaml\"",
                raw(display_order = "2"))]
    Switch {
        #[structopt(name = "service", help = "Service name",
                    raw(possible_values = r#"&["atcoder", "atcoderbeta", "hackerrank"]"#))]
        service: ServiceName,
        #[structopt(name = "contest", help = "Contest name")]
        contest: String,
    },

    #[structopt(name = "login", about = "Logges in to a service", raw(display_order = "3"))]
    Login {
        #[structopt(name = "service", help = "Service name",
                    raw(possible_values = r#"&["atcoder", "atcoderbeta", "hackerrank"]"#))]
        service: ServiceName,
    },

    #[structopt(name = "participate", about = "Participates in a contest",
                raw(display_order = "4"))]
    Participate {
        #[structopt(name = "service", help = "Service name",
                    raw(possible_values = r#"&["atcoder", "atcoderbeta"]"#))]
        service: ServiceName,
        #[structopt(name = "contest", help = "Contest name")]
        contest: String,
    },

    #[structopt(name = "download", about = "Downloads test cases",
                usage = "snowchains download [--open-browser]", raw(display_order = "5"))]
    Download {
        #[structopt(long = "open-browser", help = "Opens the pages with your default browser",
                    raw(display_order = "1"))]
        open_browser: bool,
    },

    #[structopt(name = "append", about = "Appends a test case to a test suite file",
                raw(display_order = "6"))]
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
    },

    #[structopt(name = "judge", about = "Tests a binary or script", raw(display_order = "7"))]
    Judge {
        #[structopt(name = "target", help = "Target name")]
        target: String,
        #[structopt(name = "language", help = "Language name")]
        language: Option<String>,
    },

    #[structopt(name = "submit", about = "Submits a source code",
                usage = "snowchains submit <target> [language] [--open-browser] [--skip-judging] \
                         [--no-check]",
                raw(display_order = "8"))]
    Submit {
        #[structopt(name = "target", help = "Target name")]
        target: String,
        #[structopt(name = "language", help = "Language name")]
        language: Option<String>,
        #[structopt(long = "open-browser", help = "Opens the pages with your default browser",
                    raw(display_order = "1"))]
        open_browser: bool,
        #[structopt(long = "skip-judging", help = "Skips judging", raw(display_order = "2"))]
        skip_judging: bool,
        #[structopt(long = "no-check",
                    help = "Submits even if the contest is active and your code is already \
                            accepted",
                    raw(display_order = "3"))]
        no_check: bool,
    },
}
