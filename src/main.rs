#![recursion_limit = "1024"]

#[macro_use]
extern crate clap;
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

extern crate bincode;
extern crate chrono;
extern crate cookie;
extern crate env_logger;
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

use config::{Config, PropertyKey, ServiceName};
use errors::SnowchainsResult;
use service::{atcoder, atcoder_beta, hackerrank};
use testsuite::{SuiteFileExtension, SuiteFilePath};

use clap::{AppSettings, Arg, SubCommand};

quick_main_colored!(|| -> SnowchainsResult<()> {
    env_logger::init()?;

    fn arg_default_lang() -> Arg<'static, 'static> {
        Arg::with_name("default-lang")
            .help("Default language")
            .required(true)
    }

    fn arg_dir() -> Arg<'static, 'static> {
        Arg::with_name("dir")
            .help("Directory to create \"snowchains.yml\"")
            .required(true)
    }

    fn arg_key() -> Arg<'static, 'static> {
        Arg::with_name("key")
            .help("Property key")
            .possible_value("service")
            .possible_value("contest")
            .possible_value("testsuites")
            .possible_value("extension_on_downloading")
            .possible_value("default_lang")
            .required(true)
    }

    fn arg_value() -> Arg<'static, 'static> {
        Arg::with_name("value")
            .help("Property value")
            .required(true)
    }

    fn arg_service() -> Arg<'static, 'static> {
        Arg::with_name("service")
            .possible_values(&["atcoder", "atcoderbeta", "hackerrank"])
            .help("Service name")
            .required(true)
    }

    fn arg_service_for_participate() -> Arg<'static, 'static> {
        Arg::with_name("service")
            .possible_values(&["atcoder", "atcoderbeta"])
            .help("Service name")
            .required(true)
    }

    fn arg_contest() -> Arg<'static, 'static> {
        Arg::with_name("contest")
            .help("Contest name")
            .required(true)
    }

    fn arg_open_browser() -> Arg<'static, 'static> {
        Arg::with_name("open-browser")
            .long("open-browser")
            .help("Whether to open the browser")
    }

    fn arg_target() -> Arg<'static, 'static> {
        Arg::with_name("target").help("Target name").required(true)
    }

    fn arg_extension() -> Arg<'static, 'static> {
        Arg::with_name("extension")
            .help("Extension")
            .possible_values(&["json", "toml", "yaml", "yml"])
            .required(true)
    }

    fn arg_input() -> Arg<'static, 'static> {
        Arg::with_name("input")
            .help("\"input\" value to append")
            .required(true)
    }

    fn arg_output() -> Arg<'static, 'static> {
        Arg::with_name("output").help("\"expected\" value to append")
    }

    fn arg_lang() -> Arg<'static, 'static> {
        Arg::with_name("lang").help("Language name")
    }

    fn arg_skip_judging() -> Arg<'static, 'static> {
        Arg::with_name("skip-judging")
            .long("skip-judging")
            .help("Whether to skip judging")
    }

    fn arg_force() -> Arg<'static, 'static> {
        Arg::with_name("force").long("force").help(
            "Whether to submit even if the contest is active and your code is already accepted",
        )
    }

    static USAGE_INIT_CONFIG: &'static str = "snowchains init-config <default-lang> <dir>";
    static USAGE_SET: &'static str = "snowchains set <key> <value>";
    static USAGE_LOGIN: &'static str = "snowchains login <service>";
    static USAGE_PARTICIPATE: &'static str = "snowchains participate <service> <contest>";
    static USAGE_DOWNLOAD: &'static str = "snowchains download [--open-browser]";
    static USAGE_APPEND: &'static str = "snowchains append <target> <extension> <input> [output]";
    static USAGE_JUDGE: &'static str = "snowchains judge <target> [lang]";
    static USAGE_SUBMIT: &'static str = "snowchains submit <target> [lang] [--open-browser] \
                                         [--skip-judging] [--force]";

    let subcommand_init_config = SubCommand::with_name("init-config")
        .about("Creates 'snowchains.yml'")
        .usage(USAGE_INIT_CONFIG)
        .arg(arg_default_lang().display_order(1))
        .arg(arg_dir().display_order(2));

    let subcommand_set = SubCommand::with_name("set")
        .about("Sets a property in 'snowchains.yml'")
        .usage(USAGE_SET)
        .arg(arg_key().display_order(1))
        .arg(arg_value().display_order(2));

    let subcommand_login = SubCommand::with_name("login")
        .about("Logins to a service")
        .usage(USAGE_LOGIN)
        .arg(arg_service().display_order(1));

    let subcommand_participate = SubCommand::with_name("participate")
        .about("Participates in a contest")
        .usage(USAGE_PARTICIPATE)
        .arg(arg_service_for_participate().display_order(1))
        .arg(arg_contest().display_order(2));

    let subcommand_download = SubCommand::with_name("download")
        .about("Downloads test cases")
        .usage(USAGE_DOWNLOAD)
        .arg(arg_open_browser().display_order(1));

    let subcommand_append = SubCommand::with_name("append")
        .about("Appends a test case to a test suite file")
        .usage(USAGE_APPEND)
        .arg(arg_target().display_order(1))
        .arg(arg_extension().display_order(2))
        .arg(arg_input().display_order(3))
        .arg(arg_output().display_order(4));

    let subcommand_judge = SubCommand::with_name("judge")
        .about("Tests a binary or script")
        .usage(USAGE_JUDGE)
        .arg(arg_target().display_order(1))
        .arg(arg_lang().display_order(2));

    let subcommand_submit = SubCommand::with_name("submit")
        .about("Submits code")
        .usage(USAGE_SUBMIT)
        .arg(arg_target())
        .arg(arg_lang())
        .arg(arg_open_browser().display_order(1))
        .arg(arg_skip_judging().display_order(2))
        .arg(arg_force().display_order(3));

    let matches = app_from_crate!()
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .setting(AppSettings::VersionlessSubcommands)
        .usage(
            format!(
                "{}\n    {}\n    {}\n    {}\n    {}\n    {}\n    {}\n    {}",
                USAGE_INIT_CONFIG,
                USAGE_SET,
                USAGE_LOGIN,
                USAGE_PARTICIPATE,
                USAGE_DOWNLOAD,
                USAGE_APPEND,
                USAGE_JUDGE,
                USAGE_SUBMIT
            ).as_str(),
        )
        .subcommand(subcommand_init_config.display_order(1))
        .subcommand(subcommand_set.display_order(2))
        .subcommand(subcommand_login.display_order(3))
        .subcommand(subcommand_participate.display_order(4))
        .subcommand(subcommand_download.display_order(5))
        .subcommand(subcommand_append.display_order(6))
        .subcommand(subcommand_judge.display_order(7))
        .subcommand(subcommand_submit.display_order(8))
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("init-config") {
        info!("Running command \"init-config\"");
        let lang = matches.value_of("default-lang").unwrap();
        let dir = matches.value_of("dir").unwrap();
        return Ok(config::create_config_file(lang, dir)?);
    } else if let Some(matches) = matches.subcommand_matches("set") {
        info!("Running command \"set\"");
        let key = value_t!(matches, "key", PropertyKey).unwrap();
        let value = matches.value_of("value").unwrap();
        return Ok(config::set_property(key, value)?);
    } else if let Some(matches) = matches.subcommand_matches("login") {
        info!("Running command \"login\"");
        let service = value_t!(matches, "service", ServiceName).unwrap();
        return Ok(match service {
            ServiceName::AtCoder => atcoder::login(),
            ServiceName::AtCoderBeta => atcoder_beta::login(),
            ServiceName::HackerRank => hackerrank::login(),
        }?);
    } else if let Some(matches) = matches.subcommand_matches("participate") {
        info!("Running command \"participate\"");
        let service = value_t!(matches, "service", ServiceName).unwrap();
        let contest = matches.value_of("contest").unwrap();
        return Ok(match service {
            ServiceName::AtCoder => atcoder::participate(contest),
            ServiceName::AtCoderBeta => atcoder_beta::participate(contest),
            ServiceName::HackerRank => unreachable!(),
        }?);
    } else if let Some(matches) = matches.subcommand_matches("download") {
        info!("Running command \"download\"");
        let config = Config::load_from_file()?;
        let open_browser = matches.is_present("open-browser");
        let service = config.service_name()?;
        let contest = config.contest_name()?;
        let dir_to_save = config.suite_dir()?;
        let extension = config.get_extension_on_downloading();
        return Ok(match service {
            ServiceName::AtCoder => atcoder::download(&contest, &dir_to_save, extension),
            ServiceName::AtCoderBeta => {
                atcoder_beta::download(&contest, &dir_to_save, extension, open_browser)
            }
            ServiceName::HackerRank => {
                hackerrank::download(&contest, &dir_to_save, extension, open_browser)
            }
        }?);
    } else if let Some(matches) = matches.subcommand_matches("append") {
        info!("Running command \"append\"");
        let config = Config::load_from_file()?;
        let target = matches.value_of("target").unwrap();
        let extension = value_t!(matches, "extension", SuiteFileExtension).unwrap();
        let input = matches.value_of("input").unwrap();
        let output = matches.value_of("output");
        let dir = config.suite_dir()?;
        let path = SuiteFilePath::new(&dir, target, extension);
        return Ok(testsuite::append(&path, input, output)?);
    } else if let Some(matches) = matches.subcommand_matches("judge") {
        info!("Running command \"judge\"");
        let config = Config::load_from_file()?;
        let target = matches.value_of("target").unwrap();
        let lang = matches.value_of("lang");
        let paths = config.suite_paths(target)?;
        let solver = config.construct_solver(target, lang)?;
        let compilation = config.construct_compilation_command(target, lang)?;
        judging::judge(&paths, solver, compilation)?;
        return Ok(());
    } else if let Some(matches) = matches.subcommand_matches("submit") {
        info!("Running command \"submit\"");
        let config = Config::load_from_file()?;
        let target = matches.value_of("target").unwrap();
        let lang = matches.value_of("lang");
        let open_browser = matches.is_present("open-browser");
        let skip_judging = matches.is_present("skip-judging");
        let force = matches.is_present("force");
        let service = config.service_name()?;
        let contest = config.contest_name()?;
        let src_path = config.src_path(target, lang)?;
        if !skip_judging {
            let paths = config.suite_paths(target)?;
            let solver = config.construct_solver(target, lang)?;
            let compilation = config.construct_compilation_command(target, lang)?;
            judging::judge(&paths, solver, compilation)?;
            println!("");
        }
        return Ok(match service {
            ServiceName::AtCoder => unimplemented!(),
            ServiceName::AtCoderBeta => {
                let lang_id = config.atcoder_lang_id(lang)?;
                atcoder_beta::submit(&contest, &target, lang_id, &src_path, open_browser, force)
            }
            ServiceName::HackerRank => unimplemented!(),
        }?);
    }
    unreachable!();
});
