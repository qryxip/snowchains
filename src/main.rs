#![recursion_limit = "1024"]

extern crate cookie;
extern crate pbr;
extern crate regex;
extern crate reqwest;
extern crate rpassword;
extern crate rusqlite;
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
extern crate clap;
#[macro_use]
extern crate custom_derive;
#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate newtype_derive;
#[macro_use]
extern crate serde_derive;

#[macro_use]
mod macros;

mod config;
mod error;
mod judge;
mod service;
mod testcase;
mod util;

use config::{Config, PropertyKey, ServiceName};
use error::SnowchainsResult;
use service::{atcoder, atcoder_beta, hackerrank};

use clap::{AppSettings, Arg, SubCommand};


quick_main_colored!(|| -> SnowchainsResult<()> {
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
            .possible_value("testcases")
            .possible_value("testcase_extension")
            .possible_value("default_lang")
            .required(true)
    }

    fn arg_value() -> Arg<'static, 'static> {
        Arg::with_name("value").help("Property value").required(
            true,
        )
    }

    fn arg_service() -> Arg<'static, 'static> {
        Arg::with_name("service")
            .possible_values(&["atcoder", "atcoder-beta", "hackerrank"])
            .help("Service name")
            .required(true)
    }

    fn arg_service_for_participate() -> Arg<'static, 'static> {
        Arg::with_name("service")
            .possible_values(&["atcoder", "atcoder-beta"])
            .help("Service name")
            .required(true)
    }

    fn arg_contest() -> Arg<'static, 'static> {
        Arg::with_name("contest").help("Contest name").required(
            true,
        )
    }

    fn arg_open_browser() -> Arg<'static, 'static> {
        Arg::with_name("open-browser").long("open-browser").help(
            "Whether to open the browser",
        )
    }

    fn arg_target() -> Arg<'static, 'static> {
        Arg::with_name("target").help("Target name").required(true)
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
        Arg::with_name("skip-judging").long("skip-judging").help(
            "Whether to skip judging",
        )
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
    static USAGE_APPEND: &'static str = "snowchains append <target> <input> [output]";
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
        .about("Appends a test case to a file")
        .usage(USAGE_APPEND)
        .arg(arg_target().display_order(1))
        .arg(arg_input().display_order(2))
        .arg(arg_output().display_order(3));

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
        let lang = matches.value_of("default-lang").unwrap();
        let dir = matches.value_of("dir").unwrap();
        return Ok(config::create_config_file(lang, dir)?);
    } else if let Some(matches) = matches.subcommand_matches("set") {
        let key = value_t!(matches, "key", PropertyKey).unwrap();
        let value = matches.value_of("value").unwrap();
        return Ok(config::set_property(key, value)?);
    } else if let Some(matches) = matches.subcommand_matches("login") {
        let service_name = value_t!(matches, "service", ServiceName).unwrap();
        return Ok(match service_name {
            ServiceName::AtCoder => atcoder::login(),
            ServiceName::AtCoderBeta => atcoder_beta::login(),
            ServiceName::HackerRank => hackerrank::login(),
        }?);
    } else if let Some(matches) = matches.subcommand_matches("participate") {
        let service_name = value_t!(matches, "service", ServiceName).unwrap();
        let contest_name = matches.value_of("contest").unwrap();
        return Ok(match service_name {
            ServiceName::AtCoder => atcoder::participate(contest_name),
            ServiceName::AtCoderBeta => atcoder_beta::participate(contest_name),
            ServiceName::HackerRank => unreachable!(),
        }?);
    } else if let Some(matches) = matches.subcommand_matches("download") {
        let config = Config::load_from_file()?;
        let open_browser = matches.is_present("open-browser");
        let service_name = config.service_name()?;
        let contest_name = config.contest_name()?;
        let dir_to_save = config.testcase_dir()?;
        let extension = config.testcase_extension();
        return Ok(match service_name {
            ServiceName::AtCoder => atcoder::download(&contest_name, &dir_to_save, extension),
            ServiceName::AtCoderBeta => {
                atcoder_beta::download(&contest_name, &dir_to_save, extension, open_browser)
            }
            ServiceName::HackerRank => {
                hackerrank::download(&contest_name, &dir_to_save, extension, open_browser)
            }
        }?);
    } else if let Some(matches) = matches.subcommand_matches("append") {
        let config = Config::load_from_file()?;
        let target = matches.value_of("target").unwrap();
        let input = matches.value_of("input").unwrap();
        let output = matches.value_of("output");
        let path = config.testcase_path(target)?;
        return Ok(testcase::append(&path, input, output)?);
    } else if let Some(matches) = matches.subcommand_matches("judge") {
        let config = Config::load_from_file()?;
        let target = matches.value_of("target").unwrap();
        let lang = matches.value_of("lang");
        let testcase_path = config.testcase_path(target)?;
        let run_command = config.construct_run_command(target, lang)?;
        let build_command = config.construct_build_command(target, lang)?;
        return Ok(judge::judge(testcase_path, run_command, build_command)?);
    } else if let Some(matches) = matches.subcommand_matches("submit") {
        let config = Config::load_from_file()?;
        let target = matches.value_of("target").unwrap();
        let lang = matches.value_of("lang");
        let open_browser = matches.is_present("open-browser");
        let skip_judging = matches.is_present("skip-judging");
        let force = matches.is_present("force");
        let service_name = config.service_name()?;
        let contest_name = config.contest_name()?;
        let src_path = config.src_path(target, lang)?;
        if !skip_judging {
            let testcase_path = config.testcase_path(target)?;
            let run_command = config.construct_run_command(target, lang)?;
            let build_command = config.construct_build_command(target, lang)?;
            judge::judge(testcase_path, run_command, build_command)?;
            println!("");
        }
        return Ok(match service_name {
            ServiceName::AtCoder => unimplemented!(),
            ServiceName::AtCoderBeta => {
                let lang_id = config.atcoder_lang_id(lang)?;
                atcoder_beta::submit(
                    &contest_name,
                    &target,
                    lang_id,
                    &src_path,
                    open_browser,
                    force,
                )
            }
            ServiceName::HackerRank => unimplemented!(),
        }?);
    }
    unreachable!();
});
