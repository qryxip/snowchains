#![recursion_limit = "1024"]

extern crate cookie;
extern crate regex;
extern crate reqwest;
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
#[macro_use]
extern crate clap;
#[macro_use]
extern crate error_chain;
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

use self::config::{Config, ServiceName};
use self::error::SnowchainsResult;
use self::service::{atcoder, atcoder_beta};
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

    fn arg_property_or_target() -> Arg<'static, 'static> {
        Arg::with_name("property-or-target")
            .help(
                "Property name (\"service\", \"contest\", \"testcases\", \
                 \"testcase_extension\", \"default_lang\") or target name",
            )
            .required(true)
    }

    fn arg_value() -> Arg<'static, 'static> {
        Arg::with_name("value").required(true)
    }

    fn arg_service() -> Arg<'static, 'static> {
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

    fn arg_lang() -> Arg<'static, 'static> {
        Arg::with_name("lang").help("Language name")
    }

    static USAGE_INIT_CONFIG: &'static str = "snowchains init-config <default-lang> <dir>";
    static USAGE_SET: &'static str = "snowchains set <property-or-terget> <value>";
    static USAGE_LOGIN: &'static str = "snowchains login <service>";
    static USAGE_PARTICIPATE: &'static str = "snowchains participate <service> <contest>";
    static USAGE_DOWNLOAD: &'static str = "snowchains download [--open-browser]";
    static USAGE_JUDGE: &'static str = "snowchains judge <target> [lang]";
    static USAGE_SUBMIT: &'static str = "snowchains submit <target> [lang] [--open-browser]";

    let subcommand_init_config = SubCommand::with_name("init-config")
        .about("Creates 'snowchains.yml'")
        .usage(USAGE_INIT_CONFIG)
        .arg(arg_default_lang())
        .arg(arg_dir());

    let subcommand_set = SubCommand::with_name("set")
        .about("Sets a property in 'snowchains.yml'")
        .usage(USAGE_SET)
        .arg(arg_property_or_target())
        .arg(arg_value());

    let subcommand_login = SubCommand::with_name("login")
        .about("Logins to a service")
        .usage(USAGE_LOGIN)
        .arg(arg_service());

    let subcommand_participate = SubCommand::with_name("participate")
        .about("Participates in a contest")
        .usage(USAGE_PARTICIPATE)
        .arg(arg_service())
        .arg(arg_contest());

    let subcommand_download = SubCommand::with_name("download")
        .about("Downloads test cases")
        .usage(USAGE_DOWNLOAD)
        .arg(arg_open_browser());

    let subcommand_judge = SubCommand::with_name("judge")
        .about("Tests a binary or script")
        .usage(USAGE_JUDGE)
        .arg(arg_target())
        .arg(arg_lang());

    let subcommand_submit = SubCommand::with_name("submit")
        .about("Submits code")
        .usage(USAGE_SUBMIT)
        .arg(arg_target())
        .arg(arg_lang())
        .arg(arg_open_browser());

    let matches = app_from_crate!()
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .setting(AppSettings::VersionlessSubcommands)
        .usage(
            format!(
                "{}\n    {}\n    {}\n    {}\n    {}\n    {}\n    {}",
                USAGE_INIT_CONFIG,
                USAGE_SET,
                USAGE_LOGIN,
                USAGE_PARTICIPATE,
                USAGE_DOWNLOAD,
                USAGE_JUDGE,
                USAGE_SUBMIT
            ).as_str(),
        )
        .subcommand(subcommand_init_config.display_order(1))
        .subcommand(subcommand_set.display_order(2))
        .subcommand(subcommand_login.display_order(3))
        .subcommand(subcommand_participate.display_order(4))
        .subcommand(subcommand_download.display_order(5))
        .subcommand(subcommand_judge.display_order(6))
        .subcommand(subcommand_submit.display_order(7))
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("init-config") {
        let lang = matches.value_of("default-lang").unwrap();
        let dir = matches.value_of("dir").unwrap();
        return Ok(config::create_config_file(lang, dir)?);
    } else if let Some(matches) = matches.subcommand_matches("set") {
        let key = matches.value_of("property-or-target").unwrap();
        let value = matches.value_of("value").unwrap();
        return Ok(config::set_property(key, value)?);
    } else if let Some(matches) = matches.subcommand_matches("login") {
        let service_name = matches.value_of("service").unwrap();
        if service_name == "atcoder" {
            return Ok(atcoder::login()?);
        } else if service_name == "atcoder-beta" {
            return Ok(atcoder_beta::login()?);
        }
    } else if let Some(matches) = matches.subcommand_matches("participate") {
        let contest = matches.value_of("contest").unwrap();
        let service_name = matches.value_of("service").unwrap();
        if service_name == "atcoder" {
            return Ok(atcoder::participate(&contest)?);
        } else if service_name == "atcoder-beta" {
            return Ok(atcoder_beta::participate(&contest)?);
        }
    } else if let Some(matches) = matches.subcommand_matches("download") {
        let config = Config::load_from_file()?;
        let open_browser = matches.is_present("open-browser");
        let contest = config.contest()?;
        let path = config.testcase_dir()?;
        let extension = config.testcase_extension();
        return Ok(match config.service()? {
            ServiceName::AtCoder => atcoder::download(&contest, &path, extension),
            ServiceName::AtCoderBeta => {
                atcoder_beta::download(&contest, &path, extension, open_browser)
            }
        }?);
    } else if let Some(matches) = matches.subcommand_matches("judge") {
        let config = Config::load_from_file()?;
        let target = matches.value_of("target").unwrap();
        let lang = matches.value_of("lang");
        let cases = config.testcase_path(target)?;
        let run_command = config.construct_run_command(target, lang)?;
        let build_command = config.construct_build_command(lang)?;
        return Ok(judge::judge(cases, run_command, build_command)?);
    } else if let Some(matches) = matches.subcommand_matches("submit") {
        let config = Config::load_from_file()?;
        let target = matches.value_of("target").unwrap();
        let lang = matches.value_of("lang");
        let open_browser = matches.is_present("open-browser");
        let contest = config.contest()?;
        let cases = config.testcase_path(target)?;
        let src = config.src_path(target, lang)?;
        let run_command = config.construct_run_command(target, lang)?;
        let build_command = config.construct_build_command(lang)?;
        judge::judge(cases, run_command, build_command)?;
        return Ok(match config.service()? {
            ServiceName::AtCoder => unimplemented!(),
            ServiceName::AtCoderBeta => {
                let lang_id = config.atcoder_lang_id(lang)?;
                atcoder_beta::submit(&contest, &target, lang_id, &src, open_browser)
            }
        }?);
    }
    unreachable!();
});
