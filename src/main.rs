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
use self::error::OrExit1;
use self::service::{atcoder, atcoder_beta};
use clap::{AppSettings, Arg, SubCommand};


fn main() {
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

    fn arg_target() -> Arg<'static, 'static> {
        Arg::with_name("target").help("Target name").required(true)
    }

    let subcommand_init_config = SubCommand::with_name("init-config")
        .arg(arg_default_lang())
        .arg(arg_dir());

    let subcommand_set = SubCommand::with_name("set")
        .arg(arg_property_or_target())
        .arg(arg_value());

    let subcommand_login = SubCommand::with_name("login").arg(arg_service());

    let subcommand_participate = SubCommand::with_name("participate")
        .arg(arg_service())
        .arg(arg_contest());

    let subcommand_download = SubCommand::with_name("download");

    let subcommand_judge = SubCommand::with_name("judge").arg(arg_target());

    let subcommand_submit = SubCommand::with_name("submit").arg(arg_target());

    let matches = app_from_crate!()
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .setting(AppSettings::VersionlessSubcommands)
        .subcommand(subcommand_init_config)
        .subcommand(subcommand_set)
        .subcommand(subcommand_login)
        .subcommand(subcommand_participate)
        .subcommand(subcommand_download)
        .subcommand(subcommand_judge)
        .subcommand(subcommand_submit)
        .get_matches();

    fn config() -> Config {
        Config::load_from_file().or_exit1()
    }

    if let Some(matches) = matches.subcommand_matches("init-config") {
        let lang = matches.value_of("default-lang").unwrap();
        let dir = matches.value_of("dir").unwrap();
        return config::create_config_file(lang, dir).or_exit1();
    } else if let Some(matches) = matches.subcommand_matches("set") {
        let key = matches.value_of("property-or-target").unwrap();
        let value = matches.value_of("value").unwrap();
        return config::set_property(key, value).or_exit1();
    } else if let Some(matches) = matches.subcommand_matches("login") {
        let service_name = matches.value_of("service").unwrap();
        if service_name == "atcoder" {
            return atcoder::login().or_exit1();
        } else if service_name == "atcoder-beta" {
            return atcoder_beta::login().or_exit1();
        }
    } else if let Some(matches) = matches.subcommand_matches("participate") {
        let contest = matches.value_of("contest").unwrap();
        let service_name = matches.value_of("service").unwrap();
        if service_name == "atcoder" {
            return atcoder::participate(&contest).or_exit1();
        } else if service_name == "atcoder-beta" {
            return atcoder_beta::participate(&contest).or_exit1();
        }
    } else if let Some(_) = matches.subcommand_matches("download") {
        let config = config();
        let contest = config.contest().or_exit1();
        let path = config.testcase_dir().or_exit1();
        let extension = config.testcase_extension();
        return match config.service().or_exit1() {
            ServiceName::AtCoder => atcoder::download(&contest, &path, extension),
            ServiceName::AtCoderBeta => atcoder_beta::download(&contest, &path, extension),
        }.or_exit1();
    } else if let Some(matches) = matches.subcommand_matches("judge") {
        let config = config();
        let target = matches.value_of("target").unwrap();
        let cases = config.testcase_path(target).or_exit1();
        let run_command = config.construct_run_command(target).or_exit1();
        let build_command = config.construct_build_command(target).or_exit1();
        return judge::judge(cases, run_command, build_command).or_exit1();
    } else if let Some(matches) = matches.subcommand_matches("submit") {
        let config = config();
        let target = matches.value_of("target").unwrap();
        let contest = config.contest().or_exit1();
        let cases = config.testcase_path(target).or_exit1();
        let src = config.src_path(&target).or_exit1();
        let run_command = config.construct_run_command(target).or_exit1();
        let build_command = config.construct_build_command(target).or_exit1();
        judge::judge(cases, run_command, build_command).or_exit1();
        return match config.service().or_exit1() {
            ServiceName::AtCoder => unimplemented!(),
            ServiceName::AtCoderBeta => {
                let lang_id = config.atcoder_lang_id(&target).or_exit1();
                atcoder_beta::submit(&contest, &target, lang_id, &src)
            }
        }.or_exit1();
    }
    unreachable!();
}
