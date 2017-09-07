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
    fn arg_service() -> Arg<'static, 'static> {
        Arg::with_name("service")
            .possible_values(&["atcoder", "atcoder-beta"])
            .help("Service name")
            .required(true)
    }

    let subcommand_init_config = SubCommand::with_name("init-config")
        .version(crate_version!())
        .arg(
            Arg::with_name("default-lang")
                .help("Default language")
                .required(true),
        )
        .arg(
            Arg::with_name("dir")
                .help("Directory to create \"snowchains.yml\"")
                .required(true),
        );

    let subcommand_set = SubCommand::with_name("set")
        .version(crate_version!())
        .arg(
            Arg::with_name("property-or-target")
                .help(
                    "Property name (\"service\", \"contest\", \"testcases\", \
                     \"testcase_extension\", \"default_lang\") or target name",
                )
                .required(true),
        )
        .arg(Arg::with_name("value").required(true));

    let subcommand_login = SubCommand::with_name("login")
        .version(crate_version!())
        .arg(arg_service());

    let subcommand_participate = SubCommand::with_name("participate")
        .version(crate_version!())
        .arg(arg_service())
        .arg(Arg::with_name("contest").help("Contest name").required(
            true,
        ));

    let subcommand_download = SubCommand::with_name("download").version(crate_version!());

    let subcommand_judge = SubCommand::with_name("judge")
        .version(crate_version!())
        .arg(
            Arg::with_name("target")
                .help("Target name in \"snowchains.yml\"")
                .required(true),
        );

    let matches = app_from_crate!()
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .subcommand(subcommand_init_config)
        .subcommand(subcommand_set)
        .subcommand(subcommand_login)
        .subcommand(subcommand_participate)
        .subcommand(subcommand_download)
        .subcommand(subcommand_judge)
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
        let build_command = config.construct_build_command(target).or_exit1();
        let run_command = config.construct_run_command(target).or_exit1();
        return judge::judge(&cases, run_command, build_command).or_exit1();
    }
    unreachable!();
}
