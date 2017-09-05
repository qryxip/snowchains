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
    let subcommand_login = SubCommand::with_name("login")
        .version(crate_version!())
        .arg(
            Arg::with_name("service")
                .possible_values(&["atcoder", "atcoder-beta"])
                .help("Service name")
                .required(true),
        );

    let subcommand_participate = SubCommand::with_name("participate")
        .version(crate_version!())
        .arg(
            Arg::with_name("service")
                .possible_values(&["atcoder", "atcoder-beta"])
                .help("Service name")
                .required(true),
        )
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
        .subcommand(subcommand_login)
        .subcommand(subcommand_participate)
        .subcommand(subcommand_download)
        .subcommand(subcommand_judge)
        .get_matches();

    fn config() -> Config {
        Config::load_from_file().or_exit1()
    }

    if let Some(matches) = matches.subcommand_matches("login") {
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
        let command_params = config.construct_run_command(target).or_exit1();
        config.build_if_needed(target).or_exit1();
        return judge::run_judge(&cases, command_params).or_exit1();
    }
    unreachable!();
}
