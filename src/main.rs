#![recursion_limit = "1024"]

extern crate cookie;
extern crate html5ever;
extern crate regex;
extern crate reqwest;
extern crate serde;
extern crate serde_json;
extern crate serde_urlencoded;
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

mod cargo;
mod error;
mod judge;
mod service;
mod util;

use self::error::OrExit1;
use self::service::atcoder;
use self::util::IntoStrVec;
use clap::{AppSettings, Arg, SubCommand};
use std::path::Path;


fn main() {
    let subcommand_login = SubCommand::with_name("login")
        .version(crate_version!())
        .arg(
            Arg::with_name("service")
                .possible_value("atcoder")
                .required(true),
        );

    let subcommand_participate = SubCommand::with_name("participate")
        .version(crate_version!())
        .arg(
            Arg::with_name("service")
                .possible_value("atcoder")
                .required(true),
        )
        .arg(Arg::with_name("contest").required(true));

    let subcommand_download = SubCommand::with_name("download")
        .version(crate_version!())
        .arg(
            Arg::with_name("service")
                .possible_value("atcoder")
                .required(true),
        )
        .arg(Arg::with_name("contest").required(true))
        .arg(Arg::with_name("path").required(true));

    let subcommand_judge = SubCommand::with_name("judge")
        .version(crate_version!())
        .after_help(
            "If you want to use flags in <args>, insert '--' anywhere before the flags.",
        )
        .arg(
            Arg::with_name("cases")
                .help("Relative or absolute path to the test file")
                .required(true),
        )
        .arg(
            Arg::with_name("target")
                .help("Relative or absolute path to the target")
                .required(true),
        )
        .arg(Arg::with_name("args").multiple(true));

    let subcommand_judge_cargo = SubCommand::with_name("judge-cargo")
        .version(crate_version!())
        .after_help(
            "If you want to use flags in <args>, insert '--' anywhere before the flags.",
        )
        .arg(
            Arg::with_name("cases")
                .help("Relative path to the test file from the crate root")
                .required(true),
        )
        .arg(
            Arg::with_name("target")
                .help(
                    "Relative path to the target from <crate root>/target/release",
                )
                .required(true),
        );

    let matches = app_from_crate!()
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .subcommand(subcommand_login)
        .subcommand(subcommand_participate)
        .subcommand(subcommand_download)
        .subcommand(subcommand_judge)
        .subcommand(subcommand_judge_cargo)
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("login") {
        if matches.value_of("service") == Some("atcoder") {
            return (atcoder::login()).or_exit1();
        }
    } else if let Some(matches) = matches.subcommand_matches("participate") {
        let contest = matches.value_of("contest").unwrap();
        if matches.value_of("service") == Some("atcoder") {
            return atcoder::participate(contest).or_exit1();
        }
    } else if let Some(matches) = matches.subcommand_matches("download") {
        let contest = matches.value_of("contest").unwrap();
        let path = Path::new(matches.value_of("path").unwrap());
        if matches.value_of("service") == Some("atcoder") {
            return atcoder::download(contest, &path, "toml").or_exit1();
        }
    } else if let Some(matches) = matches.subcommand_matches("judge") {
        let cases = matches.value_of("cases").unwrap();
        let target = matches.value_of("target").unwrap();
        let args = matches.values_of("args").into_str_vec();
        return judge::judge(cases, target, &args).or_exit1();
    } else if let Some(matches) = matches.subcommand_matches("judge-cargo") {
        let cases = matches.value_of("cases").unwrap();
        let target = matches.value_of("target").unwrap();
        let args = matches.values_of("args").into_str_vec();
        return cargo::judge(cases, target, &args).or_exit1();
    }
    unreachable!();
}
