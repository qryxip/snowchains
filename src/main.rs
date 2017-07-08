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
use clap::{App, AppSettings, Arg, SubCommand};
use std::path::Path;

fn main() {
    fn subcommand_participate_x(service: &'static str) -> App<'static, 'static> {
        SubCommand::with_name(service)
            .version(crate_version!())
            .arg(Arg::with_name("contest").required(true))
    }

    fn subcommand_download_x(service: &'static str) -> App<'static, 'static> {
        SubCommand::with_name(service)
            .version(crate_version!())
            .arg(Arg::with_name("contest").required(true))
            .arg(Arg::with_name("path").required(true))
    }

    fn subcommand_judge_xs(builds: &[&'static str]) -> App<'static, 'static> {
        let mut subcommand = SubCommand::with_name("judge")
            .setting(AppSettings::SubcommandRequiredElseHelp)
            .version(crate_version!());
        for build in builds {
            subcommand =
                subcommand.subcommand(SubCommand::with_name(build)
                                          .version(crate_version!())
                                          .arg(Arg::with_name("target").required(true))
                                          .arg(Arg::with_name("cases").required(true)));
        }
        subcommand
    }

    let subcommand_participate = SubCommand::with_name("participate")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .version(crate_version!())
        .subcommand(subcommand_participate_x("atcoder"));

    let subcommand_login =
        SubCommand::with_name("login")
            .setting(AppSettings::SubcommandRequiredElseHelp)
            .version(crate_version!())
            .subcommand(SubCommand::with_name("atcoder").version(crate_version!()));

    let subcommand_download = SubCommand::with_name("download")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .version(crate_version!())
        .subcommand(subcommand_download_x("atcoder"));

    let matches = app_from_crate!()
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .subcommand(subcommand_participate)
        .subcommand(subcommand_login)
        .subcommand(subcommand_download)
        .subcommand(subcommand_judge_xs(&["cargo"]))
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("participate") {
        if let Some(matches) = matches.subcommand_matches("atcoder") {
            let contest = matches.value_of("contest").unwrap();
            return atcoder::participate(contest).or_exit1();
        }
    } else if let Some(matches) = matches.subcommand_matches("login") {
        if let Some(_) = matches.subcommand_matches("atcoder") {
            return atcoder::login().or_exit1();
        }
    } else if let Some(matches) = matches.subcommand_matches("download") {
        if let Some(matches) = matches.subcommand_matches("atcoder") {
            let contest = matches.value_of("contest").unwrap();
            let path = Path::new(matches.value_of("path").unwrap());
            return atcoder::download(contest, &path, "toml").or_exit1();
        }
    } else if let Some(matches) = matches.subcommand_matches("judge") {
        if let Some(matches) = matches.subcommand_matches("cargo") {
            let target = matches.value_of("target").unwrap();
            let cases = matches.value_of("cases").unwrap();
            return cargo::judge(target, cases).or_exit1();
        }
    }
    unreachable!();
}
