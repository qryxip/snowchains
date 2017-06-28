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
mod util;

use self::error::OrExit1;
use clap::{AppSettings, Arg, SubCommand};

fn main() {
    static USAGE: &'static str = "snowchains cargo judge <problem>";

    let subcommand_cargo_judge =
        SubCommand::with_name("judge")
            .template("snowchains cargo judge (snowchains {version})\n\
                       {about}\n\n\
                       USAGE:\n    {usage}\n\n\
                       FLAGS:\n{flags}\n\n\
                       ARGS:\n{positionals}")
            .version(crate_version!())
            .about("Target: <crate root>/target/release/<problem>\n\
                    Test Cases: <crate root>/cases/<problem>.toml")
            .usage(USAGE)
            .args(&[Arg::with_name("target").help("Target").required(true),
                    Arg::with_name("cases").help("Test cases").required(true)]);

    let subcommand_cargo = SubCommand::with_name("cargo")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .template("snowchains cargo (snowchains {version})\n\n\
                   USAGE:\n    {usage}\n\n\
                   FLAGS:\n{flags}")
        .version(crate_version!())
        .usage(USAGE)
        .subcommand(subcommand_cargo_judge);

    let matches = app_from_crate!()
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .usage(USAGE)
        .subcommand(subcommand_cargo)
        .get_matches();

    let matches_cargo_judge = matches
        .subcommand_matches("cargo")
        .unwrap()
        .subcommand_matches("judge")
        .unwrap();
    let target = matches_cargo_judge.value_of("target").unwrap();
    let cases = matches_cargo_judge.value_of("cases").unwrap();
    cargo::judge(target, cases).or_exit1();
}
