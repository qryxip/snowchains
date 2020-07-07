mod config;

use std::ffi::OsString;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
pub enum Opt {}

pub fn run<I: IntoIterator<Item = S>, S: Into<OsString> + Clone>(args: I) -> anyhow::Result<()> {
    match Opt::from_iter_safe(args)? {}
}
