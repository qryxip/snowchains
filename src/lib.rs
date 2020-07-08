mod commands;
mod config;
mod shell;

pub use crate::commands::init::OptInit;
use std::{io::BufRead, path::PathBuf};
use structopt::StructOpt;
use strum::{EnumString, EnumVariantNames};
use termcolor::WriteColor;

#[derive(StructOpt, Debug)]
pub enum Opt {
    Init(OptInit),
}

impl Opt {
    pub fn color(&self) -> crate::ColorChoice {
        match *self {
            Self::Init(OptInit { color, .. }) => color,
        }
    }
}

#[derive(EnumVariantNames, EnumString, Debug, Clone, Copy)]
#[strum(serialize_all = "lowercase")]
pub enum ColorChoice {
    Auto,
    Always,
    Never,
}

#[derive(Debug)]
pub struct Context<R, W1, W2> {
    pub cwd: PathBuf,
    pub stdin: R,
    pub stdout: W1,
    pub stderr: W2,
    pub draw_progress: bool,
}

pub fn run<R: BufRead, W1: WriteColor, W2: WriteColor>(
    opt: Opt,
    ctx: Context<R, W1, W2>,
) -> anyhow::Result<()> {
    match opt {
        Opt::Init(opt) => commands::init::run(opt, ctx),
    }
}
