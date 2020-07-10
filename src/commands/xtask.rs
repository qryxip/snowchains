use crate::config;
use anyhow::{bail, ensure};
use std::ffi::OsString;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
pub struct OptXtask {
    /// Name
    pub subcommand: String,

    /// Arguments for the subcommand
    #[structopt(parse(from_os_str))]
    pub args: Vec<OsString>,
}

pub(crate) fn run(
    opt: OptXtask,
    ctx: crate::Context<impl Sized, impl Sized, impl Sized>,
) -> anyhow::Result<()> {
    let OptXtask { subcommand, args } = opt;

    let crate::Context {
        cwd,
        stdin: _,
        stdout: _,
        stderr: _,
        draw_progress: _,
    } = ctx;

    let cmd = config::xtask(&cwd, None, &subcommand)?;

    ensure!(!cmd.is_empty(), "Empty arguments");

    let status = std::process::Command::new(&cmd[0])
        .args(&cmd[1..])
        .args(args)
        .stdin(std::process::Stdio::inherit())
        .stdout(std::process::Stdio::inherit())
        .stderr(std::process::Stdio::inherit())
        .status()?;

    if !status.success() {
        match status.code() {
            Some(code) => bail!("The custom subcommand exited with {}", code),
            None => bail!("The custom subcommand was terminated by signal"),
        }
    }

    Ok(())
}
