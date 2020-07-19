use crate::config;
use anyhow::{bail, ensure};
use std::{ffi::OsString, io::Write as _};
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
        stdin_process_redirection,
        stdout_process_redirection,
        stderr_process_redirection,
        draw_progress: _,
    } = ctx;

    let config::Script {
        program,
        extension,
        content,
    } = config::xtask(&cwd, None, &subcommand)?;

    let mut tempfile = tempfile::Builder::new()
        .prefix(&format!("snowchains-xtask-{}", subcommand))
        .suffix(&format!(".{}", extension))
        .tempfile()?;

    tempfile.write_all(content.as_ref())?;
    tempfile.flush()?;

    let status = std::process::Command::new(program)
        .arg(tempfile.path())
        .args(args)
        .stdin(stdin_process_redirection)
        .stdout(stdout_process_redirection)
        .stderr(stderr_process_redirection)
        .status()?;

    if !status.success() {
        match status.code() {
            Some(code) => bail!("The custom subcommand exited with {}", code),
            None => bail!("The custom subcommand was terminated by signal"),
        }
    }

    tempfile.close()?;
    Ok(())
}
