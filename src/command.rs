use crate::errors::{
    HookCommandErrorKind, HookCommandResult, JudgeErrorKind, JudgeResult, StdError,
};
use crate::path::AbsPathBuf;
use crate::terminal::{HasTermProps, WriteExt as _};
use crate::util::collections::NonEmptyVec;

use failure::Fail as _;
use itertools::Itertools as _;
use serde_derive::Serialize;
use termcolor::WriteColor;
use tokio_process::CommandExt as _;

use std::collections::HashMap;
use std::ffi::OsString;
use std::iter::FromIterator;
use std::process::{Command, ExitStatus, Stdio};
use std::time::Instant;
use std::{env, io};

#[derive(Debug)]
pub(crate) struct HookCommands(Option<NonEmptyVec<HookCommand>>);

impl HookCommands {
    pub(crate) fn run(
        &self,
        stdout: impl HasTermProps,
        mut stderr: impl WriteColor + HasTermProps,
    ) -> HookCommandResult<()> {
        if let HookCommands(Some(this)) = self {
            stderr.set_color(color!(bold))?;
            stderr.write_str("Running hooks...")?;
            stderr.reset()?;
            writeln!(stderr)?;
            stderr.flush()?;
            for cmd in this {
                HookCommand::run(cmd, &stdout, &stderr)?;
            }
            stderr.set_color(color!(bold))?;
            stderr.write_str("Done.")?;
            stderr.reset()?;
            writeln!(stderr)?;
            stderr.flush()?;
        }
        Ok(())
    }
}

impl FromIterator<HookCommand> for HookCommands {
    fn from_iter<T: IntoIterator<Item = HookCommand>>(iter: T) -> Self {
        HookCommands(NonEmptyVec::try_new(iter.into_iter().collect()))
    }
}

#[derive(Debug)]
pub(crate) struct HookCommand {
    inner: Inner,
}

impl HookCommand {
    pub(crate) fn try_new(
        args: Vec<OsString>,
        working_dir: AbsPathBuf,
        envs: HashMap<String, OsString>,
    ) -> std::result::Result<Self, (OsString, which::Error)> {
        let inner = Inner::try_new(args, working_dir, envs)?;
        Ok(Self { inner })
    }

    fn run(&self, stdout: impl HasTermProps, stderr: impl HasTermProps) -> HookCommandResult<()> {
        let status = self
            .inner
            .build_checking_wd()?
            .stdin(Stdio::null())
            .stdout(stdout.process_redirection())
            .stderr(stderr.process_redirection())
            .status()
            .map_err(|e| {
                let arg0 = self.inner.args[0].clone();
                StdError::from(e).context(HookCommandErrorKind::Start(arg0))
            })?;
        if status.success() {
            Ok(())
        } else {
            let arg0 = self.inner.args[0].clone();
            Err(HookCommandErrorKind::NotSuccess(arg0, status).into())
        }
    }
}

/// Transpilation command.
#[derive(PartialEq, Eq, Debug, Hash, Serialize)]
#[serde(transparent)]
pub(crate) struct TranspilationCommand(BuildCommand);

impl TranspilationCommand {
    /// Constructs a new `TranspilationCommand`.
    pub(crate) fn try_new(
        args: Vec<OsString>,
        working_dir: AbsPathBuf,
        src: AbsPathBuf,
        transpiled: Option<AbsPathBuf>,
        envs: HashMap<String, OsString>,
    ) -> std::result::Result<Self, (OsString, which::Error)> {
        BuildCommand::try_new(args, working_dir, src, transpiled, "transpiled", envs).map(Self)
    }

    /// Executes the command.
    pub(crate) fn run(
        &self,
        stdout: impl HasTermProps,
        stderr: impl WriteColor + HasTermProps,
        force: bool,
    ) -> JudgeResult<()> {
        let messages = Messages {
            command: "Transpilation Command:",
            wd: "Working directory:    ",
            finished: "Finished transpiling",
        };
        let error = |status| JudgeErrorKind::Build {
            noun: "transpilation",
            status,
        };
        self.0.run(stdout, stderr, force, messages, error)
    }
}

/// Compilation command.
#[derive(Debug, PartialEq, Eq, Hash, Serialize)]
#[serde(transparent)]
pub(crate) struct CompilationCommand(BuildCommand);

impl CompilationCommand {
    /// Constructs a new `CompilationCommand`.
    pub(crate) fn try_new(
        args: Vec<OsString>,
        working_dir: AbsPathBuf,
        src: AbsPathBuf,
        bin: Option<AbsPathBuf>,
        envs: HashMap<String, OsString>,
    ) -> std::result::Result<Self, (OsString, which::Error)> {
        BuildCommand::try_new(args, working_dir, src, bin, "bin", envs).map(Self)
    }

    /// Executes the command.
    pub(crate) fn run(
        &self,
        stdout: impl HasTermProps,
        stderr: impl WriteColor + HasTermProps,
        force: bool,
    ) -> JudgeResult<()> {
        let messages = Messages {
            command: "Compilation Command:",
            wd: "Working directory:  ",
            finished: "Finished compiling",
        };
        let error = |status| JudgeErrorKind::Build {
            noun: "compilation",
            status,
        };
        self.0.run(stdout, stderr, force, messages, error)
    }
}

#[derive(Debug, Clone, Copy)]
struct Messages {
    command: &'static str,
    wd: &'static str,
    finished: &'static str,
}

#[derive(PartialEq, Eq, Debug, Hash, Serialize)]
struct BuildCommand {
    inner: Inner,
    src: AbsPathBuf,
    target: Option<AbsPathBuf>,
    target_name: &'static str,
}

impl BuildCommand {
    fn try_new(
        args: Vec<OsString>,
        working_dir: AbsPathBuf,
        src: AbsPathBuf,
        target: Option<AbsPathBuf>,
        target_name: &'static str,
        envs: HashMap<String, OsString>,
    ) -> std::result::Result<Self, (OsString, which::Error)> {
        let envs = envs.into_iter().collect();
        let inner = Inner::try_new(args, working_dir, envs)?;
        Ok(Self {
            inner,
            src,
            target,
            target_name,
        })
    }

    fn run(
        &self,
        stdout: impl HasTermProps,
        mut stderr: impl WriteColor + HasTermProps,
        force: bool,
        messages: Messages,
        error: fn(ExitStatus) -> JudgeErrorKind,
    ) -> JudgeResult<()> {
        if !self.src.exists() {
            stderr.set_color(color!(fg(Yellow), intense))?;
            write!(stderr, "Warning: {} not found", self.src.display())?;
            stderr.reset()?;
            writeln!(stderr)?;
            stderr.flush()?;
        }
        if self.target.as_ref().is_none() {
            stderr.set_color(color!(fg(Yellow), intense))?;
            write!(stderr, "Warning: {:?} not present", self.target_name)?;
            stderr.reset()?;
            writeln!(stderr)?;
            stderr.flush()?;
        }

        if let Some(target) = &self.target {
            if self.src.exists() && target.exists() {
                if !force && self.src.metadata()?.modified()? < target.metadata()?.modified()? {
                    writeln!(stderr, "{} is up to date.", target.display())?;
                    return stderr.flush().map_err(Into::into);
                }
            } else if let Some(parent) = target.parent() {
                if !parent.exists() {
                    crate::fs::create_dir_all(&parent)?;
                    writeln!(stderr, "Created {}", parent.display())?;
                    stderr.flush()?;
                }
            }
        }

        let args = self.inner.format_args();
        let wd = self.inner.working_dir.display().to_string();
        write_info(&mut stderr, messages.command, &args)?;
        write_info(&mut stderr, messages.wd, &[&wd])?;
        writeln!(stderr)?;
        stderr.flush()?;

        let mut proc = self.inner.build_checking_wd()?;
        proc.stdin(Stdio::null())
            .stdout(stdout.process_redirection())
            .stderr(stderr.process_redirection());
        let start = Instant::now();
        let status = proc.status().map_err(|e| {
            StdError::from(e).context(JudgeErrorKind::Command(self.inner.args[0].clone()))
        })?;
        let elapsed = Instant::now() - start;

        match status.code() {
            Some(0) => {
                stderr.set_color(color!(fg(Green), intense, bold))?;
                writeln!(stderr, "{} in {:?}.", messages.finished, elapsed)?;
            }
            Some(code) => {
                stderr.set_color(color!(fg(Red), intense, bold))?;
                writeln!(stderr, "Exited with {} in {:?}.", code, elapsed)?;
            }
            None => {
                stderr.set_color(color!(fg(Yellow), intense, bold))?;
                writeln!(stderr, "Exited without code in {:?}.", elapsed)?;
            }
        }

        if status.success() {
            if let Some(target) = &self.target {
                if !target.exists() {
                    stderr.set_color(color!(fg(Yellow), intense))?;
                    writeln!(stderr, "Warning: {} not created", target.display())?;
                    stderr.reset()?;
                    stderr.flush()?;
                }
            }
            Ok(())
        } else {
            Err(error(status).into())
        }
    }
}

/// Command for batch/interactive testing.
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Serialize)]
pub(crate) struct JudgingCommand {
    inner: Inner,
    crlf_to_lf: bool,
}

impl JudgingCommand {
    /// Constructs a new `JudgingCommand`.
    pub(crate) fn try_new(
        args: Vec<OsString>,
        working_dir: AbsPathBuf,
        crlf_to_lf: bool,
        envs: HashMap<String, OsString>,
    ) -> std::result::Result<Self, (OsString, which::Error)> {
        Inner::try_new(args, working_dir, envs).map(|inner| JudgingCommand { inner, crlf_to_lf })
    }

    pub(crate) fn crlf_to_lf(&self) -> bool {
        self.crlf_to_lf
    }

    /// Prints the arguments and working directory.
    ///
    /// Format:
    /// """
    /// Command:           "arg0" "arg1" "arg2" ...
    /// Working directory: /path/to/working/dir/
    /// Test files:        /path/to/testfiles/{a.json, a.yaml}
    /// """
    pub(crate) fn write_info(
        &self,
        mut out: impl WriteColor + HasTermProps,
        paths_formatted: &str,
    ) -> io::Result<()> {
        let args = self.inner.format_args();
        let wd = self.inner.working_dir.display().to_string();
        write_info(&mut out, "Command:          ", &args)?;
        write_info(&mut out, "Working directory:", &[&wd])?;
        write_info(&mut out, "Test files:       ", &[paths_formatted])?;
        if self.crlf_to_lf {
            write_info(&mut out, "CRLF to LF:       ", &["true"])?;
        } else if cfg!(windows) {
            write_info(&mut out, "CRLF to LF:       ", &["false"])?;
        }
        Ok(())
    }

    pub(crate) fn spawn_async_piped(&self) -> JudgeResult<tokio_process::Child> {
        self.inner
            .build_checking_wd()?
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn_async()
            .map_err(|e| {
                StdError::from(e)
                    .context(JudgeErrorKind::Command(self.inner.args[0].clone()))
                    .into()
            })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
struct Inner {
    args: NonEmptyVec<OsString>,
    working_dir: AbsPathBuf,
    envs: Vec<(String, OsString)>,
}

impl Inner {
    fn try_new(
        args: Vec<OsString>,
        working_dir: AbsPathBuf,
        envs: HashMap<String, OsString>,
    ) -> std::result::Result<Self, (OsString, which::Error)> {
        let mut args = NonEmptyVec::try_new(args).unwrap_or_default();
        // https://github.com/rust-lang/rust/issues/37519
        let path = envs
            .get("PATH")
            .map(ToOwned::to_owned)
            .or_else(|| env::var_os("PATH"));
        args[0] = which::which_in(&args[0], path, &working_dir)
            .map_err(|e| (args[0].to_owned(), e))?
            .into();
        Ok(Self {
            args,
            working_dir,
            envs: envs.into_iter().collect(),
        })
    }

    fn format_args(&self) -> Vec<String> {
        self.args.iter().map(|s| format!("{:?}", s)).collect()
    }

    fn build_checking_wd(&self) -> io::Result<Command> {
        if let Err(err) = self.working_dir.metadata() {
            Err(io::Error::new(
                err.kind(),
                format!(
                    "Failed to access {:?}. Check \"working_directory\" in snowchains.toml",
                    self.working_dir,
                ),
            ))
        } else {
            let mut command = Command::new(&self.args[0]);
            command.args(&self.args[1..]).current_dir(&self.working_dir);
            command.envs(self.envs.iter().map(|(k, v)| (k, v)));
            Ok(command)
        }
    }
}

fn write_info(
    mut out: impl WriteColor + HasTermProps,
    title: &str,
    rest: &[impl AsRef<str>],
) -> io::Result<()> {
    out.set_color(color!(fg(Magenta), intense, bold))?;
    out.write_str(title)?;
    out.reset()?;
    out.write_str(" ")?;
    if let Some(w) = out.columns() {
        let o = out.str_width(title) + 1;
        let mut x = 0;
        macro_rules! next_line {
            () => {
                writeln!(out)?;
                out.write_spaces(o)?;
                x = 0;
            };
        }
        for s in rest.iter().map(AsRef::as_ref) {
            let l = out.str_width(s);
            if o + x + l > w && x > 0 {
                next_line!();
            }
            if o + l > w && x == 0 {
                for c in s.chars() {
                    let l = out.char_width(c).unwrap_or(0);
                    if o + x + l > w {
                        next_line!();
                    }
                    write!(out, "{}", c)?;
                    x += l;
                }
            } else if x == 0 {
                write!(out, "{}", s)?;
                x += l;
            } else {
                write!(out, " {}", s)?;
                x += l + 1;
            }
        }
        writeln!(out)
    } else {
        writeln!(out, "{}", rest.iter().map(AsRef::as_ref).format(" "))
    }
}
