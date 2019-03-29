use crate::errors::{
    HookCommandErrorKind, HookCommandResult, JudgeErrorKind, JudgeResult, StdError,
};
use crate::path::AbsPathBuf;
use crate::terminal::{TermOut, WriteSpaces as _};
use crate::util::collections::NonEmptyVec;

use failure::Fail as _;
use itertools::Itertools as _;
use tokio_process::CommandExt as _;

use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::OsString;
use std::io;
use std::iter::FromIterator;
use std::path::Path;
use std::process::{Command, ExitStatus, Stdio};
use std::time::Instant;

pub(crate) struct HookCommands(Option<NonEmptyVec<HookCommand>>);

impl HookCommands {
    pub(crate) fn run<O: TermOut, E: TermOut>(&self, mut stderr: E) -> HookCommandResult<()> {
        if let HookCommands(Some(this)) = self {
            stderr.with_reset(|o| o.bold()?.write_str("Running hooks...\n"))?;
            stderr.flush()?;
            this.iter().try_for_each(|c| c.run::<O, E>())?;
            stderr.with_reset(|o| o.bold()?.write_str("Done.\n"))?;
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

pub(crate) struct HookCommand {
    inner: Inner,
}

impl HookCommand {
    pub(crate) fn new(
        args: Vec<OsString>,
        working_dir: AbsPathBuf,
        envs: HashMap<String, OsString>,
    ) -> Self {
        Self {
            inner: Inner::new(args, working_dir, envs),
        }
    }

    fn run<O: TermOut, E: TermOut>(&self) -> HookCommandResult<()> {
        let status = self
            .inner
            .build_checking_wd()?
            .stdin(Stdio::null())
            .stdout(O::process_redirection())
            .stderr(E::process_redirection())
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
#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq, Eq, Hash)]
pub(crate) struct TranspilationCommand {
    repr: BuildCommand,
}

impl TranspilationCommand {
    /// Constructs a new `TranspilationCommand`.
    pub(crate) fn new(
        args: Vec<OsString>,
        working_dir: AbsPathBuf,
        src: AbsPathBuf,
        transpiled: Option<AbsPathBuf>,
        envs: HashMap<String, OsString>,
    ) -> Self {
        Self {
            repr: BuildCommand::new(args, working_dir, src, transpiled, "transpiled", envs),
        }
    }

    /// Executes the command.
    pub(crate) fn run<O: TermOut, E: TermOut>(&self, stderr: E, force: bool) -> JudgeResult<()> {
        let messages = Messages {
            command: "Transpilation Command:",
            wd: "Working directory:    ",
            finished: "Finished transpiling",
        };
        let error = |status| JudgeErrorKind::Build {
            noun: "transpilation",
            status,
        };
        self.repr.run::<O, _>(stderr, force, messages, error)
    }
}

/// Compilation command.
#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq, Eq, Hash)]
pub(crate) struct CompilationCommand {
    repr: BuildCommand,
}

impl CompilationCommand {
    /// Constructs a new `CompilationCommand`.
    pub(crate) fn new(
        args: Vec<OsString>,
        working_dir: AbsPathBuf,
        src: AbsPathBuf,
        bin: Option<AbsPathBuf>,
        envs: HashMap<String, OsString>,
    ) -> Self {
        Self {
            repr: BuildCommand::new(args, working_dir, src, bin, "bin", envs),
        }
    }

    /// Executes the command.
    pub(crate) fn run<O: TermOut, E: TermOut>(&self, stderr: E, force: bool) -> JudgeResult<()> {
        let messages = Messages {
            command: "Compilation Command:",
            wd: "Working directory:  ",
            finished: "Finished compiling",
        };
        let error = |status| JudgeErrorKind::Build {
            noun: "compilation",
            status,
        };
        self.repr.run::<O, _>(stderr, force, messages, error)
    }
}

#[derive(Clone, Copy)]
struct Messages {
    command: &'static str,
    wd: &'static str,
    finished: &'static str,
}

#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq, Eq, Hash)]
struct BuildCommand {
    inner: Inner,
    src: AbsPathBuf,
    target: Option<AbsPathBuf>,
    target_name: &'static str,
}

impl BuildCommand {
    fn new(
        args: Vec<OsString>,
        working_dir: AbsPathBuf,
        src: AbsPathBuf,
        target: Option<AbsPathBuf>,
        target_name: &'static str,
        envs: HashMap<String, OsString>,
    ) -> Self {
        let envs = envs.into_iter().collect();
        let inner = Inner::new(args, working_dir, envs);
        Self {
            inner,
            src,
            target,
            target_name,
        }
    }

    fn run<O: TermOut, E: TermOut>(
        &self,
        mut stderr: E,
        force: bool,
        messages: Messages,
        error: fn(ExitStatus) -> JudgeErrorKind,
    ) -> JudgeResult<()> {
        if !self.src.exists() {
            stderr
                .with_reset(|o| writeln!(o.fg(11)?, "Warning: {} not found", self.src.display()))?;
            stderr.flush()?;
        }
        if self.target.as_ref().is_none() {
            stderr.with_reset(|o| {
                writeln!(o.fg(11)?, "Warning: {:?} not present", self.target_name)
            })?;
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
            .stdout(O::process_redirection())
            .stderr(E::process_redirection());
        let start = Instant::now();
        let status = proc.status().map_err(|e| {
            StdError::from(e).context(JudgeErrorKind::Command(self.inner.args[0].clone()))
        })?;
        let elapsed = Instant::now() - start;

        let (mes, color) = match status.code() {
            Some(0) => (Cow::from(messages.finished), 10),
            Some(code) => (Cow::from(format!("Exited with {}", code)), 9),
            None => (Cow::from("Exited without code"), 11),
        };
        stderr.with_reset(|o| writeln!(o.fg(color)?.bold()?, "{} in {:?}.", mes, elapsed))?;

        if status.success() {
            if let Some(target) = &self.target {
                if !target.exists() {
                    stderr.with_reset(|o| {
                        writeln!(o.fg(11)?, "Warning: {} not created", target.display())
                    })?;
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
#[cfg_attr(test, derive(Debug, PartialEq))]
pub(crate) struct JudgingCommand {
    inner: Inner,
    crlf_to_lf: bool,
}

impl JudgingCommand {
    /// Constructs a new `JudgingCommand`.
    pub(crate) fn new(
        args: Vec<OsString>,
        working_dir: AbsPathBuf,
        crlf_to_lf: bool,
        envs: HashMap<String, OsString>,
    ) -> Self {
        JudgingCommand {
            inner: Inner::new(args, working_dir, envs),
            crlf_to_lf,
        }
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
        mut out: impl TermOut,
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

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, PartialEq, Eq, Hash)]
struct Inner {
    args: NonEmptyVec<OsString>,
    working_dir: AbsPathBuf,
    envs: Vec<(String, OsString)>,
}

impl Inner {
    fn new(args: Vec<OsString>, working_dir: AbsPathBuf, envs: HashMap<String, OsString>) -> Self {
        let mut args = NonEmptyVec::try_new(args).unwrap_or_default();
        // https://github.com/rust-lang/rust/issues/37519
        if cfg!(windows) && Path::new(&args[0]).is_relative() {
            let abs = working_dir.join_canonicalizing_lossy(&args[0]);
            if abs.exists() {
                args[0] = abs.into();
            }
        }
        Self {
            args,
            working_dir,
            envs: envs.into_iter().collect(),
        }
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

fn write_info<O: TermOut>(mut out: O, title: &str, rest: &[impl AsRef<str>]) -> io::Result<()> {
    out.with_reset(|o| o.fg(13)?.bold()?.write_str(title))?;
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
                    let l = out.char_width_or_zero(c);
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
