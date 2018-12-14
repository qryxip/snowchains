use crate::errors::{JudgeErrorKind, JudgeResult, StdError};
use crate::path::AbsPathBuf;
use crate::terminal::{TermOut, WriteSpaces as _WriteSpaces};
use crate::util::collections::NonEmptyVec;

use failure::Fail as _Fail;
use itertools::Itertools as _Itertools;
use tokio_process::CommandExt as _CommandExt;

use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::OsString;
use std::io;
use std::path::Path;
use std::process::{Command, Stdio};
use std::time::Instant;

/// Compilation command.
#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq, Eq, Hash)]
pub(crate) struct CompilationCommand {
    inner: Inner,
    src: AbsPathBuf,
    bin: AbsPathBuf,
}

impl CompilationCommand {
    /// Constructs a new `CompilationCommand`.
    pub(crate) fn new(
        args: Vec<OsString>,
        working_dir: AbsPathBuf,
        src: AbsPathBuf,
        bin: AbsPathBuf,
        envs: HashMap<OsString, OsString>,
    ) -> Self {
        let envs = envs.into_iter().collect();
        let inner = Inner::new(args, working_dir, envs);
        Self { inner, src, bin }
    }

    /// Executes the command.
    pub fn run<O: TermOut, E: TermOut>(
        &self,
        mut stdout: O,
        mut stderr: E,
        force_compile: bool,
    ) -> JudgeResult<()> {
        if !self.src.exists() {
            stderr
                .with_reset(|o| writeln!(o.fg(11)?, "Warning: {} not found", self.src.display()))?;
            stderr.flush()?;
        }
        if self.src.exists() && self.bin.exists() {
            if !force_compile
                && self.src.metadata()?.modified()? < self.bin.metadata()?.modified()?
            {
                writeln!(stdout, "{} is up to date.", self.bin.display())?;
                return stdout.flush().map_err(Into::into);
            }
        } else if let Some(parent) = self.bin.parent() {
            if !parent.exists() {
                crate::fs::create_dir_all(&parent)?;
                writeln!(stdout, "Created {}", parent.display())?;
                stdout.flush()?;
            }
        }

        let args = self.inner.format_args();
        let wd = self.inner.working_dir.display().to_string();
        write_info(&mut stdout, "Compilation Command:", &args)?;
        write_info(&mut stdout, "Working directory:  ", &[&wd])?;
        writeln!(stdout)?;
        stdout.flush()?;

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
            Some(0) => (Cow::from("Finished compiling"), 10),
            Some(code) => (Cow::from(format!("Exited with {}", code)), 9),
            None => (Cow::from("Exited without code"), 11),
        };
        stdout.with_reset(|o| writeln!(o.fg(color)?.bold()?, "{} in {:?}.", mes, elapsed))?;

        if status.success() {
            if !self.bin.exists() {
                stderr.with_reset(|o| {
                    writeln!(o.fg(11)?, "Warning: {} not created", self.bin.display())
                })?;
                stderr.flush()?;
            }
            Ok(())
        } else {
            Err(JudgeErrorKind::Compile(status).into())
        }
    }
}

/// Command for simple/interactive testing.
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
        envs: HashMap<OsString, OsString>,
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
    envs: Vec<(OsString, OsString)>,
}

impl Inner {
    fn new(
        args: Vec<OsString>,
        working_dir: AbsPathBuf,
        envs: HashMap<OsString, OsString>,
    ) -> Self {
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
                    "Failed to access {:?}. Check \"working_directory\" in snowchains.yaml",
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
