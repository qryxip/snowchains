use console::{ConsoleOut, Palette, Printer};
use errors::{JudgeError, JudgeResult};
use path::AbsPathBuf;

use std::ffi::{OsStr, OsString};
use std::fmt::Write as _FmtWrite;
use std::io::{self, Write};
use std::process::{Child, Command, Stdio};

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
    pub fn new(
        args: &[impl AsRef<OsStr>],
        working_dir: AbsPathBuf,
        src: AbsPathBuf,
        bin: AbsPathBuf,
    ) -> Self {
        Self {
            inner: Inner::new(args, working_dir),
            src,
            bin,
        }
    }

    /// Executes the command.
    pub fn run(&self, out: &mut ConsoleOut<impl Write, impl Write>) -> JudgeResult<()> {
        if !self.src.exists() {
            writeln!(
                out.stderr().bold(Palette::Warning),
                "Warning: {} not found",
                self.src.display()
            )?;
            out.stderr().flush()?;
        }
        if self.src.exists() && self.bin.exists() {
            if self.src.metadata()?.modified()? < self.bin.metadata()?.modified()? {
                writeln!(out.stdout(), "{} is up to date.", self.bin.display())?;
                out.stdout().flush()?;
                return Ok(());
            }
        } else if let Some(parent) = self.bin.parent() {
            if !parent.exists() {
                ::fs::create_dir_all(&parent)?;
                writeln!(out.stdout(), "Created {}", parent.display())?;
                out.stdout().flush()?;
            }
        }
        {
            let mut stdout = out.stdout();
            write!(stdout.bold(Palette::CommandInfo), "Compilation Command:")?;
            writeln!(stdout, " {}", self.inner.display_args())?;
            write!(stdout.bold(Palette::CommandInfo), "Working directory:")?;
            writeln!(stdout, "   {}", self.inner.working_dir.display())?;
            stdout.flush()?;
        }
        let status = self
            .inner
            .build_checking_wd()?
            .stdin(Stdio::null())
            .status()
            .map_err(|e| JudgeError::Command(self.inner.arg0.clone(), e))?;
        if status.success() {
            if !self.bin.exists() {
                let mut stderr = out.stderr();
                writeln!(
                    stderr.plain(Palette::Warning),
                    "Warning: {} not created",
                    self.bin.display()
                )?;
                stderr.flush()?;
            }
            Ok(())
        } else {
            Err(JudgeError::Compile(status))
        }
    }
}

/// Command for simple/interactive testing.
#[cfg_attr(test, derive(Debug, PartialEq))]
pub(crate) struct JudgingCommand(Inner);

impl JudgingCommand {
    /// Constructs a new `JudgingCommand`.
    ///
    /// Wraps `command` in `sh` or `cmd` if necessary.
    pub fn new(args: &[impl AsRef<OsStr>], working_dir: AbsPathBuf) -> Self {
        JudgingCommand(Inner::new(args, working_dir))
    }

    #[cfg(test)]
    pub fn from_args<S: AsRef<OsStr>>(arg0: S, rest_args: &[S], working_dir: AbsPathBuf) -> Self {
        JudgingCommand(Inner {
            arg0: arg0.as_ref().to_owned(),
            rest_args: rest_args
                .iter()
                .map(|arg| arg.as_ref().to_owned())
                .collect(),
            working_dir,
        })
    }

    /// Prints the arguments and working directory.
    ///
    /// Format:
    /// """
    /// Command:           "arg0" "arg1" "arg2" ...
    /// Working directory: /path/to/working/dir/
    /// Test files:        /path/to/testfiles/{a.json, a.yaml}
    /// """
    pub fn write_info(
        &self,
        mut printer: Printer<impl Write>,
        testfiles_matched: &str,
    ) -> io::Result<()> {
        write!(printer.bold(Palette::CommandInfo), "Command:")?;
        writeln!(printer, "           {}", self.0.display_args())?;
        write!(printer.bold(Palette::CommandInfo), "Working directory:")?;
        writeln!(printer, " {}", self.0.working_dir.display())?;
        write!(printer.bold(Palette::CommandInfo), "Test files:")?;
        writeln!(printer, "        {}", testfiles_matched)
    }

    /// Returns a `Child` which stdin & stdout & stderr are piped.
    pub fn spawn_piped(&self) -> JudgeResult<Child> {
        // TODO
        self.0
            .build_checking_wd()?
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| JudgeError::Command(self.0.arg0.clone(), e))
    }
}

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, PartialEq, Eq, Hash)]
struct Inner {
    arg0: OsString,
    rest_args: Vec<OsString>,
    working_dir: AbsPathBuf,
}

impl Inner {
    fn new(args: &[impl AsRef<OsStr>], working_dir: AbsPathBuf) -> Self {
        let (arg0, rest_args) = if args.is_empty() {
            (OsString::new(), vec![])
        } else {
            let rest_args = args.iter().skip(1).map(|s| s.as_ref().to_owned()).collect();
            (args[0].as_ref().to_owned(), rest_args)
        };
        Self {
            arg0,
            rest_args,
            working_dir,
        }
    }

    fn display_args(&self) -> String {
        let arg0 = format!("{:?}", self.arg0);
        self.rest_args.iter().fold(arg0, |mut s, arg| {
            write!(s, " {:?}", arg).unwrap();
            s
        })
    }

    fn build_checking_wd(&self) -> io::Result<Command> {
        if self.working_dir.exists() {
            let mut command = Command::new(&self.arg0);
            command.args(&self.rest_args).current_dir(&self.working_dir);
            Ok(command)
        } else {
            Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!(
                    "{} does not exist. Check \"working_directory\" in snowchains.yaml",
                    self.working_dir.display()
                ),
            ))
        }
    }
}
