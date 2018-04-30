use errors::{JudgeErrorKind, JudgeResult, JudgeResultExt};
use terminal::Color;
use util;

use std::ffi::{OsStr, OsString};
use std::fmt::Write;
use std::io;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};

/// Compilation command.
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct CompilationCommand {
    command: CommandProperty,
    src: PathBuf,
    bin: PathBuf,
}

impl CompilationCommand {
    /// Constructs a new `CompilationCommand`.
    ///
    /// Wraps `command` in `sh` or `cmd` if necessary.
    pub(crate) fn new<S: AsRef<OsStr>>(
        args: &[S],
        working_dir: PathBuf,
        src: PathBuf,
        bin: PathBuf,
    ) -> Self {
        Self {
            command: CommandProperty::new(args, working_dir),
            src,
            bin,
        }
    }

    /// Executes the command.
    pub fn execute(&self) -> JudgeResult<()> {
        if !self.src.exists() {
            eprintln_bold!(Color::Warning, "Warning: {} not found", self.src.display());
        }
        if self.src.exists() && self.bin.exists() {
            if self.src.metadata()?.modified()? < self.bin.metadata()?.modified()? {
                println!("{} is up to date.", self.bin.display());
                return Ok(());
            }
        } else if let Some(dir) = self.bin.parent() {
            if !dir.exists() {
                util::fs::create_dir_all(dir)?;
                println!("Created {}", dir.display());
            }
        }
        print_bold!(Color::CommandInfo, "Compilation Command: ");
        print!("{}\n", self.command.display_args());
        print_bold!(Color::CommandInfo, "Working directory:   ");
        println!("{}", self.command.working_dir.display());
        let status = self.command
            .build_checking_wd()?
            .stdin(Stdio::null())
            .status()
            .chain_err(|| JudgeErrorKind::Command(self.command.arg0.clone()))?;
        if status.success() {
            if !self.bin.exists() {
                eprintln_bold!(
                    Color::Warning,
                    "Warning: {} not created",
                    self.bin.display()
                );
            }
            Ok(())
        } else {
            bail!(JudgeErrorKind::Compile(status));
        }
    }
}

/// Command for simple/interactive testing.
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct JudgingCommand(CommandProperty);

impl JudgingCommand {
    /// Constructs a new `JudgingCommand`.
    ///
    /// Wraps `command` in `sh` or `cmd` if necessary.
    pub(crate) fn new<S: AsRef<OsStr>>(args: &[S], working_dir: PathBuf) -> Self {
        JudgingCommand(CommandProperty::new(args, working_dir))
    }

    #[cfg(test)]
    pub fn from_args(arg0: &str, rest_args: &[&str]) -> io::Result<Self> {
        use std::env;
        let working_dir = env::current_dir()?;
        Ok(JudgingCommand(CommandProperty {
            arg0: arg0.into(),
            rest_args: rest_args.iter().map(|arg| (*arg).into()).collect(),
            working_dir,
        }))
    }

    /// Creates a new `JudgingCommand` which `working_dir` equals
    /// `self.working_dir`.
    ///
    /// FIXME
    #[allow(unused)]
    pub fn new_in_same_dir<S: Into<String>>(&self, _: S) -> Self {
        unimplemented!()
    }

    /// Prints the arguments and working directory.
    ///
    /// Format:
    /// """
    /// Command:           "arg0" "arg1" "arg2" ...
    /// Working directory: /path/to/working/dir/
    /// Test files:        /path/to/testfiles/{a.json, a.yaml}
    /// """
    pub fn print_info(&self, testfiles_matched: &str) {
        print_bold!(Color::CommandInfo, "Command:           ");
        print!("{}\n", self.0.display_args());
        print_bold!(Color::CommandInfo, "Working directory: ");
        print!("{}\n", self.0.working_dir.display());
        print_bold!(Color::CommandInfo, "Test files:        ");
        println!("{}", testfiles_matched);
    }

    /// Gets the first argument name.
    #[allow(unused)]
    pub fn arg0(&self) -> &OsStr {
        &self.0.arg0
    }

    /// Returns a `Child` which stdin & stdout & stderr are piped.
    pub fn spawn_piped(&self) -> JudgeResult<Child> {
        self.0
            .build_checking_wd()?
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .chain_err(|| JudgeErrorKind::Command(self.0.arg0.clone()))
    }
}

#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Clone)]
struct CommandProperty {
    arg0: OsString,
    rest_args: Vec<OsString>,
    working_dir: PathBuf,
}

impl CommandProperty {
    fn new<S: AsRef<OsStr>>(args: &[S], working_dir: PathBuf) -> Self {
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
