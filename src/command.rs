use errors::{JudgeErrorKind, JudgeResult};
use terminal::Color;

use std::{fs, io};
use std::fmt::Write;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};

/// Compilation command.
pub struct CompilationCommand {
    command: CommandProperty,
    src: PathBuf,
    bin: PathBuf,
}

impl CompilationCommand {
    /// Constructs a new `CompilationCommand`.
    ///
    /// Wraps `command` in `sh` or `cmd` if necessary.
    pub fn new(command: String, working_dir: PathBuf, src: PathBuf, bin: PathBuf) -> Self {
        Self {
            command: CommandProperty::new(command, working_dir),
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
                fs::create_dir_all(dir)?;
                println!("Created {}", dir.display());
            }
        }
        self.command.print_args_and_working_dir();
        let status = self.command
            .build_checking_wd()?
            .stdin(Stdio::null())
            .status()
            .wrap_not_found_error(&self.command.arg0)?;
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
pub struct JudgingCommand(CommandProperty);

impl JudgingCommand {
    /// Constructs a new `JudgingCommand`.
    ///
    /// Wraps `command` in `sh` or `cmd` if necessary.
    pub fn new(command: String, working_dir: PathBuf) -> Self {
        JudgingCommand(CommandProperty::new(command, working_dir))
    }

    #[cfg(test)]
    pub fn from_args(arg0: &str, rest_args: &[&str]) -> io::Result<Self> {
        use std::env;
        let working_dir = env::current_dir()?;
        Ok(JudgingCommand(CommandProperty {
            arg0: arg0.to_owned(),
            rest_args: rest_args.iter().map(|arg| (*arg).to_owned()).collect(),
            working_dir,
        }))
    }

    /// Creates a new `JudgingCommand` which `working_dir` equals
    /// `self.working_dir`.
    pub fn new_in_same_dir<S: Into<String>>(&self, command: S) -> Self {
        JudgingCommand(CommandProperty::new(
            command.into(),
            self.0.working_dir.clone(),
        ))
    }

    /// Prints the arguments and working directory.
    ///
    /// Format:
    /// """
    /// Command:           "arg0" "arg1" "arg2" ...
    /// Working directory: /path/to/working/dir/
    /// """
    pub fn print_args_and_working_dir(&self) {
        self.0.print_args_and_working_dir();
    }

    /// Gets the first argument name.
    pub fn arg0_name(&self) -> String {
        self.0.arg0.clone()
    }

    /// Returns a `Child` which stdin & stdout & stderr are piped.
    pub fn spawn_piped(&self) -> io::Result<Child> {
        self.0
            .build_checking_wd()?
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .wrap_not_found_error(&self.0.arg0)
    }
}

trait WrapNotFoundError
where
    Self: Sized,
{
    fn wrap_not_found_error(self, arg0: &str) -> Self;
}

impl<T> WrapNotFoundError for io::Result<T> {
    fn wrap_not_found_error(self, arg0: &str) -> Self {
        self.map_err(|e| match e.kind() {
            io::ErrorKind::NotFound => {
                io::Error::new(io::ErrorKind::NotFound, format!("{:?} not found", arg0))
            }
            _ => e,
        })
    }
}

#[derive(Clone)]
struct CommandProperty {
    arg0: String,
    rest_args: Vec<String>,
    working_dir: PathBuf,
}

impl CommandProperty {
    fn new(command: String, working_dir: PathBuf) -> Self {
        fn wrap_if_necessary(command: String) -> (String, Vec<String>) {
            if command
                .find(|c| "@#$^&*;|?\\<>()[]{}'\"".contains(c))
                .is_some()
            {
                let (arg0, c) = if cfg!(target_os = "windows") {
                    ("cmd".to_owned(), "/C".to_owned())
                } else {
                    ("sh".to_owned(), "-c".to_owned())
                };
                (arg0, vec![c, command])
            } else if command.find(char::is_whitespace).is_some() {
                let mut it = command.split_whitespace();
                let arg0 = it.next().unwrap_or_default().to_owned();
                let rest_args = it.map(str::to_owned).collect();
                (arg0, rest_args)
            } else {
                (command, vec![])
            }
        }

        let (arg0, rest_args) = wrap_if_necessary(command);
        Self {
            arg0,
            rest_args,
            working_dir,
        }
    }

    fn print_args_and_working_dir(&self) {
        print_bold!(Color::CommandInfo, "Command:           ");
        println!("{}", self.display_args());
        print_bold!(Color::CommandInfo, "Working directory: ");
        println!("{}", self.working_dir.display());
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

#[cfg(test)]
mod tests {
    use super::CommandProperty;

    use std::path::PathBuf;

    #[test]
    fn it_wraps_commands_in_sh_or_cmd() {
        fn display(command: &'static str) -> String {
            CommandProperty::new(command.to_owned(), PathBuf::new()).display_args()
        }

        fn wrap(command: &'static str) -> String {
            if cfg!(target_os = "windows") {
                format!("\"cmd\" \"/C\" {:?}", command)
            } else {
                format!("\"sh\" \"-c\" {:?}", command)
            }
        }

        let command = "cargo build --release";
        assert_eq!(r#""cargo" "build" "--release""#, display(command));
        let command = "sleep 1 && cargo build --release";
        assert_eq!(wrap(command), display(command));
        let command = "echo 'Hello, World!'";
        assert_eq!(wrap(command), display(command));
    }
}
