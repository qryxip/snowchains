use error::{JudgingErrorKind, JudgingResult};

use term::color;

use std::fmt::Write;
use std::fs;
use std::io;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};


/// Compilation command.
pub struct CompilationCommand {
    command: CommandProperty,
    src_and_bin: Option<(PathBuf, PathBuf)>,
}

impl CompilationCommand {
    /// Constructs a new `CompilationCommand`.
    ///
    /// Wraps `command` in `sh` or `cmd` if necessary.
    pub fn new(
        command: String,
        working_dir: PathBuf,
        src_and_bin: Option<(PathBuf, PathBuf)>,
    ) -> Self {
        Self {
            command: CommandProperty::new(command, working_dir),
            src_and_bin: src_and_bin,
        }
    }

    /// Executes the command.
    pub fn execute(&self) -> JudgingResult<()> {
        if let &Some((ref src, ref bin)) = &self.src_and_bin {
            if let (Ok(src_meta), Ok(bin_meta)) = (src.metadata(), bin.metadata()) {
                if let (Ok(t1), Ok(t2)) = (src_meta.modified(), bin_meta.modified()) {
                    if t1 < t2 {
                        return Ok(println!("{} is up to date.", bin.display()));
                    }
                }
            } else if let Some(dir) = bin.parent() {
                if !dir.exists() {
                    fs::create_dir_all(dir)?;
                    println!("Created {}", dir.display());
                }
            }
        }
        self.command.print_args_and_working_dir();
        let status = Command::new(&self.command.arg0)
            .args(&self.command.rest_args)
            .current_dir(&self.command.working_dir)
            .stdin(Stdio::null())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status()
            .map_err(|e| match e.kind() {
                io::ErrorKind::NotFound => {
                    io::Error::new(
                        io::ErrorKind::NotFound,
                        format!("{:?} not found", self.command.arg0.clone()),
                    )
                }
                _ => e,
            })?;
        if status.success() {
            Ok(())
        } else {
            bail!(JudgingErrorKind::CompilationFailure(status));
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

    /// Creates a new `JudgingCommand` which `working_dir` equals `self.working_dir`.
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
        Command::new(&self.0.arg0)
            .args(&self.0.rest_args)
            .current_dir(&self.0.working_dir)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
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
            arg0: arg0,
            rest_args: rest_args,
            working_dir: working_dir,
        }
    }

    fn print_args_and_working_dir(&self) {
        print_bold!(Some(color::CYAN), "Command:           ");
        println!("{}", self.display_args());
        print_bold!(Some(color::CYAN), "Working directory: ");
        println!("{}", self.working_dir.display());
    }

    fn display_args(&self) -> String {
        let arg0 = format!("{:?}", self.arg0);
        self.rest_args.iter().fold(arg0, |mut s, arg| {
            write!(s, " {:?}", arg).unwrap();
            s
        })
    }
}


#[cfg(test)]
mod tests {
    use super::CommandProperty;

    use std::path::PathBuf;


    #[test]
    fn assert_commands_wrapped_in_sh_or_cmd() {
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
        assert_eq!("\"cargo\" \"build\" \"--release\"", display(command));
        let command = "sleep 1 && cargo build --release";
        assert_eq!(wrap(command), display(command));
        let command = "echo 'Hello, World!'";
        assert_eq!(wrap(command), display(command));
    }
}
