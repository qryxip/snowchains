use error::{JudgeError, JudgeErrorKind, JudgeResult};
use testsuite::{self, SuiteFileExtension, TestCase};
use util::{self, Foreach, UnwrapAsRefMut};

use std::fmt::{self, Write as _FmtWrite};
use std::fs;
use std::io::{self, Write as _IoWrite};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Stdio};
use std::sync::Arc;
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};
use term::{Attr, color};


/// Tests for given test case file path, executaion command, and compilation command.
///
/// # Errors
///
/// Returns `Err` if compilation or execution command fails, or any test fails.
pub fn judge(
    suite_dir: &Path,
    suite_file_stem: &str,
    suite_extensions: &[SuiteFileExtension],
    judging_command: JudgingCommand,
    compilation_command: Option<CompilationCommand>,
) -> JudgeResult<()> {
    fn judge_cases(cases: Vec<TestCase>, command: Arc<JudgingCommand>) -> JudgeResult<()> {
        let num_cases = cases.len();
        let suf = if num_cases > 1 { "s" } else { "" };

        command.print_args_and_working_dir();
        println!("Running {} test{}...", num_cases, suf);

        let mut last_path = None;
        let outputs = cases
            .into_iter()
            .enumerate()
            .map(|(i, case)| {
                let path = case.get_path();
                if Some(&path) != last_path.as_ref() {
                    println!("Running test cases in {}", path.display());
                    last_path = Some(case.get_path());
                }
                let output = judge_one(case, command.clone())?;
                output.print_title(i, num_cases);
                Ok(output)
            })
            .collect::<JudgeResult<Vec<_>>>()?;

        let num_failures = outputs.iter().filter(|output| output.failure()).count();
        if num_failures == 0 {
            Ok(println!("All of the {} test{} passed.", num_cases, suf))
        } else {
            outputs.into_iter().enumerate().foreach(|(i, output)| {
                eprintln!("");
                output.eprint_title(i, num_cases);
                output.eprint_details();
            });
            bail!(JudgeErrorKind::TestFailure(num_failures))
        }
    }

    fn judge_one(case: TestCase, command: Arc<JudgingCommand>) -> JudgeResult<JudgeOutput> {
        let (tx, rx) = mpsc::channel();
        let case = Arc::new(case);
        let case_cloned = case.clone();
        let command_cloned = command.clone();
        thread::spawn(move || {
            let _ = tx.send(run_program(&case_cloned, &command_cloned));
        });
        if let (input, expected, Some(timelimit)) = case.values() {
            rx.recv_timeout(Duration::from_millis(timelimit + 50))
                .unwrap_or_else(|_| Ok(JudgeOutput::Tle(timelimit, input, expected)))
        } else {
            rx.recv()?
        }.wrap_not_found_error_message(|| command.get_arg0())
    }

    fn run_program(case: &TestCase, command: &JudgingCommand) -> io::Result<JudgeOutput> {
        let (input, expected, timelimit) = case.values();
        let mut child = command.spawn_piped()?;
        let start = Instant::now();
        child.stdin.unwrap_as_ref_mut().write_all(input.as_bytes())?;

        let status = child.wait()?;
        let t = start.elapsed();
        let t = (1000000000 * t.as_secs() + t.subsec_nanos() as u64 + 999999) / 1000000;
        let stdout = Arc::new(util::string_from_read(child.stdout.unwrap())?);
        let stderr = Arc::new(util::string_from_read(child.stderr.unwrap())?);

        // `expected` is empty IFF omitted.
        if timelimit.is_some() && t > timelimit.unwrap() {
            Ok(JudgeOutput::Tle(timelimit.unwrap(), input, expected))
        } else if status.success() && (expected.is_empty() || expected == stdout) {
            Ok(JudgeOutput::Ac(t, input, stdout, stderr))
        } else if status.success() {
            Ok(JudgeOutput::Wa(t, input, expected, stdout, stderr))
        } else {
            Ok(JudgeOutput::Re(t, input, expected, stdout, stderr, status))
        }
    }

    if let Some(compilation_command) = compilation_command {
        compilation_command.execute()?;
        println!("");
    }
    let cases = testsuite::load_and_merge_all_cases(suite_dir, suite_file_stem, suite_extensions)?;
    let judging_command = Arc::new(judging_command);
    judge_cases(cases, judging_command)
}


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

    fn print_args_and_working_dir(&self) {
        self.command.print_args_and_working_dir();
    }

    fn execute(&self) -> JudgeResult<()> {
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
        self.print_args_and_working_dir();
        let status = Command::new(&self.command.arg0)
            .args(&self.command.rest_args)
            .current_dir(&self.command.working_dir)
            .stdin(Stdio::null())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status()
            .wrap_not_found_error_message(|| self.command.arg0.clone())?;
        if status.success() {
            Ok(())
        } else {
            bail!(JudgeErrorKind::CompilationFailure(status));
        }
    }
}


pub struct JudgingCommand(CommandProperty);

impl JudgingCommand {
    /// Constructs a new `JudgingCommand`.
    ///
    /// Wraps `command` in `sh` or `cmd` if necessary.
    pub fn new(command: String, working_dir: PathBuf) -> Self {
        JudgingCommand(CommandProperty::new(command, working_dir))
    }

    fn print_args_and_working_dir(&self) {
        self.0.print_args_and_working_dir();
    }

    fn get_arg0(&self) -> String {
        self.0.arg0.clone()
    }

    fn spawn_piped(&self) -> io::Result<Child> {
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
        print_decorated!(Attr::Bold, Some(color::CYAN), "Command:           ");
        println!("{}", self.display_args());
        print_decorated!(Attr::Bold, Some(color::CYAN), "Working directory: ");
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


enum JudgeOutput {
    // Each string may be empty.
    // (<elapsed>, <input>, <stdout>, <stderr>)
    Ac(u64, Arc<String>, Arc<String>, Arc<String>),
    // (<timelimit>, <input>, <expected>)
    Tle(u64, Arc<String>, Arc<String>),
    // (<elapsed>, <input>, <expected>, <stdout>, <stderr>)
    Wa(u64, Arc<String>, Arc<String>, Arc<String>, Arc<String>),
    // (<elapsed>, <input>, <expected>, <stdout>, <stderr>, <status>)
    Re(u64, Arc<String>, Arc<String>, Arc<String>, Arc<String>, ExitStatus),
}

impl fmt::Display for JudgeOutput {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            JudgeOutput::Ac(t, ..) => write!(f, "AC ({}ms)", t),
            JudgeOutput::Tle(t, ..) => write!(f, "TLE ({}ms)", t),
            JudgeOutput::Wa(t, ..) => write!(f, "WA ({}ms)", t),
            JudgeOutput::Re(t, .., status) => write!(f, "RE ({}, {}ms)", status, t),
        }
    }
}

impl JudgeOutput {
    fn failure(&self) -> bool {
        match *self {
            JudgeOutput::Ac(..) => false,
            _ => true,
        }
    }

    fn print_title(&self, i: usize, n: usize) {
        (0..format!("{}", n).len() - format!("{}", i + 1).len()).foreach(|_| print!(" "));
        print_decorated!(Attr::Bold, None, "{}/{} ", i + 1, n);
        println_decorated!(Attr::Bold, Some(self.color()), "{}", self);
    }

    fn eprint_title(&self, i: usize, n: usize) {
        (0..format!("{}", n).len() - format!("{}", i + 1).len()).foreach(|_| eprint!(" "));
        eprint_decorated!(Attr::Bold, None, "{}/{} ", i + 1, n);
        eprintln_decorated!(Attr::Bold, Some(self.color()), "{}", self);
    }

    fn eprint_details(&self) {
        const THRESHOLD_TO_OMIT: usize = 1024;

        fn eprint_size(num_bytes: usize) {
            let (attr, color) = (Attr::Bold, Some(color::YELLOW));
            if num_bytes > 10 * 1024 * 1024 {
                eprintln_decorated!(attr, color, "OMITTED ({}MB)", num_bytes / (1024 * 1024));
            } else if num_bytes > 10 * 1024 {
                eprintln_decorated!(attr, color, "OMITTED ({}KB)", num_bytes / 1024);
            } else {
                eprintln_decorated!(attr, color, "OMITTED ({}B)", num_bytes);
            }
        }

        fn eprint_section(head: &'static str, content: &str) {
            let num_bytes = content.as_bytes().len();
            eprintln_decorated!(Attr::Bold, Some(color::MAGENTA), "{}:", head);
            if num_bytes == 0 {
                eprintln_decorated!(Attr::Bold, Some(color::YELLOW), "EMPTY");
            } else if num_bytes > THRESHOLD_TO_OMIT {
                eprint_size(num_bytes);
            } else {
                util::eprintln_trimming_last_newline(content);
            }
        }

        fn eprint_section_unless_empty(head: &'static str, content: &str) {
            let num_bytes = content.as_bytes().len();
            if num_bytes > THRESHOLD_TO_OMIT {
                eprint_size(num_bytes);
            } else if num_bytes > 0 {
                eprintln_decorated!(Attr::Bold, Some(color::MAGENTA), "{}:", head);
                util::eprintln_trimming_last_newline(content);
            }
        }

        match *self {
            JudgeOutput::Ac(_, ref input, ref stdout, ref stderr) => {
                eprint_section("input", input);
                eprint_section("stdout", stdout);
                eprint_section_unless_empty("stderr", stderr);
            }
            JudgeOutput::Tle(_, ref input, ref expected) => {
                eprint_section("input", input);
                eprint_section_unless_empty("expected", expected);
            }
            JudgeOutput::Wa(_, ref input, ref expected, ref stdout, ref stderr) => {
                eprint_section("input", input);
                eprint_section("expected", expected);
                eprint_section("stdout", stdout);
                eprint_section_unless_empty("stderr", stderr);
            }
            JudgeOutput::Re(_, ref input, ref expected, ref stdout, ref stderr, _) => {
                eprint_section("input", input);
                eprint_section_unless_empty("expected", expected);
                eprint_section_unless_empty("stdout", stdout);
                eprint_section("stderr", stderr);
            }
        }
    }

    fn color(&self) -> u16 {
        match *self {
            JudgeOutput::Ac(..) => color::GREEN,
            JudgeOutput::Tle(..) => color::RED,
            JudgeOutput::Wa(..) => color::YELLOW,
            JudgeOutput::Re(..) => color::YELLOW,
        }
    }
}


trait WrapNotFoundErrorMessage {
    type Item;
    fn wrap_not_found_error_message<F: FnOnce() -> String>(
        self,
        arg0: F,
    ) -> JudgeResult<Self::Item>;
}

impl<T> WrapNotFoundErrorMessage for io::Result<T> {
    type Item = T;

    fn wrap_not_found_error_message<F: FnOnce() -> String>(self, arg0: F) -> JudgeResult<T> {
        self.map_err(|e| -> JudgeError {
            match e.kind() {
                io::ErrorKind::NotFound => JudgeErrorKind::CommandNotFound(arg0()).into(),
                _ => e.into(),
            }
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
