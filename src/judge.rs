use super::error::{JudgeErrorKind, JudgeResult};
use super::testcase::{Case, Cases, TestCaseFilePath};
use super::util::{self, UnwrapAsRefMut};
use std::fmt::{self, Display, Formatter};
use std::io::{self, Write};
use std::path::PathBuf;
use std::process::{Child, Command, ExitStatus, Stdio};
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};
use term::{Attr, color};


pub fn judge(
    cases_path: TestCaseFilePath,
    run_command: CommandParameters,
    build_command: Option<CommandParameters>,
) -> JudgeResult<()> {
    fn build(build_command: CommandParameters) -> JudgeResult<()> {
        let status = build_command.status_inherited()?;
        if status.success() {
            Ok(())
        } else {
            bail!(JudgeErrorKind::BuildFailure(status));
        }
    }

    fn judge_one(case: Case, run_command: CommandParameters) -> JudgeResult<JudgeOutput> {
        fn run_program(case: Case, run_command: CommandParameters) -> io::Result<JudgeOutput> {
            let (input, expected, timelimit) = case.into();
            let mut child = run_command.spawn_piped()?;
            let start = Instant::now();
            child.stdin.unwrap_as_ref_mut().write_all(input.as_bytes())?;

            let status = child.wait()?;
            let t = {
                let t = start.elapsed();
                (1000000000 * t.as_secs() + t.subsec_nanos() as u64 + 999999) / 1000000
            };
            let stdout = util::string_from_read(child.stdout.unwrap())?;
            let stderr = util::string_from_read(child.stderr.unwrap())?;

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

        let (tx, rx) = mpsc::channel();
        let case_cloned = case.clone();
        let command_arg0 = run_command.arg0.clone();
        thread::spawn(move || {
            let _ = tx.send(run_program(case_cloned, run_command));
        });
        if let (input, expected, Some(timelimit)) = case.into() {
            match rx.recv_timeout(Duration::from_millis(timelimit + 50)) {
                Err(_) => Ok(JudgeOutput::Tle(timelimit, input, expected)),
                Ok(Err(ref e)) if e.kind() == io::ErrorKind::NotFound => {
                    bail!(io::Error::new(
                        io::ErrorKind::NotFound,
                        format!("Command not found: '{}'", command_arg0),
                    ))
                }
                Ok(output) => Ok(output?),
            }
        } else {
            Ok(rx.recv()??)
        }
    }

    if let Some(build_command) = build_command {
        build_command.print("Build command:");
        build(build_command)?;
        println!("");
    }

    let cases = Cases::load(&cases_path)?;
    let num_cases = cases.num_cases();
    let suf = if num_cases > 1 { "s" } else { "" };
    let mut all_outputs = vec![];
    let mut num_failures = 0;

    cases_path.print(9);
    run_command.print("Command:");
    println!("Running {} test{}...", num_cases, suf);
    for (i, case) in cases.into_iter().enumerate() {
        let output = judge_one(case, run_command.clone())?;
        output.print_title(i, num_cases);
        match output {
            JudgeOutput::Ac(..) => {}
            _ => num_failures += 1,
        }
        all_outputs.push(output);
    }

    if num_failures == 0 {
        println!("All of the {} test{} passed.", num_cases, suf);
        Ok(())
    } else {
        for (i, output) in all_outputs.into_iter().enumerate() {
            eprintln!("");
            output.eprint_title(i, num_cases);
            output.eprint_details();
        }
        bail!(JudgeErrorKind::TestFailed(num_failures))
    }
}


#[derive(Clone)]
pub struct CommandParameters {
    arg0: String,
    rest_args: Vec<String>,
    working_dir: PathBuf,
}

impl CommandParameters {
    pub fn new(arg0: String, rest_args: Vec<String>, working_dir: PathBuf) -> Self {
        Self {
            arg0: arg0,
            rest_args: rest_args,
            working_dir: working_dir,
        }
    }

    pub fn wrap_in_sh_or_cmd_if_necessary(command: String, working_dir: PathBuf) -> Self {
        if command.find(|c| " \t\n#&|;".contains(c)).is_none() {
            Self::new(command, vec![], working_dir)
        } else {
            let (arg0, c) = if cfg!(target_os = "windows") {
                ("cmd".to_owned(), "/C".to_owned())
            } else {
                ("sh".to_owned(), "-c".to_owned())
            };
            Self::new(arg0, vec![c, command], working_dir)
        }
    }

    fn print(&self, command: &'static str) {
        print_decorated!(Attr::Bold, Some(color::CYAN), "{}", command);
        let l = command.len();
        for _ in 0..if l > 19 { 0 } else { 19 - l } {
            print!(" ");
        }
        print!("{:?}", self.arg0);
        for arg in &self.rest_args {
            print!(" {:?}", arg);
        }
        print_decorated!(Attr::Bold, Some(color::CYAN), "\nWorking directory:");
        println!(" {}", self.working_dir.display());
    }

    fn status_inherited(self) -> io::Result<ExitStatus> {
        Command::new(&self.arg0)
            .args(self.rest_args)
            .current_dir(self.working_dir)
            .stdin(Stdio::null())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status()
    }

    fn spawn_piped(self) -> io::Result<Child> {
        Command::new(&self.arg0)
            .args(self.rest_args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .current_dir(self.working_dir)
            .spawn()
    }
}


enum JudgeOutput {
    // Each string may be empty.
    // (<elapsed>, <input>, <expected = stdout>, <stderr>)
    Ac(u64, String, String, String),
    // (<timelimit>, <input>, <expected>)
    Tle(u64, String, String),
    // (<elapsed>, <input>, <expected>, <stdout>, <stderr>)
    Wa(u64, String, String, String, String),
    // (<elapsed>, <input>, <expected>, <stdout>, <stderr>, <status>)
    Re(u64, String, String, String, String, ExitStatus),
}


impl Display for JudgeOutput {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            JudgeOutput::Ac(t, ..) => write!(f, "AC ({}ms)", t),
            JudgeOutput::Tle(t, ..) => write!(f, "TLE ({}ms)", t),
            JudgeOutput::Wa(t, ..) => write!(f, "WA ({}ms)", t),
            JudgeOutput::Re(t, .., status) => write!(f, "RE ({}, {}ms)", status, t),
        }
    }
}

impl JudgeOutput {
    fn print_title(&self, i: usize, n: usize) {
        for _ in 0..format!("{}", n).len() - format!("{}", i + 1).len() {
            print!(" ");
        }
        print_decorated!(Attr::Bold, None, "{}/{} ", i + 1, n);
        println_decorated!(Attr::Bold, Some(self.color()), "{}", self);
    }

    fn eprint_title(&self, i: usize, n: usize) {
        for _ in 0..format!("{}", n).len() - format!("{}", i + 1).len() {
            eprint!(" ");
        }
        eprint_decorated!(Attr::Bold, None, "{}/{} ", i + 1, n);
        eprintln_decorated!(Attr::Bold, Some(self.color()), "{}", self);
    }

    fn eprint_details(&self) {
        fn eprint_section(head: &'static str, s: &str) {
            eprintln_decorated!(Attr::Bold, Some(color::MAGENTA), "{}:", head);
            if s.is_empty() {
                eprintln_decorated!(Attr::Bold, Some(color::YELLOW), "EMPTY");
            } else {
                util::eprintln_trimming_last_newline(s);
            }
        }

        fn eprint_section_unless_empty(head: &'static str, s: &str) {
            if !s.is_empty() {
                eprintln_decorated!(Attr::Bold, Some(color::MAGENTA), "{}:", head);
                util::eprintln_trimming_last_newline(s);
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
