use super::error::{JudgeErrorKind, JudgeResult};
use super::testcase::{Case, Cases};
use super::util::UnwrapAsRefMut;
use std::convert::From;
use std::fmt::{self, Debug, Formatter};
use std::io::{self, Read, Write};
use std::path::Path;
use std::process::{Command, ExitStatus, Stdio};
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};
use term::{Attr, color};


pub fn run_judge(cases: &str, target: &str, args: &[&str]) -> JudgeResult<()> {
    let (cases, target) = (Cases::load(Path::new(cases))?, Path::new(target));
    judge_all(cases, target, args)
}


pub fn judge_all(cases: Cases, target: &Path, args: &[&str]) -> JudgeResult<()> {
    fn judge(
        case: Case,
        program: String,
        args: Vec<String>,
        timelimit: u64,
    ) -> io::Result<JudgeOutput> {
        fn run_program(
            case: Case,
            timelimit: u64,
            program: String,
            args: Vec<String>,
        ) -> io::Result<JudgeOutput> {
            let (expected, input) = case.into();
            let mut child = Command::new(program)
                .args(args)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()?;
            let start = Instant::now();
            child.stdin.unwrap_as_ref_mut().write_all(input.as_bytes())?;

            let status = child.wait()?;
            let t = {
                let t = start.elapsed();
                (1000000000 * t.as_secs() + t.subsec_nanos() as u64) / 1000000
            };
            let (stdout, stderr) = {
                let (mut stdout, mut stderr) = (String::new(), String::new());
                child.stdout.unwrap().read_to_string(&mut stdout)?;
                child.stderr.unwrap().read_to_string(&mut stderr)?;
                (stdout, stderr)
            };

            if t > timelimit {
                Ok(JudgeOutput::Tle(timelimit, input, expected))
            } else if status.success() && expected == stdout {
                Ok(JudgeOutput::Ac(t, input, stdout, stderr))
            } else if status.success() {
                Ok(JudgeOutput::Wa(t, input, expected, stdout, stderr))
            } else {
                Ok(JudgeOutput::Re(t, input, expected, stdout, stderr, status))
            }
        }

        let (tx, rx) = mpsc::channel();
        let case_cloned = case.clone();
        thread::spawn(move || {
            let _ = tx.send(run_program(case_cloned, timelimit, program, args));
        });
        match rx.recv_timeout(Duration::from_millis(timelimit + 50)) {
            Ok(output) => output,
            Err(_) => {
                let (expected, input) = case.into();
                Ok(JudgeOutput::Tle(timelimit, input, expected))
            }
        }
    }

    let (timelimit, num_tests) = (cases.timelimit(), cases.num_cases());
    let suf = if num_tests > 1 { "s" } else { "" };
    let mut all_outputs = vec![];
    let mut num_failures = 0;

    println!("\nRunning {} test{}...", num_tests, suf);
    for (i, case) in cases.into_cases().into_iter().enumerate() {
        let target = match target.to_str() {
            Some(s) => s.to_owned(),
            None => bail!(io::Error::from(io::ErrorKind::InvalidInput)),
        };
        let args = args.iter().map(|&arg| arg.into()).collect();
        let output = judge(case, target, args, timelimit)?;
        output.print_title(i, num_tests);
        match output {
            JudgeOutput::Ac(..) => {}
            _ => num_failures += 1,
        }
        all_outputs.push(output);
    }

    if num_failures == 0 {
        println!("All of the {} test{} passed.", num_tests, suf);
        Ok(())
    } else {
        for (i, output) in all_outputs.into_iter().enumerate() {
            eprintln!("");
            output.eprint_title(i, num_tests);
            output.eprint_details();
        }
        bail!(JudgeErrorKind::TestFailed(num_failures))
    }
}


enum JudgeOutput {
    // (<elapsed>, <input>, <expected = stdout>, <stderr>)
    Ac(u64, String, String, String),
    // (<timelimit>, <input>, <expected>)
    Tle(u64, String, String),
    // (<elapsed>, <input>, <expected>, <stdout>, <stderr>)
    Wa(u64, String, String, String, String),
    // (<elapsed>, <input>, <expected>, <stdout>, <stderr>, <status>)
    Re(u64, String, String, String, String, ExitStatus),
}


impl Debug for JudgeOutput {
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
        println_decorated!(Attr::Bold, Some(self.color()), "{:?}", self);
    }

    fn eprint_title(&self, i: usize, n: usize) {
        for _ in 0..format!("{}", n).len() - format!("{}", i + 1).len() {
            eprint!(" ");
        }
        eprint_decorated!(Attr::Bold, None, "{}/{} ", i + 1, n);
        eprintln_decorated!(Attr::Bold, Some(self.color()), "{:?}", self);
    }

    fn eprint_details(&self) {
        fn eprintln_trimming_last_newline(s: &str) {
            if s.chars().last() == Some('\n') {
                eprint_and_flush!("{}", s);
            } else {
                eprintln!("{}", s);
            }
        }

        fn eprint_section(head: &'static str, s: &str) {
            eprintln_decorated!(Attr::Bold, Some(color::MAGENTA), "{}:", head);
            if s.is_empty() {
                eprintln_decorated!(Attr::Bold, Some(color::YELLOW), "EMPTY");
            } else {
                eprintln_trimming_last_newline(s);
            }
        }

        fn eprint_section_unless_empty(head: &'static str, s: &str) {
            if !s.is_empty() {
                eprintln_decorated!(Attr::Bold, Some(color::MAGENTA), "{}:", head);
                eprintln_trimming_last_newline(s);
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
                eprint_section("expected", expected);
            }
            JudgeOutput::Wa(_, ref input, ref expected, ref stdout, ref stderr) => {
                eprint_section("input", input);
                eprint_section("expected", expected);
                eprint_section("stdout", stdout);
                eprint_section_unless_empty("stderr", stderr);
            }
            JudgeOutput::Re(_, ref input, ref expected, ref stdout, ref stderr, _) => {
                eprint_section("input", input);
                eprint_section("expected", expected);
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
