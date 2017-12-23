use command::JudgingCommand;
use errors::JudgingResult;
use judging::{JudgingOutput, MillisRoundedUp};
use testsuite::SimpleCase;
use util;

use term::color;

use std::{fmt, thread};
use std::io::{self, Write};
use std::process::ExitStatus;
use std::sync::{mpsc, Arc};
use std::time::{Duration, Instant};

/// Tests for `case` and `solver` and returns one `SimpleOutput`.
pub fn judge(case: SimpleCase, solver: Arc<JudgingCommand>) -> JudgingResult<SimpleOutput> {
    let (tx, rx) = mpsc::channel();
    let case = Arc::new(case);
    let case_cloned = case.clone();
    let solver_cloned = solver.clone();
    thread::spawn(move || {
        let _ = tx.send(run(&case_cloned, &solver_cloned));
    });
    Ok(if let (input, expected, Some(timelimit)) = case.values() {
        rx.recv_timeout(timelimit + Duration::from_millis(50))
            .unwrap_or_else(|_| Ok(SimpleOutput::Tle(timelimit, input, expected)))
    } else {
        rx.recv()?
    }?)
}

fn run(case: &SimpleCase, solver: &JudgingCommand) -> io::Result<SimpleOutput> {
    let (input, expected, timelimit) = case.values();
    let mut solver = solver.spawn_piped()?;
    let start = Instant::now();
    solver.stdin.as_mut().unwrap().write_all(input.as_bytes())?;

    let status = solver.wait()?;
    let t = start.elapsed();
    let stdout = Arc::new(util::string_from_read(solver.stdout.unwrap())?);
    let stderr = Arc::new(util::string_from_read(solver.stderr.unwrap())?);

    // `expected` is empty IFF omitted.
    if timelimit.is_some() && t > timelimit.unwrap() {
        Ok(SimpleOutput::Tle(timelimit.unwrap(), input, expected))
    } else if status.success() && (expected.is_empty() || expected == stdout) {
        Ok(SimpleOutput::Ac(t, input, stdout, stderr))
    } else if status.success() {
        Ok(SimpleOutput::Wa(t, input, expected, stdout, stderr))
    } else {
        Ok(SimpleOutput::Re(t, input, expected, stdout, stderr, status))
    }
}

/// Test result.
pub enum SimpleOutput {
    // Each string may be empty.
    // (<elapsed>, <input>, <stdout>, <stderr>)
    Ac(Duration, Arc<String>, Arc<String>, Arc<String>),
    // (<timelimit>, <input>, <expected>)
    Tle(Duration, Arc<String>, Arc<String>),
    // (<elapsed>, <input>, <expected>, <stdout>, <stderr>)
    Wa(Duration, Arc<String>, Arc<String>, Arc<String>, Arc<String>),
    // (<elapsed>, <input>, <expected>, <stdout>, <stderr>, <status>)
    Re(
        Duration,
        Arc<String>,
        Arc<String>,
        Arc<String>,
        Arc<String>,
        ExitStatus,
    ),
}

impl fmt::Display for SimpleOutput {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SimpleOutput::Ac(t, ..) => write!(f, "AC ({}ms)", t.millis_rounded_up()),
            SimpleOutput::Tle(t, ..) => write!(f, "TLE ({}ms)", t.millis_rounded_up()),
            SimpleOutput::Wa(t, ..) => write!(f, "WA ({}ms)", t.millis_rounded_up()),
            SimpleOutput::Re(t, .., status) => {
                write!(f, "RE ({}, {}ms)", status, t.millis_rounded_up())
            }
        }
    }
}

impl JudgingOutput for SimpleOutput {
    fn failure(&self) -> bool {
        match *self {
            SimpleOutput::Ac(..) => false,
            _ => true,
        }
    }

    fn color(&self) -> u16 {
        match *self {
            SimpleOutput::Ac(..) => color::GREEN,
            SimpleOutput::Tle(..) => color::RED,
            SimpleOutput::Wa(..) => color::YELLOW,
            SimpleOutput::Re(..) => color::YELLOW,
        }
    }

    fn eprint_details(&self) {
        const THRESHOLD_TO_OMIT: usize = 1024;

        fn eprint_size(num_bytes: usize) {
            if num_bytes > 10 * 1024 * 1024 {
                let mb = num_bytes / (1024 * 1024);
                eprintln_bold!(Some(color::YELLOW), "OMITTED ({}MB)", mb);
            } else if num_bytes > 10 * 1024 {
                let kb = num_bytes / 1024;
                eprintln_bold!(Some(color::YELLOW), "OMITTED ({}KB)", kb);
            } else {
                eprintln_bold!(Some(color::YELLOW), "OMITTED ({}B)", num_bytes);
            }
        }

        fn eprint_section(head: &'static str, content: &str) {
            let num_bytes = content.as_bytes().len();
            eprintln_bold!(Some(color::MAGENTA), "{}:", head);
            if num_bytes == 0 {
                eprintln_bold!(Some(color::YELLOW), "EMPTY");
            } else if num_bytes > THRESHOLD_TO_OMIT {
                eprint_size(num_bytes);
            } else {
                util::eprintln_trimming_trailing_newline(content);
            }
        }

        fn eprint_section_unless_empty(head: &'static str, content: &str) {
            let num_bytes = content.as_bytes().len();
            if num_bytes > THRESHOLD_TO_OMIT {
                eprint_size(num_bytes);
            } else if num_bytes > 0 {
                eprintln_bold!(Some(color::MAGENTA), "{}:", head);
                util::eprintln_trimming_trailing_newline(content);
            }
        }

        match *self {
            SimpleOutput::Ac(_, ref input, ref stdout, ref stderr) => {
                eprint_section("input", input);
                eprint_section("stdout", stdout);
                eprint_section_unless_empty("stderr", stderr);
            }
            SimpleOutput::Tle(_, ref input, ref expected) => {
                eprint_section("input", input);
                eprint_section_unless_empty("expected", expected);
            }
            SimpleOutput::Wa(_, ref input, ref expected, ref stdout, ref stderr) => {
                eprint_section("input", input);
                eprint_section("expected", expected);
                eprint_section("stdout", stdout);
                eprint_section_unless_empty("stderr", stderr);
            }
            SimpleOutput::Re(_, ref input, ref expected, ref stdout, ref stderr, _) => {
                eprint_section("input", input);
                eprint_section_unless_empty("expected", expected);
                eprint_section_unless_empty("stdout", stdout);
                eprint_section("stderr", stderr);
            }
        }
    }
}
