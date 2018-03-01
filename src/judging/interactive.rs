use errors::JudgeResult;
use judging::{JudgingCommand, JudgingOutput, MillisRoundedUp, WrapNotFoundErrorMessage};
use testsuite::InteractiveCase;
use util;

use term::color;

use std::fmt;
use std::io::{self, BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, ChildStdout};
use std::sync::Arc;
use std::sync::mpsc::{self, Sender};
use std::thread;
use std::time::{Duration, Instant};

/// Tests for `case` and `solver` and returns one `InteractiveOutput`.
#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
pub fn judge(
    case: InteractiveCase,
    solver: &Arc<JudgingCommand>,
) -> JudgeResult<InteractiveOutput> {
    let (cout_tx, cout_rx) = mpsc::channel();
    let (result_tx, result_rx) = mpsc::channel();
    let solver_cloned = solver.clone();
    let tester = solver.new_in_same_dir(case.get_tester());
    let timelimit = case.get_timelimit();
    thread::spawn(move || {
        let _ = result_tx.send(run(&solver_cloned, &tester, cout_tx));
    });
    if let Some(timelimit) = timelimit {
        let result = result_rx.recv_timeout(timelimit + Duration::from_millis(50));
        let couts = cout_rx.try_iter().collect::<Vec<_>>();
        match result {
            Err(_) => Ok(InteractiveOutput::exceeded(timelimit, couts)),
            Ok(result) => {
                let (s, t, e1, e2) = result.wrap_not_found_error_message(|| solver.arg0_name())?;
                Ok(InteractiveOutput::new(Some(timelimit), t, s, couts, e1, e2))
            }
        }
    } else {
        let result = result_rx.recv()?;
        let couts = cout_rx.try_iter().collect::<Vec<_>>();
        result
            .map(|(s, t, e1, e2)| InteractiveOutput::new(None, t, s, couts, e1, e2))
            .wrap_not_found_error_message(|| solver.arg0_name())
    }
}

fn run(
    solver: &JudgingCommand,
    tester: &JudgingCommand,
    mut cout_tx: Sender<InteractiveConsoleOut>,
) -> io::Result<(bool, Duration, String, String)> {
    // TODO: Capture stderrs.
    use self::InteractiveConsoleOut::{SolverTerminated, TesterTerminated};

    enum Terminated {
        Solver,
        Tester,
    }

    let mut solver = solver.spawn_piped()?;
    let mut tester = tester.spawn_piped()?;
    let start = Instant::now();

    let finished = {
        let mut solver = InteractiveProcess::new(start, true, &mut solver);
        let mut tester = InteractiveProcess::new(start, false, &mut tester);
        if let Some(mut last_out) = interact(&mut tester, &mut cout_tx, None)? {
            loop {
                last_out = match interact(&mut solver, &mut cout_tx, Some(&last_out))? {
                    Some(out) => out,
                    None => break Terminated::Solver,
                };
                last_out = match interact(&mut tester, &mut cout_tx, Some(&last_out))? {
                    Some(last_out) => last_out,
                    None => break Terminated::Tester,
                };
            }
        } else {
            Terminated::Tester
        }
    };

    match finished {
        Terminated::Solver => {
            let solver_status_code = solver.wait()?.code();
            let elapsed = start.elapsed();
            let _ = cout_tx.send(SolverTerminated(solver_status_code, elapsed));
            let tester_status_code = tester.wait()?.code();
            let _ = cout_tx.send(TesterTerminated(tester_status_code, elapsed));
            Ok((
                tester_status_code == Some(0),
                elapsed,
                util::string_from_read(solver.stderr.as_mut().unwrap())?,
                util::string_from_read(tester.stderr.as_mut().unwrap())?,
            ))
        }
        Terminated::Tester => {
            let tester_status_code = tester.wait()?.code();
            let elapsed = start.elapsed();
            let _ = cout_tx.send(TesterTerminated(tester_status_code, elapsed));
            if let Some(solver_status) = solver.try_wait()? {
                let _ = cout_tx.send(TesterTerminated(solver_status.code(), elapsed));
            } else {
                solver.kill()?;
                let _ = cout_tx.send(TesterTerminated(None, elapsed));
            }
            Ok((
                tester_status_code == Some(0),
                elapsed,
                util::string_from_read(solver.stderr.as_mut().unwrap())?,
                util::string_from_read(tester.stderr.as_mut().unwrap())?,
            ))
        }
    }
}

fn interact(
    process: &mut InteractiveProcess,
    cout_tx: &mut Sender<InteractiveConsoleOut>,
    last_out: Option<&str>,
) -> io::Result<Option<Arc<String>>> {
    // Returns `Ok(Err(exit_status))` if the stdin is closed.
    if let Some(last_out) = last_out {
        if let Err(e) = process.stdin.write_all(last_out.as_bytes()) {
            let errno_brokenpipe = if cfg!(target_os = "windows") { 6 } else { 32 };
            return if e.raw_os_error() == Some(errno_brokenpipe) {
                Ok(None)
            } else {
                Err(e)
            };
        }
    }
    let out = match read_pipe(&mut process.stdout)? {
        Some(out) => Arc::new(out),
        None => return Ok(None),
    };
    let elapsed = process.start.elapsed();
    let out_cloned = out.clone();
    let _ = cout_tx.send(if process.solver_s {
        InteractiveConsoleOut::SolverStdout(out, elapsed)
    } else {
        InteractiveConsoleOut::TesterStdout(out, elapsed)
    });
    Ok(Some(out_cloned))
}

fn read_pipe(pipe: &mut BufReader<&mut ChildStdout>) -> io::Result<Option<String>> {
    const INIT_BUF_SIZE: usize = 4096;
    let mut out = String::with_capacity(INIT_BUF_SIZE);
    pipe.read_line(&mut out)?;
    Ok(if out.is_empty() { None } else { Some(out) })
}

struct InteractiveProcess<'a> {
    start: Instant,
    solver_s: bool,
    stdin: &'a mut ChildStdin,
    stdout: BufReader<&'a mut ChildStdout>,
}

impl<'a> InteractiveProcess<'a> {
    fn new(start: Instant, solver_s: bool, child: &'a mut Child) -> Self {
        Self {
            start,
            solver_s,
            stdin: child.stdin.as_mut().unwrap(),
            stdout: BufReader::new(child.stdout.as_mut().unwrap()),
        }
    }
}

pub struct InteractiveOutput {
    kind: InteractiveOutputKind,
    elapsed: Duration,
    console_outs: Vec<InteractiveConsoleOut>,
    solver_stderr: String,
    tester_stderr: String,
}

impl fmt::Display for InteractiveOutput {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            InteractiveOutputKind::Success => {
                write!(f, "Success ({}ms)", self.elapsed.millis_rounded_up())
            }
            InteractiveOutputKind::Failure => {
                write!(f, "Failure ({}ms)", self.elapsed.millis_rounded_up())
            }
        }
    }
}

impl JudgingOutput for InteractiveOutput {
    fn failure(&self) -> bool {
        match self.kind {
            InteractiveOutputKind::Success => false,
            _ => true,
        }
    }

    fn color(&self) -> u32 {
        match self.kind {
            InteractiveOutputKind::Success => color::GREEN,
            InteractiveOutputKind::Failure => color::RED,
        }
    }

    fn eprint_details(&self) {
        let max_length = |f: for<'a> fn(&'a InteractiveConsoleOut) -> bool| -> usize {
            self.console_outs
                .iter()
                .filter(|cout| f(cout))
                .map(InteractiveConsoleOut::len)
                .max()
                .unwrap_or(1)
        };

        if self.solver_stderr.is_empty() && self.tester_stderr.is_empty()
            && self.console_outs.is_empty()
        {
            return eprintln_bold!(Some(color::YELLOW), "EMPTY");
        }
        let tester_len = max_length(InteractiveConsoleOut::is_tester_s);
        let solver_len = max_length(InteractiveConsoleOut::is_solver_s);
        for cout in &self.console_outs {
            cout.eprint_aligned(tester_len, solver_len);
        }
        if !self.solver_stderr.is_empty() || !self.tester_stderr.is_empty() {
            eprintln!("");
        }
        if !self.solver_stderr.is_empty() {
            eprintln_bold!(Some(color::MAGENTA), "Solver stderr:");
            util::eprintln_trimming_trailing_newline(&self.solver_stderr);
        }
        if !self.tester_stderr.is_empty() {
            eprintln_bold!(Some(color::MAGENTA), "Tester stderr:");
            util::eprintln_trimming_trailing_newline(&self.tester_stderr);
        }
    }
}

impl InteractiveOutput {
    fn new(
        timelimit: Option<Duration>,
        elapsed: Duration,
        succeeded: bool,
        console_outs: Vec<InteractiveConsoleOut>,
        solver_stderr: String,
        tester_stderr: String,
    ) -> Self {
        let kind = if succeeded && (timelimit.is_none() || timelimit.unwrap() >= elapsed) {
            InteractiveOutputKind::Success
        } else {
            InteractiveOutputKind::Failure
        };
        Self {
            kind,
            elapsed,
            console_outs,
            solver_stderr,
            tester_stderr,
        }
    }

    fn exceeded(timelimit: Duration, console_outs: Vec<InteractiveConsoleOut>) -> Self {
        Self {
            kind: InteractiveOutputKind::Failure,
            elapsed: timelimit,
            console_outs,
            solver_stderr: "".to_owned(),
            tester_stderr: "".to_owned(),
        }
    }
}

enum InteractiveOutputKind {
    Success,
    Failure,
}

enum InteractiveConsoleOut {
    SolverStdout(Arc<String>, Duration),
    #[allow(dead_code)]
    SolverStderr(Arc<String>, Duration),
    SolverTerminated(Option<i32>, Duration),
    TesterStdout(Arc<String>, Duration),
    #[allow(dead_code)]
    TesterStderr(Arc<String>, Duration),
    TesterTerminated(Option<i32>, Duration),
}

impl InteractiveConsoleOut {
    fn is_solver_s(&self) -> bool {
        match *self {
            InteractiveConsoleOut::SolverStdout(..)
            | InteractiveConsoleOut::SolverStderr(..)
            | InteractiveConsoleOut::SolverTerminated(..) => true,
            _ => false,
        }
    }

    fn is_tester_s(&self) -> bool {
        !self.is_solver_s()
    }

    fn len(&self) -> usize {
        // Assuming all text are ASCII.
        self.content().chars().count()
    }

    fn content(&self) -> String {
        match *self {
            InteractiveConsoleOut::SolverStdout(ref s, _)
            | InteractiveConsoleOut::SolverStderr(ref s, _)
            | InteractiveConsoleOut::TesterStdout(ref s, _)
            | InteractiveConsoleOut::TesterStderr(ref s, _) => format!("{:?}", s),
            InteractiveConsoleOut::SolverTerminated(Some(c), _)
            | InteractiveConsoleOut::TesterTerminated(Some(c), _) => format!("exit code: {}", c),
            InteractiveConsoleOut::SolverTerminated(None, _)
            | InteractiveConsoleOut::TesterTerminated(None, _) => "killed".to_owned(),
        }
    }

    fn eprint_aligned(&self, tester_length: usize, solver_length: usize) {
        fn eprint_spaces(n: usize) {
            for _ in 0..n {
                eprint!(" ");
            }
        }

        let (kind, color, elapsed) = match *self {
            InteractiveConsoleOut::SolverStdout(_, t) => {
                ("Solver stdout", color::CYAN, t) //
            }
            InteractiveConsoleOut::SolverStderr(_, t) => {
                ("Solver,stderr", color::BRIGHT_MAGENTA, t)
            }
            InteractiveConsoleOut::SolverTerminated(Some(0), t) => {
                ("Solver finished", color::BRIGHT_CYAN, t)
            }
            InteractiveConsoleOut::SolverTerminated(Some(_), t) => {
                ("Solver finished", color::RED, t) //
            }
            InteractiveConsoleOut::SolverTerminated(None, t) => {
                ("Solver killed", color::YELLOW, t) //
            }
            InteractiveConsoleOut::TesterStdout(_, t) => {
                ("Tester stdout", color::GREEN, t) //
            }
            InteractiveConsoleOut::TesterStderr(_, t) => {
                ("Tester stderr", color::MAGENTA, t) //
            }
            InteractiveConsoleOut::TesterTerminated(Some(0), t) => {
                ("Tester finished", color::BRIGHT_GREEN, t)
            }
            InteractiveConsoleOut::TesterTerminated(Some(_), t) => {
                ("Tester finished", color::RED, t) //
            }
            InteractiveConsoleOut::TesterTerminated(None, t) => {
                ("Tester killed", color::YELLOW, t) //
            }
        };
        let elapsed = elapsed.millis_rounded_up();
        let content = self.content();
        let length = content.chars().count();
        eprint_bold!(Some(color), "{:<15}", kind);
        eprint!("│");
        if self.is_solver_s() {
            eprint_spaces(tester_length);
            eprint!("│");
            eprint_bold!(Some(color), "{}", content);
            eprint_spaces(solver_length - length);
        } else {
            eprint_bold!(Some(color), "{}", content);
            eprint_spaces(tester_length - length);
            eprint!("│");
            eprint_spaces(solver_length);
        }
        eprintln!("│{:>4}ms", elapsed);
    }
}
