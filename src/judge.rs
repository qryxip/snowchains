use error::{JudgeError, JudgeErrorKind, JudgeResult};
use testsuite::{self, InteractiveCase, SimpleCase, SuiteFileExtension, TestCases};
use util::{self, Foreach};

use std::{fs, thread};
use std::fmt::{self, Write as _FmtWrite};
use std::io::{self, BufRead, BufReader, Write as _IoWrite};
use std::iter::FromIterator;
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, ChildStdout, Command, ExitStatus, Stdio};
use std::sync::Arc;
use std::sync::mpsc::{self, Sender};
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
    solver: JudgingCommand,
    compilation: Option<CompilationCommand>,
) -> JudgeResult<()> {
    fn judge_all(cases: TestCases, solver: Arc<JudgingCommand>) -> JudgeResult<()> {
        let num_cases = cases.len();
        solver.print_args_and_working_dir();
        let suf = if num_cases > 1 { "s" } else { "" };
        println!("Running {} test{}...", num_cases, suf);

        let mut last_path = None;

        macro_rules! judge_all { ($cases: expr, $method: ident) => {
            $cases.into_iter().enumerate().map(|(i, case)| {
                let path = case.get_path();
                if Some(&path) != last_path.as_ref() {
                    println!("Running test cases in {}", path.display());
                    last_path = Some(case.get_path());
                }
                let output = $method(case, solver.clone())?;
                output.print_title(i, num_cases);
                Ok(output)
            }).collect::<JudgeResult<JudgingOutputs>>()?
        } }

        match cases {
            TestCases::Simple(cases) => judge_all!(cases, judge_simple),
            TestCases::Interactive(cases) => judge_all!(cases, judge_interactive),
        }.show_result(num_cases)
    }

    fn judge_simple(case: SimpleCase, solver: Arc<JudgingCommand>) -> JudgeResult<SimpleOutput> {
        let (tx, rx) = mpsc::channel();
        let case = Arc::new(case);
        let case_cloned = case.clone();
        let solver_cloned = solver.clone();
        thread::spawn(move || {
            let _ = tx.send(run_simple(&case_cloned, &solver_cloned));
        });
        if let (input, expected, Some(timelimit)) = case.values() {
            rx.recv_timeout(Duration::from_millis(timelimit + 50))
                .unwrap_or_else(|_| Ok(SimpleOutput::Tle(timelimit, input, expected)))
        } else {
            rx.recv()?
        }.wrap_not_found_error_message(|| solver.get_arg0())
    }

    fn run_simple(case: &SimpleCase, solver: &JudgingCommand) -> io::Result<SimpleOutput> {
        let (input, expected, timelimit) = case.values();
        let mut child = solver.spawn_piped()?;
        let start = Instant::now();
        child.stdin.as_mut().unwrap().write_all(input.as_bytes())?;

        let status = child.wait()?;
        let t = elapsed_time_as_ms(start);
        let stdout = Arc::new(util::string_from_read(child.stdout.unwrap())?);
        let stderr = Arc::new(util::string_from_read(child.stderr.unwrap())?);

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

    fn judge_interactive(
        case: InteractiveCase,
        solver: Arc<JudgingCommand>,
    ) -> JudgeResult<InteractiveOutput> {
        let (cout_tx, cout_rx) = mpsc::channel();
        let (result_tx, result_rx) = mpsc::channel();
        let solver_cloned = solver.clone();
        let tester = solver.new_in_same_dir(case.get_tester());
        let timelimit = case.get_timelimit();
        thread::spawn(move || {
            let _ = result_tx.send(run_interactive(&solver_cloned, &tester, cout_tx));
        });
        if let Some(timelimit) = timelimit {
            let result = result_rx.recv_timeout(Duration::from_millis(timelimit + 50));
            let couts = cout_rx.try_iter().collect::<Vec<_>>();
            match result {
                Err(_) => Ok(InteractiveOutput::exceeded(timelimit, couts)),
                Ok(result) => {
                    let (succeeded, t) = result.wrap_not_found_error_message(|| solver.get_arg0())?;
                    Ok(InteractiveOutput::new(Some(timelimit), t, succeeded, couts))
                }
            }
        } else {
            let result = result_rx.recv()?;
            let couts = cout_rx.try_iter().collect::<Vec<_>>();
            result
                .wrap_not_found_error_message(|| solver.get_arg0())
                .map(|(succeeded, t)| {
                    InteractiveOutput::new(None, t, succeeded, couts)
                })
        }
    }

    fn run_interactive(
        solver: &JudgingCommand,
        tester: &JudgingCommand,
        mut cout_tx: Sender<InteractiveConsoleOut>,
    ) -> io::Result<(bool, u64)> {
        // TODO: Capture stderrs.
        use self::InteractiveConsoleOut::{SolverFinished, TesterFinished};

        enum Finished {
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
                        None => break Finished::Solver,
                    };
                    last_out = match interact(&mut tester, &mut cout_tx, Some(&last_out))? {
                        Some(last_out) => last_out,
                        None => break Finished::Tester,
                    };
                }
            } else {
                Finished::Tester
            }
        };

        match finished {
            Finished::Solver => {
                let solver_status_code = solver.wait()?.code();
                let elapsed = elapsed_time_as_ms(start);
                let _ = cout_tx.send(SolverFinished(solver_status_code, elapsed));
                let tester_status_code = tester.wait()?.code();
                let _ = cout_tx.send(TesterFinished(tester_status_code, elapsed));
                Ok((tester_status_code == Some(0), elapsed))
            }
            Finished::Tester => {
                let tester_status_code = tester.wait()?.code();
                let elapsed = elapsed_time_as_ms(start);
                let _ = cout_tx.send(TesterFinished(tester_status_code, elapsed));
                if let Some(solver_status) = solver.try_wait()? {
                    let _ = cout_tx.send(SolverFinished(solver_status.code(), elapsed));
                } else {
                    let _ = solver.kill()?;
                    let _ = cout_tx.send(SolverFinished(None, elapsed));
                }
                Ok((tester_status_code == Some(0), elapsed))
            }
        }
    }

    // Returns `Ok(Err(exit_status))` if the stdin is closed.
    fn interact(
        process: &mut InteractiveProcess,
        cout_tx: &mut Sender<InteractiveConsoleOut>,
        last_out: Option<&str>,
    ) -> io::Result<Option<Arc<String>>> {
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
        let elapsed = elapsed_time_as_ms(process.start);
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

    fn elapsed_time_as_ms(start: Instant) -> u64 {
        let t = start.elapsed();
        (1000000000 * t.as_secs() + t.subsec_nanos() as u64 + 999999) / 1000000
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
                start: start,
                solver_s: solver_s,
                stdin: child.stdin.as_mut().unwrap(),
                stdout: BufReader::new(child.stdout.as_mut().unwrap()),
            }
        }
    }

    if let Some(compilation) = compilation {
        compilation.execute()?;
        println!("");
    }
    let cases = testsuite::load_and_merge_all_cases(suite_dir, suite_file_stem, suite_extensions)?;
    let solver = Arc::new(solver);
    judge_all(cases, solver)
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

    /// Creates a new `JudgingCommand` which `working_dir` is `self.working_dir`.
    fn new_in_same_dir<S: Into<String>>(&self, command: S) -> Self {
        JudgingCommand(CommandProperty::new(
            command.into(),
            self.0.working_dir.clone(),
        ))
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


enum JudgingOutputs {
    Simple(Vec<SimpleOutput>),
    Interactive(Vec<InteractiveOutput>),
}

impl FromIterator<SimpleOutput> for JudgingOutputs {
    fn from_iter<T: IntoIterator<Item = SimpleOutput>>(iter: T) -> Self {
        JudgingOutputs::Simple(iter.into_iter().collect())
    }
}

impl FromIterator<InteractiveOutput> for JudgingOutputs {
    fn from_iter<T: IntoIterator<Item = InteractiveOutput>>(iter: T) -> Self {
        JudgingOutputs::Interactive(iter.into_iter().collect())
    }
}

impl JudgingOutputs {
    fn show_result(self, num_cases: usize) -> JudgeResult<()> {
        fn count_num_failures<O: JudgingOutput>(outputs: &[O]) -> usize {
            outputs.iter().filter(|o| o.failure()).count()
        }

        fn eprint_failure_details<O: JudgingOutput>(outputs: &[O], num_cases: usize) {
            outputs.iter().enumerate().foreach(|(i, o)| {
                eprintln!("");
                o.eprint_title(i, num_cases);
                o.eprint_details();
            })
        }

        let suf = if num_cases > 1 { "s" } else { "" };
        let num_failures = match self {
            JudgingOutputs::Simple(ref os) => count_num_failures(os),
            JudgingOutputs::Interactive(ref os) => count_num_failures(os),
        };
        if num_failures == 0 {
            Ok(println!("All of the {} test{} passed.", num_cases, suf))
        } else {
            match self {
                JudgingOutputs::Simple(ref os) => eprint_failure_details(os, num_cases),
                JudgingOutputs::Interactive(ref os) => eprint_failure_details(os, num_cases),
            }
            bail!(JudgeErrorKind::TestFailure(num_failures, num_cases))
        }
    }
}


trait JudgingOutput
where
    Self: fmt::Display,
{
    fn failure(&self) -> bool;
    fn color(&self) -> u16;
    fn eprint_details(&self);

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
}


enum SimpleOutput {
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

impl fmt::Display for SimpleOutput {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SimpleOutput::Ac(t, ..) => write!(f, "AC ({}ms)", t),
            SimpleOutput::Tle(t, ..) => write!(f, "TLE ({}ms)", t),
            SimpleOutput::Wa(t, ..) => write!(f, "WA ({}ms)", t),
            SimpleOutput::Re(t, .., status) => write!(f, "RE ({}, {}ms)", status, t),
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


struct InteractiveOutput {
    kind: InteractiveOutputKind,
    elapsed: u64,
    console_outs: Vec<InteractiveConsoleOut>,
}

impl fmt::Display for InteractiveOutput {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            InteractiveOutputKind::Success => write!(f, "Success ({}ms)", self.elapsed),
            InteractiveOutputKind::Failure => write!(f, "Failure ({}ms)", self.elapsed),
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

    fn color(&self) -> u16 {
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

        if self.console_outs.is_empty() {
            return eprintln_decorated!(Attr::Bold, Some(color::YELLOW), "EMPTY");
        }
        let tester_len = max_length(InteractiveConsoleOut::is_tester_s);
        let solver_len = max_length(InteractiveConsoleOut::is_solver_s);
        for cout in &self.console_outs {
            cout.eprint_aligned(tester_len, solver_len);
        }
    }
}

impl InteractiveOutput {
    fn new(
        timelimit: Option<u64>,
        elapsed: u64,
        succeeded: bool,
        console_outs: Vec<InteractiveConsoleOut>,
    ) -> Self {
        let kind = if succeeded && (timelimit.is_none() || timelimit.unwrap() >= elapsed) {
            InteractiveOutputKind::Success
        } else {
            InteractiveOutputKind::Failure
        };
        Self {
            kind: kind,
            elapsed: elapsed,
            console_outs: console_outs,
        }
    }

    fn exceeded(timelimit: u64, console_outs: Vec<InteractiveConsoleOut>) -> Self {
        Self {
            kind: InteractiveOutputKind::Failure,
            elapsed: timelimit,
            console_outs: console_outs,
        }
    }
}


enum InteractiveOutputKind {
    Success,
    Failure,
}


enum InteractiveConsoleOut {
    SolverStdout(Arc<String>, u64),
    #[allow(dead_code)]
    SolverStderr(Arc<String>, u64),
    SolverFinished(Option<i32>, u64),
    TesterStdout(Arc<String>, u64),
    #[allow(dead_code)]
    TesterStderr(Arc<String>, u64),
    TesterFinished(Option<i32>, u64),
}

impl InteractiveConsoleOut {
    fn is_solver_s(&self) -> bool {
        match *self {
            InteractiveConsoleOut::SolverStdout(..) |
            InteractiveConsoleOut::SolverStderr(..) |
            InteractiveConsoleOut::SolverFinished(..) => true,
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

    fn content<'a>(&'a self) -> String {
        match *self {
            InteractiveConsoleOut::SolverStdout(ref s, _) => format!("{:?}", s),
            InteractiveConsoleOut::SolverStderr(ref s, _) => format!("{:?}", s),
            InteractiveConsoleOut::SolverFinished(Some(c), _) => format!("exit code: {}", c),
            InteractiveConsoleOut::SolverFinished(None, _) => "killed".to_owned(),
            InteractiveConsoleOut::TesterStdout(ref s, _) => format!("{:?}", s),
            InteractiveConsoleOut::TesterStderr(ref s, _) => format!("{:?}", s),
            InteractiveConsoleOut::TesterFinished(Some(c), _) => format!("exit code: {}", c),
            InteractiveConsoleOut::TesterFinished(None, _) => "killed".to_owned(),
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
                ("Solver,stderr", color::BRIGHT_MAGENTA, t) //
            }
            InteractiveConsoleOut::SolverFinished(Some(0), t) => {
                ("Solver finished", color::BRIGHT_CYAN, t) //
            }
            InteractiveConsoleOut::SolverFinished(Some(_), t) => {
                ("Solver finished", color::RED, t) //
            }
            InteractiveConsoleOut::SolverFinished(None, t) => {
                ("Solver killed", color::YELLOW, t) //
            }
            InteractiveConsoleOut::TesterStdout(_, t) => {
                ("Tester stdout", color::GREEN, t) //
            }
            InteractiveConsoleOut::TesterStderr(_, t) => {
                ("Tester stderr", color::MAGENTA, t) //
            }
            InteractiveConsoleOut::TesterFinished(Some(0), t) => {
                ("Tester finished", color::BRIGHT_GREEN, t) //
            }
            InteractiveConsoleOut::TesterFinished(Some(_), t) => {
                ("Tester finished", color::RED, t) //
            }
            InteractiveConsoleOut::TesterFinished(None, t) => {
                ("Tester killed", color::YELLOW, t) //
            }
        };
        let content = self.content();
        let length = content.chars().count();
        eprint_decorated!(Attr::Bold, Some(color), "{:<15}", kind);
        eprint!("│");
        if self.is_solver_s() {
            eprint_spaces(tester_length);
            eprint!("│");
            eprint_decorated!(Attr::Bold, Some(color), "{}", content);
            eprint_spaces(solver_length - length);
        } else {
            eprint_decorated!(Attr::Bold, Some(color), "{}", content);
            eprint_spaces(tester_length - length);
            eprint!("│");
            eprint_spaces(solver_length);
        }
        eprintln!("│{:>4}ms", elapsed);
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
