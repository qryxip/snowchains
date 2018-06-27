use term::{self, Attr, Terminal};

use std::fmt;
use std::io::Write;
use std::sync::Mutex;

#[derive(Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TerminalMode {
    Plain,
    Prefer8Color,
    Prefer16Color,
    Prefer256Color,
}

impl fmt::Display for TerminalMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TerminalMode::Plain => write!(f, "plain"),
            TerminalMode::Prefer8Color => write!(f, "prefer8color"),
            TerminalMode::Prefer16Color => write!(f, "prefer16color"),
            TerminalMode::Prefer256Color => write!(f, "prefer256color"),
        }
    }
}

#[derive(Clone, Copy)]
pub enum Color {
    None,
    Success,
    Warning,
    Fatal,
    Url,
    Title,
    CommandInfo,
    SolverStdout,
    SolverStderr,
    TesterStdout,
    TesterStderr,
}

impl Color {
    fn colors(self) -> &'static [Option<u32>] {
        let n = match *TERMINAL_MODE.lock().unwrap() {
            TerminalMode::Plain => 4,
            TerminalMode::Prefer8Color => 2,
            TerminalMode::Prefer16Color => 1,
            TerminalMode::Prefer256Color => 0,
        };
        &(match self {
            Color::None => &[None, None, None, None],
            Color::Success | Color::TesterStdout => &[Some(118), Some(10), Some(2), None],
            Color::Warning => &[Some(190), Some(11), Some(3), None],
            Color::Fatal => &[Some(196), Some(9), Some(1), None],
            Color::Url => &[Some(123), Some(14), Some(6), None],
            Color::Title | Color::TesterStderr => &[Some(99), Some(13), Some(5), None],
            Color::CommandInfo | Color::SolverStdout => &[Some(50), Some(14), Some(6), None],
            Color::SolverStderr => &[Some(198), Some(13), Some(8), None],
        })[n..]
    }
}

impl Into<&'static [u32]> for Color {
    fn into(self) -> &'static [u32] {
        let n = match *TERMINAL_MODE.lock().unwrap() {
            TerminalMode::Plain => 3,
            TerminalMode::Prefer8Color => 2,
            TerminalMode::Prefer16Color => 1,
            TerminalMode::Prefer256Color => 0,
        };
        &(match self {
            Color::None => &[0, 0, 0],
            Color::Success | Color::TesterStdout => &[118, 10, 2],
            Color::Warning => &[190, 11, 3],
            Color::Fatal => &[196, 9, 1],
            Color::Url => &[123, 14, 6],
            Color::Title | Color::TesterStderr => &[99, 13, 5],
            Color::CommandInfo | Color::SolverStdout => &[50, 14, 6],
            Color::SolverStderr => &[198, 13, 8],
        })[n..]
    }
}

pub(crate) fn terminal_mode(terminal_mode: TerminalMode) {
    *TERMINAL_MODE.lock().unwrap() = terminal_mode;
}

lazy_static! {
    static ref TERMINAL_MODE: Mutex<TerminalMode> = Mutex::new(TerminalMode::Plain);
}

pub fn print_bold(color: Color, args: fmt::Arguments) {
    if let Some(term) = term::stdout() {
        if write(color.colors(), Some(Attr::Bold), term, args) {
            return;
        }
    }
    print!("{}", args);
}

pub fn eprint_bold(color: Color, args: fmt::Arguments) {
    if let Some(term) = term::stderr() {
        if write(color.colors(), Some(Attr::Bold), term, args) {
            return;
        }
    }
    eprint!("{}", args);
}

pub fn println_bold(color: Color, args: fmt::Arguments) {
    print_bold(color, args);
    println!();
}

pub fn eprintln_bold(color: Color, args: fmt::Arguments) {
    eprint_bold(color, args);
    eprintln!();
}

fn write<O: Write>(
    colors: &[Option<u32>],
    attr: Option<Attr>,
    mut term: Box<Terminal<Output = O>>,
    args: fmt::Arguments,
) -> bool {
    let mut try_write = |color: Option<u32>| {
        if let Some(attr) = attr {
            if term.attr(attr).is_err() {
                let _ = term.reset();
                return false;
            }
        }
        if let Some(color) = color {
            if term.fg(color).is_err() {
                let _ = term.reset();
                return false;
            }
        }
        let success = term.write_fmt(args).is_ok();
        term.reset().is_ok() || success
    };

    for &color in colors {
        if try_write(color) {
            return true;
        }
    }
    false
}
