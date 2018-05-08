use term::{self, Attr, Terminal};

use std::fmt;
use std::io::Write;
use std::sync::atomic::{self, AtomicBool};

#[derive(Clone, Copy)]
pub enum Color {
    Success,
    Warning,
    Fatal,
    Title,
    CommandInfo,
    SolverStdout,
    SolverStderr,
    TesterStdout,
    TesterStderr,
}

impl Into<&'static [u32]> for Color {
    fn into(self) -> &'static [u32] {
        if COLOR_ENABLED.load(atomic::Ordering::Relaxed) {
            match self {
                Color::Success | Color::TesterStdout => &[118, 10, 2],
                Color::Warning => &[190, 11, 3],
                Color::Fatal => &[196, 9, 1],
                Color::Title | Color::TesterStderr => &[99, 13, 5],
                Color::CommandInfo | Color::SolverStdout => &[50, 14, 6],
                Color::SolverStderr => &[198, 13, 8],
            }
        } else {
            &[]
        }
    }
}

pub fn disable_color() {
    COLOR_ENABLED.swap(false, atomic::Ordering::Relaxed);
}

lazy_static! {
    static ref COLOR_ENABLED: AtomicBool = AtomicBool::new(true);
}

pub fn print_bold<C: Into<Option<Color>>>(color: C, args: fmt::Arguments) {
    if let Some(term) = term::stdout() {
        if write(color.into().map(Into::into), Some(Attr::Bold), term, args) {
            return;
        }
    }
    print!("{}", args);
}

pub fn eprint_bold<C: Into<Option<Color>>>(color: C, args: fmt::Arguments) {
    if let Some(term) = term::stderr() {
        if write(color.into().map(Into::into), Some(Attr::Bold), term, args) {
            return;
        }
    }
    eprint!("{}", args);
}

pub fn println_bold<C: Into<Option<Color>>>(color: C, args: fmt::Arguments) {
    print_bold(color, args);
    println!();
}

pub fn eprintln_bold<C: Into<Option<Color>>>(color: C, args: fmt::Arguments) {
    eprint_bold(color, args);
    eprintln!();
}

fn write<O: Write>(
    colors: Option<&[u32]>,
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

    if let Some(colors) = colors {
        for &color in colors {
            if try_write(Some(color)) {
                return true;
            }
        }
        false
    } else {
        try_write(None)
    }
}
