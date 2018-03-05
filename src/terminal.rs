use term::{self, Attr, Terminal};

use std::fmt;
use std::io::{self, Write};

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

impl Into<(u32, u32, u32)> for Color {
    fn into(self) -> (u32, u32, u32) {
        match self {
            Color::Success | Color::TesterStdout => (2, 10, 118),
            Color::Warning => (3, 11, 190),
            Color::Fatal => (1, 9, 196),
            Color::Title | Color::TesterStderr => (5, 13, 99),
            Color::CommandInfo | Color::SolverStdout => (6, 14, 50),
            Color::SolverStderr => (8, 13, 198),
        }
    }
}

pub fn print_bold<C: Into<Option<Color>>>(color: C, args: fmt::Arguments) {
    if let Some(term) = term::stdout() {
        if write(color.into().map(Into::into), Some(Attr::Bold), term, args) {
            return;
        }
    }
    io::stdout().write_fmt(args).unwrap();
}

pub fn eprint_bold<C: Into<Option<Color>>>(color: C, args: fmt::Arguments) {
    if let Some(term) = term::stderr() {
        if write(color.into().map(Into::into), Some(Attr::Bold), term, args) {
            return;
        }
    }
    io::stderr().write_fmt(args).unwrap();
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
    color: Option<(u32, u32, u32)>,
    attr: Option<Attr>,
    mut term: Box<Terminal<Output = O> + Send>,
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

    for color in &[
        color.map(|cs| cs.2),
        color.map(|cs| cs.1),
        color.map(|cs| cs.0),
    ] {
        if try_write(*color) {
            return true;
        }
    }
    false
}
