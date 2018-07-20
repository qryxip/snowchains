use ansi_term::{ANSIGenericString, Colour, Style};
use term::{Terminal as _Terminal, TerminfoTerminal};
use Never;

use std::borrow::Cow;
use std::str::FromStr;
use std::sync::atomic::{self, AtomicBool, AtomicIsize};
use std::{self, fmt, io};

static ANSI_ENABLED: AtomicBool = AtomicBool::new(false);
static COLOR_RANGE: AtomicIsize = AtomicIsize::new(2);

#[cfg(not(windows))]
pub(crate) fn try_enable_ansi(choice: ColorChoice) {
    let p = match choice {
        ColorChoice::Never => false,
        ColorChoice::Always => true,
        ColorChoice::Auto => {
            TerminfoTerminal::new(io::sink()).map_or(false, |t| t.supports_color())
        }
    };
    if p {
        ANSI_ENABLED.store(true, atomic::Ordering::Relaxed);
    }
}

#[cfg(windows)]
pub(crate) fn try_enable_ansi(choice: ColorChoice) {
    match choice {
        ColorChoice::Never => {}
        ColorChoice::Auto => {
            if ansi_term::enable_ansi_support().is_ok() {
                ANSI_ENABLED.store(true, atomic::Ordering::Relaxed);
            }
        }
        ColorChoice::Always => {
            if let Err(code) = ansi_term::enable_ansi_support() {
                eprintln!(
                    "Failed to enable VIRTUAL_TERMINAL_PROCESSING (error code: {})\n\
                     Run with \"-C auto\" or \"-C never\".\n",
                    code
                );
            } else {
                ANSI_ENABLED.store(true, atomic::Ordering::Relaxed);
            }
        }
    }
}

#[derive(Clone, Copy, Serialize, Deserialize)]
pub enum ColorRange {
    #[serde(rename = "8color")]
    Color8,
    #[serde(rename = "16color")]
    Color16,
    #[serde(rename = "256color")]
    Color256,
}

impl ColorRange {
    fn current() -> Self {
        match COLOR_RANGE.load(atomic::Ordering::Relaxed) {
            0 => ColorRange::Color8,
            1 => ColorRange::Color16,
            2 => ColorRange::Color256,
            _ => unreachable!(),
        }
    }

    pub(crate) fn set_globally(self) {
        let n = match self {
            ColorRange::Color8 => 0,
            ColorRange::Color16 => 1,
            ColorRange::Color256 => 2,
        };
        COLOR_RANGE.store(n, atomic::Ordering::Relaxed);
    }
}

impl Default for ColorRange {
    fn default() -> Self {
        ColorRange::Color256
    }
}

impl fmt::Display for ColorRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ColorRange::Color8 => write!(f, "8color"),
            ColorRange::Color16 => write!(f, "16color"),
            ColorRange::Color256 => write!(f, "256color"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ColorChoice {
    Never,
    Auto,
    Always,
}

impl ColorChoice {
    pub(crate) fn clap_help() -> &'static str {
        "Use colors"
    }

    pub(crate) fn clap_default_value() -> &'static str {
        "auto"
    }

    pub(crate) fn clap_possible_values() -> &'static [&'static str] {
        &["never", "auto", "always"]
    }
}

impl FromStr for ColorChoice {
    type Err = Never;

    fn from_str(s: &str) -> std::result::Result<Self, Never> {
        match s {
            "never" => Ok(ColorChoice::Never),
            "auto" => Ok(ColorChoice::Auto),
            "always" => Ok(ColorChoice::Always),
            _ => Err(Never),
        }
    }
}

impl fmt::Display for ColorChoice {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ColorChoice::Never => write!(f, "never"),
            ColorChoice::Auto => write!(f, "auto"),
            ColorChoice::Always => write!(f, "always"),
        }
    }
}

#[derive(Clone, Copy)]
pub enum Palette {
    Plain,
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

impl Palette {
    pub(crate) fn paint<'a, S: 'a + ToOwned + ?Sized>(
        self,
        input: impl Into<Cow<'a, S>>,
    ) -> ANSIGenericString<'a, S>
    where
        S::Owned: fmt::Debug,
    {
        if ANSI_ENABLED.load(atomic::Ordering::Relaxed) {
            Style {
                foreground: self.colour(),
                ..Style::default()
            }
        } else {
            Style::default()
        }.paint(input)
    }

    pub fn bold(self) -> Style {
        if ANSI_ENABLED.load(atomic::Ordering::Relaxed) {
            Style {
                foreground: self.colour(),
                is_bold: true,
                ..Style::default()
            }
        } else {
            Style::default()
        }
    }

    fn colour(self) -> Option<Colour> {
        use self::{ColorRange::*, Palette::*};
        use ansi_term::Colour::*;
        match (self, ColorRange::current()) {
            (Plain, _) => None,
            (Success, Color8) | (TesterStdout, Color8) => Some(Green),
            (Success, Color16) | (TesterStdout, Color16) => Some(Fixed(10)),
            (Success, Color256) | (TesterStdout, Color256) => Some(Fixed(118)),
            (Warning, Color8) => Some(Yellow),
            (Warning, Color16) => Some(Fixed(11)),
            (Warning, Color256) => Some(Fixed(190)),
            (Fatal, Color8) => Some(Red),
            (Fatal, Color16) => Some(Fixed(9)),
            (Fatal, Color256) => Some(Fixed(196)),
            (Url, Color8) => Some(Cyan),
            (Url, Color16) => Some(Fixed(14)),
            (Url, Color256) => Some(Fixed(123)),
            (Title, Color8) | (TesterStderr, Color8) => Some(Purple),
            (Title, Color16) | (TesterStderr, Color16) => Some(Fixed(13)),
            (Title, Color256) | (TesterStderr, Color256) => Some(Fixed(99)),
            (CommandInfo, Color8) | (SolverStdout, Color8) => Some(Cyan),
            (CommandInfo, Color16) | (SolverStdout, Color16) => Some(Fixed(14)),
            (CommandInfo, Color256) | (SolverStdout, Color256) => Some(Fixed(50)),
            (SolverStderr, Color8) => Some(Purple),
            (SolverStderr, Color16) => Some(Fixed(13)),
            (SolverStderr, Color256) => Some(Fixed(198)),
        }
    }
}
