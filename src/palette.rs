use ansi_term::{ANSIGenericString, Colour, Style};
use term::{Terminal as _Terminal, TerminfoTerminal};

use std::borrow::Cow;
use std::sync::atomic::{self, AtomicBool, AtomicUsize};
use std::{fmt, io};

static ANSI_MODE: AtomicBool = AtomicBool::new(false);

#[cfg(windows)]
pub(crate) fn enable_ansi_support(mode: ColorMode) {
    if should_enable_ansi_support(mode) && !ANSI_MODE.swap(true, atomic::Ordering::Relaxed) {
        if let Err(code) = ansi_term::enable_ansi_support() {
            info!(
                "Failed to enable VIRTUAL_TERMINAL_PROCESSING (error code: {})",
                code
            );
        }
    }
}

#[cfg(not(windows))]
pub(crate) fn enable_ansi_support(mode: ColorMode) {
    if should_enable_ansi_support(mode) {
        ANSI_MODE.store(true, atomic::Ordering::Relaxed);
    }
}

fn should_enable_ansi_support(mode: ColorMode) -> bool {
    match mode {
        ColorMode::Never => false,
        ColorMode::Always => true,
        ColorMode::Auto => {
            let p = TerminfoTerminal::new(io::sink()).map_or(false, |t| t.supports_color());
            if !p {
                info!("The terminal does not support colors");
            }
            p
        }
    }
}

#[derive(Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ColorMode {
    Never,
    Auto,
    Always,
}

impl Default for ColorMode {
    fn default() -> Self {
        ColorMode::Never
    }
}

impl fmt::Display for ColorMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ColorMode::Never => write!(f, "never"),
            ColorMode::Auto => write!(f, "auto"),
            ColorMode::Always => write!(f, "always"),
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
        if ANSI_MODE.load(atomic::Ordering::Relaxed) {
            Style {
                foreground: self.colour(),
                ..Style::default()
            }
        } else {
            Style::default()
        }.paint(input)
    }

    pub fn bold(self) -> Style {
        if ANSI_MODE.load(atomic::Ordering::Relaxed) {
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
        static RESOLVED: AtomicBool = AtomicBool::new(false);
        static NUM_SKIPS: AtomicUsize = AtomicUsize::new(0);
        if !RESOLVED.swap(true, atomic::Ordering::Relaxed) {
            if let Some(mut term) = TerminfoTerminal::new(io::sink()) {
                let n = if term.fg(255).is_ok() {
                    0
                } else if term.fg(15).is_ok() {
                    1
                } else if term.fg(7).is_ok() {
                    2
                } else {
                    3
                };
                NUM_SKIPS.store(n, atomic::Ordering::Relaxed);
            }
        }
        let i = NUM_SKIPS.load(atomic::Ordering::Relaxed);
        let colours: &[_] = match self {
            Palette::Plain => &[],
            Palette::Success | Palette::TesterStdout => &[118, 10, 2],
            Palette::Warning => &[190, 11, 3],
            Palette::Fatal => &[196, 9, 1],
            Palette::Url => &[123, 14, 6],
            Palette::Title | Palette::TesterStderr => &[99, 13, 5],
            Palette::CommandInfo | Palette::SolverStdout => &[50, 14, 6],
            Palette::SolverStderr => &[198, 13, 8],
        };
        colours.get(i).cloned().map(Colour::Fixed)
    }
}
