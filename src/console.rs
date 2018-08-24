use Never;

use ansi_term::{Colour, Style};
use rpassword;
use unicode_width::UnicodeWidthStr as _UnicodeWidthStr;

#[cfg(windows)]
use ansi_term;

#[cfg(not(windows))]
use term::{Terminal as _Terminal, TerminfoTerminal};

use std::io::{self, BufRead, Write};
use std::str::FromStr;
use std::{self, env, fmt};

#[derive(Default, Serialize, Deserialize)]
pub(crate) struct Conf {
    #[serde(default)]
    color: ColorRange,
    #[serde(default = "cjk_default")]
    cjk: bool,
}

fn cjk_default() -> bool {
    true
}

impl Conf {
    fn colours(&self) -> [Option<Colour>; 10] {
        match self.color {
            ColorRange::_8 => [
                Some(Colour::Green),
                Some(Colour::Yellow),
                Some(Colour::Red),
                Some(Colour::Cyan),
                Some(Colour::Purple),
                Some(Colour::Cyan),
                Some(Colour::Cyan),
                Some(Colour::Purple),
                Some(Colour::Green),
                Some(Colour::Cyan),
            ],
            ColorRange::_16 => [
                Some(Colour::Fixed(10)),
                Some(Colour::Fixed(11)),
                Some(Colour::Fixed(9)),
                Some(Colour::Fixed(14)),
                Some(Colour::Fixed(13)),
                Some(Colour::Fixed(14)),
                Some(Colour::Fixed(14)),
                Some(Colour::Fixed(13)),
                Some(Colour::Fixed(10)),
                Some(Colour::Fixed(14)),
            ],
            ColorRange::_256 => [
                Some(Colour::Fixed(118)),
                Some(Colour::Fixed(190)),
                Some(Colour::Fixed(196)),
                Some(Colour::Fixed(123)),
                Some(Colour::Fixed(99)),
                Some(Colour::Fixed(50)),
                Some(Colour::Fixed(50)),
                Some(Colour::Fixed(198)),
                Some(Colour::Fixed(118)),
                Some(Colour::Fixed(99)),
            ],
        }
    }
}

#[derive(Clone, Copy, Serialize, Deserialize)]
pub(crate) enum ColorRange {
    #[serde(rename = "8color")]
    _8,
    #[serde(rename = "16color")]
    _16,
    #[serde(rename = "256color")]
    _256,
}

impl Default for ColorRange {
    fn default() -> Self {
        ColorRange::_256
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

#[derive(Debug)]
pub struct Console<I: BufRead, O: Write, E: Write> {
    stdin: I,
    stdout: O,
    stderr: E,
    colours: [Option<Colour>; 10],
    cjk: bool,
    enabled: Option<bool>,
}

impl Console<io::Empty, io::Sink, io::Sink> {
    pub fn null() -> Self {
        Self {
            stdin: io::empty(),
            stdout: io::sink(),
            stderr: io::sink(),
            colours: [None; 10],
            cjk: false,
            enabled: None,
        }
    }
}

impl<I: BufRead, O: Write, E: Write> Console<I, O, E> {
    pub fn new(stdin: I, stdout: O, stderr: E) -> Self {
        Self {
            stdin,
            stdout,
            stderr,
            colours: [None; 10],
            cjk: cjk_default(),
            enabled: None,
        }
    }

    pub(crate) fn out(&mut self) -> ConsoleOut<O, E> {
        ConsoleOut {
            stdout: &mut self.stdout,
            stderr: &mut self.stderr,
            colours: &self.colours,
            cjk: self.cjk,
        }
    }

    pub fn stdout(&mut self) -> self::Stdout<O> {
        Stdout {
            wrt: &mut self.stdout,
            colours: &self.colours,
            cjk: self.cjk,
            style: Style::default(),
        }
    }

    pub fn stderr(&mut self) -> self::Stderr<E> {
        Stderr {
            wrt: &mut self.stderr,
            colours: &self.colours,
            cjk: self.cjk,
            style: Style::default(),
        }
    }

    pub(crate) fn fill_palettes(&mut self, choice: ColorChoice, conf: &Conf) -> io::Result<()> {
        let enabled = match (self.enabled, choice) {
            (Some(p), _) => p,
            (None, ColorChoice::Never) => false,
            (None, ColorChoice::Auto) => self.fill_palettes_on_auto(),
            (None, ColorChoice::Always) => self.fill_palettes_on_always()?,
        };
        self.enabled = Some(enabled);
        if enabled {
            self.colours = conf.colours();
        }
        Ok(())
    }

    #[cfg(not(windows))]
    fn fill_palettes_on_auto(&mut self) -> bool {
        TerminfoTerminal::new(io::sink()).map_or(false, |t| t.supports_color())
    }

    #[cfg(windows)]
    fn fill_palettes_on_auto(&mut self) -> bool {
        env::var_os("MSYSTEM").is_some() || ansi_term::enable_ansi_support().is_ok()
    }

    #[cfg(not(windows))]
    fn fill_palettes_on_always(&mut self) -> io::Result<bool> {
        Ok(true)
    }

    #[cfg(windows)]
    fn fill_palettes_on_always(&mut self) -> io::Result<bool> {
        if let Err(code) = ansi_term::enable_ansi_support() {
            if env::var_os("MSYSTEM").is_none() {
                writeln!(
                    self.stderr,
                    "Failed to enable VIRTUAL_TERMINAL_PROCESSING (error code: {})\n\
                     Run with \"-C auto\" or \"-C never\".\n",
                    code
                )?;
                self.stderr.flush()?;
                return Ok(false);
            }
        }
        Ok(true)
    }

    pub(crate) fn ask_yes_or_no(&mut self, mes: &str, default: bool) -> io::Result<bool> {
        let prompt = format!("{}{} ", mes, if default { "(Y/n)" } else { "(y/N)" });
        loop {
            match &self.prompt_reply_stderr(&prompt)? {
                s if s.is_empty() => break Ok(default),
                s if s.eq_ignore_ascii_case("y") || s.eq_ignore_ascii_case("yes") => break Ok(true),
                s if s.eq_ignore_ascii_case("n") || s.eq_ignore_ascii_case("no") => break Ok(false),
                _ => self
                    .stderr()
                    .plain(Palette::Warning)
                    .write_all(b"Answer \"y\", \"yes\", \"n\", \"no\", or \"\".")?,
            }
        }
    }

    pub(crate) fn prompt_reply_stderr(&mut self, prompt: &str) -> io::Result<String> {
        self.prompt_stderr(prompt)?;
        let mut reply = "".to_owned();
        self.stdin.read_line(&mut reply)?;
        if reply.ends_with("\r\n") {
            reply.pop();
            reply.pop();
            Ok(reply)
        } else if reply.ends_with('\n') {
            reply.pop();
            Ok(reply)
        } else {
            Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "unexpected end of file",
            ))
        }
    }

    pub(crate) fn prompt_password_stderr(&mut self, prompt: &str) -> io::Result<String> {
        if cfg!(windows) && env::var_os("MSYSTEM").is_some() {
            self.stderr()
                .plain(Palette::Warning)
                .write_all(b"$MSYSTEM is present. The input won't be hidden.\n")?;
            self.prompt_reply_stderr(prompt)
        } else {
            self.prompt_stderr(prompt)?;
            rpassword::read_password_with_reader(Some(&mut self.stdin))
        }
    }

    fn prompt_stderr(&mut self, prompt: &str) -> io::Result<()> {
        self.stderr.write_all(prompt.as_bytes())?;
        self.stderr.flush()
    }
}

pub(crate) struct ConsoleOut<'a, O: Write + 'a, E: Write + 'a> {
    stdout: &'a mut O,
    stderr: &'a mut E,
    colours: &'a [Option<Colour>; 10],
    cjk: bool,
}

impl<'a, O: Write + 'a, E: Write + 'a> ConsoleOut<'a, O, E> {
    pub(crate) fn stdout<'b>(&'b mut self) -> Stdout<'b, O>
    where
        'a: 'b,
    {
        Stdout {
            wrt: &mut self.stdout,
            colours: &self.colours,
            cjk: self.cjk,
            style: Style::default(),
        }
    }

    pub(crate) fn stderr<'b>(&'b mut self) -> Stderr<'b, E>
    where
        'a: 'b,
    {
        Stderr {
            wrt: &mut self.stderr,
            colours: &self.colours,
            cjk: self.cjk,
            style: Style::default(),
        }
    }

    pub(crate) fn width(&self, s: &str) -> usize {
        if self.cjk {
            s.width_cjk()
        } else {
            s.width()
        }
    }
}

pub(crate) type Stdout<'a, W> = Printer<'a, W>;
pub(crate) type Stderr<'a, W> = Printer<'a, W>;

pub struct Printer<'a, W: Write + 'a> {
    wrt: &'a mut W,
    colours: &'a [Option<Colour>; 10],
    cjk: bool,
    style: Style,
}

impl<'a, W: Write + 'a> Printer<'a, W> {
    pub(crate) fn inner(&mut self) -> &mut W {
        &mut self.wrt
    }

    pub(crate) fn reborrow(&mut self) -> Printer<W> {
        Printer {
            wrt: &mut self.wrt,
            colours: &self.colours,
            cjk: self.cjk,
            style: self.style,
        }
    }

    pub(crate) fn plain(&mut self, palette: Palette) -> Printer<W> {
        Printer {
            wrt: &mut self.wrt,
            colours: &self.colours,
            cjk: self.cjk,
            style: Style {
                foreground: palette.pick_fg_colour(self.colours),
                ..self.style
            },
        }
    }

    pub fn bold(&mut self, palette: impl Into<Option<Palette>>) -> Printer<W> {
        let foreground = palette.into().and_then(|p| p.pick_fg_colour(self.colours));
        Printer {
            wrt: &mut self.wrt,
            colours: &self.colours,
            cjk: self.cjk,
            style: Style {
                foreground,
                is_bold: true,
                ..self.style
            },
        }
    }

    pub(crate) fn width(&self, s: &str) -> usize {
        if self.cjk {
            s.width_cjk()
        } else {
            s.width()
        }
    }

    pub fn write_spaces(&mut self, n: usize) -> io::Result<()> {
        (0..n).try_for_each(|_| self.wrt.write_all(b" "))
    }
}

impl<'a, W: Write + 'a> Write for Printer<'a, W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.write_all(buf).map(|()| buf.len())
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        write!(self.wrt, "{}", self.style.prefix())?;
        self.wrt.write_all(buf)?;
        write!(self.wrt, "{}", self.style.suffix())
    }

    fn flush(&mut self) -> io::Result<()> {
        self.wrt.flush()
    }
}

#[derive(Clone, Copy)]
pub enum Palette {
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
    fn pick_fg_colour(self, colours: &[Option<Colour>; 10]) -> Option<Colour> {
        match self {
            Palette::Success => colours[0],
            Palette::Warning => colours[1],
            Palette::Fatal => colours[2],
            Palette::Url => colours[3],
            Palette::Title => colours[4],
            Palette::CommandInfo => colours[5],
            Palette::SolverStdout => colours[6],
            Palette::SolverStderr => colours[7],
            Palette::TesterStdout => colours[8],
            Palette::TesterStderr => colours[9],
        }
    }
}
