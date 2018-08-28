use Never;

use ansi_term::{Colour, Style};
use rpassword;
use unicode_width::UnicodeWidthStr as _UnicodeWidthStr;

#[cfg(windows)]
use ansi_term;

#[cfg(not(windows))]
use term::{Terminal as _Terminal, TerminfoTerminal};

use std::io::{self, BufRead, Write};
use std::rc::Rc;
use std::str::FromStr;
use std::{self, env, fmt};

#[derive(Default, Serialize, Deserialize)]
pub struct Conf {
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

pub trait ConsoleReadWrite {
    type Stdin: BufRead;
    type Stdout: Write;
    type Stderr: Write;

    fn by_mutable(&mut self) -> &mut Console<Self::Stdin, Self::Stdout, Self::Stderr>;

    fn stdout_and_stderr(&mut self) -> (Printer<&mut Self::Stdout>, Printer<&mut Self::Stderr>) {
        let this = self.by_mutable();
        let stdout = Printer {
            wrt: &mut this.stdout,
            colours: this.colours.clone(),
            cjk: this.cjk,
            style: Style::default(),
        };
        let stderr = Printer {
            wrt: &mut this.stderr,
            colours: this.colours.clone(),
            cjk: this.cjk,
            style: Style::default(),
        };
        (stdout, stderr)
    }

    fn stdout(&mut self) -> Printer<&mut Self::Stdout> {
        let this = self.by_mutable();
        Printer {
            wrt: &mut this.stdout,
            colours: this.colours.clone(),
            cjk: this.cjk,
            style: Style::default(),
        }
    }

    fn stderr(&mut self) -> Printer<&mut Self::Stderr> {
        let this = self.by_mutable();
        Printer {
            wrt: &mut this.stderr,
            colours: this.colours.clone(),
            cjk: this.cjk,
            style: Style::default(),
        }
    }

    fn fill_palettes(&mut self, choice: ColorChoice, conf: &Conf) -> io::Result<()> {
        let this = self.by_mutable();
        let enabled = match (this.enabled, choice) {
            (Some(p), _) => p,
            (None, ColorChoice::Never) => false,
            (None, ColorChoice::Auto) => this.fill_palettes_on_auto(),
            (None, ColorChoice::Always) => this.fill_palettes_on_always()?,
        };
        this.enabled = Some(enabled);
        if enabled {
            this.colours = Rc::new(conf.colours());
        }
        Ok(())
    }

    fn ask_yes_or_no(&mut self, mes: &str, default: bool) -> io::Result<bool> {
        let prompt = format!("{}{} ", mes, if default { "(Y/n)" } else { "(y/N)" });
        loop {
            match &self.prompt_reply_stderr(&prompt)? {
                s if s.is_empty() => break Ok(default),
                s if s.eq_ignore_ascii_case("y") || s.eq_ignore_ascii_case("yes") => break Ok(true),
                s if s.eq_ignore_ascii_case("n") || s.eq_ignore_ascii_case("no") => break Ok(false),
                _ => self
                    .by_mutable()
                    .stderr()
                    .plain(Palette::Warning)
                    .write_all(b"Answer \"y\", \"yes\", \"n\", \"no\", or \"\".")?,
            }
        }
    }

    fn prompt_reply_stderr(&mut self, prompt: &str) -> io::Result<String> {
        let this = self.by_mutable();
        this.prompt_stderr(prompt)?;
        let mut reply = "".to_owned();
        this.stdin.read_line(&mut reply)?;
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

    fn prompt_password_stderr(&mut self, prompt: &str) -> io::Result<String> {
        let this = self.by_mutable();
        if cfg!(windows) && env::var_os("MSYSTEM").is_some() {
            this.stderr()
                .plain(Palette::Warning)
                .write_all(b"$MSYSTEM is present. The input won't be hidden.\n")?;
            this.prompt_reply_stderr(prompt)
        } else {
            this.prompt_stderr(prompt)?;
            rpassword::read_password_with_reader(Some(&mut this.stdin))
        }
    }
}

impl<'a, RW: ConsoleReadWrite> ConsoleReadWrite for &'a mut RW {
    type Stdin = RW::Stdin;
    type Stdout = RW::Stdout;
    type Stderr = RW::Stderr;

    fn by_mutable(&mut self) -> &mut Console<RW::Stdin, RW::Stdout, RW::Stderr> {
        (**self).by_mutable()
    }
}

#[derive(Debug)]
pub struct Console<I: BufRead, O: Write, E: Write> {
    stdin: I,
    stdout: O,
    stderr: E,
    colours: Rc<[Option<Colour>; 10]>,
    cjk: bool,
    enabled: Option<bool>,
}

impl Console<io::Empty, io::Sink, io::Sink> {
    pub fn null() -> Self {
        Self {
            stdin: io::empty(),
            stdout: io::sink(),
            stderr: io::sink(),
            colours: Rc::default(),
            cjk: bool::default(),
            enabled: Some(false),
        }
    }
}

impl<I: BufRead, O: Write, E: Write> Console<I, O, E> {
    pub fn new(stdin: I, stdout: O, stderr: E) -> Self {
        Self {
            stdin,
            stdout,
            stderr,
            colours: Rc::new([None; 10]),
            cjk: cjk_default(),
            enabled: None,
        }
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

    fn prompt_stderr(&mut self, prompt: &str) -> io::Result<()> {
        self.stderr.write_all(prompt.as_bytes())?;
        self.stderr.flush()
    }
}

impl<I: BufRead, O: Write, E: Write> ConsoleReadWrite for Console<I, O, E> {
    type Stdin = I;
    type Stdout = O;
    type Stderr = E;

    fn by_mutable(&mut self) -> &mut Console<I, O, E> {
        self
    }
}

pub trait ConsoleWrite: Write {
    type Inner: Write;

    fn by_immutable(&self) -> &Printer<Self::Inner>;

    fn by_mutable(&mut self) -> &mut Printer<Self::Inner>;

    fn inner_writer(&mut self) -> &mut Self::Inner {
        &mut self.by_mutable().wrt
    }

    fn plain(&mut self, palette: Palette) -> Printer<&mut Self::Inner> {
        let this = self.by_mutable();
        let foreground = palette.pick_fg_colour(&this.colours);
        Printer {
            wrt: &mut this.wrt,
            colours: this.colours.clone(),
            cjk: this.cjk,
            style: Style {
                foreground,
                ..this.style
            },
        }
    }

    fn bold(&mut self, palette: impl Into<Option<Palette>>) -> Printer<&mut Self::Inner> {
        let this = self.by_mutable();
        let foreground = palette.into().and_then(|p| p.pick_fg_colour(&this.colours));
        Printer {
            wrt: &mut this.wrt,
            colours: this.colours.clone(),
            cjk: this.cjk,
            style: Style {
                foreground,
                is_bold: true,
                ..this.style
            },
        }
    }

    fn width(&self, s: &str) -> usize {
        if self.by_immutable().cjk {
            s.width_cjk()
        } else {
            s.width()
        }
    }

    fn write_spaces(&mut self, n: usize) -> io::Result<()> {
        (0..n).try_for_each(|_| self.inner_writer().write_all(b" "))
    }
}

impl<'a, C: ConsoleWrite> ConsoleWrite for &'a mut C {
    type Inner = C::Inner;

    fn by_immutable(&self) -> &Printer<Self::Inner> {
        (**self).by_immutable()
    }

    fn by_mutable(&mut self) -> &mut Printer<Self::Inner> {
        (**self).by_mutable()
    }
}

pub struct Printer<W: Write> {
    wrt: W,
    colours: Rc<[Option<Colour>; 10]>,
    cjk: bool,
    style: Style,
}

#[cfg(test)]
impl Printer<io::Sink> {
    pub(crate) fn null() -> Self {
        Self {
            wrt: io::sink(),
            colours: Rc::default(),
            cjk: bool::default(),
            style: Style::default(),
        }
    }
}

impl<W: Write> Write for Printer<W> {
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

impl<W: Write> ConsoleWrite for Printer<W> {
    type Inner = W;

    fn by_immutable(&self) -> &Printer<W> {
        self
    }

    fn by_mutable(&mut self) -> &mut Self {
        self
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
