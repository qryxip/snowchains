use Never;

use ansi_term::{Colour, Style};
use term::{Terminal as _Terminal, TerminfoTerminal};
use unicode_width::{UnicodeWidthChar as _UnicodeWidthChar, UnicodeWidthStr};
use {atty, rpassword};

#[cfg(any(target_os = "linux", target_os = "macos"))]
use libc;

#[cfg(windows)]
use {ansi_term, winapi};

use std::io::{self, BufRead, BufReader, BufWriter, StderrLock, StdinLock, StdoutLock, Write};
use std::rc::Rc;
use std::str::FromStr;
use std::{self, env, fmt, process};

#[cfg(any(target_os = "linux", target_os = "macos", windows))]
use std::mem;

#[derive(Default, Serialize, Deserialize)]
pub struct Conf {
    #[serde(default)]
    color: ColorRange,
    #[serde(default = "cjk_default")]
    cjk: bool,
}

impl Conf {
    fn colours(&self) -> [Colour; 7] {
        match self.color {
            ColorRange::_8 => [
                Colour::Green,
                Colour::Yellow,
                Colour::Red,
                Colour::Cyan,
                Colour::Purple,
                Colour::Cyan,
                Colour::Cyan,
            ],
            ColorRange::_16 => [
                Colour::Fixed(10),
                Colour::Fixed(11),
                Colour::Fixed(9),
                Colour::Fixed(14),
                Colour::Fixed(13),
                Colour::Fixed(14),
                Colour::Fixed(14),
            ],
            ColorRange::_256 => [
                Colour::Fixed(118),
                Colour::Fixed(190),
                Colour::Fixed(196),
                Colour::Fixed(123),
                Colour::Fixed(99),
                Colour::Fixed(50),
                Colour::Fixed(50),
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

    fn process_redirection() -> process::Stdio;

    fn stdout_columns() -> Option<usize>;

    fn stderr_columns() -> Option<usize>;

    fn inner(&mut self) -> &mut ConsoleInner<Self::Stdin, Self::Stdout, Self::Stderr>;

    fn stdout_and_stderr(&mut self) -> (Printer<&mut Self::Stdout>, Printer<&mut Self::Stderr>) {
        let inner = self.inner();
        let stdout = Printer {
            wrt: &mut inner.stdout,
            colours: inner.stdout_colours.clone(),
            cjk: inner.cjk,
            style: Style::default(),
            process_redirection: Self::process_redirection,
            columns: Self::stdout_columns,
        };
        let stderr = Printer {
            wrt: &mut inner.stderr,
            colours: inner.stderr_colours.clone(),
            cjk: inner.cjk,
            style: Style::default(),
            process_redirection: Self::process_redirection,
            columns: Self::stderr_columns,
        };
        (stdout, stderr)
    }

    fn stdout(&mut self) -> Printer<&mut Self::Stdout> {
        self.stdout_and_stderr().0
    }

    fn stderr(&mut self) -> Printer<&mut Self::Stderr> {
        self.stdout_and_stderr().1
    }

    fn fill_palettes(&mut self, choice: ColorChoice, conf: &Conf) -> io::Result<()> {
        let inner = self.inner();
        let p1 = inner.stdout_colours.is_some();
        let p2 = inner.stderr_colours.is_some();
        if !(p1 && p2) {
            let (p3, p4) = match choice {
                ColorChoice::Never => (false, false),
                ColorChoice::Auto => try_enable_on_auto(),
                ColorChoice::Always => try_enable_on_always(),
            };
            if p1 || p3 {
                inner.stdout_colours = Some(Rc::new(conf.colours()));
            }
            if p2 || p4 {
                inner.stderr_colours = Some(Rc::new(conf.colours()));
            }
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
                    .stderr()
                    .plain(Palette::Warning)
                    .write_all(b"Answer \"y\", \"yes\", \"n\", \"no\", or \"\".")?,
            }
        }
    }

    fn prompt_reply_stderr(&mut self, prompt: &str) -> io::Result<String> {
        let inner = self.inner();
        inner.prompt_stderr(prompt)?;
        let mut reply = "".to_owned();
        inner.stdin.read_line(&mut reply)?;
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
        if cfg!(windows) && env::var_os("MSYSTEM").is_some() {
            self.stderr()
                .plain(Palette::Warning)
                .write_all(b"$MSYSTEM is present. The input won't be hidden.\n")?;
            self.prompt_reply_stderr(prompt)
        } else {
            let inner = self.inner();
            inner.prompt_stderr(prompt)?;
            rpassword::read_password_with_reader(Some(&mut inner.stdin))
        }
    }
}

impl<'a, RW: ConsoleReadWrite> ConsoleReadWrite for &'a mut RW {
    type Stdin = RW::Stdin;
    type Stdout = RW::Stdout;
    type Stderr = RW::Stderr;

    fn process_redirection() -> process::Stdio {
        RW::process_redirection()
    }

    fn stdout_columns() -> Option<usize> {
        RW::stdout_columns()
    }

    fn stderr_columns() -> Option<usize> {
        RW::stderr_columns()
    }

    fn inner(&mut self) -> &mut ConsoleInner<RW::Stdin, RW::Stdout, RW::Stderr> {
        (**self).inner()
    }
}

pub struct Console<'a> {
    inner: ConsoleInner<
        BufReader<StdinLock<'a>>,
        BufWriter<StdoutLock<'a>>,
        BufWriter<StderrLock<'a>>,
    >,
}

impl<'a> Console<'a> {
    pub fn new(stdin: StdinLock<'a>, stdout: StdoutLock<'a>, stderr: StderrLock<'a>) -> Self {
        Self {
            inner: ConsoleInner {
                stdin: BufReader::new(stdin),
                stdout: BufWriter::new(stdout),
                stderr: BufWriter::new(stderr),
                stdout_colours: None,
                stderr_colours: None,
                cjk: cjk_default(),
                enabled: None,
            },
        }
    }
}

impl<'a> ConsoleReadWrite for Console<'a> {
    type Stdin = BufReader<StdinLock<'a>>;
    type Stdout = BufWriter<StdoutLock<'a>>;
    type Stderr = BufWriter<StderrLock<'a>>;

    fn process_redirection() -> process::Stdio {
        process::Stdio::inherit()
    }

    fn stdout_columns() -> Option<usize> {
        stdout_columns()
    }

    fn stderr_columns() -> Option<usize> {
        stderr_columns()
    }

    fn inner(&mut self) -> &mut ConsoleInner<Self::Stdin, Self::Stdout, Self::Stderr> {
        &mut self.inner
    }
}

pub struct NullConsole {
    inner: ConsoleInner<io::Empty, io::Sink, io::Sink>,
}

impl NullConsole {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for NullConsole {
    fn default() -> Self {
        Self {
            inner: ConsoleInner {
                stdin: io::empty(),
                stdout: io::sink(),
                stderr: io::sink(),
                stdout_colours: None,
                stderr_colours: None,
                cjk: bool::default(),
                enabled: Some(false),
            },
        }
    }
}

impl ConsoleReadWrite for NullConsole {
    type Stdin = io::Empty;
    type Stdout = io::Sink;
    type Stderr = io::Sink;

    fn process_redirection() -> process::Stdio {
        process::Stdio::null()
    }

    fn stdout_columns() -> Option<usize> {
        None
    }

    fn stderr_columns() -> Option<usize> {
        None
    }

    fn inner(&mut self) -> &mut ConsoleInner<io::Empty, io::Sink, io::Sink> {
        &mut self.inner
    }
}

#[derive(Debug)]
pub struct ConsoleInner<I: BufRead, O: Write, E: Write> {
    stdin: I,
    stdout: O,
    stderr: E,
    stdout_colours: Option<Rc<[Colour; 7]>>,
    stderr_colours: Option<Rc<[Colour; 7]>>,
    cjk: bool,
    enabled: Option<bool>,
}

impl<I: BufRead, O: Write, E: Write> ConsoleInner<I, O, E> {
    pub fn new(stdin: I, stdout: O, stderr: E) -> Self {
        Self {
            stdin,
            stdout,
            stderr,
            stdout_colours: None,
            stderr_colours: None,
            cjk: cjk_default(),
            enabled: None,
        }
    }

    fn prompt_stderr(&mut self, prompt: &str) -> io::Result<()> {
        self.stderr.write_all(prompt.as_bytes())?;
        self.stderr.flush()
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
        self.by_mutable().apply(palette, None, |_| ())
    }

    fn bold(&mut self, palette: impl Into<Option<Palette>>) -> Printer<&mut Self::Inner> {
        self.by_mutable().apply(palette, None, |s| s.is_bold = true)
    }

    fn underline(&mut self, palette: impl Into<Option<Palette>>) -> Printer<&mut Self::Inner> {
        self.by_mutable()
            .apply(palette, None, |s| s.is_underline = true)
    }

    fn supports_color(&self) -> bool {
        self.by_immutable().colours.is_some()
    }

    fn process_redirection(&self) -> process::Stdio {
        (self.by_immutable().process_redirection)()
    }

    fn columns(&self) -> Option<usize> {
        (self.by_immutable().columns)()
    }

    fn width(&self, s: &str) -> usize {
        self.str_width_fn()(s)
    }

    fn char_width_or_zero(&self, c: char) -> usize {
        if self.by_immutable().cjk {
            c.width_cjk().unwrap_or(0)
        } else {
            c.width().unwrap_or(0)
        }
    }

    fn str_width_fn(&self) -> fn(&str) -> usize {
        if self.by_immutable().cjk {
            <str as UnicodeWidthStr>::width_cjk
        } else {
            <str as UnicodeWidthStr>::width
        }
    }

    fn write_spaces(&mut self, n: usize) -> io::Result<()> {
        (0..n).try_for_each(|_| self.inner_writer().write_all(b" "))
    }

    fn fill_bg(&mut self, n: usize, bg: Palette) -> io::Result<()> {
        let this = self.by_mutable().apply(None, bg, |_| ());
        write!(this.wrt, "{}", this.style.prefix())?;
        (0..n).try_for_each(|_| this.wrt.write_all(b" "))?;
        write!(this.wrt, "{}", this.style.suffix())
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
    colours: Option<Rc<[Colour; 7]>>,
    cjk: bool,
    style: Style,
    process_redirection: fn() -> process::Stdio,
    columns: fn() -> Option<usize>,
}

#[cfg(test)]
impl Printer<io::Sink> {
    pub(crate) fn null() -> Self {
        Self {
            wrt: io::sink(),
            colours: None,
            cjk: bool::default(),
            style: Style::default(),
            process_redirection: process::Stdio::null,
            columns: || None,
        }
    }
}

impl<W: Write> Printer<W> {
    fn apply(
        &mut self,
        fg: impl Into<Option<Palette>>,
        bg: impl Into<Option<Palette>>,
        modify_style: fn(&mut Style),
    ) -> Printer<&mut W> {
        let (fg, bg, attr) = match (fg.into(), bg.into(), self.colours.as_ref()) {
            (fg, bg, Some(cs)) => {
                let fg = fg.map(|fg| fg.pick_fg_colour(&cs));
                let bg = bg.map(|bg| bg.pick_fg_colour(&cs));
                (fg, bg, true)
            }
            _ => (None, None, false),
        };
        let mut style = Style {
            foreground: fg.or(self.style.foreground),
            background: bg.or(self.style.background),
            ..self.style
        };
        if attr {
            modify_style(&mut style);
        }
        Printer {
            wrt: &mut self.wrt,
            colours: self.colours.clone(),
            cjk: self.cjk,
            style,
            process_redirection: self.process_redirection,
            columns: self.columns,
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
    Info,
    Number,
}

impl Palette {
    fn pick_fg_colour(self, colours: &[Colour; 7]) -> Colour {
        match self {
            Palette::Success => colours[0],
            Palette::Warning => colours[1],
            Palette::Fatal => colours[2],
            Palette::Url => colours[3],
            Palette::Title => colours[4],
            Palette::Info => colours[5],
            Palette::Number => colours[6],
        }
    }
}

#[cfg(not(windows))]
fn try_enable_on_auto() -> (bool, bool) {
    filter_tty(detect_with_env())
}

#[cfg(windows)]
fn try_enable_on_auto() -> (bool, bool) {
    filter_tty(
        ansi_term::enable_ansi_support().is_ok()
            || env::var("MSYSTEM").is_ok() && detect_with_env(),
    )
}

fn detect_with_env() -> bool {
    TerminfoTerminal::new(io::sink()).map_or(false, |t| t.supports_color())
}

fn filter_tty(p: bool) -> (bool, bool) {
    (
        p && atty::is(atty::Stream::Stdout),
        p && atty::is(atty::Stream::Stderr),
    )
}

#[cfg(not(windows))]
fn try_enable_on_always() -> (bool, bool) {
    (true, true)
}

#[cfg(windows)]
fn try_enable_on_always() -> (bool, bool) {
    let _ = ansi_term::enable_ansi_support();
    (true, true)
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn stdout_columns() -> Option<usize> {
    unsafe { columns_with_libc(libc::STDOUT_FILENO) }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn stderr_columns() -> Option<usize> {
    unsafe { columns_with_libc(libc::STDERR_FILENO) }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
unsafe fn columns_with_libc(fd: libc::c_int) -> Option<usize> {
    let mut winsize = mem::zeroed::<libc::winsize>();
    if libc::ioctl(fd, libc::TIOCGWINSZ, &mut winsize) != 0 || winsize.ws_col == 0 {
        None
    } else {
        Some(winsize.ws_col as usize)
    }
}

#[cfg(windows)]
fn stdout_columns() -> Option<usize> {
    unsafe { columns_with_winapi(winapi::um::winbase::STD_OUTPUT_HANDLE) }
}

#[cfg(windows)]
fn stderr_columns() -> Option<usize> {
    unsafe { columns_with_winapi(winapi::um::winbase::STD_ERROR_HANDLE) }
}

#[cfg(windows)]
unsafe fn columns_with_winapi(h: winapi::shared::minwindef::DWORD) -> Option<usize> {
    use winapi::um::processenv::GetStdHandle;
    use winapi::um::wincon::{GetConsoleScreenBufferInfo, CONSOLE_SCREEN_BUFFER_INFO};
    let h = GetStdHandle(h);
    let mut info = mem::zeroed::<CONSOLE_SCREEN_BUFFER_INFO>();
    if GetConsoleScreenBufferInfo(h, &mut info) == 0 {
        None
    } else {
        Some((info.srWindow.Right - info.srWindow.Left) as usize)
    }
}

#[cfg(not(any(target_os = "linux", target_os = "macos", windows)))]
fn stdout_columns() -> Option<usize> {
    None
}

#[cfg(not(any(target_os = "linux", target_os = "macos", windows)))]
fn stderr_columns() -> Option<usize> {
    None
}

fn cjk_default() -> bool {
    true
}
