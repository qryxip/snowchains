use crate::config;

use strum_macros::EnumString;
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

use std::io::{self, BufRead as _, BufWriter, Read, Stderr, Stdin, StdinLock, StdoutLock, Write};
use std::{env, process};

pub trait WriteSpaces {
    fn write_spaces(&mut self, n: usize) -> io::Result<()>;
}

impl<W: Write> WriteSpaces for W {
    #[inline]
    fn write_spaces(&mut self, n: usize) -> io::Result<()> {
        (0..n).try_for_each(|_| self.write_all(b" "))
    }
}

pub(crate) trait HasTerm {
    type Term: Term;

    fn term(&mut self) -> &mut Self::Term;

    #[inline]
    fn stdout(&mut self) -> &mut <Self::Term as Term>::Stdout {
        self.term().stdout()
    }

    #[inline]
    fn stderr(&mut self) -> &mut <Self::Term as Term>::Stderr {
        self.term().stderr()
    }

    #[inline]
    fn ask_yes_or_no(&mut self, mes: &str, default: bool) -> io::Result<bool> {
        self.term().ask_yes_or_no(mes, default)
    }

    #[inline]
    fn prompt_reply_stderr(&mut self, prompt: &str) -> io::Result<String> {
        self.term().prompt_reply_stderr(prompt)
    }

    #[inline]
    fn prompt_password_stderr(&mut self, prompt: &str) -> io::Result<String> {
        self.term().prompt_password_stderr(prompt)
    }
}

pub trait Term {
    type Stdin: TermIn;
    type Stdout: TermOut;
    type Stderr: TermOut;

    fn split_mut(&mut self) -> (&mut Self::Stdin, &mut Self::Stdout, &mut Self::Stderr);
    fn ask_yes_or_no(&mut self, mes: &str, default: bool) -> io::Result<bool>;
    fn prompt_reply_stderr(&mut self, prompt: &str) -> io::Result<String>;
    fn prompt_password_stderr(&mut self, prompt: &str) -> io::Result<String>;

    #[inline]
    fn stdout(&mut self) -> &mut Self::Stdout {
        self.split_mut().1
    }

    #[inline]
    fn stderr(&mut self) -> &mut Self::Stderr {
        self.split_mut().2
    }

    fn attempt_enable_ansi(&mut self, choice: AnsiColorChoice) {
        self.stdout().attempt_enable_ansi(choice);
        self.stderr().attempt_enable_ansi(choice);
    }

    fn apply_conf(&mut self, conf: &config::Console) {
        self.stdout().apply_conf(conf);
        self.stderr().apply_conf(conf);
    }
}

impl<'a, T: Term + ?Sized> Term for &'a mut T {
    type Stdin = T::Stdin;
    type Stdout = T::Stdout;
    type Stderr = T::Stderr;

    #[inline]
    fn split_mut(&mut self) -> (&mut T::Stdin, &mut T::Stdout, &mut T::Stderr) {
        (**self).split_mut()
    }

    fn ask_yes_or_no(&mut self, mes: &str, default: bool) -> io::Result<bool> {
        (**self).ask_yes_or_no(mes, default)
    }

    fn prompt_reply_stderr(&mut self, prompt: &str) -> io::Result<String> {
        (**self).prompt_reply_stderr(prompt)
    }

    fn prompt_password_stderr(&mut self, prompt: &str) -> io::Result<String> {
        (**self).prompt_password_stderr(prompt)
    }
}

pub trait TermIn: Read {
    /// Reads a line from `self` removing the trailing "\n" or "\r\n".
    ///
    /// # Errors
    ///
    /// - Encounters an I/O error
    /// - The string does not end with "\n" or "\r\n" (same as `rprompt`)
    fn read_reply(&mut self) -> io::Result<String>;
    /// Reads a line from `self` trimming a trailing "\n" or "\r\n".
    ///
    /// # Errors
    ///
    /// - Encounters an I/O error
    fn read_password(&mut self) -> io::Result<String>;
}

impl<'a> TermIn for &'a [u8] {
    fn read_reply(&mut self) -> io::Result<String> {
        let mut ret = "".to_owned();
        let _ = self.read_line(&mut ret)?;
        check_remove_trailing_newilne(ret)
    }

    fn read_password(&mut self) -> io::Result<String> {
        let mut ret = "".to_owned();
        let _ = self.read_line(&mut ret)?;
        Ok(remove_trailing_newilne(ret))
    }
}

impl TermIn for io::Empty {
    fn read_reply(&mut self) -> io::Result<String> {
        check_remove_trailing_newilne("".to_owned())
    }

    fn read_password(&mut self) -> io::Result<String> {
        Ok("".to_owned())
    }
}

pub trait TermOut: WriteAnsi {
    fn process_redirection() -> process::Stdio;
    fn columns(&self) -> Option<usize>;
    fn str_width_fn(&self) -> fn(&str) -> usize;
    fn char_width_fn(&self) -> fn(char) -> Option<usize>;
    fn attempt_enable_ansi(&mut self, choice: AnsiColorChoice);
    fn apply_conf(&mut self, conf: &config::Console);

    #[inline]
    fn str_width(&self, s: &str) -> usize {
        self.str_width_fn()(s)
    }

    #[inline]
    fn char_width_or_zero(&self, c: char) -> usize {
        self.char_width_fn()(c).unwrap_or(0)
    }
}

impl<'a, W: TermOut + ?Sized> TermOut for &'a mut W {
    fn process_redirection() -> process::Stdio {
        W::process_redirection()
    }

    fn columns(&self) -> Option<usize> {
        (**self).columns()
    }

    #[inline]
    fn str_width_fn(&self) -> fn(&str) -> usize {
        (**self).str_width_fn()
    }

    #[inline]
    fn char_width_fn(&self) -> fn(char) -> Option<usize> {
        (**self).char_width_fn()
    }

    fn attempt_enable_ansi(&mut self, choice: AnsiColorChoice) {
        (**self).attempt_enable_ansi(choice);
    }

    fn apply_conf(&mut self, conf: &config::Console) {
        (**self).apply_conf(conf)
    }
}

pub trait StandardOutput: Write {
    fn process_redirection() -> process::Stdio;

    fn is_tty() -> bool;

    #[cfg(not(windows))]
    fn columns() -> Option<usize>;

    #[cfg(windows)]
    #[cfg_attr(tarpaulin, skip)]
    fn columns() -> Option<usize> {
        let handle = Self::windows_handle_ref()?;
        let info = winapi_util::console::screen_buffer_info(handle).ok()?;
        match info.size() {
            (w, _) if w >= 0 => Some(w as usize),
            _ => None,
        }
    }

    #[cfg(windows)]
    fn windows_handle_ref() -> Option<winapi_util::HandleRef>;
}

impl<W: StandardOutput> StandardOutput for BufWriter<W> {
    fn process_redirection() -> process::Stdio {
        W::process_redirection()
    }

    fn is_tty() -> bool {
        W::is_tty()
    }

    #[cfg(not(windows))]
    fn columns() -> Option<usize> {
        W::columns()
    }

    #[cfg(windows)]
    #[cfg_attr(tarpaulin, skip)]
    fn windows_handle_ref() -> Option<winapi_util::HandleRef> {
        W::windows_handle_ref()
    }
}

impl StandardOutput for StdoutLock<'_> {
    fn process_redirection() -> process::Stdio {
        process::Stdio::inherit()
    }

    fn is_tty() -> bool {
        atty::is(atty::Stream::Stdout)
    }

    #[cfg(not(windows))]
    fn columns() -> Option<usize> {
        term_size::dimensions_stdout().map(|(c, _)| c)
    }

    #[cfg(windows)]
    #[cfg_attr(tarpaulin, skip)]
    fn windows_handle_ref() -> Option<winapi_util::HandleRef> {
        Some(winapi_util::HandleRef::stdout())
    }
}

impl StandardOutput for Stderr {
    fn process_redirection() -> process::Stdio {
        process::Stdio::inherit()
    }

    fn is_tty() -> bool {
        atty::is(atty::Stream::Stdout)
    }

    #[cfg(not(windows))]
    fn columns() -> Option<usize> {
        term_size::dimensions_stdout().map(|(c, _)| c)
    }

    #[cfg(windows)]
    #[cfg_attr(tarpaulin, skip)]
    fn windows_handle_ref() -> Option<winapi_util::HandleRef> {
        Some(winapi_util::HandleRef::stdout())
    }
}

impl StandardOutput for Vec<u8> {
    fn process_redirection() -> process::Stdio {
        process::Stdio::null()
    }

    fn is_tty() -> bool {
        false
    }

    #[cfg(not(windows))]
    fn columns() -> Option<usize> {
        None
    }

    #[cfg(windows)]
    #[cfg_attr(tarpaulin, skip)]
    fn windows_handle_ref() -> Option<winapi_util::HandleRef> {
        None
    }
}

impl StandardOutput for io::Sink {
    fn process_redirection() -> process::Stdio {
        process::Stdio::null()
    }

    fn is_tty() -> bool {
        false
    }

    #[cfg(not(windows))]
    fn columns() -> Option<usize> {
        None
    }

    #[cfg(windows)]
    #[cfg_attr(tarpaulin, skip)]
    fn windows_handle_ref() -> Option<winapi_util::HandleRef> {
        None
    }
}

pub trait WriteAnsi: Write + Sized {
    fn supports_color(&self) -> bool;

    #[inline]
    fn with_reset(&mut self, f: impl FnOnce(&mut Self) -> io::Result<()>) -> io::Result<()> {
        f(self.by_ref())?;
        if self.supports_color() {
            self.write_all(b"\x1b[0m")?;
        }
        Ok(())
    }

    #[inline]
    fn fg(&mut self, fg: u8) -> io::Result<&mut Self> {
        write_color(self.by_ref(), fg, true).map(|()| self)
    }

    #[inline]
    fn bg(&mut self, bg: u8) -> io::Result<&mut Self> {
        write_color(self.by_ref(), bg, false).map(|()| self)
    }

    #[inline]
    fn bold(&mut self) -> io::Result<&mut Self> {
        if self.supports_color() {
            self.write_all(b"\x1b[1m")?;
        }
        Ok(self)
    }

    #[inline]
    fn underline(&mut self) -> io::Result<&mut Self> {
        if self.supports_color() {
            self.write_all(b"\x1b[4m")?;
        }
        Ok(self)
    }

    #[inline]
    fn write_str(&mut self, s: impl AsRef<str>) -> io::Result<()> {
        self.write_all(s.as_ref().as_bytes())
    }
}

#[inline]
fn write_color<W: WriteAnsi>(mut wtr: W, code: u8, fg: bool) -> io::Result<()> {
    if wtr.supports_color() {
        let mut buf: [_; 11] = *b"\x1b[48;5;mmmm";
        if fg {
            buf[2] = b'3';
        }
        let l;
        match (code / 100, (code / 10) % 10, code % 10) {
            (0, 0, c3) => {
                buf[7] = c3 + b'0';
                l = 9;
            }
            (0, c2, c3) => {
                buf[7] = c2 + b'0';
                buf[8] = c3 + b'0';
                l = 10;
            }
            (c1, c2, c3) => {
                buf[7] = c1 + b'0';
                buf[8] = c2 + b'0';
                buf[9] = c3 + b'0';
                l = 11;
            }
        }
        wtr.write_all(&buf[0..l])?;
    }
    Ok(())
}

impl<'a, W: WriteAnsi> WriteAnsi for &'a mut W {
    #[inline]
    fn supports_color(&self) -> bool {
        (**self).supports_color()
    }
}

pub struct TermImpl<I: TermIn, O: StandardOutput, E: StandardOutput> {
    stdin: I,
    stdout: TermOutImpl<O>,
    stderr: TermOutImpl<E>,
}

impl<I: TermIn, O: StandardOutput, E: StandardOutput> TermImpl<I, O, E> {
    pub fn new(stdin: I, stdout: O, stderr: E) -> Self {
        Self {
            stdin,
            stdout: TermOutImpl::new(stdout),
            stderr: TermOutImpl::new(stderr),
        }
    }
}

impl TermImpl<io::Empty, io::Sink, io::Sink> {
    pub fn null() -> Self {
        Self {
            stdin: io::empty(),
            stdout: TermOutImpl::new(io::sink()),
            stderr: TermOutImpl::new(io::sink()),
        }
    }
}

impl<I: TermIn, O: StandardOutput, E: StandardOutput> Term for TermImpl<I, O, E> {
    type Stdin = I;
    type Stdout = TermOutImpl<O>;
    type Stderr = TermOutImpl<E>;

    #[inline]
    fn split_mut(&mut self) -> (&mut I, &mut TermOutImpl<O>, &mut TermOutImpl<E>) {
        (&mut self.stdin, &mut self.stdout, &mut self.stderr)
    }

    fn ask_yes_or_no(&mut self, mes: &str, default: bool) -> io::Result<bool> {
        let prompt = format!("{}{} ", mes, if default { "(Y/n)" } else { "(y/N)" });
        loop {
            match &self.prompt_reply_stderr(&prompt)? {
                s if s.is_empty() => break Ok(default),
                s if s.eq_ignore_ascii_case("y") || s.eq_ignore_ascii_case("yes") => break Ok(true),
                s if s.eq_ignore_ascii_case("n") || s.eq_ignore_ascii_case("no") => break Ok(false),
                _ => self.stderr().with_reset(|o| {
                    o.fg(11)?
                        .write_str("Answer \"y\", \"yes\", \"n\", \"no\", or \"\".")
                })?,
            }
        }
    }

    fn prompt_reply_stderr(&mut self, prompt: &str) -> io::Result<String> {
        self.stderr.write_all(prompt.as_bytes())?;
        self.stderr.flush()?;
        self.stdin.read_reply()
    }

    fn prompt_password_stderr(&mut self, prompt: &str) -> io::Result<String> {
        self.stderr.write_all(prompt.as_bytes())?;
        self.stderr.flush()?;
        self.stdin.read_password().or_else(|_| {
            self.stderr.write_all(prompt.as_bytes())?;
            self.stderr.flush()?;
            self.stdin.read_reply()
        })
    }
}

pub enum TermInImpl<'a> {
    Tty,
    Piped(StdinLock<'a>),
}

impl<'a> Read for TermInImpl<'a> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        match self {
            TermInImpl::Tty => io::stdin().read(buf),
            TermInImpl::Piped(stdin) => stdin.read(buf),
        }
    }
}

impl<'a> TermIn for TermInImpl<'a> {
    fn read_reply(&mut self) -> io::Result<String> {
        let mut ret = "".to_owned();
        let _ = match self {
            TermInImpl::Tty => io::stdin().read_line(&mut ret),
            TermInImpl::Piped(stdin) => stdin.read_line(&mut ret),
        }?;
        check_remove_trailing_newilne(ret)
    }

    fn read_password(&mut self) -> io::Result<String> {
        match self {
            TermInImpl::Tty => rpassword::read_password_from_tty(None),
            TermInImpl::Piped(stdin) => {
                let mut ret = "".to_owned();
                let _ = stdin.read_line(&mut ret)?;
                Ok(remove_trailing_newilne(ret))
            }
        }
    }
}

impl<'a> TermInImpl<'a> {
    /// Creates a new `TermInImpl`.
    ///
    /// Returns `Tty` if the stdin is a TTY, otherwise `Piped`.
    pub fn new(stdin: &'a Stdin) -> Self {
        if atty::is(atty::Stream::Stdin) && !(cfg!(windows) && env::var_os("MSYSTEM").is_some()) {
            TermInImpl::Tty
        } else {
            TermInImpl::Piped(stdin.lock())
        }
    }
}

pub struct TermOutImpl<W: StandardOutput> {
    wtr: W,
    supports_color: bool,
    str_width_fn: fn(&str) -> usize,
    char_width_fn: fn(char) -> Option<usize>,
    alt_columns: Option<usize>,
}

impl<W: StandardOutput> TermOutImpl<W> {
    fn new(wtr: W) -> Self {
        Self {
            wtr,
            supports_color: false,
            str_width_fn: <str as UnicodeWidthStr>::width,
            char_width_fn: <char as UnicodeWidthChar>::width,
            alt_columns: None,
        }
    }

    pub fn get_ref(&self) -> &W {
        &self.wtr
    }
}

impl<W: StandardOutput> Write for TermOutImpl<W> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.wtr.write(buf)
    }

    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        self.wtr.flush()
    }
}

impl<W: StandardOutput> WriteAnsi for TermOutImpl<W> {
    #[inline]
    fn supports_color(&self) -> bool {
        self.supports_color
    }
}

impl<W: StandardOutput> TermOut for TermOutImpl<W> {
    fn process_redirection() -> process::Stdio {
        W::process_redirection()
    }

    fn columns(&self) -> Option<usize> {
        W::columns().or(self.alt_columns)
    }

    #[inline]
    fn str_width_fn(&self) -> fn(&str) -> usize {
        self.str_width_fn
    }

    #[inline]
    fn char_width_fn(&self) -> fn(char) -> Option<usize> {
        self.char_width_fn
    }

    #[cfg(not(windows))]
    fn attempt_enable_ansi(&mut self, choice: AnsiColorChoice) {
        let p = match choice {
            AnsiColorChoice::Never => false,
            AnsiColorChoice::Always => true,
            AnsiColorChoice::Auto => {
                W::is_tty() && env::var_os("TERM").map_or(false, |v| v != "dumb")
            }
        };
        if p {
            self.supports_color = true;
        }
    }

    #[cfg(windows)]
    #[cfg_attr(tarpaulin, skip)]
    fn attempt_enable_ansi(&mut self, choice: AnsiColorChoice) {
        fn virtual_terminal_processing_enabled(handle: &winapi_util::HandleRef) -> bool {
            use winapi::um::wincon::ENABLE_VIRTUAL_TERMINAL_PROCESSING;
            winapi_util::console::mode(handle)
                .ok()
                .map_or(false, |mode| mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING != 0)
        }

        enum EnvKind {
            DumbOrCygwin,
            Msys,
            PossiblyWinConsole,
        }

        self.supports_color = match choice {
            AnsiColorChoice::Never => false,
            AnsiColorChoice::Always => true,
            AnsiColorChoice::Auto => {
                let term = env::var("TERM");
                let term = term.as_ref().map(String::as_str);
                let kind = if term == Ok("dumb") || term == Ok("cygwin") {
                    EnvKind::DumbOrCygwin
                } else if env::var_os("MSYSTEM").is_some() && term.is_ok() {
                    EnvKind::Msys
                } else {
                    EnvKind::PossiblyWinConsole
                };
                match kind {
                    EnvKind::DumbOrCygwin => false,
                    EnvKind::Msys => W::is_tty(),
                    EnvKind::PossiblyWinConsole => {
                        W::is_tty()
                            && W::windows_handle_ref()
                                .map_or(false, |h| virtual_terminal_processing_enabled(&h))
                    }
                }
            }
        };
    }

    fn apply_conf(&mut self, conf: &config::Console) {
        if conf.cjk {
            self.str_width_fn = <str as UnicodeWidthStr>::width_cjk;
            self.char_width_fn = <char as UnicodeWidthChar>::width_cjk;
        } else {
            self.str_width_fn = <str as UnicodeWidthStr>::width;
            self.char_width_fn = <char as UnicodeWidthChar>::width;
        }
        self.alt_columns = conf.alt_width;
    }
}

#[cfg(test)]
pub(crate) struct Ansi<W: Write>(W);

#[cfg(test)]
impl<W: Write> Ansi<W> {
    pub(crate) fn new(wtr: W) -> Self {
        Ansi(wtr)
    }

    pub(crate) fn get_ref(&self) -> &W {
        &self.0
    }
}

#[cfg(test)]
impl<W: Write> Write for Ansi<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}

#[cfg(test)]
impl<W: Write> WriteAnsi for Ansi<W> {
    fn supports_color(&self) -> bool {
        true
    }
}

#[cfg(test)]
impl<W: Write> TermOut for Ansi<W> {
    fn process_redirection() -> process::Stdio {
        process::Stdio::null()
    }

    fn columns(&self) -> Option<usize> {
        None
    }

    fn str_width_fn(&self) -> fn(&str) -> usize {
        UnicodeWidthStr::width
    }

    fn char_width_fn(&self) -> fn(char) -> Option<usize> {
        UnicodeWidthChar::width
    }

    fn attempt_enable_ansi(&mut self, _: AnsiColorChoice) {}

    fn apply_conf(&mut self, _: &config::Console) {}
}

#[derive(Clone, Copy, Debug, strum_macros::Display, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum AnsiColorChoice {
    Never,
    Auto,
    Always,
}

fn remove_trailing_newilne(mut s: String) -> String {
    if s.ends_with("\r\n") {
        s.pop();
        s.pop();
    } else if s.ends_with('\n') {
        s.pop();
    }
    s
}

fn check_remove_trailing_newilne(mut s: String) -> io::Result<String> {
    if s.ends_with("\r\n") {
        s.pop();
        s.pop();
        Ok(s)
    } else if s.ends_with('\n') {
        s.pop();
        Ok(s)
    } else {
        Err(io::Error::new(
            io::ErrorKind::UnexpectedEof,
            "input must ends with a newline",
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::terminal::{
        Ansi, AnsiColorChoice, Term as _, TermImpl, TermOut as _, WriteAnsi as _, WriteSpaces as _,
    };

    use failure::Fallible;

    use std::borrow::Borrow as _;
    use std::{io, str};

    #[test]
    fn test_write_spaces() -> Fallible<()> {
        let mut wtr = Vec::<u8>::new();
        wtr.write_spaces(0)?;
        assert_eq!(str::from_utf8(&wtr)?, "");
        wtr.write_spaces(10)?;
        assert_eq!(str::from_utf8(&wtr)?, " ".repeat(10));
        Ok(())
    }

    #[test]
    fn test_write_ansi() -> Fallible<()> {
        let mut wtr = Ansi::new(Vec::<u8>::new());
        wtr.with_reset(|w| w.fg(4)?.write_str("foo"))?;
        wtr.with_reset(|w| w.fg(14)?.write_str("bar"))?;
        wtr.with_reset(|w| w.bg(195)?.write_str("baz"))?;
        wtr.with_reset(|w| w.bold()?.write_str("qux"))?;
        wtr.with_reset(|w| w.underline()?.write_str("quux"))?;
        assert_eq!(
            str::from_utf8(wtr.get_ref())?,
            "\x1b[38;5;4mfoo\x1b[0m\x1b[38;5;14mbar\x1b[0m\x1b[48;5;195mbaz\x1b[0m\x1b[1mqux\x1b[0m\
             \x1b[4mquux\x1b[0m",
        );
        Ok(())
    }

    #[test]
    fn test_ask_yes_or_no() -> Fallible<()> {
        let mut term = TermImpl::new("y\nn\n\ny\nn\nヌ\n\n".as_bytes(), vec![], vec![]);
        term.stderr.attempt_enable_ansi(AnsiColorChoice::Always);

        assert_eq!(term.ask_yes_or_no("Yes?: ", true)?, true);
        assert_eq!(term.ask_yes_or_no("Yes?: ", true)?, false);
        assert_eq!(term.ask_yes_or_no("Yes?: ", true)?, true);
        assert_eq!(term.ask_yes_or_no("No?: ", false)?, true);
        assert_eq!(term.ask_yes_or_no("No?: ", false)?, false);
        assert_eq!(term.ask_yes_or_no("No?: ", false)?, false);
        let err = term.ask_yes_or_no("Yes?: ", true).unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::UnexpectedEof);

        assert!(term.stdout.wtr.is_empty());
        assert_eq!(
            str::from_utf8(&term.stderr.wtr)?,
            "Yes?: (Y/n) Yes?: (Y/n) Yes?: (Y/n) No?: (y/N) No?: (y/N) No?: (y/N) \
             \x1b[38;5;11mAnswer \"y\", \"yes\", \"n\", \"no\", or \"\".\x1b[0m\
             No?: (y/N) Yes?: (Y/n) ",
        );
        Ok(())
    }

    #[test]
    fn test_prompt_reply_password_stderr() -> Fallible<()> {
        type Method =
            fn(&mut TermImpl<&'static [u8], Vec<u8>, Vec<u8>>, prompt: &str) -> io::Result<String>;
        static METHODS: &[Method] = &[
            TermImpl::prompt_reply_stderr,
            TermImpl::prompt_password_stderr,
        ];
        for method in METHODS {
            let mut term = TermImpl::new(b"foo\nbar\n".borrow(), vec![], vec![]);
            assert_eq!(method(&mut term, "Prompt: ")?, "foo");
            assert_eq!(method(&mut term, "Prompt: ")?, "bar");
            assert!(term.stdout.wtr.is_empty());
            assert_eq!(str::from_utf8(&term.stderr.wtr)?, "Prompt: Prompt: ");
        }

        for &input in &["", "foo"] {
            let mut term = TermImpl::new(input.as_bytes(), vec![], vec![]);
            assert_eq!(term.prompt_password_stderr("Prompt: ")?, input);
            assert!(term.stdout.wtr.is_empty());
            assert_eq!(str::from_utf8(&term.stderr.wtr)?, "Prompt: ");

            let mut term = TermImpl::new(input.as_bytes(), vec![], vec![]);
            let err = term.prompt_reply_stderr("Prompt: ").unwrap_err();
            assert_eq!(err.kind(), io::ErrorKind::UnexpectedEof);
        }
        Ok(())
    }
}
