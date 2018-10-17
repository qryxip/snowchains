use {config, Never};

use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};
use {atty, rpassword};

#[cfg(any(target_os = "linux", target_os = "macos"))]
use libc;

#[cfg(windows)]
use winapi_util;

use std::io::{
    self, BufRead, BufWriter, Stderr, StderrLock, Stdin, StdinLock, Stdout, StdoutLock, Write,
};
use std::str::FromStr;
use std::{self, env, fmt, process};

#[cfg(any(target_os = "linux", target_os = "macos"))]
use std::mem;

pub trait WriteSpaces {
    fn write_spaces(&mut self, n: usize) -> io::Result<()>;
}

impl<W: Write> WriteSpaces for W {
    #[inline]
    fn write_spaces(&mut self, n: usize) -> io::Result<()> {
        (0..n).try_for_each(|_| self.write_all(b" "))
    }
}

pub trait Term {
    type Stdin: BufRead;
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

    fn setup(&mut self, choice: AnsiColorChoice, conf: &config::Console) {
        self.stdout().attempt_enable_ansi(choice);
        self.stderr().attempt_enable_ansi(choice);
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

pub trait TermOut: WriteAnsi {
    fn process_redirection() -> process::Stdio;
    fn columns() -> Option<usize>;
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

    fn setup(&mut self, choice: AnsiColorChoice, conf: &config::Console) {
        self.attempt_enable_ansi(choice);
        self.apply_conf(conf);
    }
}

impl<'a, W: TermOut + ?Sized> TermOut for &'a mut W {
    fn process_redirection() -> process::Stdio {
        W::process_redirection()
    }

    fn columns() -> Option<usize> {
        W::columns()
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
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    unsafe fn libc_fileno() -> Option<libc::c_int>;
    #[cfg(windows)]
    fn windows_handle_ref() -> Option<winapi_util::HandleRef>;

    #[cfg(not(any(windows, target_os = "linux", target_os = "macos")))]
    fn columns() -> Option<usize> {
        None
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    fn columns() -> Option<usize> {
        unsafe {
            let fd = Self::libc_fileno()?;
            let mut winsize = mem::zeroed::<libc::winsize>();
            if libc::ioctl(fd, libc::TIOCGWINSZ, &mut winsize) != 0 || winsize.ws_col == 0 {
                None
            } else {
                Some(winsize.ws_col as usize)
            }
        }
    }

    #[cfg(windows)]
    fn columns() -> Option<usize> {
        let info = winapi_util::console::screen_buffer_info(Self::windows_handle_ref()?).ok()?;
        let (columns, _) = info.size();
        if columns > 0 {
            Some(columns as usize)
        } else {
            None
        }
    }
}

impl<'a> StandardOutput for BufWriter<StdoutLock<'a>> {
    fn process_redirection() -> process::Stdio {
        process::Stdio::inherit()
    }

    fn is_tty() -> bool {
        atty::is(atty::Stream::Stdout)
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    unsafe fn libc_fileno() -> Option<libc::c_int> {
        Some(libc::STDOUT_FILENO)
    }

    #[cfg(windows)]
    fn windows_handle_ref() -> Option<winapi_util::HandleRef> {
        Some(winapi_util::HandleRef::stdout())
    }
}

impl<'a> StandardOutput for BufWriter<StderrLock<'a>> {
    fn process_redirection() -> process::Stdio {
        process::Stdio::inherit()
    }

    fn is_tty() -> bool {
        atty::is(atty::Stream::Stderr)
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    unsafe fn libc_fileno() -> Option<libc::c_int> {
        Some(libc::STDERR_FILENO)
    }

    #[cfg(windows)]
    fn windows_handle_ref() -> Option<winapi_util::HandleRef> {
        Some(winapi_util::HandleRef::stderr())
    }
}

impl StandardOutput for io::Sink {
    fn process_redirection() -> process::Stdio {
        process::Stdio::null()
    }

    fn is_tty() -> bool {
        false
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    unsafe fn libc_fileno() -> Option<libc::c_int> {
        None
    }

    #[cfg(windows)]
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

pub struct TermImpl<I: BufRead, O: StandardOutput, E: StandardOutput> {
    stdin: I,
    stdout: TermOutImpl<O>,
    stderr: TermOutImpl<E>,
}

impl<'a> TermImpl<StdinLock<'a>, BufWriter<StdoutLock<'a>>, BufWriter<StderrLock<'a>>> {
    pub fn new(stdin: &'a Stdin, stdout: &'a Stdout, stderr: &'a Stderr) -> Self {
        Self {
            stdin: stdin.lock(),
            stdout: TermOutImpl::new(BufWriter::new(stdout.lock())),
            stderr: TermOutImpl::new(BufWriter::new(stderr.lock())),
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

impl<I: BufRead, O: StandardOutput, E: StandardOutput> Term for TermImpl<I, O, E> {
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

    fn prompt_password_stderr(&mut self, prompt: &str) -> io::Result<String> {
        if cfg!(windows) && env::var_os("MSYSTEM").is_some() {
            self.stderr.with_reset(|o| {
                o.fg(11)?
                    .write_str("$MSYSTEM is present. The input won't be hidden.\n")
            })?;
            self.prompt_reply_stderr(prompt)
        } else {
            self.stderr.write_all(prompt.as_bytes())?;
            self.stderr.flush()?;
            rpassword::read_password_with_reader(Some(&mut self.stdin))
        }
    }
}

pub struct TermOutImpl<W: StandardOutput> {
    wtr: W,
    supports_color: bool,
    str_width_fn: fn(&str) -> usize,
    char_width_fn: fn(char) -> Option<usize>,
}

impl<W: StandardOutput> TermOutImpl<W> {
    fn new(wtr: W) -> Self {
        Self {
            wtr,
            supports_color: false,
            str_width_fn: <str as UnicodeWidthStr>::width,
            char_width_fn: <char as UnicodeWidthChar>::width,
        }
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

    fn columns() -> Option<usize> {
        W::columns()
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
    fn attempt_enable_ansi(&mut self, choice: AnsiColorChoice) {
        fn enable_virtual_terminal_processing(handle: &winapi_util::HandleRef) -> io::Result<()> {
            use winapi::um::wincon::ENABLE_VIRTUAL_TERMINAL_PROCESSING;

            let current = winapi_util::console::mode(handle)?;
            let new = current | ENABLE_VIRTUAL_TERMINAL_PROCESSING;
            if current != new {
                winapi_util::console::set_mode(handle, new)?;
            }
            Ok(())
        }

        enum EnvKind {
            DumbOrCygwin,
            Msys,
            PossiblyWinConsole,
        }

        let term = env::var("TERM");
        let term = term.as_ref().map(String::as_str);
        let kind = if term == Ok("dumb") || term == Ok("cygwin") {
            EnvKind::DumbOrCygwin
        } else if env::var_os("MSYSTEM").is_some() && term.is_ok() {
            EnvKind::Msys
        } else {
            EnvKind::PossiblyWinConsole
        };
        let p = match choice {
            AnsiColorChoice::Never => false,
            AnsiColorChoice::Always => {
                if let EnvKind::PossiblyWinConsole = kind {
                    if let Some(h) = W::windows_handle_ref() {
                        let _ = enable_virtual_terminal_processing(&h);
                    }
                }
                true
            }
            AnsiColorChoice::Auto => {
                W::is_tty() && match kind {
                    EnvKind::DumbOrCygwin => false,
                    EnvKind::Msys => true,
                    EnvKind::PossiblyWinConsole => W::windows_handle_ref()
                        .map_or(false, |h| enable_virtual_terminal_processing(&h).is_ok()),
                }
            }
        };
        if p {
            self.supports_color = true;
        }
    }

    fn apply_conf(&mut self, conf: &config::Console) {
        if conf.cjk {
            self.str_width_fn = <str as UnicodeWidthStr>::width_cjk;
            self.char_width_fn = <char as UnicodeWidthChar>::width_cjk;
        } else {
            self.str_width_fn = <str as UnicodeWidthStr>::width;
            self.char_width_fn = <char as UnicodeWidthChar>::width;
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum AnsiColorChoice {
    Never,
    Auto,
    Always,
}

impl FromStr for AnsiColorChoice {
    type Err = Never;

    fn from_str(s: &str) -> std::result::Result<Self, Never> {
        match s {
            "never" => Ok(AnsiColorChoice::Never),
            "auto" => Ok(AnsiColorChoice::Auto),
            "always" => Ok(AnsiColorChoice::Always),
            _ => Err(Never),
        }
    }
}

impl fmt::Display for AnsiColorChoice {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AnsiColorChoice::Never => write!(f, "never"),
            AnsiColorChoice::Auto => write!(f, "auto"),
            AnsiColorChoice::Always => write!(f, "always"),
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::WriteAnsi;

    use std::io::{self, Write};

    pub(crate) struct Ansi<W>(W);

    impl<W: Write> Ansi<W> {
        pub(crate) fn new(wtr: W) -> Self {
            Ansi(wtr)
        }

        pub(crate) fn get_ref(&self) -> &W {
            &self.0
        }
    }

    impl<W: Write> Write for Ansi<W> {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            self.0.write(buf)
        }

        fn flush(&mut self) -> io::Result<()> {
            self.0.flush()
        }
    }

    impl<W: Write> WriteAnsi for Ansi<W> {
        fn supports_color(&self) -> bool {
            true
        }
    }
}
