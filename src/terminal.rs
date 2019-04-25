use either::Either;
use serde_derive::Serialize;
use strum_macros::EnumString;
use termcolor::{Ansi, Color, ColorSpec, WriteColor};
use tokio::io::AsyncWrite;
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

use std::io::{self, BufRead, BufWriter, Stderr, Stdin, StdinLock, Stdout, Write};
use std::{env, fmt, process};

pub trait Input {
    fn read_reply(&mut self) -> io::Result<String>;
    fn read_password(&mut self) -> io::Result<String>;
}

impl<'a, I: Input + ?Sized> Input for &'a mut I {
    fn read_reply(&mut self) -> io::Result<String> {
        (**self).read_reply()
    }

    fn read_password(&mut self) -> io::Result<String> {
        (**self).read_password()
    }
}

#[derive(Debug)]
pub enum TtyOrPiped<R: BufRead> {
    Tty,
    Piped(R),
}

impl<'a> TtyOrPiped<StdinLock<'a>> {
    /// Creates a new `TtyOrPiped`.
    ///
    /// Returns `Tty` if the stdin is a TTY, otherwise `Piped`.
    pub fn auto(stdin: &'a Stdin) -> Self {
        if atty::is(atty::Stream::Stdin) && !(cfg!(windows) && env::var_os("MSYSTEM").is_some()) {
            TtyOrPiped::Tty
        } else {
            TtyOrPiped::Piped(stdin.lock())
        }
    }
}

impl<R: BufRead> Input for TtyOrPiped<R> {
    fn read_reply(&mut self) -> io::Result<String> {
        let mut ret = "".to_owned();
        let _ = match self {
            TtyOrPiped::Tty => io::stdin().read_line(&mut ret),
            TtyOrPiped::Piped(rdr) => rdr.read_line(&mut ret),
        }?;

        // Returns an `Err` when the input does not end with `'\n'` as `rprompt` does.
        // <https://github.com/conradkdotcom/rprompt/blob/ffcb74304e42f31e03efa44b72163b40d2dde7c2/src/lib.rs#L24-L32>
        if ret.pop() == Some('\n') {
            if ret.ends_with('\r') {
                ret.pop();
            }
            Ok(ret)
        } else {
            Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "input must ends with a newline",
            ))
        }
    }

    fn read_password(&mut self) -> io::Result<String> {
        match self {
            TtyOrPiped::Tty => rpassword::read_password_from_tty(None),
            TtyOrPiped::Piped(rdr) => {
                let mut ret = "".to_owned();
                let _ = rdr.read_line(&mut ret)?;

                // Allows a string which does not end with `'\n'` as `rpassword` does.
                if ret.ends_with('\n') {
                    ret.pop();
                    if ret.ends_with('\r') {
                        ret.pop();
                    }
                }
                Ok(ret)
            }
        }
    }
}

#[derive(Clone, Copy, Debug, strum_macros::Display, EnumString, Serialize)]
#[strum(serialize_all = "snake_case")]
#[serde(rename_all = "snake_case")]
pub enum AnsiColorChoice {
    Never,
    Auto,
    Always,
}

pub(crate) trait WriteExt: Write {
    fn write_str(&mut self, s: impl AsRef<str>) -> io::Result<()> {
        self.write_all(s.as_ref().as_ref())
    }

    fn write_spaces(&mut self, n: usize) -> io::Result<()> {
        (0..n).try_for_each(|_| self.write_str(" "))
    }
}

impl<W: Write + ?Sized> WriteExt for W {}

pub(crate) trait WriteColorExt: WriteColor {
    fn with_reset(
        &mut self,
        f: impl FnOnce(&mut WithReset<&mut Self>) -> io::Result<()>,
    ) -> io::Result<()> {
        f(&mut WithReset {
            wtr: self,
            spec: ColorSpec::new(),
        })?;
        self.reset()
    }
}

impl<W: WriteColor + ?Sized> WriteColorExt for W {}

#[derive(Debug)]
pub(crate) struct WithReset<W: WriteColor> {
    wtr: W,
    spec: ColorSpec,
}

impl<W: WriteColor> WithReset<W> {
    pub(crate) fn fg(&mut self, color: impl IntoColor) -> &mut Self {
        self.spec.set_fg(Some(color.into_color()));
        self
    }

    pub(crate) fn bg(&mut self, color: impl IntoColor) -> &mut Self {
        self.spec.set_bg(Some(color.into_color()));
        self
    }

    pub(crate) fn bold(&mut self) -> &mut Self {
        self.spec.set_bold(true);
        self
    }

    pub(crate) fn underline(&mut self) -> &mut Self {
        self.spec.set_underline(true);
        self
    }

    pub(crate) fn set(&mut self) -> io::Result<&mut W> {
        self.wtr.set_color(&self.spec)?;
        Ok(&mut self.wtr)
    }
}

pub(crate) trait IntoColor {
    fn into_color(self) -> Color;
}

impl IntoColor for u8 {
    fn into_color(self) -> Color {
        Color::Ansi256(self)
    }
}

pub trait AttemptEnableColor: WriteColor {
    fn attempt_enable_color(&mut self, choice: AnsiColorChoice);
}

impl<'a, W: AttemptEnableColor> AttemptEnableColor for &'a mut W {
    fn attempt_enable_color(&mut self, choice: AnsiColorChoice) {
        (**self).attempt_enable_color(choice);
    }
}

pub trait HasTermProps {
    type AnsiAsyncWrite: AsyncWrite + Send + 'static;

    fn term_props(&self) -> &TermProps<Self::AnsiAsyncWrite>;

    fn ansi_async_wtr(&self) -> Self::AnsiAsyncWrite {
        (self.term_props().ansi_async_wtr)()
    }

    fn process_redirection(&self) -> process::Stdio {
        (self.term_props().process_redirection)()
    }

    fn columns(&self) -> Option<usize> {
        (self.term_props().columns)()
    }

    fn char_width_fn(&self) -> fn(char) -> Option<usize> {
        self.term_props().char_width
    }

    fn str_width_fn(&self) -> fn(&str) -> usize {
        self.term_props().str_width
    }

    fn char_width(&self, c: char) -> Option<usize> {
        self.char_width_fn()(c)
    }

    fn str_width(&self, s: &str) -> usize {
        self.str_width_fn()(s)
    }
}

impl<'a, W: HasTermProps> HasTermProps for &'a W {
    type AnsiAsyncWrite = W::AnsiAsyncWrite;

    fn term_props(&self) -> &TermProps<Self::AnsiAsyncWrite> {
        (**self).term_props()
    }
}

impl<'a, W: HasTermProps> HasTermProps for &'a mut W {
    type AnsiAsyncWrite = W::AnsiAsyncWrite;

    fn term_props(&self) -> &TermProps<Self::AnsiAsyncWrite> {
        (**self).term_props()
    }
}

pub trait ModifyTermProps: HasTermProps {
    fn modify_term_props(&mut self, f: impl FnOnce(&mut TermProps<Self::AnsiAsyncWrite>));
}

impl<'a, W: ModifyTermProps> ModifyTermProps for &'a mut W {
    fn modify_term_props(&mut self, f: impl FnOnce(&mut TermProps<Self::AnsiAsyncWrite>)) {
        (**self).modify_term_props(f)
    }
}

pub struct TermProps<W: AsyncWrite + Send + 'static> {
    pub ansi_async_wtr: fn() -> W,
    pub process_redirection: fn() -> process::Stdio,
    pub columns: fn() -> Option<usize>,
    pub char_width: fn(char) -> Option<usize>,
    pub str_width: fn(&str) -> usize,
}

impl<W: AsyncWrite + Send + 'static> fmt::Debug for TermProps<W> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str("TermProps { .. }")
    }
}

pub struct AnsiStandardStream<W: AnsiStandardOutput> {
    wtr: Either<W, Ansi<W>>,
    props: TermProps<W::AnsiAsyncWrite>,
}

impl<W: AnsiStandardOutput> AnsiStandardStream<W> {
    pub fn new(wtr: W) -> Self {
        Self {
            wtr: Either::Left(wtr),
            props: TermProps {
                ansi_async_wtr: W::ansi_async_wtr,
                process_redirection: process::Stdio::inherit,
                columns: W::columns,
                char_width: UnicodeWidthChar::width,
                str_width: UnicodeWidthStr::width,
            },
        }
    }

    fn inner_mut(&mut self) -> &mut W {
        match &mut self.wtr {
            Either::Left(w) => w,
            Either::Right(w) => w.get_mut(),
        }
    }
}

impl<W: AnsiStandardOutput + fmt::Debug> fmt::Debug for AnsiStandardStream<W> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let wtr = match &self.wtr {
            Either::Left(w) => Either::Left(w),
            Either::Right(_) => Either::Right(format_args!("_")),
        };
        fmt.debug_struct("AnsiStandardStream")
            .field("wtr", &wtr)
            .field("props", &self.props)
            .finish()
    }
}

impl<W: AnsiStandardOutput> Write for AnsiStandardStream<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.inner_mut().write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner_mut().flush()
    }
}

impl<W: AnsiStandardOutput> WriteColor for AnsiStandardStream<W> {
    fn supports_color(&self) -> bool {
        self.wtr.is_right()
    }

    fn set_color(&mut self, spec: &ColorSpec) -> io::Result<()> {
        match &mut self.wtr {
            Either::Left(_) => Ok(()),
            Either::Right(w) => w.set_color(spec),
        }
    }

    fn reset(&mut self) -> io::Result<()> {
        match &mut self.wtr {
            Either::Left(_) => Ok(()),
            Either::Right(w) => w.reset(),
        }
    }
}

impl<W: AnsiStandardOutput> AttemptEnableColor for AnsiStandardStream<W> {
    fn attempt_enable_color(&mut self, choice: AnsiColorChoice) {
        #[cfg(windows)]
        fn should_enable_color_on_auto<W: AnsiStandardOutput>() -> bool {
            use winapi::um::wincon::ENABLE_VIRTUAL_TERMINAL_PROCESSING;

            let term = env::var("TERM");
            let term = term.as_ref().map(String::as_str);
            if term == Ok("dumb") || term == Ok("cygwin") {
                false
            } else if env::var_os("MSYSTEM").is_some() && term.is_ok() {
                atty::is(W::atty_stream())
            } else {
                atty::is(W::atty_stream())
                    && winapi_util::console::mode(W::windows_handle_ref())
                        .ok()
                        .map_or(false, |m| m & ENABLE_VIRTUAL_TERMINAL_PROCESSING != 0)
            }
        }

        #[cfg(not(windows))]
        fn should_enable_color_on_auto<W: AnsiStandardOutput>() -> bool {
            atty::is(W::atty_stream()) && env::var_os("TERM").map_or(false, |v| v != "dumb")
        }

        let should_enable_color = match choice {
            AnsiColorChoice::Never => false,
            AnsiColorChoice::Always => true,
            AnsiColorChoice::Auto => should_enable_color_on_auto::<W>(),
        };

        take_mut::take(&mut self.wtr, |wtr| {
            let wtr = match wtr {
                Either::Left(w) => w,
                Either::Right(w) => w.into_inner(),
            };
            if should_enable_color {
                Either::Right(Ansi::new(wtr))
            } else {
                Either::Left(wtr)
            }
        });
    }
}

impl<W: AnsiStandardOutput> HasTermProps for AnsiStandardStream<W> {
    type AnsiAsyncWrite = W::AnsiAsyncWrite;

    fn term_props(&self) -> &TermProps<W::AnsiAsyncWrite> {
        &self.props
    }
}

impl<W: AnsiStandardOutput> ModifyTermProps for AnsiStandardStream<W> {
    fn modify_term_props(&mut self, f: impl FnOnce(&mut TermProps<W::AnsiAsyncWrite>)) {
        f(&mut self.props);
    }
}

pub trait AnsiStandardOutput: Write {
    type AnsiAsyncWrite: AsyncWrite + Send + 'static;

    fn ansi_async_wtr() -> Self::AnsiAsyncWrite;

    #[cfg(windows)]
    fn columns() -> Option<usize> {
        let handle = Self::windows_handle_ref();
        let info = winapi_util::console::screen_buffer_info(handle).ok()?;
        match info.size() {
            (w, _) if w >= 0 => Some(w as usize),
            _ => None,
        }
    }

    #[cfg(not(windows))]
    fn columns() -> Option<usize>;

    fn atty_stream() -> atty::Stream;

    #[cfg(windows)]
    fn windows_handle_ref() -> winapi_util::HandleRef;
}

impl<W: AnsiStandardOutput> AnsiStandardOutput for BufWriter<W> {
    type AnsiAsyncWrite = W::AnsiAsyncWrite;

    fn ansi_async_wtr() -> W::AnsiAsyncWrite {
        W::ansi_async_wtr()
    }

    fn columns() -> Option<usize> {
        W::columns()
    }

    fn atty_stream() -> atty::Stream {
        W::atty_stream()
    }

    #[cfg(windows)]
    fn windows_handle_ref() -> winapi_util::HandleRef {
        W::windows_handle_ref()
    }
}

impl AnsiStandardOutput for Stdout {
    type AnsiAsyncWrite = tokio::io::Stdout;

    fn ansi_async_wtr() -> tokio::io::Stdout {
        tokio::io::stdout()
    }

    #[cfg(not(windows))]
    fn columns() -> Option<usize> {
        term_size::dimensions_stdout().map(|(w, _)| w)
    }

    fn atty_stream() -> atty::Stream {
        atty::Stream::Stdout
    }

    #[cfg(windows)]
    fn windows_handle_ref() -> winapi_util::HandleRef {
        winapi_util::HandleRef::stdout()
    }
}

impl AnsiStandardOutput for Stderr {
    type AnsiAsyncWrite = tokio::io::Stderr;

    fn ansi_async_wtr() -> tokio::io::Stderr {
        tokio::io::stderr()
    }

    #[cfg(not(windows))]
    fn columns() -> Option<usize> {
        term_size::dimensions_stderr().map(|(w, _)| w)
    }

    fn atty_stream() -> atty::Stream {
        atty::Stream::Stderr
    }

    #[cfg(windows)]
    fn windows_handle_ref() -> winapi_util::HandleRef {
        winapi_util::HandleRef::stderr()
    }
}

#[cfg(test)]
mod tests {
    use crate::terminal::{Input as _, TtyOrPiped, WriteColorExt as _, WriteExt as _};

    use failure::Fallible;
    use once_cell::sync::Lazy;
    use once_cell::sync_lazy;
    use pretty_assertions::assert_eq;
    use termcolor::{Ansi, Color, ColorSpec, WriteColor as _};

    use std::io::{self, Write as _};
    use std::str;

    #[test]
    fn test_write_spaces() -> Fallible<()> {
        let mut wtr = vec![];
        wtr.write_spaces(0)?;
        assert_eq!(str::from_utf8(&wtr)?, "");
        wtr.write_spaces(10)?;
        assert_eq!(str::from_utf8(&wtr)?, " ".repeat(10));
        Ok(())
    }

    #[test]
    fn test_write_ansi() -> Fallible<()> {
        let mut actual = Ansi::new(vec![]);
        actual.with_reset(|w| w.fg(4).set().unwrap().write_str("foo"))?;
        actual.with_reset(|w| w.fg(14).set().unwrap().write_str("bar"))?;
        actual.with_reset(|w| w.bg(195).set().unwrap().write_str("baz"))?;
        actual.with_reset(|w| w.bold().set().unwrap().write_str("qux"))?;
        actual.with_reset(|w| w.underline().set().unwrap().write_str("quux"))?;

        static EXPECTED: Lazy<String> = sync_lazy! {
            let mut expected = Ansi::new(vec![]);
            expected
                .set_color(ColorSpec::new().set_fg(Some(Color::Ansi256(4))))
                .unwrap();
            expected.write_all(b"foo").unwrap();
            expected.reset().unwrap();
            expected
                .set_color(ColorSpec::new().set_fg(Some(Color::Ansi256(14))))
                .unwrap();
            expected.write_all(b"bar").unwrap();
            expected.reset().unwrap();
            expected
                .set_color(ColorSpec::new().set_bg(Some(Color::Ansi256(195))))
                .unwrap();
            expected.write_all(b"baz").unwrap();
            expected.reset().unwrap();
            expected.set_color(ColorSpec::new().set_bold(true)).unwrap();
            expected.write_all(b"qux").unwrap();
            expected.reset().unwrap();
            expected
                .set_color(ColorSpec::new().set_underline(true))
                .unwrap();
            expected.write_all(b"quux").unwrap();
            expected.reset().unwrap();
            String::from_utf8(expected.into_inner()).unwrap()
        };

        let actual = str::from_utf8(actual.get_ref())?;
        assert_eq!(actual, &*EXPECTED);
        Ok(())
    }

    #[test]
    fn test_read_reply() -> io::Result<()> {
        let mut piped = TtyOrPiped::Piped(&b"foo\nbar\nbaz"[..]);
        assert_eq!(piped.read_reply()?, "foo");
        assert_eq!(piped.read_reply()?, "bar");
        let err = piped.read_reply().unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::UnexpectedEof);
        Ok(())
    }

    #[test]
    fn test_read_password() -> io::Result<()> {
        let mut rdr = &b"foo\nbar\nbaz"[..];
        let mut piped = TtyOrPiped::Piped(&mut rdr);
        assert_eq!(piped.read_password()?, "foo");
        assert_eq!(piped.read_password()?, "bar");
        assert_eq!(piped.read_password()?, "baz");
        assert_eq!(piped.read_password()?, "");
        assert_eq!(rdr, b"");
        Ok(())
    }
}
