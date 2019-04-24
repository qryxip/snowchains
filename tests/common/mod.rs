pub(crate) mod service;

use snowchains::terminal::{
    AnsiColorChoice, AttemptEnableColor, HasTermProps, ModifyTermProps, TermProps,
};

use termcolor::{ColorSpec, WriteColor};
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

use std::convert::TryFrom;
use std::io::{self, Sink, Write};
use std::process;
use std::string::FromUtf8Error;

pub(crate) struct Dumb {
    buf: Vec<u8>,
    props: TermProps<Sink>,
}

impl Dumb {
    pub(crate) fn new() -> Self {
        Self::default()
    }
}

impl Default for Dumb {
    fn default() -> Self {
        Self {
            buf: vec![],
            props: TermProps {
                ansi_async_wtr: io::sink,
                process_redirection: process::Stdio::null,
                columns: || None,
                char_width: UnicodeWidthChar::width,
                str_width: UnicodeWidthStr::width,
            },
        }
    }
}

impl AsRef<[u8]> for Dumb {
    fn as_ref(&self) -> &[u8] {
        &self.buf
    }
}

impl TryFrom<Dumb> for String {
    type Error = FromUtf8Error;

    fn try_from(dumb: Dumb) -> Result<Self, FromUtf8Error> {
        String::from_utf8(dumb.buf)
    }
}

impl Write for Dumb {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.buf.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl WriteColor for Dumb {
    fn supports_color(&self) -> bool {
        false
    }

    fn set_color(&mut self, _: &ColorSpec) -> io::Result<()> {
        Ok(())
    }

    fn reset(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl AttemptEnableColor for Dumb {
    fn attempt_enable_color(&mut self, _: AnsiColorChoice) {}
}

impl HasTermProps for Dumb {
    type AnsiAsyncWrite = Sink;

    fn term_props(&self) -> &TermProps<Sink> {
        &self.props
    }
}

impl ModifyTermProps for Dumb {
    fn modify_term_props(&mut self, f: impl FnOnce(&mut TermProps<Sink>)) {
        f(&mut self.props);
    }
}
