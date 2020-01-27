use futures01::{task, try_ready, Async, Poll};
use termcolor::{Color, ColorSpec};
use tokio01::io::AsyncWrite;

use std::fmt::{self, Write as _};
use std::{io, str};

#[derive(Debug)]
pub(crate) struct AsyncBufferedWriter<W: AsyncWrite> {
    wtr: W,
    buf: String,
    st: State,
}

impl<W: AsyncWrite> AsyncBufferedWriter<W> {
    pub(crate) fn new(wtr: W) -> Self {
        Self {
            wtr,
            buf: "".to_owned(),
            st: State::Empty,
        }
    }

    pub(crate) fn poll_flush_buf(&mut self) -> Poll<(), io::Error> {
        match self.st {
            State::Empty => {
                if self.buf.is_empty() {
                    Ok(Async::Ready(()))
                } else {
                    self.st = State::Writing(0);
                    task::current().notify();
                    Ok(Async::NotReady)
                }
            }
            State::Writing(pos) => {
                let pos = pos + try_ready!(self.wtr.poll_write(&self.buf.as_bytes()[pos..]));
                task::current().notify();
                if pos < self.buf.len() {
                    self.st = State::Writing(pos);
                    Ok(Async::NotReady)
                } else {
                    self.buf.clear();
                    self.st = State::Flush;
                    Ok(Async::NotReady)
                }
            }
            State::Flush => {
                try_ready!(self.wtr.poll_flush());
                self.st = State::Empty;
                Ok(Async::Ready(()))
            }
        }
    }

    pub(crate) fn write_fmt_to_buf(&mut self, args: fmt::Arguments) {
        self.buf.write_fmt(args).unwrap();
    }

    pub(crate) fn push_char(&mut self, c: char) {
        self.buf.push(c);
    }

    pub(crate) fn push_str(&mut self, s: &str) {
        self.buf.push_str(s);
    }

    pub(crate) fn push_ansi_reset(&mut self) {
        self.buf.push_str("\x1b[0m");
    }

    pub(crate) fn push_ansi_color(&mut self, spec: &ColorSpec) {
        fn try_to_ansi_256(color: Color, intense: bool) -> Option<u8> {
            match color {
                Color::Black if intense => Some(8),
                Color::Red if intense => Some(9),
                Color::Green if intense => Some(10),
                Color::Yellow if intense => Some(11),
                Color::Blue if intense => Some(12),
                Color::Magenta if intense => Some(13),
                Color::Cyan if intense => Some(14),
                Color::White if intense => Some(15),
                Color::Black => Some(0),
                Color::Red => Some(1),
                Color::Green => Some(2),
                Color::Yellow => Some(3),
                Color::Blue => Some(4),
                Color::Magenta => Some(5),
                Color::Cyan => Some(6),
                Color::White => Some(7),
                Color::Ansi256(n) => Some(n),
                _ => None,
            }
        }

        if spec.bold() {
            self.buf.push_str("\x1b[1m");
        }
        if spec.underline() {
            self.buf.push_str("\x1b[4m");
        }
        if let Some(fg) = spec.fg().and_then(|&c| try_to_ansi_256(c, spec.intense())) {
            write_ansi_256_color(&mut self.buf, fg, true);
        }
        if let Some(bg) = spec.bg().and_then(|&c| try_to_ansi_256(c, spec.intense())) {
            write_ansi_256_color(&mut self.buf, bg, true);
        }
    }

    pub(crate) fn push_ansi_bold(&mut self) {
        self.buf.push_str("\x1b[1m");
    }

    pub(crate) fn push_ansi_cursor_next_line(&mut self, n: usize) {
        self.write_fmt_to_buf(format_args!("\x1b[{}E", n));
    }

    pub(crate) fn push_ansi_cursor_previous_line(&mut self, n: usize) {
        self.write_fmt_to_buf(format_args!("\x1b[{}F", n));
    }

    pub(crate) fn push_ansi_cursor_horizontal_absolute(&mut self, n: usize) {
        self.write_fmt_to_buf(format_args!("\x1b[{}G", n));
    }

    pub(crate) fn push_ansi_erase_in_line_entire(&mut self) {
        self.buf.push_str("\x1b[2K");
    }
}

#[derive(Debug, Clone, Copy)]
enum State {
    Empty,
    Writing(usize),
    Flush,
}

fn write_ansi_256_color(wtr: &mut String, code: u8, fg: bool) {
    let mut buf: [_; 11] = *b"\x1b[48;5;mmmm";
    if fg {
        buf[2] = b'3';
    }
    let len;
    match (code / 100, (code / 10) % 10, code % 10) {
        (0, 0, c3) => {
            buf[7] = c3 + b'0';
            len = 9;
        }
        (0, c2, c3) => {
            buf[7] = c2 + b'0';
            buf[8] = c3 + b'0';
            len = 10;
        }
        (c1, c2, c3) => {
            buf[7] = c1 + b'0';
            buf[8] = c2 + b'0';
            buf[9] = c3 + b'0';
            len = 11;
        }
    }
    debug_assert!(str::from_utf8(&buf).is_ok());
    unsafe { wtr.as_mut_vec() }.extend_from_slice(&buf[0..len]);
}
