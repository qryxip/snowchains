use indicatif::ProgressDrawTarget;
use snowchains_core::{color_spec, web::StatusCodeColor};
use std::{
    env, fmt,
    io::{self, BufRead, Stdin, StdinLock, Write},
    process::Stdio,
};
use termcolor::{BufferedStandardStream, Color, WriteColor};

pub struct Shell<R, W1, W2> {
    pub stdin: TtyOrPiped<R>,
    pub stdout: W1,
    pub stderr: W2,
    pub stderr_tty: bool,
    pub stdin_process_redirection: fn() -> Stdio,
    pub stdout_process_redirection: fn() -> Stdio,
    pub stderr_process_redirection: fn() -> Stdio,
}

impl<'a> Shell<StdinLock<'a>, BufferedStandardStream, BufferedStandardStream> {
    pub fn new(stdin: &'a Stdin, color: crate::ColorChoice) -> Self {
        let convert_with_atty_fitler = |stream| match (color, atty::is(stream)) {
            (crate::ColorChoice::Auto, true) => termcolor::ColorChoice::Auto,
            (crate::ColorChoice::Always, _) => termcolor::ColorChoice::Always,
            _ => termcolor::ColorChoice::Never,
        };

        Self {
            stdin: TtyOrPiped::auto(stdin),
            stdout: BufferedStandardStream::stdout(convert_with_atty_fitler(atty::Stream::Stdout)),
            stderr: BufferedStandardStream::stderr(convert_with_atty_fitler(atty::Stream::Stderr)),
            stderr_tty: atty::is(atty::Stream::Stderr),
            stdin_process_redirection: Stdio::inherit,
            stdout_process_redirection: Stdio::inherit,
            stderr_process_redirection: Stdio::inherit,
        }
    }
}

impl<R, W1, W2: WriteColor> Shell<R, W1, W2> {
    pub(crate) fn info(&mut self, message: impl fmt::Display) -> io::Result<()> {
        self.stderr.set_color(color_spec!(Bold, Fg(Color::Cyan)))?;
        write!(self.stderr, "info:")?;
        self.stderr.reset()?;
        writeln!(self.stderr, " {}", message)?;
        self.stderr.flush()
    }

    pub(crate) fn warn(&mut self, message: impl fmt::Display) -> io::Result<()> {
        self.stderr
            .set_color(color_spec!(Bold, Fg(Color::Yellow)))?;
        write!(self.stderr, "warning:")?;
        self.stderr.reset()?;
        writeln!(self.stderr, " {}", message)?;
        self.stderr.flush()
    }
}

impl<R: BufRead, W1, W2: Write> Shell<R, W1, W2> {
    pub(crate) fn read_reply(&mut self, prompt: &'static str) -> io::Result<String> {
        write!(self.stderr, "{}", prompt)?;
        self.stderr.flush()?;
        self.stdin.read_reply()
    }

    pub(crate) fn read_password(&mut self, prompt: &'static str) -> io::Result<String> {
        write!(self.stderr, "{}", prompt)?;
        self.stderr.flush()?;
        self.stdin.read_password()
    }
}

impl<R, W1, W2> Shell<R, W1, W2> {
    pub(crate) fn progress_draw_target(&self) -> ProgressDrawTarget {
        if self.stderr_tty {
            ProgressDrawTarget::stderr()
        } else {
            ProgressDrawTarget::hidden()
        }
    }
}

impl<R, W1, W2: WriteColor> snowchains_core::web::Shell for Shell<R, W1, W2> {
    fn progress_draw_target(&self) -> ProgressDrawTarget {
        self.progress_draw_target()
    }

    fn info<T: fmt::Display>(&mut self, message: T) -> io::Result<()> {
        self.info(message)
    }

    fn warn<T: fmt::Display>(&mut self, message: T) -> io::Result<()> {
        self.warn(message)
    }

    fn on_request(&mut self, req: &reqwest::blocking::Request) -> io::Result<()> {
        self.stderr.set_color(color_spec!(Bold))?;
        write!(self.stderr, "{}", req.method())?;
        self.stderr.reset()?;

        write!(self.stderr, " ")?;

        self.stderr.set_color(color_spec!(Fg(Color::Cyan)))?;
        write!(self.stderr, "{}", req.url())?;
        self.stderr.reset()?;

        write!(self.stderr, " ... ")?;

        self.stderr.flush()
    }

    fn on_response(
        &mut self,
        res: &reqwest::blocking::Response,
        status_code_color: StatusCodeColor,
    ) -> io::Result<()> {
        let fg = match status_code_color {
            StatusCodeColor::Ok => Some(Color::Green),
            StatusCodeColor::Warn => Some(Color::Yellow),
            StatusCodeColor::Error => Some(Color::Red),
            StatusCodeColor::Unknown => None,
        };

        self.stderr.set_color(color_spec!(Bold).set_fg(fg))?;
        write!(self.stderr, "{}", res.status())?;
        self.stderr.reset()?;
        writeln!(self.stderr)?;
        self.stderr.flush()
    }
}

#[derive(Debug)]
pub enum TtyOrPiped<R> {
    Tty,
    Piped(R),
}

impl<'a> TtyOrPiped<StdinLock<'a>> {
    fn auto(stdin: &'a Stdin) -> Self {
        if atty::is(atty::Stream::Stdin) && !(cfg!(windows) && env::var_os("MSYSTEM").is_some()) {
            TtyOrPiped::Tty
        } else {
            TtyOrPiped::Piped(stdin.lock())
        }
    }
}

impl<R: BufRead> TtyOrPiped<R> {
    fn read_reply(&mut self) -> io::Result<String> {
        match self {
            Self::Tty => rprompt::read_reply(),
            Self::Piped(r) => rpassword::read_password_with_reader(Some(r)),
        }
    }

    fn read_password(&mut self) -> io::Result<String> {
        match self {
            Self::Tty => rpassword::read_password_from_tty(None),
            Self::Piped(r) => rpassword::read_password_with_reader(Some(r)),
        }
    }
}
