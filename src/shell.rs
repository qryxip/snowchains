use indicatif::ProgressDrawTarget;
use snowchains_core::web::StatusCodeColor;
use std::{
    fmt,
    io::BufRead,
    sync::{Arc, Mutex},
};
use termcolor::{Color, ColorSpec, WriteColor};

pub(crate) struct Shell<R, W> {
    input: Arc<Mutex<R>>,
    wtr: W,
    draw_progress: bool,
}

impl<R: BufRead, W: WriteColor> self::Shell<R, W> {
    pub(crate) fn new(input: &Arc<Mutex<R>>, wtr: W, draw_progress: bool) -> Self {
        Self {
            input: input.clone(),
            wtr,
            draw_progress,
        }
    }

    pub(crate) fn info(&mut self, message: impl fmt::Display) -> anyhow::Result<()> {
        writeln!(self.wtr, "{}", message)?;
        self.wtr.flush().map_err(Into::into)
    }

    pub(crate) fn warn(&mut self, message: impl fmt::Display) -> anyhow::Result<()> {
        self.wtr.set_color(&color_spec(Some(Color::Yellow), true))?;
        write!(self.wtr, "warning:")?;
        self.wtr.reset()?;
        writeln!(self.wtr, " {}", message)?;
        self.wtr.flush().map_err(Into::into)
    }
}

impl<R: BufRead, W: WriteColor> snowchains_core::web::Shell for self::Shell<R, W> {
    fn progress_draw_target(&self) -> ProgressDrawTarget {
        if self.draw_progress && self.wtr.supports_color() {
            ProgressDrawTarget::stderr()
        } else {
            ProgressDrawTarget::hidden()
        }
    }

    fn info<T: fmt::Display>(&mut self, message: T) -> anyhow::Result<()> {
        self.info(message)
    }

    fn warn<T: fmt::Display>(&mut self, message: T) -> anyhow::Result<()> {
        self.warn(message)
    }

    fn wait_for_enter_key(&mut self, prompt: &'static str) -> anyhow::Result<()> {
        write!(self.wtr, "{}", prompt)?;
        self.wtr.flush()?;
        self.input.lock().unwrap().read_line(&mut "".to_owned())?;
        Ok(())
    }

    fn on_request(&mut self, req: &reqwest::blocking::Request) -> anyhow::Result<()> {
        self.wtr.set_color(&color_spec(None, true))?;
        write!(self.wtr, "{}", req.method())?;
        self.wtr.reset()?;

        write!(self.wtr, " ")?;

        self.wtr.set_color(&color_spec(Some(Color::Cyan), false))?;
        write!(self.wtr, "{}", req.url())?;
        self.wtr.reset()?;

        write!(self.wtr, " ... ")?;

        self.wtr.flush().map_err(Into::into)
    }

    fn on_response(
        &mut self,
        res: &reqwest::blocking::Response,
        status_code_color: StatusCodeColor,
    ) -> anyhow::Result<()> {
        let fg = match status_code_color {
            StatusCodeColor::Ok => Some(Color::Green),
            StatusCodeColor::Warn => Some(Color::Yellow),
            StatusCodeColor::Error => Some(Color::Red),
            StatusCodeColor::Unknown => None,
        };

        self.wtr.set_color(&color_spec(fg, true))?;
        write!(self.wtr, "{}", res.status())?;
        self.wtr.reset()?;

        writeln!(self.wtr)?;

        self.wtr.flush().map_err(Into::into)
    }
}

fn color_spec(fg: Option<Color>, bold: bool) -> ColorSpec {
    let mut spec = ColorSpec::new();
    spec.set_reset(false).set_bold(bold).set_fg(fg);
    spec
}
