use indicatif::ProgressDrawTarget;
use snowchains_core::web::StatusCodeColor;
use std::{cell::RefCell, fmt, io};
use termcolor::{Color, ColorSpec, WriteColor};

pub(crate) struct Shell<'a, W, F> {
    wtr: &'a RefCell<W>,
    wait_for_enter_key: F,
    draw_progress: bool,
}

impl<'a, W: WriteColor, F: FnMut() -> io::Result<()>> self::Shell<'a, W, F> {
    pub(crate) fn new(wtr: &'a RefCell<W>, wait_for_enter_key: F, draw_progress: bool) -> Self {
        Self {
            wtr,
            wait_for_enter_key,
            draw_progress,
        }
    }

    pub(crate) fn info(&mut self, message: impl fmt::Display) -> anyhow::Result<()> {
        let mut wtr = self.wtr.borrow_mut();

        writeln!(wtr, "{}", message)?;
        wtr.flush().map_err(Into::into)
    }

    pub(crate) fn warn(&mut self, message: impl fmt::Display) -> anyhow::Result<()> {
        let mut wtr = self.wtr.borrow_mut();

        wtr.set_color(&color_spec(Some(Color::Yellow), true))?;
        write!(wtr, "warning:")?;
        wtr.reset()?;
        writeln!(wtr, " {}", message)?;
        wtr.flush().map_err(Into::into)
    }
}

impl<'a, W: WriteColor, F: FnMut() -> io::Result<()>> snowchains_core::web::Shell
    for self::Shell<'a, W, F>
{
    fn progress_draw_target(&self) -> ProgressDrawTarget {
        let wtr = self.wtr.borrow();

        if self.draw_progress && wtr.supports_color() {
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
        let mut wtr = self.wtr.borrow_mut();

        write!(wtr, "{}", prompt)?;
        wtr.flush()?;
        (self.wait_for_enter_key)().map_err(Into::into)
    }

    fn on_request(&mut self, req: &reqwest::blocking::Request) -> anyhow::Result<()> {
        let mut wtr = self.wtr.borrow_mut();

        wtr.set_color(&color_spec(None, true))?;
        write!(wtr, "{}", req.method())?;
        wtr.reset()?;

        write!(wtr, " ")?;

        wtr.set_color(&color_spec(Some(Color::Cyan), false))?;
        write!(wtr, "{}", req.url())?;
        wtr.reset()?;

        write!(wtr, " ... ")?;

        wtr.flush().map_err(Into::into)
    }

    fn on_response(
        &mut self,
        res: &reqwest::blocking::Response,
        status_code_color: StatusCodeColor,
    ) -> anyhow::Result<()> {
        let mut wtr = self.wtr.borrow_mut();

        let fg = match status_code_color {
            StatusCodeColor::Ok => Some(Color::Green),
            StatusCodeColor::Warn => Some(Color::Yellow),
            StatusCodeColor::Error => Some(Color::Red),
            StatusCodeColor::Unknown => None,
        };

        wtr.set_color(&color_spec(fg, true))?;
        write!(wtr, "{}", res.status())?;
        wtr.reset()?;

        writeln!(wtr)?;

        wtr.flush().map_err(Into::into)
    }
}

fn color_spec(fg: Option<Color>, bold: bool) -> ColorSpec {
    let mut spec = ColorSpec::new();
    spec.set_reset(false).set_bold(bold).set_fg(fg);
    spec
}
