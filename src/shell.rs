use indicatif::ProgressDrawTarget;
use snowchains_core::{color_spec, web::StatusCodeColor};
use std::{cell::RefCell, fmt};
use termcolor::{Color, WriteColor};

pub(crate) struct Shell<'a, W> {
    wtr: &'a RefCell<W>,
    draw_progress: bool,
}

impl<'a, W: WriteColor> self::Shell<'a, W> {
    pub(crate) fn new(wtr: &'a RefCell<W>, draw_progress: bool) -> Self {
        Self { wtr, draw_progress }
    }

    pub(crate) fn info(&mut self, message: impl fmt::Display) -> anyhow::Result<()> {
        let mut wtr = self.wtr.borrow_mut();

        writeln!(wtr, "{}", message)?;
        wtr.flush().map_err(Into::into)
    }

    pub(crate) fn warn(&mut self, message: impl fmt::Display) -> anyhow::Result<()> {
        let mut wtr = self.wtr.borrow_mut();

        wtr.set_color(color_spec!(Bold, Fg(Color::Yellow)))?;
        write!(wtr, "warning:")?;
        wtr.reset()?;
        writeln!(wtr, " {}", message)?;
        wtr.flush().map_err(Into::into)
    }
}

impl<'a, W: WriteColor> snowchains_core::web::Shell for self::Shell<'a, W> {
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

    fn on_request(&mut self, req: &reqwest::blocking::Request) -> anyhow::Result<()> {
        let mut wtr = self.wtr.borrow_mut();

        wtr.set_color(color_spec!(Bold))?;
        write!(wtr, "{}", req.method())?;
        wtr.reset()?;

        write!(wtr, " ")?;

        wtr.set_color(color_spec!(Fg(Color::Cyan)))?;
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

        wtr.set_color(color_spec!(Bold).set_fg(fg))?;
        write!(wtr, "{}", res.status())?;
        wtr.reset()?;

        writeln!(wtr)?;

        wtr.flush().map_err(Into::into)
    }
}
