use crate::terminal::HasTermProps;

use serde::Serialize;
use termcolor::WriteColor;

use std::io;

pub(crate) trait Outcome: Serialize {
    fn is_success(&self) -> bool;
    fn print_pretty(&self, verbose: bool, stdout: impl WriteColor + HasTermProps)
        -> io::Result<()>;
}

impl<'a, O: Outcome> Outcome for &'a O {
    fn is_success(&self) -> bool {
        (**self).is_success()
    }

    fn print_pretty(
        &self,
        verbose: bool,
        stdout: impl WriteColor + HasTermProps,
    ) -> io::Result<()> {
        (**self).print_pretty(verbose, stdout)
    }
}
