use crate::TtyOrPiped;
use std::{cell::RefCell, io::BufRead};
use termcolor::WriteColor;

pub(crate) fn username_and_password<'a, R: BufRead + 'a, W: WriteColor>(
    mut stdin: TtyOrPiped<R>,
    stderr: &'a RefCell<W>,
    username_prompt: &'static str,
) -> impl FnMut() -> anyhow::Result<(String, String)> + 'a {
    move || -> _ {
        let mut stderr = stderr.borrow_mut();

        write!(stderr, "{}", username_prompt)?;
        stderr.flush()?;
        let username = stdin.read_reply()?;

        write!(stderr, "Password: ")?;
        stderr.flush()?;
        let password = stdin.read_password()?;

        Ok((username, password))
    }
}
