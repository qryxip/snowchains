use snowchains::app::{App, Opt};
use snowchains::path::AbsPathBuf;
use snowchains::terminal::{Term, TermImpl, WriteAnsi, WriteSpaces};

use failure::{Fail, Fallible};
use structopt::StructOpt;

use std::io::{self, BufWriter, Write};
use std::process;

fn main() -> Fallible<()> {
    env_logger::init();
    snowchains::signal::start_catching_ctrl_c()?;
    let opt = Opt::from_args();
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let mut term = TermImpl::new(
        stdin.lock(),
        BufWriter::new(stdout.lock()),
        BufWriter::new(io::stderr()),
    );
    let result = App {
        working_dir: AbsPathBuf::cwd()?,
        login_retries: None,
        term: &mut term,
    }
    .run(opt);
    if let Err(err) = result {
        let (_, stdout, stderr) = term.split_mut();
        stdout.flush()?;
        writeln!(stderr)?;
        for (i, cause) in Fail::iter_chain(&err).enumerate() {
            let head = if i == 0 && err.cause().is_none() {
                "error: "
            } else if i == 0 {
                "    error: "
            } else {
                "caused by: "
            };
            stderr.with_reset(|o| o.fg(1)?.bold()?.write_str(head))?;
            for (i, line) in cause.to_string().lines().enumerate() {
                if i > 0 {
                    stderr.write_spaces(head.len())?;
                }
                writeln!(stderr, "{}", line)?;
            }
        }
        if let Some(backtrace) = err.backtrace() {
            writeln!(stderr, "{:?}", backtrace)?;
        }
        stderr.flush()?;
        process::exit(1)
    }
    Ok(())
}
