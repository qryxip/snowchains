use snowchains::app::{App, Opt};
use snowchains::path::AbsPathBuf;
use snowchains::terminal::{AnsiStandardStream, TtyOrPiped};

use failure::{Fail, Fallible};
use structopt::StructOpt;
use termcolor::{Color, ColorSpec, WriteColor as _};

use std::io::{self, BufWriter, Write as _};
use std::process;

fn main() -> Fallible<()> {
    snowchains::signal::start_catching_ctrl_c()?;
    let code = {
        let opt = Opt::from_args();
        let stdin = io::stdin();
        let mut stdout = AnsiStandardStream::new(BufWriter::new(io::stdout()));
        let mut stderr = AnsiStandardStream::new(BufWriter::new(io::stderr()));
        let result = App {
            working_dir: AbsPathBuf::cwd()?,
            login_retries: None,
            stdin: TtyOrPiped::auto(&stdin),
            stdout: &mut stdout,
            stderr: &mut stderr,
        }
        .run(opt);
        match result {
            Ok(code) => code,
            Err(err) => {
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
                    stderr.set_color(
                        ColorSpec::new()
                            .set_fg(Some(Color::Ansi256(1)))
                            .set_bold(true),
                    )?;
                    stderr.write_all(head.as_ref())?;
                    stderr.reset()?;
                    for (i, line) in cause.to_string().lines().enumerate() {
                        if i > 0 {
                            (0..head.len()).try_for_each(|_| stderr.write_all(b" "))?;
                        }
                        writeln!(stderr, "{}", line)?;
                    }
                }
                let backtrace = err.backtrace().unwrap().to_string();
                if !backtrace.is_empty() {
                    writeln!(stderr, "\n{}", backtrace)?;
                }
                stderr.flush()?;
                1
            }
        }
    };
    process::exit(code)
}
