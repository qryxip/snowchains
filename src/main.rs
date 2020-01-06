use snowchains::path::AbsPathBuf;
use snowchains::terminal::{AnsiStandardStream, TtyOrPiped};
use snowchains::Opt;

use failure::{Fail, Fallible};
use structopt::StructOpt as _;
use termcolor::{Color, ColorSpec, WriteColor as _};

use std::io::{self, BufWriter, Write as _};
use std::process;

fn main() -> Fallible<()> {
    snowchains::signal::start_catching_ctrl_c()?;
    let code = {
        let opt = Opt::from_args();
        let stdin = io::stdin();
        let mut ctx = snowchains::Context {
            cwd: AbsPathBuf::cwd()?,
            login_retries: None,
            stdin: TtyOrPiped::auto(&stdin),
            stdout: AnsiStandardStream::new(BufWriter::new(io::stdout())),
            stderr: AnsiStandardStream::new(BufWriter::new(io::stderr())),
        };
        match snowchains::run(opt, &mut ctx) {
            Ok(code) => code,
            Err(err) => {
                let snowchains::Context {
                    mut stdout,
                    mut stderr,
                    ..
                } = ctx;
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
