extern crate snowchains;

extern crate env_logger;
extern crate failure;
extern crate structopt;

use snowchains::app::{App, Opt};
use snowchains::console::{Console, Palette};
use snowchains::path::AbsPathBuf;
use snowchains::service::Credentials;

use failure::Fail;
use structopt::StructOpt as _StructOpt;

use std::env;
use std::io::{self, BufRead, BufReader, BufWriter, Write};
use std::process;

fn main() {
    env_logger::init();
    let (stdin, stdout, stderr) = (io::stdin(), io::stdout(), io::stderr());
    let console = Console::new(
        BufReader::new(stdin.lock()),
        BufWriter::new(stdout.lock()),
        BufWriter::new(stderr.lock()),
    );
    let (mut console, result) = run(console);
    if let Err(err) = result {
        console.stdout().flush().unwrap();
        let mut stderr = console.stderr();
        writeln!(stderr).unwrap();
        for (i, cause) in (&err as &Fail).iter_chain().enumerate() {
            let head = if i == 0 && err.cause().is_none() {
                "Error: "
            } else if i == 0 {
                "    Error: "
            } else {
                "Caused by: "
            };
            write!(stderr.bold(Palette::Fatal), "{}", head).unwrap();
            for (i, line) in cause.to_string().lines().enumerate() {
                if i > 0 {
                    stderr.write_spaces(head.len()).unwrap();
                }
                writeln!(stderr, "{}", line).unwrap();
            }
        }
        if let Some(backtrace) = err.backtrace() {
            writeln!(stderr, "{:?}", backtrace).unwrap();
        }
        stderr.flush().unwrap();
        process::exit(1);
    }
}

fn run<I: BufRead, O: Write, E: Write>(
    console: Console<I, O, E>,
) -> (Console<I, O, E>, snowchains::Result<()>) {
    let working_dir = match env::current_dir() {
        Ok(wd) => AbsPathBuf::new_or_panic(wd),
        Err(err) => return (console, Err(snowchains::Error::Getcwd(err))),
    };
    let mut app = App {
        working_dir,
        cookies_on_init: "~/.local/share/snowchains/$service".into(),
        credentials: Credentials::default(),
        console,
    };
    let result = app.run(Opt::from_args());
    (app.console, result)
}
