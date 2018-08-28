extern crate snowchains;

extern crate env_logger;
extern crate failure;
extern crate structopt;

use snowchains::app::{App, Opt};
use snowchains::console::{Console, ConsoleReadWrite, ConsoleWrite as _ConsoleWrite, Palette};
use snowchains::path::AbsPathBuf;
use snowchains::service::Credentials;

use failure::Fail;
use structopt::StructOpt as _StructOpt;

use std::env;
use std::io::{self, BufReader, BufWriter, Write as _Write};
use std::process;

fn main() {
    env_logger::init();
    let (stdin, stdout, stderr) = (io::stdin(), io::stdout(), io::stderr());
    let mut console = Console::new(
        BufReader::new(stdin.lock()),
        BufWriter::new(stdout.lock()),
        BufWriter::new(stderr.lock()),
    );
    if let Err(err) = run(&mut console) {
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

fn run(console: impl ConsoleReadWrite) -> snowchains::Result<()> {
    let working_dir = env::current_dir()
        .map(AbsPathBuf::new_or_panic)
        .map_err(snowchains::Error::Getcwd)?;
    let mut app = App {
        working_dir,
        cookies_on_init: "~/.local/share/snowchains/$service".into(),
        credentials: Credentials::default(),
        console,
    };
    app.run(Opt::from_args())
}
