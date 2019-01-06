use snowchains::app::{App, Opt};
use snowchains::path::AbsPathBuf;
use snowchains::service::Credentials;
use snowchains::terminal::{Term, TermImpl, WriteAnsi, WriteSpaces};

use failure::{Fail, Fallible};
use if_chain::if_chain;
use structopt::StructOpt;

use std::io::{self, Write};
use std::path::Path;
use std::process;

fn main() -> Fallible<()> {
    env_logger::init();
    ctrlc::set_handler(|| ())?;
    let opt = Opt::from_args();
    let (stdin, stdout, stderr) = (io::stdin(), io::stdout(), io::stderr());
    let mut term = TermImpl::new(&stdin, &stdout, &stderr);
    if let Err(err) = run(opt, &mut term) {
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
    } else {
        Ok(())
    }
}

fn run(opt: Opt, term: impl Term) -> snowchains::Result<()> {
    let working_dir = AbsPathBuf::cwd()?;
    let data_local_dir = if_chain! {
        if let (Some(home), Some(local)) = (dirs::home_dir(), dirs::data_local_dir());
        if let Ok(path) = local.strip_prefix(&home);
        then {
            Path::new("~").join(path).join("snowchains")
        } else {
            Path::new("~").join(".local").join("share").join("snowchains")
        }
    };
    App {
        working_dir,
        cookies_on_init: data_local_dir.join("$service").display().to_string(),
        dropbox_auth_on_init: data_local_dir.join("dropbox.json").display().to_string(),
        enable_dropbox_on_init: false,
        credentials: Credentials::default(),
        term,
    }
    .run(opt)
}
