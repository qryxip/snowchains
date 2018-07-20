extern crate snowchains;

extern crate env_logger;
extern crate failure;
extern crate structopt;

use snowchains::app::{Opt, Prop};
use snowchains::palette::Palette;

use failure::Fail;
use structopt::StructOpt as _StructOpt;

use std::process;

fn main() {
    env_logger::init();
    if let Err(err) = Prop::new().and_then(|prop| Opt::from_args().run(&prop)) {
        eprintln!();
        for (i, cause) in err.causes().enumerate() {
            let head = if i == 0 && err.cause().is_none() {
                "Error: "
            } else if i == 0 {
                "    Error: "
            } else {
                "Caused by: "
            };
            eprint!("{}", Palette::Fatal.bold().paint(head));
            for (i, line) in cause.to_string().lines().enumerate() {
                if i > 0 {
                    (0..head.len()).for_each(|_| eprint!(" "));
                }
                eprintln!("{}", line);
            }
        }
        if let Some(backtrace) = err.backtrace() {
            eprintln!("{:?}", backtrace);
        }
        process::exit(1)
    }
}
