#[macro_use]
extern crate snowchains;

extern crate env_logger;
extern crate error_chain;
extern crate structopt;

use snowchains::terminal::Color;
use snowchains::{Opt, Prop};

use structopt::StructOpt as _StructOpt;

use std::process;

fn main() {
    env_logger::init();
    if let Err(e) = Prop::new().and_then(|prop| Opt::from_args().run(&prop)) {
        eprintln!();
        if e.iter().count() > 1 {
            eprint!("    ");
        }
        for (i, e) in e.iter().enumerate() {
            let head = if i == 0 { "Error: " } else { "Caused by: " };
            eprint_bold!(Color::Fatal, "{}", head);
            eprintln_bold!(Color::None, "{}", e);
        }
        if let Some(backtrace) = e.backtrace() {
            eprintln!("{:?}", backtrace);
        }
        process::exit(1)
    }
}
