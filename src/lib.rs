#[macro_use]
extern crate failure;
#[macro_use]
extern crate futures;
#[macro_use]
extern crate log;
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate once_cell;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate serde_json;
#[macro_use]
extern crate structopt;

extern crate atty;
extern crate bincode;
extern crate chrono;
extern crate combine;
extern crate cookie;
extern crate diff;
extern crate dirs;
extern crate fs2;
extern crate heck;
extern crate itertools;
extern crate pbr;
extern crate regex;
extern crate reqwest;
extern crate robots_txt;
extern crate rpassword;
extern crate rprompt;
extern crate select;
extern crate serde;
extern crate serde_urlencoded;
extern crate serde_yaml;
extern crate tokio;
extern crate tokio_core;
extern crate tokio_process;
extern crate toml;
extern crate unicode_width;
extern crate url;
extern crate webbrowser;
extern crate yaml_rust;
extern crate zip;

#[cfg(any(target_os = "linux", target_os = "macos"))]
extern crate libc;

#[cfg(windows)]
extern crate winapi;
#[cfg(windows)]
extern crate winapi_util;

#[cfg(test)]
#[macro_use]
extern crate nickel;

#[cfg(test)]
extern crate env_logger;
#[cfg(test)]
extern crate tempdir;

#[macro_use]
mod macros;

pub mod app;
pub mod path;
pub mod service;
pub mod terminal;
pub mod util;

mod config;
mod errors;
mod fs;
mod judging;
mod replacer;
mod template;
mod testsuite;
mod yaml;

pub use errors::{Error, Result};

use std::fmt;

#[derive(Debug)]
pub struct Never;

impl fmt::Display for Never {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        panic!("should be filtered by `clap::Arg::possible_values`")
    }
}
