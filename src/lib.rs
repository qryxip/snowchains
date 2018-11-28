#![recursion_limit = "128"]
extern crate snowchains_proc_macros;

extern crate atty;
extern crate bincode;
extern crate chrono;
extern crate combine;
extern crate cookie;
extern crate derive_more;
extern crate derive_new;
extern crate diff;
extern crate dirs;
extern crate failure;
extern crate fs2;
extern crate futures;
extern crate heck;
extern crate itertools;
extern crate log;
extern crate maplit;
extern crate mime;
extern crate multipart;
extern crate once_cell;
extern crate pbr;
extern crate regex;
extern crate reqwest;
extern crate robots_txt;
extern crate rpassword;
extern crate rprompt;
extern crate select;
extern crate serde;
extern crate serde_derive;
extern crate serde_json;
extern crate serde_urlencoded;
extern crate serde_yaml;
extern crate structopt;
extern crate strum;
extern crate strum_macros;
extern crate term_size;
extern crate tokio;
extern crate tokio_process;
extern crate tokio_signal;
extern crate toml;
extern crate unicode_width;
extern crate url;
extern crate webbrowser;
extern crate yaml_rust;
extern crate zip;

#[cfg(windows)]
extern crate winapi;
#[cfg(windows)]
extern crate winapi_util;

#[cfg(test)]
extern crate nickel;

#[cfg(test)]
extern crate env_logger;
#[cfg(test)]
extern crate if_chain;
#[cfg(test)]
extern crate tempdir;

#[macro_use]
mod macros;

pub mod app;
pub mod errors;
pub mod path;
pub mod service;
pub mod terminal;

mod config;
mod fs;
mod judging;
mod replacer;
mod template;
mod testsuite;
mod time;
mod util;
mod yaml;

pub use crate::errors::{Error, ErrorKind, Result};
