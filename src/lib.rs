#![recursion_limit = "1024"]

#[macro_use]
extern crate custom_derive;
#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
#[macro_use]
extern crate newtype_derive;
#[macro_use]
extern crate serde_derive;

extern crate bincode;
extern crate chrono;
extern crate cookie;
extern crate decimal;
extern crate futures;
extern crate httpsession;
extern crate pbr;
extern crate regex;
extern crate robots_txt;
extern crate rpassword;
extern crate rprompt;
extern crate select;
extern crate serde;
extern crate serde_json;
extern crate serde_urlencoded;
extern crate serde_yaml;
extern crate term;
extern crate toml;
extern crate webbrowser;
extern crate zip;

#[cfg(test)]
#[macro_use]
extern crate nickel;

#[cfg(test)]
extern crate env_logger;

#[macro_use]
pub mod macros;

pub mod config;
pub mod errors;
pub mod judging;
pub mod service;
pub mod terminal;
pub mod testsuite;
pub mod util;

mod command;
mod replacer;
mod template;

pub use errors::{ErrorKind, Result};
