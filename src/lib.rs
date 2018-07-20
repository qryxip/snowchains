#![recursion_limit = "1024"]

#[macro_use]
extern crate failure;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate serde_json;
#[macro_use]
extern crate structopt;

extern crate ansi_term;
extern crate bincode;
extern crate chrono;
extern crate combine;
extern crate cookie;
extern crate dirs;
extern crate fs2;
extern crate futures;
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
extern crate tokio_core;
extern crate toml;
extern crate unicode_width;
extern crate url;
extern crate webbrowser;
extern crate yaml_rust;
extern crate zip;

#[cfg(not(windows))]
extern crate term;

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
pub mod palette;
pub mod path;
pub mod service;
pub mod util;

mod command;
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
use std::str::FromStr;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ServiceName {
    Atcoder,
    Hackerrank,
    Yukicoder,
    Other,
}

impl Default for ServiceName {
    fn default() -> Self {
        ServiceName::Other
    }
}

impl fmt::Display for ServiceName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl FromStr for ServiceName {
    type Err = Never;

    fn from_str(s: &str) -> std::result::Result<Self, Never> {
        match s {
            s if s.eq_ignore_ascii_case("atcoder") => Ok(ServiceName::Atcoder),
            s if s.eq_ignore_ascii_case("hackerrank") => Ok(ServiceName::Hackerrank),
            s if s.eq_ignore_ascii_case("yukicoder") => Ok(ServiceName::Yukicoder),
            s if s.eq_ignore_ascii_case("other") => Ok(ServiceName::Other),
            _ => Err(Never),
        }
    }
}

impl ServiceName {
    pub fn to_str(self) -> &'static str {
        match self {
            ServiceName::Atcoder => "atcoder",
            ServiceName::Hackerrank => "hackerrank",
            ServiceName::Yukicoder => "yukicoder",
            ServiceName::Other => "other",
        }
    }

    pub(crate) fn domain(self) -> Option<&'static str> {
        match self {
            ServiceName::Atcoder => Some("beta.atcoder.jp"),
            ServiceName::Hackerrank => Some("www.hackerrank.com"),
            ServiceName::Yukicoder => Some("yukicoder.me"),
            ServiceName::Other => None,
        }
    }
}

#[derive(Debug)]
pub struct Never;

impl fmt::Display for Never {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        panic!("should be filtered by `clap::Arg::possible_values`")
    }
}
