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
extern crate maplit;
#[macro_use]
extern crate newtype_derive;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate structopt;

extern crate chrono;
extern crate combine;
extern crate futures;
extern crate heck;
extern crate httpsession;
extern crate itertools;
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
extern crate unicode_width;
extern crate webbrowser;
extern crate zip;

#[cfg(test)]
extern crate env_logger;
#[cfg(test)]
extern crate tempdir;

#[macro_use]
pub mod macros;

pub mod terminal;
pub mod util;

mod command;
mod config;
mod entrypoint;
mod errors;
mod judging;
mod replacer;
mod service;
mod template;
mod testsuite;

pub use entrypoint::{Opt, Prop};
pub use errors::{Error, ErrorKind, Result};
pub use service::Credentials;

use std::fmt;
use std::str::FromStr;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ServiceName {
    AtCoder,
    HackerRank,
    Other,
}

impl Default for ServiceName {
    fn default() -> Self {
        ServiceName::Other
    }
}

impl fmt::Display for ServiceName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl FromStr for ServiceName {
    type Err = Never;

    fn from_str(s: &str) -> std::result::Result<Self, Never> {
        match s {
            s if s.eq_ignore_ascii_case("atcoder") => Ok(ServiceName::AtCoder),
            s if s.eq_ignore_ascii_case("hackerrank") => Ok(ServiceName::HackerRank),
            s if s.eq_ignore_ascii_case("other") => Ok(ServiceName::Other),
            _ => Err(Never),
        }
    }
}

impl ServiceName {
    pub(crate) fn as_str(self) -> &'static str {
        match self {
            ServiceName::AtCoder => "atcoder",
            ServiceName::HackerRank => "hackerrank",
            ServiceName::Other => "other",
        }
    }

    pub(crate) fn domain(self) -> Option<&'static str> {
        match self {
            ServiceName::AtCoder => Some("beta.atcoder.jp"),
            ServiceName::HackerRank => Some("www.hackerrank.com"),
            ServiceName::Other => None,
        }
    }
}

#[derive(Debug)]
pub struct Never;

impl fmt::Display for Never {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        unreachable!("should be filtered by `clap::Arg::possible_values`")
    }
}
