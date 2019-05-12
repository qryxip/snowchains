#![recursion_limit = "128"]

#[macro_use]
mod macros;

pub mod app;
pub mod config;
pub mod errors;
pub mod path;
pub mod service;
pub mod signal;
pub mod terminal;
pub mod testsuite;

mod command;
mod fs;
mod judging;
mod outcome;
mod template;
mod time;
mod util;

pub use crate::errors::{Error, ErrorKind, Result};
