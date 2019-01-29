pub(crate) mod collections;
pub(crate) mod num;
pub(crate) mod std_unstable;

use serde_derive::{Deserialize, Serialize};

use std::io::{self, Read};

pub(crate) fn string_from_read(mut read: impl Read, capacity: usize) -> io::Result<String> {
    let mut buf = String::with_capacity(capacity);
    read.read_to_string(&mut buf)?;
    Ok(buf)
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum ScalarOrArray<T> {
    Scalar(T),
    Array(Vec<T>),
}
