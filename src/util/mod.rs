pub(crate) mod collections;
pub(crate) mod combine;
pub(crate) mod num;
pub(crate) mod serde;
pub(crate) mod std_unstable;
pub(crate) mod str;

use std::io::{self, Read};

pub(crate) fn string_from_read(mut read: impl Read, capacity: usize) -> io::Result<String> {
    let mut buf = String::with_capacity(capacity);
    read.read_to_string(&mut buf)?;
    Ok(buf)
}
