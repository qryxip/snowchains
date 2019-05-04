pub(crate) mod collections;
pub(crate) mod indexmap;
pub(crate) mod io;
pub(crate) mod num;
pub(crate) mod scraper;
pub(crate) mod serde;
pub(crate) mod str;

use std::io::Read;

pub(crate) fn string_from_read(mut read: impl Read, capacity: usize) -> std::io::Result<String> {
    let mut buf = String::with_capacity(capacity);
    read.read_to_string(&mut buf)?;
    Ok(buf)
}
