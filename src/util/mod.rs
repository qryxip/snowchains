pub(crate) mod collections;
pub(crate) mod combine;
pub(crate) mod lang_unstable;
pub(crate) mod num;
pub(crate) mod std_unstable;
pub(crate) mod str;

use std::io::{self, Read};

pub(crate) fn string_from_read(mut read: impl Read, capacity: usize) -> io::Result<String> {
    let mut buf = String::with_capacity(capacity);
    read.read_to_string(&mut buf)?;
    Ok(buf)
}

pub(crate) trait Lookup {
    type Key;
    type Value;

    fn lookup(&self, key: &Self::Key) -> Option<&Self::Value>;
}

impl<K: PartialEq, V> Lookup for [(K, V)] {
    type Key = K;
    type Value = V;

    fn lookup(&self, key: &K) -> Option<&V> {
        self.iter().filter(|(k, _)| k == key).map(|(_, v)| v).next()
    }
}
