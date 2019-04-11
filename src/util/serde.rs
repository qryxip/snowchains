use indexmap::IndexMap;
use serde::ser::SerializeMap as _;
use serde::{Serialize, Serializer};

use std::hash::Hash;
use std::path::Path;

pub(crate) fn ser_as_ref_str<S: Serializer>(
    s: impl AsRef<str>,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    serializer.serialize_str(s.as_ref())
}

pub(crate) fn ser_as_ref_path<S: Serializer, P: AsRef<Path>>(
    path: P,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    path.as_ref().serialize(serializer)
}

pub(crate) fn ser_indexmap_with_as_ref_str_keys<
    'a,
    S: Serializer,
    K: 'a + AsRef<str> + Hash + Eq,
    V: 'a + Serialize,
>(
    map: &'a IndexMap<K, V>,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    let mut serializer = serializer.serialize_map(Some(map.len()))?;
    for (key, value) in map {
        serializer.serialize_entry(key.as_ref(), value)?;
    }
    serializer.end()
}
