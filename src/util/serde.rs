use indexmap::IndexMap;
use reqwest::StatusCode;
use serde::de::DeserializeOwned;
use serde::ser::{SerializeMap as _, SerializeSeq as _};
use serde::{Deserializer, Serialize, Serializer};

use std::fmt;
use std::hash::Hash;
use std::path::Path;
use std::process::ExitStatus;
use std::sync::Arc;

#[allow(clippy::trivially_copy_pass_by_ref)]
pub(crate) fn ser_exit_status<S: Serializer>(
    status: &ExitStatus,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    status.code().serialize(serializer)
}

#[allow(clippy::trivially_copy_pass_by_ref)]
pub(crate) fn ser_http_status<S: Serializer>(
    status: &StatusCode,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    status.as_u16().serialize(serializer)
}

pub(crate) fn ser_arc<T: Serialize, S: Serializer>(
    arc: &Arc<T>,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    (&*arc).serialize(serializer)
}

pub(crate) fn ser_option_arc<T: Serialize, S: Serializer>(
    arc: &Option<Arc<T>>,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    arc.as_ref().map(|x| &**x).serialize(serializer)
}

pub(crate) fn ser_option_display<T: fmt::Display, S: Serializer>(
    display: &Option<T>,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    display
        .as_ref()
        .map(ToString::to_string)
        .serialize(serializer)
}

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

pub(crate) fn ser_str_slice<S: Serializer>(
    slice: &[impl AsRef<str>],
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    let mut serializer = serializer.serialize_seq(Some(slice.len()))?;
    for s in slice {
        serializer.serialize_element(s.as_ref())?;
    }
    serializer.end()
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

pub(crate) fn de_to_arc<'de, T: DeserializeOwned, D: Deserializer<'de>>(
    deserializer: D,
) -> std::result::Result<Arc<T>, D::Error> {
    T::deserialize(deserializer).map(Arc::new)
}
