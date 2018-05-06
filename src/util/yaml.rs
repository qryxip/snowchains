use regex::Regex;
use serde::{self, Deserialize, Deserializer, Serialize, Serializer};

use std::{self, str};

pub(crate) fn serialize_regex<S: Serializer>(
    regex: &Regex,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    format!("/{}/", regex).serialize(serializer)
}

pub(crate) fn deserialize_regex<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> std::result::Result<Regex, D::Error> {
    let regex = String::deserialize(deserializer)?;
    let regex = if regex.starts_with('/') && regex.ends_with('/') {
        let n = regex.len();
        unsafe { str::from_utf8_unchecked(&regex.as_bytes()[1..n - 1]) }
    } else {
        &regex
    };
    Regex::new(&regex).map_err(serde::de::Error::custom)
}
