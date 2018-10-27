use serde::{self, Deserialize, Deserializer};

use std::time::Duration;

pub(crate) fn non_zero_secs<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> std::result::Result<Option<Duration>, D::Error> {
    let d = Option::<u64>::deserialize(deserializer)?;
    if d == Some(0) {
        Err(serde::de::Error::custom("0 is not acceptable"))
    } else {
        Ok(d.map(Duration::from_secs))
    }
}

pub(crate) fn millis<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> std::result::Result<Option<Duration>, D::Error> {
    Option::<u64>::deserialize(deserializer).map(|d| d.map(Duration::from_millis))
}
