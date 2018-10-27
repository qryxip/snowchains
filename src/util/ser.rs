use serde::{Serialize, Serializer};

use std::time::Duration;

pub(crate) fn secs<S: Serializer>(
    duration: &Option<Duration>,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    duration.map(|d| d.as_secs()).serialize(serializer)
}

pub(crate) fn millis<S: Serializer>(
    duration: &Option<Duration>,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    duration
        .map(|d| ((1_000_000_000 * d.as_secs() + u64::from(d.subsec_nanos())) / 1_000_000))
        .serialize(serializer)
}
