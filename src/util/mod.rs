pub(crate) mod de;
pub(crate) mod ser;
pub(crate) mod std_unstable;

use std::io::{self, Read};

/// Returns a `String` read from `read`.
pub(crate) fn string_from_read(mut read: impl Read, capacity: usize) -> io::Result<String> {
    let mut buf = String::with_capacity(capacity);
    read.read_to_string(&mut buf)?;
    Ok(buf)
}

pub(crate) trait OkAsRefOr {
    type Item;
    /// Get the value `Ok(&x)` if `Some(x) = self`.
    ///
    /// # Errors
    ///
    /// Returns `Err(e)` if `self` is `None`.
    fn ok_as_ref_or<E>(&self, e: E) -> Result<&Self::Item, E>;
}

impl<T> OkAsRefOr for Option<T> {
    type Item = T;

    fn ok_as_ref_or<E>(&self, e: E) -> Result<&T, E> {
        match self {
            Some(x) => Ok(x),
            None => Err(e),
        }
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum ScalarOrArray<T> {
    Scalar(T),
    Array(Vec<T>),
}
