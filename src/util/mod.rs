pub(crate) mod de;
pub(crate) mod ser;
pub(crate) mod std_unstable;

use std::borrow::Cow;
use std::io::{self, Read};
use std::str;

/// Returns a `String` read from `read`.
pub(crate) fn string_from_read(mut read: impl Read, capacity: usize) -> io::Result<String> {
    let mut buf = String::with_capacity(capacity);
    read.read_to_string(&mut buf)?;
    Ok(buf)
}

/// Prints `s` ignoring a trailing newline if it exists.
pub(crate) fn eprintln_trimming_trailing_newline(s: &str) {
    eprintln!("{}", trim_trailing_newline(s));
}

fn trim_trailing_newline(s: &str) -> Cow<str> {
    if s.ends_with("\r\n") {
        let n = s.len();
        String::from_utf8_lossy(&s.as_bytes()[..n - 2])
    } else if s.ends_with('\n') {
        let n = s.len();
        String::from_utf8_lossy(&s.as_bytes()[..n - 1])
    } else {
        s.into()
    }
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

#[cfg(test)]
mod tests {
    #[test]
    fn it_trims_a_trailing_newline() {
        assert_eq!("aaa", super::trim_trailing_newline("aaa\r\n"));
        assert_eq!("bbb", super::trim_trailing_newline("bbb\n"));
        assert_eq!("ccc", super::trim_trailing_newline("ccc"));
        assert_eq!("ddd\r", super::trim_trailing_newline("ddd\r"));
    }
}
