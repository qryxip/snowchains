pub mod fs;

use std::str;
use std::borrow::Cow;
use std::io::{self, Read};

/// Returns a `String` read from `read`.
pub fn string_from_read<R: Read>(mut read: R, capacity: usize) -> io::Result<String> {
    let mut buf = String::with_capacity(capacity);
    read.read_to_string(&mut buf)?;
    Ok(buf)
}

/// Prints `s` ignoring a trailing newline if it exists.
pub fn eprintln_trimming_trailing_newline(s: &str) {
    eprintln!("{}", trim_trailing_newline(s));
}

pub(self) fn trim_trailing_newline(s: &str) -> Cow<str> {
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

pub fn cfg_windows() -> bool {
    cfg!(windows)
}

pub fn is_default<T: Default + PartialEq>(x: &T) -> bool {
    x == &T::default()
}

pub trait OkAsRefOr {
    type Item;
    /// Get the value `Ok(&x)` if `Some(ref x) = self`.
    ///
    /// # Errors
    ///
    /// Returns `Err(e)` if `self` is `None`.
    fn ok_as_ref_or<E>(&self, e: E) -> Result<&Self::Item, E>;
}

impl<T> OkAsRefOr for Option<T> {
    type Item = T;

    fn ok_as_ref_or<E>(&self, e: E) -> Result<&T, E> {
        match *self {
            Some(ref x) => Ok(x),
            None => Err(e),
        }
    }
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
