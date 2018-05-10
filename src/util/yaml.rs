use regex::Regex;
use serde::{self, Deserialize, Deserializer, Serialize, Serializer};

use std::borrow::Cow;
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

pub(crate) fn repr_string(s: &str) -> Cow<str> {
    let s = s.trim();
    if [':', '{', '}', '>', '|'].iter().any(|&c| s.starts_with(c)) || s.ends_with(':')
        || ['[', ']', '\\', '\'', '"', '#', ',']
            .iter()
            .any(|&c| s.contains(c)) || s == "~"
    {
        Cow::from(format!("{:?}", s))
    } else {
        Cow::from(s)
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    #[test]
    fn it_escapes_a_string() {
        fn is_borrowed<T: ToOwned + ?Sized>(x: &Cow<T>) -> bool {
            match *x {
                Cow::Borrowed(_) => true,
                Cow::Owned(_) => false,
            }
        }

        assert!(is_borrowed(&super::repr_string("foo/{}:/bar")));
        assert!(is_borrowed(&super::repr_string("~/foo/bar")));
        assert!(!is_borrowed(&super::repr_string("~")));
        assert!(!is_borrowed(&super::repr_string("[")));
        assert!(!is_borrowed(&super::repr_string("]")));
        assert!(!is_borrowed(&super::repr_string("\\")));
        assert!(!is_borrowed(&super::repr_string("'")));
        assert!(!is_borrowed(&super::repr_string("\"")));
        assert!(!is_borrowed(&super::repr_string("#")));
        assert!(!is_borrowed(&super::repr_string(",")));
        assert!(!is_borrowed(&super::repr_string(" :foo ")));
        assert!(!is_borrowed(&super::repr_string(" {foo ")));
        assert!(!is_borrowed(&super::repr_string(" }foo ")));
        assert!(!is_borrowed(&super::repr_string(" >foo ")));
        assert!(!is_borrowed(&super::repr_string(" |foo ")));
        assert!(!is_borrowed(&super::repr_string(" foo: ")));
    }
}
