use serde::de::{Deserialize, Deserializer};
use serde::ser::{Serialize, Serializer};

use std::fmt::Write as _Write;
use std::num::FpCategory;
use std::time::Duration;
use std::{cmp, u64};

pub(crate) fn ser_secs<S: Serializer>(
    dur: &Option<Duration>,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    dur.map(format_secs).serialize(serializer)
}

fn format_secs(dur: Duration) -> String {
    let (secs, nanos) = (dur.as_secs(), dur.subsec_nanos());
    let mut r = secs.to_string();
    if nanos > 0 {
        write!(r, ".{:09}", nanos).unwrap();
        while r.ends_with('0') {
            r.pop();
        }
    }
    r += "s";
    r
}

pub(crate) fn ser_millis<S: Serializer>(
    dur: &Option<Duration>,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    dur.map(format_millis).serialize(serializer)
}

fn format_millis(dur: Duration) -> String {
    let (secs, nanos) = (dur.as_secs(), dur.subsec_nanos());
    let millis = 1000 * u128::from(secs) + u128::from(nanos) / 1_000_000;
    let submilli_nanos = nanos % 1_000_000;
    let mut r = millis.to_string();
    if submilli_nanos > 0 {
        write!(r, ".{:06}", submilli_nanos).unwrap();
        while r.ends_with('0') {
            r.pop();
        }
    }
    r += "ms";
    r
}

pub(crate) fn de_secs<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> std::result::Result<Option<Duration>, D::Error> {
    Option::<String>::deserialize(deserializer).and_then(|s| match s {
        None => Ok(None),
        Some(s) => parse_secs(&s).map(Some).map_err(serde::de::Error::custom),
    })
}

pub(crate) fn parse_secs(s: &str) -> std::result::Result<Duration, String> {
    fn extract_unit(s: &str) -> std::result::Result<(&str, f64), String> {
        if s.ends_with("das") {
            Ok((&s[..s.len() - 3], 1e10))
        } else if s.ends_with("hs") {
            Ok((&s[..s.len() - 2], 1e11))
        } else if s.ends_with("ks") {
            Ok((&s[..s.len() - 2], 1e12))
        } else if s.ends_with("Ms") {
            Ok((&s[..s.len() - 2], 1e15))
        } else if s.ends_with("Gs") {
            Ok((&s[..s.len() - 2], 1e18))
        } else if s.ends_with("Ts") {
            Ok((&s[..s.len() - 2], 1e21))
        } else if s.ends_with("Ps") {
            Ok((&s[..s.len() - 2], 1e24))
        } else if s.ends_with("Es") {
            Ok((&s[..s.len() - 2], 1e27))
        } else if s.ends_with("Zs") {
            Ok((&s[..s.len() - 2], 1e30))
        } else if s.ends_with("Ys") {
            Ok((&s[..s.len() - 2], 1e33))
        } else if s.ends_with("ds") {
            Ok((&s[..s.len() - 2], 1e8))
        } else if s.ends_with("cs") {
            Ok((&s[..s.len() - 2], 1e7))
        } else if s.ends_with("ms") {
            Ok((&s[..s.len() - 2], 1e6))
        } else if s.ends_with("μs") {
            Ok((&s[..s.len() - 3], 1e3))
        } else if s.ends_with("us") {
            Ok((&s[..s.len() - 2], 1e3))
        } else if s.ends_with("ns") {
            Ok((&s[..s.len() - 2], 1e0))
        } else if s.ends_with("ps") {
            Ok((&s[..s.len() - 2], 1e-3))
        } else if s.ends_with("fs") && !s.ends_with("infs") {
            Ok((&s[..s.len() - 2], 1e-6))
        } else if s.ends_with("as") {
            Ok((&s[..s.len() - 2], 1e-9))
        } else if s.ends_with("zs") {
            Ok((&s[..s.len() - 2], 1e-12))
        } else if s.ends_with("ys") {
            Ok((&s[..s.len() - 2], 1e-15))
        } else if s.ends_with('s') {
            Ok((&s[..s.len() - 1], 1e9))
        } else {
            Err("invalid or missing unit".to_owned())
        }
    }

    let (s, k) = extract_unit(s.trim())?;
    let nanos = k * s.trim_end().parse::<f64>().map_err(|e| e.to_string())?;
    if nanos.is_nan() {
        Err("NaN".to_owned())
    } else if nanos.is_sign_negative() {
        Err("negative".to_owned())
    } else if nanos.classify() == FpCategory::Zero {
        Ok(Duration::new(0, 0))
    } else if nanos.is_infinite() {
        Ok(Duration::new(u64::MAX, 999_999_999))
    } else {
        let nanos = nanos as u128;
        let secs = cmp::min(nanos / 1_000_000_000, u128::from(u64::MAX)) as u64;
        let subsec_nanos = if nanos > 1_000_000_000 * u128::from(u64::MAX) {
            999_999_999
        } else {
            (nanos % 1_000_000_000) as u32
        };
        Ok(Duration::new(secs, subsec_nanos))
    }
}

pub(crate) trait MillisRoundedUp {
    fn millis_rounded_up(self) -> u128;
}

impl MillisRoundedUp for Duration {
    fn millis_rounded_up(self) -> u128 {
        (1_000_000_000 * u128::from(self.as_secs()) + u128::from(self.subsec_nanos()) + 999_999)
            / 1_000_000
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;
    use std::u64;

    #[test]
    fn test_parse_secs() {
        fn test(s: &str, expected: std::result::Result<Duration, String>) {
            assert_eq!(super::parse_secs(s), expected.map_err(Into::into));
        }

        test("NaNs", Err("NaN".to_owned()));
        test("+NaNs", Err("NaN".to_owned()));
        test("-NaNs", Err("NaN".to_owned()));
        test("-infs", Err("negative".to_owned()));
        test("-42s", Err("negative".to_owned()));

        test("10Ts", Ok(Duration::new(10_000_000_000_000, 0)));
        test("100Ts", Ok(Duration::new(99_999_999_999_999, 991_611_392)));
        test("100Es", Ok(Duration::new(u64::MAX, 999_999_999)));
        test("infs", Ok(Duration::new(u64::MAX, 999_999_999)));
        test("+infs", Ok(Duration::new(u64::MAX, 999_999_999)));
        test("0s", Ok(Duration::new(0, 0)));
        test(" 10 s ", Ok(Duration::new(10, 0)));
        test("42s", Ok(Duration::new(42, 0)));
        test("123.456s", Ok(Duration::new(123, 456_000_000)));
        test("1E3s", Ok(Duration::new(1000, 0)));
        test("42ms", Ok(Duration::new(0, 42_000_000)));
        test("123.456ms", Ok(Duration::new(0, 123_456_000)));
        test("1E3ms", Ok(Duration::new(1, 0)));
        test("42μs", Ok(Duration::new(0, 42_000)));
        test("123.456μs", Ok(Duration::new(0, 123_456)));
        test("1E3μs", Ok(Duration::new(0, 1_000_000)));
        test("42ns", Ok(Duration::new(0, 42)));
        test("123.456ns", Ok(Duration::new(0, 123)));
        test("1E3ns", Ok(Duration::new(0, 1000)));
        test("1000ps", Ok(Duration::new(0, 1)));
        test("100ps", Ok(Duration::new(0, 0)));
    }

    #[test]
    fn test_format_secs() {
        fn test(dur: Duration, expected: &str) {
            assert_eq!(super::format_secs(dur), expected);
        }

        test(Duration::from_secs(42), "42s");
        test(Duration::from_millis(42), "0.042s");
        test(Duration::from_nanos(42), "0.000000042s");
        test(Duration::new(1234, 567_800_000), "1234.5678s");
    }

    #[test]
    fn test_format_millis() {
        fn test(dur: Duration, expected: &str) {
            assert_eq!(super::format_millis(dur), expected);
        }

        test(Duration::from_secs(42), "42000ms");
        test(Duration::from_millis(42), "42ms");
        test(Duration::from_nanos(42), "0.000042ms");
        test(Duration::new(1234, 567_800_000), "1234567.8ms");
    }
}
