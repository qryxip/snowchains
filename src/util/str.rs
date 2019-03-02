use heck::{CamelCase as _, KebabCase as _, MixedCase as _, SnakeCase as _};

use std::fmt;
use std::str::FromStr;

#[derive(Clone, Copy)]
pub(crate) enum CaseConversion {
    Lower,
    Upper,
    Snake,
    Kebab,
    Mixed,
    Pascal,
}

impl CaseConversion {
    pub(crate) fn apply(self, s: &str) -> String {
        match self {
            CaseConversion::Lower => s.to_lowercase(),
            CaseConversion::Upper => s.to_uppercase(),
            CaseConversion::Snake => s.to_snake_case(),
            CaseConversion::Kebab => s.to_kebab_case(),
            CaseConversion::Mixed => s.to_mixed_case(),
            CaseConversion::Pascal => s.to_camel_case(),
        }
    }
}

impl fmt::Display for CaseConversion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CaseConversion::Lower => "lower_case".fmt(f),
            CaseConversion::Upper => "upper_case".fmt(f),
            CaseConversion::Snake => "snake_case".fmt(f),
            CaseConversion::Kebab => "kebab_case".fmt(f),
            CaseConversion::Mixed => "mixed_case".fmt(f),
            CaseConversion::Pascal => "pascal_case".fmt(f),
        }
    }
}

impl FromStr for CaseConversion {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, &'static str> {
        match s {
            "lower_case" => Ok(CaseConversion::Lower),
            "upper_case" => Ok(CaseConversion::Upper),
            "snake_case" => Ok(CaseConversion::Snake),
            "kebab_case" => Ok(CaseConversion::Kebab),
            "mixed_case" => Ok(CaseConversion::Mixed),
            "pascal_case" => Ok(CaseConversion::Pascal),
            _ => Err(
                "expected \"lower_case\", \"upper_case\", \"snake_case\", \"kebab_case\", \
                 \"mixed_case\", or \"pascal_case\"",
            ),
        }
    }
}
