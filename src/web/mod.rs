pub(crate) mod cookie_storage;
pub(crate) mod credentials;

use heck::{CamelCase as _, KebabCase as _, MixedCase as _, SnakeCase as _};
use serde::Serialize;
use std::time::Duration;

pub(crate) const SESSION_TIMEOUT: Duration = Duration::from_secs(30);

#[derive(Debug, Serialize)]
pub(crate) struct CaseConversions {
    pub(crate) original: String,
    pub(crate) lower: String,
    pub(crate) upper: String,
    pub(crate) snake: String,
    pub(crate) kebab: String,
    pub(crate) mixed: String,
    pub(crate) pascal: String,
}

impl CaseConversions {
    pub(crate) fn new(s: impl AsRef<str>) -> Self {
        let s = s.as_ref();
        Self {
            original: s.to_owned(),
            lower: s.to_lowercase(),
            upper: s.to_uppercase(),
            snake: s.to_snake_case(),
            kebab: s.to_kebab_case(),
            mixed: s.to_mixed_case(),
            pascal: s.to_camel_case(),
        }
    }
}
