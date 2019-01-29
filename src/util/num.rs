use num_traits::Float;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Deserializer};
use serde_derive::Serialize;

use std::num::ParseFloatError;
use std::str::FromStr;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Copy, Debug, Serialize)]
#[serde(transparent)]
pub(crate) struct PositiveFinite<F: Float> {
    // Constraint:
    // - `inner.is_sign_positive() && inner.is_finite()`
    inner: F,
}

impl<F: Float> PositiveFinite<F> {
    pub(crate) fn try_new(value: F) -> std::result::Result<Self, &'static str> {
        if value.is_sign_positive() && value.is_finite() {
            Ok(Self { inner: value })
        } else {
            Err("the value must be positive and finite")
        }
    }
}

impl<F: Float + FromStr<Err = ParseFloatError>> FromStr for PositiveFinite<F> {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, String> {
        let value = s.parse::<F>().map_err(|e| e.to_string())?;
        Self::try_new(value).map_err(Into::into)
    }
}

impl<'de, F: Float + DeserializeOwned> Deserialize<'de> for PositiveFinite<F> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        let value = F::deserialize(deserializer)?;
        Self::try_new(value).map_err(serde::de::Error::custom)
    }
}

impl Into<f64> for PositiveFinite<f64> {
    fn into(self) -> f64 {
        self.inner
    }
}
