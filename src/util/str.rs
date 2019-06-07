use heck::{CamelCase as _, KebabCase as _, MixedCase as _, SnakeCase as _};

#[derive(Debug, Clone, Copy)]
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
