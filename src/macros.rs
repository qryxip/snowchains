macro_rules! guard {
    ($p:expr) => {
        if !$p {
            return crate::macros::TryZero::try_zero();
        }
    };
}

pub(crate) trait TryZero {
    fn try_zero() -> Self;
}

impl<T> TryZero for Option<T> {
    fn try_zero() -> Self {
        None
    }
}

impl<T, E: Default> TryZero for std::result::Result<T, E> {
    fn try_zero() -> Self {
        Err(E::default())
    }
}

macro_rules! plural {
    ($n:expr, $singular:expr, $plural:expr) => {
        format_args!("{} {}", $n, if $n > 1 { $plural } else { $singular })
    };
}

macro_rules! lazy_regex {
    ($expr:expr) => {{
        static REGEX: ::once_cell::sync::Lazy<::regex::Regex> =
            ::once_cell::sync_lazy!(::regex::Regex::new($expr).unwrap());
        &REGEX
    }};
    ($expr:expr,) => {
        lazy_regex!($expr)
    };
}

macro_rules! selector {
    ($selectors:literal) => {{
        static SELECTOR: ::once_cell::sync::Lazy<::scraper::selector::Selector> =
            ::once_cell::sync_lazy!(::scraper::selector::Selector::parse($selectors).unwrap());
        &SELECTOR
    }};
    ($selectors:literal,) => {
        selector!($selectors)
    };
}
