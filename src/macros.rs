pub(crate) trait TryZero {
    fn zero() -> Self;
}

impl<T> TryZero for Option<T> {
    fn zero() -> Self {
        None
    }
}

macro_rules! guard {
    ($p:expr) => {
        if !$p {
            return crate::macros::TryZero::zero();
        }
    };
}

macro_rules! plural {
    ($n:expr, $singular:expr, $plural:expr) => {
        format_args!("{} {}", $n, if $n > 1 { $plural } else { $singular })
    };
}

macro_rules! lazy_regex {
    ($expr:expr) => {
        ::once_cell::sync_lazy!(::regex::Regex::new($expr).unwrap())
    };
}

macro_rules! selector {
    ($($tt:tt)*) => {{
        enum Gen {}

        impl Gen {
            ::snowchains_proc_macros::def_gen_predicate!($($tt)*);
        }

        Gen::gen()
    }};
}
