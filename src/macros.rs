macro_rules! ensure_opt {
    ($x:expr) => {
        if !$x {
            return None;
        }
    };
}

macro_rules! plural {
    ($n:expr, $singular:expr, $plural:expr) => {
        format_args!("{} {}", $n, if $n > 1 { $plural } else { $singular })
    };
}

macro_rules! lazy_regex {
    ($expr:expr) => {{
        use once_cell::sync_lazy;
        sync_lazy!(::regex::Regex::new($expr).unwrap())
    }};
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
