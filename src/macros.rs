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

macro_rules! derive_from {
    ($t:ident :: $v:ident <- $e:ty) => {
        impl From<$e> for $t {
            fn from(e: $e) -> Self {
                $t::$v(e.into())
            }
        }
    };
    ($($t:ident :: $v:ident <- $e:ty),*) => {
        $(
            derive_from!($t :: $v <- $e);
        )*
    };
    ($($t:ident :: $v:ident <- $e:ty),*,) => {
        $(
            derive_from!($t :: $v <- $e);
        )*
    };
}

macro_rules! lazy_regex {
    ($expr:expr) => {
        sync_lazy!(::regex::Regex::new($expr).unwrap())
    };
}
