use termcolor::ColorSpec;

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

macro_rules! color {
    () => {
        &mut ::termcolor::ColorSpec::new()
    };
    (fg($fg:ident)) => {
        ::termcolor::ColorSpec::new().set_fg(Some(::termcolor::Color::$fg))
    };
    (bg($bg:ident)) => {
        ::termcolor::ColorSpec::new().set_bg(Some(::termcolor::Color::$bg))
    };
    (fg($fg:ident), bg($bg:ident)) => {
        ::termcolor::ColorSpec::new()
            .set_fg(Some(::termcolor::Color::$fg))
            .set_bg(Some(::termcolor::Color::$bg))
    };
    ($attr:ident) => {
        crate::macros::ColorSpecWrapper(color!()).$attr().0
    };
    (fg($fg:ident), $attr:ident) => {
        crate::macros::ColorSpecWrapper(color!(fg($fg))).$attr().0
    };
    (bg($bg:ident), $attr:ident) => {
        crate::macros::ColorSpecWrapper(color!(bg($bg))).$attr().0
    };
    (fg($fg:ident), bg($bg:ident), $attr:ident) => {
        crate::macros::ColorSpecWrapper(color!(fg($fg), bg($bg)))
            .$attr()
            .0
    };
    ($attr1:ident, $attr2:ident) => {
        crate::macros::ColorSpecWrapper(color!())
            .$attr1()
            .$attr2()
            .0
    };
    (fg($fg:ident), $attr1:ident, $attr2:ident) => {
        crate::macros::ColorSpecWrapper(color!(fg($fg)))
            .$attr1()
            .$attr2()
            .0
    };
    (bg($bg:ident), $attr1:ident, $attr2:ident) => {
        crate::macros::ColorSpecWrapper(color!(bg($bg)))
            .$attr1()
            .$attr2()
            .0
    };
    (fg($fg:ident), bg($bg:ident), $attr1:ident, $attr2:ident) => {
        crate::macros::ColorSpecWrapper(color!(fg($fg), bg($bg)))
            .$attr1()
            .$attr2()
            .0
    };
    ($attr1:ident, $attr2:ident, $attr3:ident) => {
        crate::macros::ColorSpecWrapper(color!())
            .$attr1()
            .$attr2()
            .$attr3()
            .0
    };
    (fg($fg:ident), $attr1:ident, $attr2:ident, $attr3:ident) => {
        crate::macros::ColorSpecWrapper(color!(fg($fg)))
            .$attr1()
            .$attr2()
            .$attr3()
            .0
    };
    (bg($bg:ident), $attr1:ident, $attr2:ident, $attr3:ident) => {
        crate::macros::ColorSpecWrapper(color!(bg($bg)))
            .$attr1()
            .$attr2()
            .$attr3()
            .0
    };
    (fg($fg:ident), bg($bg:ident), $attr1:ident, $attr2:ident, $attr3:ident) => {
        crate::macros::ColorSpecWrapper(color!(fg($fg), bg($bg)))
            .$attr1()
            .$attr2()
            .$attr3()
            .0
    };
}

pub(crate) struct ColorSpecWrapper<'a>(pub &'a mut ColorSpec);

impl ColorSpecWrapper<'_> {
    #[inline]
    pub(crate) fn bold(self) -> Self {
        Self(self.0.set_bold(true))
    }

    #[inline]
    pub(crate) fn intense(self) -> Self {
        Self(self.0.set_intense(true))
    }

    #[inline]
    pub(crate) fn underline(self) -> Self {
        Self(self.0.set_underline(true))
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
