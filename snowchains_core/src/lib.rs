//! # Usage
//!
//! See `examples`.

#[macro_export]
macro_rules! color_spec {
    ($($tt:tt)*) => {
        $crate::_color_spec_inner!(@acc(::termcolor::ColorSpec::new().set_reset(false)), @rest($($tt)*))
    };
}

#[macro_export]
macro_rules! _color_spec_inner {
    (@acc($acc:expr), @rest()) => {
        $acc
    };
    (@acc($acc:expr), @rest(, $($rest:tt)*)) => {
        $crate::_color_spec_inner!(@acc($acc), @rest($($rest)*))
    };
    (@acc($acc:expr), @rest(Fg($color:expr) $($rest:tt)*)) => {
        $crate::_color_spec_inner!(@acc($acc.set_fg(::std::option::Option::Some($color))), @rest($($rest)*))
    };
    (@acc($acc:expr), @rest(Bg($color:expr) $($rest:tt)*)) => {
        $crate::_color_spec_inner!(@acc($acc.set_bg(::std::option::Option::Some($color))), @rest($($rest)*))
    };
    (@acc($acc:expr), @rest(Bold $($rest:tt)*)) => {
        $crate::_color_spec_inner!(@acc($acc.set_bold(true)), @rest($($rest)*))
    };
    (@acc($acc:expr), @rest(Italic $($rest:tt)*)) => {
        $crate::_color_spec_inner!(@acc($acc.set_italic(true)), @rest($($rest)*))
    };
    (@acc($acc:expr), @rest(Underline $($rest:tt)*)) => {
        $crate::_color_spec_inner!(@acc($acc.set_underline(true)), @rest($($rest)*))
    };
    (@acc($acc:expr), @rest(Intense $($rest:tt)*)) => {
        $crate::_color_spec_inner!(@acc($acc.set_intense(true)), @rest($($rest)*))
    };
}

pub mod judge;
pub mod testsuite;
pub mod web;
