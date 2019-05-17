use std::fmt;

pub(crate) trait OptionDisplayExt {
    type Item: fmt::Display;
    fn fmt_display_or(&self, or: &'static str) -> FmtDisplayOr<&Self::Item>;
}

impl<T: fmt::Display> OptionDisplayExt for Option<T> {
    type Item = T;

    fn fmt_display_or(&self, or: &'static str) -> FmtDisplayOr<&T> {
        FmtDisplayOr {
            display: self.as_ref(),
            or,
        }
    }
}

pub(crate) trait OptionDebugExt {
    type Item: fmt::Debug;
    fn fmt_debug_or(&self, or: &'static str) -> FmtDebugOr<&Self::Item>;
}

impl<T: fmt::Debug> OptionDebugExt for Option<T> {
    type Item = T;

    fn fmt_debug_or(&self, or: &'static str) -> FmtDebugOr<&T> {
        FmtDebugOr {
            debug: self.as_ref(),
            or,
        }
    }
}

#[derive(Debug)]
pub(crate) struct FmtDisplayOr<T: fmt::Display> {
    display: Option<T>,
    or: &'static str,
}

impl<T: fmt::Display> fmt::Display for FmtDisplayOr<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match &self.display {
            None => fmt::Display::fmt(self.or, fmt),
            Some(display) => fmt::Display::fmt(display, fmt),
        }
    }
}

#[derive(Debug)]
pub(crate) struct FmtDebugOr<T: fmt::Debug> {
    debug: Option<T>,
    or: &'static str,
}

impl<T: fmt::Debug> fmt::Display for FmtDebugOr<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match &self.debug {
            None => fmt::Display::fmt(self.or, fmt),
            Some(debug) => fmt::Debug::fmt(debug, fmt),
        }
    }
}
