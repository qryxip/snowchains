use failure::{Backtrace, Fail};

use std::fmt;

#[derive(Debug)]
pub struct ParseFieldError<I: fmt::Display + AsRef<str>> {
    input: I,
    cause: combine::easy::Errors<char, I, usize>,
    grammer: &'static str,
    backtrace: Backtrace,
}

impl<I: fmt::Display + AsRef<str>> ParseFieldError<I> {
    pub(crate) fn new(
        input: I,
        cause: combine::easy::Errors<char, I, usize>,
        grammer: &'static str,
    ) -> Self {
        Self {
            input,
            cause,
            grammer,
            backtrace: Backtrace::new(),
        }
    }
}

impl<'a> ParseFieldError<&'a str> {
    pub(crate) fn into_owned(self) -> ParseFieldError<String> {
        ParseFieldError {
            input: self.input.to_owned(),
            cause: self.cause.map_range(ToOwned::to_owned),
            grammer: self.grammer,
            backtrace: self.backtrace,
        }
    }
}

impl<I: fmt::Display + AsRef<str>> fmt::Display for ParseFieldError<I> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            fmt,
            "failed to parse {:?} at {}.\ncauses:",
            self.input.as_ref(),
            self.cause.position,
        )?;
        for error in &self.cause.errors {
            match error {
                combine::easy::Error::Unexpected(c) => writeln!(fmt, "- unexpected `{}`", c),
                combine::easy::Error::Expected(s) => writeln!(fmt, "- expected `{}`", s),
                combine::easy::Error::Message(s) => writeln!(fmt, "- {}", s),
                combine::easy::Error::Other(e) => writeln!(fmt, "- {}", e),
            }?;
        }
        write!(fmt, "grammer:\n```\n{}```\n ", self.grammer)
    }
}

impl<I: fmt::Display + fmt::Debug + AsRef<str> + Send + Sync + 'static> Fail
    for ParseFieldError<I>
{
    fn cause(&self) -> Option<&dyn Fail> {
        Some(&self.cause)
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        Some(&self.backtrace)
    }
}

#[cfg(test)]
mod tests {
    use crate::util::combine::ParseFieldError;

    use difference::assert_diff;

    #[test]
    fn test_display() {
        use combine::stream::state::{IndexPositioner, State};
        use combine::{eof, Parser as _};

        static GRAMMER: &str = "Terminal ::= ''\n";
        static INPUT: &str = "foo";
        static EXPECTED: &str = r#"failed to parse "foo" at 0.
causes:
- unexpected `f`
- expected `end of input`
grammer:
```
Terminal ::= ''
```
 "#;

        let err = eof()
            .easy_parse(State::with_positioner(INPUT, IndexPositioner::new()))
            .unwrap_err();
        let err = ParseFieldError::new(INPUT, err, GRAMMER);
        assert_diff!(&err.to_string(), EXPECTED, "\n", 0);
    }
}
