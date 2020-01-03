use crate::judging::text::inner::{TextDiffInner, TextInner};
use crate::terminal::{HasTermProps, WriteExt as _};
use crate::testsuite::FloatErrors;

use crossbeam_utils::atomic::AtomicCell;
use itertools::{EitherOrBoth, Itertools as _};
use once_cell::sync::Lazy;
use serde::{Serialize, Serializer};
use smallvec::SmallVec;
use termcolor::WriteColor;
use termcolor::{Color, ColorSpec};

use std::collections::VecDeque;
use std::num::NonZeroUsize;
use std::str::FromStr;
use std::sync::Arc;
use std::{cmp, f64, fmt, io, iter, mem};

pub(super) fn actual_or_diff(
    expected: Arc<String>,
    actual: Arc<String>,
    float_errors: Option<FloatErrors>,
) -> std::result::Result<Text, TextDiff> {
    if let Some(float_errors) = float_errors {
        let diff = TextDiff::new(expected, actual, Some(float_errors));
        diff.0.try_shrink_right().map(Text).map_err(TextDiff)
    } else if expected == actual {
        Ok(Text::new(actual))
    } else {
        Err(TextDiff::new(expected, actual, None))
    }
}

#[derive(Debug)]
pub(super) struct Text(TextInner);

impl Text {
    pub(super) fn new(string: Arc<String>) -> Self {
        fn parse_words_to_slice(s: &str) -> Box<[Word]> {
            parse_words(s, false).collect()
        }

        Self(TextInner::new(string, parse_words_to_slice))
    }

    pub(super) fn is_empty(&self) -> bool {
        self.0.string().is_empty()
    }

    pub(super) fn str_len(&self) -> usize {
        self.0.string().len()
    }

    pub(super) fn print_pretty(&self, mut wtr: impl WriteColor) -> io::Result<()> {
        for word in self.0.words() {
            match word {
                Word::Plain(s) => wtr.write_str(s)?,
                Word::Float { string: s, .. } => {
                    wtr.set_color(color!(fg(Cyan), intense))?;
                    wtr.write_str(s)?;
                    wtr.reset()?;
                }
                Word::Spcs(n) => wtr.write_spaces(n.get())?,
                Word::TrailingSpcs(n) => {
                    wtr.set_color(color!(bg(Yellow), intense, bold))?;
                    wtr.write_spaces(n.get())?;
                    wtr.reset()?;
                }
                Word::Tab => {
                    wtr.set_color(color!(fg(Yellow), intense, bold))?;
                    wtr.write_str("\\t")?;
                    wtr.reset()?;
                }
                Word::Cr => {
                    wtr.set_color(color!(fg(Yellow), intense, bold))?;
                    wtr.write_str("\\r")?;
                    wtr.reset()?;
                }
                Word::UnicodeEscape(s) => {
                    wtr.set_color(color!(fg(Yellow), intense, bold))?;
                    for c in s.chars() {
                        write!(wtr, "\\u{:06x}", c as u32)?;
                    }
                    wtr.reset()?;
                }
                Word::Lf => writeln!(wtr)?,
            }
        }
        if self.0.words().last().map_or(true, |w| !w.is_lf()) {
            wtr.set_color(color!(fg(Yellow), intense, bold))?;
            wtr.write_str("<noeol>\n")?;
            wtr.reset()?;
        }
        Ok(())
    }

    pub(super) fn partial_debug<'a>(&'a self) -> impl fmt::Debug + 'a {
        struct Debug<'a>(&'a TextInner);

        impl fmt::Debug for Debug<'_> {
            fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                fmt.debug_tuple("Text")
                    .field(&self.0.partial_debug())
                    .finish()
            }
        }

        Debug(&self.0)
    }
}

impl Serialize for Text {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        #[derive(Serialize)]
        struct Repr<'a> {
            string: &'a str,
            words: &'a [Word<'a>],
        }

        let string = self.0.string();
        let words = self.0.words();
        Repr { string, words }.serialize(serializer)
    }
}

#[derive(Debug)]
pub(super) struct TextDiff(TextDiffInner);

impl TextDiff {
    fn new(expected: Arc<String>, actual: Arc<String>, float_errors: Option<FloatErrors>) -> Self {
        struct Line<'a>(Box<[Word<'a>]>);

        impl PartialEq for Line<'_> {
            fn eq(&self, other: &Self) -> bool {
                let (mut left, mut right) = (self.0.iter(), other.0.iter());
                loop {
                    match (left.next(), right.next()) {
                        (Some(left), Some(right)) => {
                            let same = match (*left, *right) {
                                (Word::Float { value: v1, .. }, Word::Float { value: v2, .. }) => {
                                    let FloatErrorsNormalized {
                                        relative: r,
                                        absolute: d,
                                    } = FLOAT_ERRORS.load();
                                    (v1 - v2).abs() <= d || ((v1 - v2) / v2).abs() <= r
                                }
                                (Word::Spcs(n1), Word::Spcs(n2))
                                | (Word::Spcs(n1), Word::TrailingSpcs(n2))
                                | (Word::TrailingSpcs(n1), Word::Spcs(n2))
                                | (Word::TrailingSpcs(n1), Word::TrailingSpcs(n2)) => n1 == n2,
                                (Word::Plain(s1), Word::Plain(s2))
                                | (Word::UnicodeEscape(s1), Word::UnicodeEscape(s2)) => s1 == s2,
                                (Word::Tab, Word::Tab)
                                | (Word::Cr, Word::Cr)
                                | (Word::Lf, Word::Lf) => true,
                                _ => false,
                            };
                            if !same {
                                break false;
                            }
                        }
                        (None, Some(_)) | (Some(_), None) => break false,
                        (None, None) => break true,
                    }
                }
            }
        }

        #[derive(Clone, Copy, Default)]
        struct FloatErrorsNormalized {
            relative: f64,
            absolute: f64,
        }

        static FLOAT_ERRORS: Lazy<AtomicCell<FloatErrorsNormalized>> =
            Lazy::new(|| AtomicCell::new(FloatErrorsNormalized::default()));

        if let Some(float_errors) = float_errors {
            FLOAT_ERRORS.store(FloatErrorsNormalized {
                relative: float_errors.relative.map(Into::into).unwrap_or(0.0),
                absolute: float_errors.absolute.map(Into::into).unwrap_or(0.0),
            })
        };

        Self(TextDiffInner::new(expected, actual, |expected, actual| {
            fn parse_words_to_lines(s: &str, floats: bool) -> Vec<Line> {
                let (mut ret, mut current) = (vec![], vec![]);
                for word in parse_words(s, floats) {
                    current.push(word);
                    if word.is_lf() {
                        ret.push(Line(mem::take(&mut current).into_boxed_slice()));
                    }
                }
                if !current.is_empty() {
                    ret.push(Line(mem::take(&mut current).into_boxed_slice()));
                }
                ret
            }

            let expected = parse_words_to_lines(expected, float_errors.is_some());
            let actual = parse_words_to_lines(actual, float_errors.is_some());
            diff::slice(&expected, &actual)
                .into_iter()
                .map(|diff| match diff {
                    diff::Result::Both(Line(expected), Line(actual)) => DiffLine::Both {
                        left: expected.clone(),
                        right: actual.clone(),
                    },
                    diff::Result::Left(Line(expected)) => DiffLine::Left(expected.clone()),
                    diff::Result::Right(Line(actual)) => DiffLine::Right(actual.clone()),
                })
                .collect()
        }))
    }
}

impl TextDiff {
    pub(super) fn print_pretty_with_title(
        &self,
        left_title: &str,
        right_title: &str,
        mut wtr: impl WriteColor + HasTermProps,
    ) -> io::Result<()> {
        let str_width = wtr.str_width_fn();
        let (mut left_paddings, mut right_paddings, left_max_width, right_max_width) = {
            let (mut wsl, mut wsr, mut wl, mut wr) = (vec![], vec![], 0, 0);
            for line in self.0.diff_lines() {
                let on_line =
                    |line: &[Word], widths: &mut Vec<usize>, current_width: &mut usize| {
                        let mut eol = false;
                        for word in line {
                            if let Some(width) = word.width(str_width) {
                                *current_width += width.get();
                            } else {
                                widths.push(mem::replace(current_width, 0));
                                eol = true
                            }
                        }
                        if !eol {
                            *current_width += "<noeol>".len();
                            widths.push(mem::replace(current_width, 0));
                        }
                    };
                match line {
                    DiffLine::Left(left) => on_line(left, &mut wsl, &mut wl),
                    DiffLine::Right(right) => on_line(right, &mut wsr, &mut wr),
                    DiffLine::Both { left, right } => {
                        on_line(left, &mut wsl, &mut wl);
                        on_line(right, &mut wsr, &mut wr);
                    }
                }
            }
            let left_max_width = wsl.iter().cloned().max().unwrap_or(0);
            let left_max_width = cmp::max(left_max_width, left_title.len());
            let right_max_width = wsr.iter().cloned().max().unwrap_or(0);
            let right_max_width = cmp::max(right_max_width, right_title.len());
            let mut left_paddings =
                iter::once(left_max_width - left_title.len()).collect::<VecDeque<_>>();
            let mut right_paddings =
                iter::once(right_max_width - right_title.len()).collect::<VecDeque<_>>();
            left_paddings.extend(wsl.into_iter().map(|w| left_max_width - w));
            right_paddings.extend(wsr.into_iter().map(|w| right_max_width - w));
            (
                left_paddings,
                right_paddings,
                left_max_width,
                right_max_width,
            )
        };

        fn print_words_same(
            mut wtr: impl WriteColor,
            words: &[Word],
            padding: usize,
        ) -> io::Result<()> {
            debug_assert!(words.iter().filter(|w| w.is_lf()).count() <= 1);
            wtr.reset()?;
            let mut eol = false;
            for word in words {
                match word {
                    Word::Plain(s) => wtr.write_str(s)?,
                    Word::Float { string: s, .. } => {
                        wtr.set_color(color!(fg(Cyan), intense))?;
                        wtr.write_str(s)?;
                        wtr.reset()?;
                    }
                    Word::Spcs(n) => wtr.write_spaces(n.get())?,
                    Word::TrailingSpcs(n) => {
                        wtr.set_color(color!(bg(Yellow), intense, bold))?;
                        wtr.write_spaces(n.get())?;
                        wtr.reset()?;
                    }
                    Word::Tab => {
                        wtr.set_color(color!(fg(Yellow), intense, bold))?;
                        wtr.write_str("\\t")?;
                        wtr.reset()?;
                    }
                    Word::Cr => {
                        wtr.set_color(color!(fg(Yellow), intense, bold))?;
                        wtr.write_str("\\r")?;
                        wtr.reset()?;
                    }
                    Word::UnicodeEscape(s) => {
                        wtr.set_color(color!(fg(Yellow), intense, bold))?;
                        for c in s.chars() {
                            write!(wtr, "\\u{:06x}", c as u32)?;
                        }
                        wtr.reset()?;
                    }
                    Word::Lf => eol = true,
                }
            }
            if !eol {
                wtr.set_color(color!(fg(Yellow), intense, bold))?;
                wtr.write_str("<noeol>")?;
                wtr.reset()?;
            }
            wtr.write_spaces(padding)
        }

        fn print_words_different(
            mut wtr: impl WriteColor,
            words: &[Word],
            padding: usize,
        ) -> io::Result<()> {
            debug_assert!(words.iter().filter(|w| w.is_lf()).count() <= 1);
            let mut eol = false;
            for word in words {
                match word {
                    Word::Plain(s) | Word::Float { string: s, .. } => {
                        wtr.set_color(color!(fg(Red), intense, underline))?;
                        wtr.write_str(s)?;
                    }
                    Word::Spcs(n) => {
                        wtr.set_color(color!(fg(Red), intense, underline))?;
                        wtr.write_spaces(n.get())?
                    }
                    Word::TrailingSpcs(n) => {
                        wtr.set_color(color!(fg(Red), bg(Yellow), intense, underline))?;
                        wtr.write_spaces(n.get())?
                    }
                    Word::Tab => {
                        wtr.set_color(color!(fg(Red), intense, bold, underline))?;
                        wtr.write_str("\\t")?;
                    }
                    Word::Cr => {
                        wtr.set_color(color!(fg(Red), intense, bold, underline))?;
                        wtr.write_str("\\r")?;
                    }
                    Word::UnicodeEscape(s) => {
                        wtr.set_color(color!(fg(Red), intense, bold, underline))?;
                        for c in s.chars() {
                            write!(wtr, "\\u{:06x}", c as u32)?;
                        }
                    }
                    Word::Lf => eol = true,
                }
            }
            if !eol {
                wtr.set_color(color!(fg(Red), intense, bold, underline))?;
                wtr.write_str("<noeol>")?;
            }
            wtr.reset()?;
            wtr.write_spaces(padding)
        }

        struct DifferentLines<'a> {
            left: Vec<&'a [Word<'a>]>,
            right: Vec<&'a [Word<'a>]>,
            left_max_width: usize,
            right_max_width: usize,
        }

        impl DifferentLines<'_> {
            fn pop_print(
                &mut self,
                left_paddings: &mut VecDeque<usize>,
                right_paddings: &mut VecDeque<usize>,
                mut wtr: impl WriteColor,
            ) -> io::Result<()> {
                for pair in self.left.iter().zip_longest(&self.right) {
                    wtr.write_str("│")?;
                    match pair {
                        EitherOrBoth::Both(left, right) => {
                            let left_padding = left_paddings.pop_front().unwrap();
                            let right_padding = right_paddings.pop_front().unwrap();
                            print_words_different(&mut wtr, left, left_padding)?;
                            wtr.write_str("│")?;
                            print_words_different(&mut wtr, right, right_padding)?;
                        }
                        EitherOrBoth::Left(left) => {
                            let left_padding = left_paddings.pop_front().unwrap();
                            print_words_different(&mut wtr, left, left_padding)?;
                            wtr.write_str("│")?;
                            wtr.write_spaces(self.right_max_width)?;
                        }
                        EitherOrBoth::Right(right) => {
                            let right_padding = right_paddings.pop_front().unwrap();
                            wtr.write_spaces(self.left_max_width)?;
                            wtr.write_str("│")?;
                            print_words_different(&mut wtr, right, right_padding)?;
                        }
                    }
                    wtr.write_str("│\n")?;
                }
                self.left.clear();
                self.right.clear();
                Ok(())
            }
        }

        let mut spec = ColorSpec::new();
        spec.set_fg(Some(Color::Magenta)).set_bold(true);
        wtr.write_str("│")?;
        wtr.set_color(&spec)?;
        wtr.write_str(left_title)?;
        wtr.reset()?;
        wtr.write_spaces(left_paddings.pop_front().unwrap())?;
        wtr.write_str("│")?;
        wtr.set_color(&spec)?;
        wtr.write_str(right_title)?;
        wtr.reset()?;
        wtr.write_spaces(right_paddings.pop_front().unwrap())?;
        wtr.write_str("│\n")?;

        let mut different = DifferentLines {
            left: vec![],
            right: vec![],
            left_max_width,
            right_max_width,
        };
        for line in self.0.diff_lines() {
            match line {
                DiffLine::Left(l) => different.left.push(l),
                DiffLine::Right(r) => different.right.push(r),
                DiffLine::Both { left, right } => {
                    different.pop_print(&mut left_paddings, &mut right_paddings, &mut wtr)?;
                    wtr.write_str("│")?;
                    print_words_same(&mut wtr, left, left_paddings.pop_front().unwrap())?;
                    wtr.write_str("│")?;
                    print_words_same(&mut wtr, right, right_paddings.pop_front().unwrap())?;
                    wtr.write_str("│\n")?;
                }
            }
        }
        different.pop_print(&mut left_paddings, &mut right_paddings, &mut wtr)
    }
}

impl Serialize for TextDiff {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        #[derive(Serialize)]
        struct Repr<'a> {
            expected: &'a str,
            actual: &'a str,
            diff: &'a [DiffLine<'a>],
        }

        Repr {
            expected: self.0.left_string(),
            actual: self.0.right_string(),
            diff: self.0.diff_lines(),
        }
        .serialize(serializer)
    }
}

fn parse_words(s: &str, floats: bool) -> impl Iterator<Item = Word> {
    use combine::char::char;
    use combine::parser::choice::or;
    use combine::parser::range::recognize;
    use combine::stream::state::{IndexPositioner, State};
    use combine::{choice, easy, eof, many, optional, satisfy, skip_many1, Parser};

    fn no_u0020<'a>(
        floats: bool,
    ) -> impl Parser<Input = easy::Stream<State<&'a str, IndexPositioner>>, Output = Word<'a>> {
        let plain_or_float = recognize(skip_many1(satisfy(|c: char| {
            !(c.is_whitespace() || c.is_control())
        })))
        .map(move |string| {
            if floats {
                match f64::from_str(string) {
                    Ok(value) => Word::Float { value, string },
                    Err(_) => Word::Plain(string),
                }
            } else {
                Word::Plain(string)
            }
        });
        let tab = recognize(char('\t')).map(|_| Word::Tab);
        let cr = recognize(char('\r')).map(|_| Word::Cr);
        let lf = recognize(char('\n')).map(|_| Word::Lf);
        let unicode_escape = recognize(skip_many1(satisfy(|c: char| {
            (c.is_whitespace() || c.is_control()) && ![' ', '\t', '\r', '\n'].contains(&c)
        })))
        .map(Word::UnicodeEscape);
        choice((plain_or_float, tab, cr, lf, unicode_escape))
    }

    let with_u0020s = recognize(skip_many1(char(' ')))
        .and(optional(no_u0020(floats)))
        .map::<_, SmallVec<[Word; 2]>>(|(s, word): (&str, _)| {
            let n = NonZeroUsize::new(s.len()).unwrap();
            match word {
                Some(Word::Lf) => [Word::TrailingSpcs(n), Word::Lf].into(),
                Some(word) => [Word::Spcs(n), word].into(),
                None => iter::once(Word::TrailingSpcs(n)).collect(),
            }
        });

    many::<Vec<_>, _>(or(
        with_u0020s,
        no_u0020(floats).map(|w| iter::once(w).collect()),
    ))
    .skip(eof())
    .easy_parse(State::with_positioner(s, IndexPositioner::new()))
    .unwrap_or_else(|e| unreachable!("{:?}", e))
    .0
    .into_iter()
    .flatten()
}

#[derive(Debug, Clone, Copy, Serialize)]
enum Word<'a> {
    Plain(&'a str),
    Spcs(NonZeroUsize),
    TrailingSpcs(NonZeroUsize),
    Tab,
    Cr,
    Lf,
    UnicodeEscape(&'a str),
    Float { value: f64, string: &'a str },
}

impl Word<'_> {
    fn is_lf(self) -> bool {
        match self {
            Word::Lf => true,
            _ => false,
        }
    }

    fn width(self, str_width: fn(&str) -> usize) -> Option<NonZeroUsize> {
        match self {
            Word::Plain(s) => NonZeroUsize::new(str_width(s)),
            Word::Spcs(n) | Word::TrailingSpcs(n) => Some(n),
            Word::Tab | Word::Cr => NonZeroUsize::new(2),
            Word::UnicodeEscape(s) => NonZeroUsize::new(8 * s.chars().count()),
            Word::Float { string, .. } => NonZeroUsize::new(str_width(string)),
            Word::Lf => None,
        }
    }
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "snake_case")]
enum DiffLine<'a> {
    Left(Box<[Word<'a>]>),
    Right(Box<[Word<'a>]>),
    Both {
        left: Box<[Word<'a>]>,
        right: Box<[Word<'a>]>,
    },
}

impl<'a> DiffLine<'a> {
    pub(self) fn is_both(&self) -> bool {
        match self {
            DiffLine::Both { .. } => true,
            _ => false,
        }
    }

    /// # Panics
    ///
    /// Panics if `self` is not `Both`.
    pub(self) fn ref_unwrap_both_right(&self) -> &[Word<'a>] {
        match self {
            DiffLine::Both { right, .. } => right,
            _ => panic!(),
        }
    }
}

mod inner {
    use crate::judging::text::{DiffLine, Word};

    use std::sync::Arc;
    use std::{fmt, mem};

    /// **NOTE:** this is a self-referential struct.
    #[derive(Debug)]
    pub(super) struct TextInner {
        // https://github.com/rust-lang/rfcs/blob/master/text/1857-stabilize-drop-order.md
        words: Box<[Word<'static>]>, // This `'static` is fake
        string: Arc<String>,
    }

    impl TextInner {
        pub(super) fn new(string: Arc<String>, words: impl FnOnce(&str) -> Box<[Word]>) -> Self {
            unsafe {
                Self {
                    words: mem::transmute(words(&string)),
                    string,
                }
            }
        }

        pub(super) fn string(&self) -> &str {
            &self.string
        }

        pub(super) fn words<'a>(&'a self) -> &[Word<'a>] {
            &self.words
        }

        pub(super) fn partial_debug<'a>(&'a self) -> impl fmt::Debug + 'a {
            struct Debug<'a>(&'a str);

            impl fmt::Debug for Debug<'_> {
                fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                    fmt.debug_struct("TextInner")
                        .field("words", &format_args!("_"))
                        .field("string", &self.0)
                        .finish()
                }
            }

            Debug(&self.string)
        }
    }

    /// **NOTE:** this is a self-referential struct.
    #[derive(Debug)]
    pub(super) struct TextDiffInner {
        // https://github.com/rust-lang/rfcs/blob/master/text/1857-stabilize-drop-order.md
        diff_lines: Box<[DiffLine<'static>]>, // This `'static` is fake
        strings: (Arc<String>, Arc<String>),
    }

    impl TextDiffInner {
        pub(super) fn new(
            expected: Arc<String>,
            actual: Arc<String>,
            diff_lines: impl for<'a> FnOnce(&'a str, &'a str) -> Box<[DiffLine<'a>]>,
        ) -> Self {
            unsafe {
                Self {
                    diff_lines: mem::transmute(diff_lines(&expected, &actual)),
                    strings: (expected, actual),
                }
            }
        }

        pub(super) fn left_string(&self) -> &str {
            &self.strings.0
        }

        pub(super) fn right_string(&self) -> &str {
            &self.strings.1
        }

        pub(super) fn diff_lines<'a>(&'a self) -> &[DiffLine<'a>] {
            &self.diff_lines
        }

        pub(super) fn try_shrink_right(self) -> std::result::Result<TextInner, Self> {
            if self.diff_lines.iter().all(DiffLine::is_both) {
                let words = self
                    .diff_lines
                    .iter()
                    .flat_map(DiffLine::ref_unwrap_both_right)
                    .cloned()
                    .collect::<Box<[_]>>();
                drop(self.diff_lines);
                drop(self.strings.0);
                Ok(TextInner {
                    string: self.strings.1,
                    words,
                })
            } else {
                Err(self)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::judging::text::{Text, TextDiff};
    use crate::terminal::AnsiWithProps;

    use failure::Fallible;
    use once_cell::sync::Lazy;
    use pretty_assertions::assert_eq;
    use stable_deref_trait::StableDeref;
    use termcolor::{Ansi, Color, ColorSpec, WriteColor};

    use std::convert::TryFrom;
    use std::io::Write;
    use std::str;
    use std::sync::Arc;

    #[test]
    fn it_parses_and_prints() -> Fallible<()> {
        fn test(s: &str, expected: &'static str) -> Fallible<()> {
            let text = Text::new(Arc::new(s.to_owned()));
            let mut wtr = AnsiWithProps::new();
            text.print_pretty(&mut wtr)?;
            assert_eq!(String::try_from(wtr)?, expected);
            Ok(())
        }

        static INPUT1: &str = "foo\n   bar\nbaz\n";
        static EXPECTED1: &str = "foo\n   bar\nbaz\n";

        static INPUT2: &str = "42.0\n";
        static EXPECTED2: &str = "42.0\n";

        static INPUT3: &str = "foo\nbar";
        static EXPECTED3: Lazy<String> = Lazy::new(|| {
            let mut wtr = Ansi::new(vec![]);
            wtr.write_all(b"foo\nbar").unwrap();
            wtr.set_color(&fg_yellow_intense_bold()).unwrap();
            wtr.write_all(b"<noeol>\n").unwrap();
            wtr.reset().unwrap();
            String::from_utf8(wtr.into_inner()).unwrap()
        });

        static INPUT4: &str = "foo ";
        static EXPECTED4: Lazy<String> = Lazy::new(|| {
            let mut wtr = Ansi::new(vec![]);
            wtr.write_all(b"foo").unwrap();
            wtr.set_color(&bg_yellow_intense_bold()).unwrap();
            wtr.write_all(b" ").unwrap();
            wtr.reset().unwrap();
            wtr.set_color(&fg_yellow_intense_bold()).unwrap();
            wtr.write_all(b"<noeol>\n").unwrap();
            wtr.reset().unwrap();
            String::from_utf8(wtr.into_inner()).unwrap()
        });

        static INPUT5: &str = "[ \x1b ]\n";
        static EXPECTED5: Lazy<String> = Lazy::new(|| {
            let mut wtr = Ansi::new(vec![]);
            wtr.write_all(b"[ ").unwrap();
            wtr.set_color(&fg_yellow_intense_bold()).unwrap();
            wtr.write_all(b"\\u00001b").unwrap();
            wtr.reset().unwrap();
            wtr.write_all(b" ]\n").unwrap();
            String::from_utf8(wtr.into_inner()).unwrap()
        });

        test(INPUT1, EXPECTED1)?;
        test(INPUT2, &EXPECTED2)?;
        test(INPUT3, &EXPECTED3)?;
        test(INPUT4, &EXPECTED4)?;
        test(INPUT5, &EXPECTED5)
    }

    #[test]
    fn it_prints_diff() -> Fallible<()> {
        fn test(left: &str, right: &str, expected: &str) -> Fallible<()> {
            let (left, right) = (Arc::new(left.to_owned()), Arc::new(right.to_owned()));
            let diff = TextDiff::new(left, right, None);
            let mut wtr = AnsiWithProps::new();
            diff.print_pretty_with_title("expected:", "actual:", &mut wtr)?;
            assert_eq!(String::try_from(wtr)?, expected);
            Ok(())
        }

        static LEFT: &str = "foo\n";
        static RIGHT: &str = "foo";
        static EXPECTED: Lazy<String> = Lazy::new(|| {
            let mut wtr = Ansi::new(vec![]);
            wtr.write_all("│".as_ref()).unwrap();
            wtr.set_color(&fg_magenta_bold()).unwrap();
            wtr.write_all(b"expected:").unwrap();
            wtr.reset().unwrap();
            wtr.write_all("│".as_ref()).unwrap();
            wtr.set_color(&fg_magenta_bold()).unwrap();
            wtr.write_all(b"actual:").unwrap();
            wtr.reset().unwrap();
            wtr.write_all("   │\n│".as_ref()).unwrap();
            wtr.set_color(&fg_red_intense_underline()).unwrap();
            wtr.write_all(b"foo").unwrap();
            wtr.reset().unwrap();
            wtr.write_all("      │".as_ref()).unwrap();
            wtr.set_color(&fg_red_intense_underline()).unwrap();
            wtr.write_all(b"foo").unwrap();
            wtr.set_color(&fg_red_intense_bold_underline()).unwrap();
            wtr.write_all(b"<noeol>").unwrap();
            wtr.reset().unwrap();
            wtr.write_all("│\n".as_ref()).unwrap();
            String::from_utf8(wtr.into_inner()).unwrap()
        });

        test(LEFT, RIGHT, &EXPECTED)
    }

    fn fg_red_intense_underline() -> ColorSpec {
        let mut ret = ColorSpec::new();
        ret.set_fg(Some(Color::Red))
            .set_intense(true)
            .set_underline(true);
        ret
    }

    fn fg_red_intense_bold_underline() -> ColorSpec {
        let mut ret = ColorSpec::new();
        ret.set_fg(Some(Color::Red))
            .set_intense(true)
            .set_bold(true)
            .set_underline(true);
        ret
    }

    fn fg_yellow_intense_bold() -> ColorSpec {
        let mut ret = ColorSpec::new();
        ret.set_fg(Some(Color::Yellow))
            .set_intense(true)
            .set_bold(true);
        ret
    }

    fn fg_magenta_bold() -> ColorSpec {
        let mut ret = ColorSpec::new();
        ret.set_fg(Some(Color::Magenta)).set_bold(true);
        ret
    }

    fn bg_yellow_intense_bold() -> ColorSpec {
        let mut ret = ColorSpec::new();
        ret.set_bg(Some(Color::Yellow))
            .set_intense(true)
            .set_bold(true);
        ret
    }

    #[allow(dead_code)]
    fn static_assert_string_and_arc_string_are_stable_deref() {
        fn pass<T: StableDeref>() {}

        pass::<String>();
        pass::<Arc<String>>();
    }
}
