use console::{ConsoleWrite, Palette};

use combine::Parser;

use std::cmp;
use std::fmt::Write as _FmtWrite;
use std::io::{self, Write as _IoWrite};
use std::sync::Arc;

pub(super) trait Width {
    fn width(&self, f: fn(&str) -> usize) -> usize;
}

pub(super) trait PrintAligned: Width {
    fn print_aligned<W: ConsoleWrite>(&self, out: W, min_width: usize) -> io::Result<()>;
}

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, PartialEq)]
pub(super) struct Text {
    lines: Vec<Line<Word>>,
}

impl Text {
    pub fn exact(s: &str) -> Self {
        Self::new(s, |s| Word::Plain(Arc::new(s)))
    }

    pub fn new(s: &str, on_plain: impl Fn(String) -> Word) -> Self {
        use combine::char::char;
        use combine::{choice, eof, many, many1, satisfy};

        fn escape_ws_cc(s: &str) -> String {
            debug_assert!(s.chars().all(|c| ![' ', '\n'].contains(&c)));
            let mut r = "".to_owned();
            for c in s.chars() {
                match c {
                    '\t' | '\r' => write!(r, "{}", c.escape_default()).unwrap(),
                    c if c.is_whitespace() || c.is_control() => {
                        write!(r, "\\u{:x}", c as u32).unwrap()
                    }
                    c => r.push(c),
                }
            }
            r
        }

        fn emph_spc(words: &mut Vec<Word>, index: usize) {
            let n = match words.get(index) {
                Some(Word::U0020(n)) => Some(*n),
                _ => None,
            };
            if let Some(n) = n {
                words[index] = Word::LeadingOrTrailingU0020(n);
            }
        }

        let mut line_parser = {
            let spc = many1::<String, _>(char(' ')).map(|s| Word::U0020(s.len()));
            let ws_cc_except_spc_lf = many1::<String, _>(satisfy(|c: char| {
                ![' ', '\n'].contains(&c) && (c.is_whitespace() || c.is_control())
            })).map(|s| Word::Escaped(Arc::new(escape_ws_cc(&s))));
            let plain =
                many1(satisfy(|c: char| !(c.is_whitespace() || c.is_control()))).map(on_plain);
            let lf = char('\n').map(|_| Word::Lf);
            let noeol = eof().map(|_| Word::Noeol);
            many::<Vec<_>, _>(choice((spc, ws_cc_except_spc_lf, plain)))
                .and(choice((lf, noeol)))
                .map(|(mut words, end)| {
                    let n = words.len();
                    emph_spc(&mut words, 0);
                    emph_spc(&mut words, cmp::max(n, 1) - 1);
                    words.push(end);
                    Line { words }
                })
        };
        let (mut lines, mut s) = (vec![], s);
        loop {
            let (line, rest_s) = line_parser.parse(s).unwrap();
            lines.push(line);
            s = rest_s;
            if s.is_empty() {
                break Self { lines };
            }
        }
    }

    pub fn lines(&self) -> &[Line<Word>] {
        &self.lines
    }

    pub fn is_empty(&self) -> bool {
        self.lines.is_empty() || self.lines.len() == 1 && {
            let Line { words } = &self.lines[0];
            match words.as_slice() {
                [Word::Noeol] => true,
                _ => false,
            }
        }
    }
}

impl Width for Text {
    fn width(&self, f: fn(&str) -> usize) -> usize {
        self.lines.iter().map(|l| l.width(f)).max().unwrap_or(0)
    }
}

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, PartialEq)]
pub(super) struct Line<W> {
    words: Vec<W>,
}

impl<W> Line<W> {
    pub fn new(words: Vec<W>) -> Self {
        Self { words }
    }

    pub fn words(&self) -> &[W] {
        &self.words
    }
}

impl<W> Default for Line<W> {
    fn default() -> Self {
        Self { words: vec![] }
    }
}

impl<W: Width> Width for Line<W> {
    fn width(&self, f: fn(&str) -> usize) -> usize {
        self.words.iter().map(|w| w.width(f)).sum()
    }
}

#[cfg_attr(test, derive(Debug))]
#[derive(Clone)]
pub(super) enum Word {
    Plain(Arc<String>),
    U0020(usize),
    LeadingOrTrailingU0020(usize),
    Escaped(Arc<String>),
    FloatLeft {
        value: f64,
        string: Arc<String>, // `str::parse<f64>` is not injective
        absolute_error: f64,
        relative_error: f64,
    },
    FloatRight {
        value: f64,
        string: Arc<String>,
    },
    Lf,
    Noeol,
}

impl Word {
    pub fn print_as_common(&self, mut out: impl ConsoleWrite) -> io::Result<()> {
        match self {
            Word::Plain(s) => out.write_all(s.as_bytes()),
            Word::U0020(n) => out.write_spaces(*n),
            Word::LeadingOrTrailingU0020(n) => out.fill_bg(*n, Palette::Warning),
            Word::Escaped(s) => out.bold(Palette::Warning).write_all(s.as_bytes()),
            Word::FloatLeft { string, .. } | Word::FloatRight { string, .. } => {
                out.bold(Palette::Number).write_all(string.as_bytes())
            }
            Word::Lf => Ok(()),
            Word::Noeol => out.bold(Palette::Warning).write_all(b"<noeol>"),
        }
    }

    pub fn print_as_difference(&self, mut out: impl ConsoleWrite) -> io::Result<()> {
        let mut out = out.underline(Palette::Fatal);
        match self {
            Word::Plain(s) => out.write_all(s.as_bytes()),
            Word::U0020(n) => (0..*n).try_for_each(|_| out.write_all(b" ")),
            Word::LeadingOrTrailingU0020(n) => out.fill_bg(*n, Palette::Warning),
            Word::Escaped(s) => out.bold(None).write_all(s.as_bytes()),
            Word::FloatLeft { string, .. } | Word::FloatRight { string, .. } => {
                out.bold(None).write_all(string.as_bytes())
            }
            Word::Lf => Ok(()),
            Word::Noeol => out.bold(None).write_all(b"<noeol>"),
        }
    }
}

impl Width for Word {
    fn width(&self, f: fn(&str) -> usize) -> usize {
        match self {
            Word::Plain(s)
            | Word::Escaped(s)
            | Word::FloatLeft { string: s, .. }
            | Word::FloatRight { string: s, .. } => f(s),
            Word::U0020(n) | Word::LeadingOrTrailingU0020(n) => *n,
            Word::Lf => 0,
            Word::Noeol => 7,
        }
    }
}

impl PartialEq for Word {
    fn eq(&self, other: &Self) -> bool {
        // not transitive
        match (self, other) {
            (Word::Plain(s1), Word::Plain(s2)) | (Word::Escaped(s1), Word::Escaped(s2)) => s1 == s2,
            (Word::U0020(n1), Word::U0020(n2))
            | (Word::LeadingOrTrailingU0020(n1), Word::LeadingOrTrailingU0020(n2)) => n1 == n2,
            (
                Word::FloatLeft {
                    value: v1,
                    absolute_error: d,
                    relative_error: r,
                    ..
                },
                Word::FloatRight { value: v2, .. },
            )
            | (
                Word::FloatRight { value: v2, .. },
                Word::FloatLeft {
                    value: v1,
                    absolute_error: d,
                    relative_error: r,
                    ..
                },
            ) => ((v1 - v2).abs() <= *d || ((v1 - v2) / v2).abs() <= *r),
            (
                Word::FloatLeft {
                    string: s1,
                    absolute_error: d1,
                    relative_error: r1,
                    ..
                },
                Word::FloatLeft {
                    string: s2,
                    absolute_error: d2,
                    relative_error: r2,
                    ..
                },
            ) => s1 == s2 && d1 == d2 && r1 == r2,
            (Word::FloatRight { string: s1, .. }, Word::FloatRight { string: s2, .. }) => s1 == s2,
            (Word::Lf, Word::Lf) | (Word::Noeol, Word::Noeol) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use judging::text::{Line, Text, Word};

    use env_logger;

    use std::sync::Arc;

    #[test]
    fn it_parses_command_output() {
        let _ = env_logger::try_init();
        static S: &str = "a b 1\n\
                          ccc 2\n  \
                          \t  \n";
        assert_eq!(
            Text::exact(S),
            text(&[
                &with_spaces(&[plain("a"), plain("b"), plain("1")], lf()),
                &with_spaces(&[plain("ccc"), plain("2")], lf()),
                &[lead_trail(2), escaped("\\t"), lead_trail(2), lf()],
            ]),
        );
        assert_eq!(Text::exact(""), text(&[&[Word::Noeol]]));
    }

    fn text(words: &[&[Word]]) -> Text {
        let lines = words.iter().map(|ws| Line { words: ws.to_vec() }).collect();
        Text { lines }
    }

    fn with_spaces(words: &[Word], end: Word) -> Vec<Word> {
        let mut r = vec![words[0].clone()];
        for w in words.iter().skip(1) {
            r.extend(vec![Word::U0020(1), w.clone()]);
        }
        r.push(end);
        r
    }

    fn plain(s: &str) -> Word {
        Word::Plain(Arc::new(s.to_owned()))
    }

    fn lead_trail(n: usize) -> Word {
        Word::LeadingOrTrailingU0020(n)
    }

    fn escaped(s: &str) -> Word {
        Word::Escaped(Arc::new(s.to_owned()))
    }

    fn lf() -> Word {
        Word::Lf
    }
}
