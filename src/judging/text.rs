use crate::terminal::{TermOut, WriteAnsi, WriteSpaces};
use crate::util::num::PositiveFinite;

use combine::Parser;
use derive_new::new;

use std::fmt::Write;
use std::sync::Arc;
use std::{cmp, f64, io};

pub(super) trait Width {
    fn width(&self, f: fn(&str) -> usize) -> usize;
}

pub(super) trait PrintAligned: Width {
    fn print_aligned<W: TermOut>(&self, out: W, min_width: usize) -> io::Result<()>;
}

#[cfg_attr(test, derive(Debug))]
#[derive(Clone)]
pub(super) struct Text {
    size: usize,
    lines: Vec<Line<Word>>,
}

impl Text {
    pub(crate) fn exact(s: &str) -> Self {
        Self::new(s, |s| Word::Plain(Arc::new(s)))
    }

    pub(crate) fn new(s: &str, on_plain: impl Fn(String) -> Word) -> Self {
        use combine::char::char;
        use combine::{choice, eof, many, many1, satisfy};

        fn emph_spc(words: &mut Vec<Word>, index: usize) {
            let n = match words.get(index) {
                Some(Word::U0020(n)) => Some(*n),
                _ => None,
            };
            if let Some(n) = n {
                words[index] = Word::LeadTrailU0020(n);
            }
        }

        let mut line_parser = {
            let spc = many1::<String, _>(char(' ')).map(|s| Word::U0020(s.len()));
            let tab = many1::<String, _>(char('\t')).map(|s| Word::Tab(s.len()));
            let cr = many1::<String, _>(char('\r')).map(|s| Word::Cr(s.len()));
            let codepoints = many1::<String, _>(satisfy(|c: char| {
                ![' ', '\t', '\r', '\n'].contains(&c) && (c.is_whitespace() || c.is_control())
            }))
            .map(|s| {
                Word::CodePoints(Arc::new(s.chars().fold("".to_owned(), |mut s, c| {
                    write!(s, "{}", c.escape_unicode()).unwrap();
                    s
                })))
            });
            let plain =
                many1(satisfy(|c: char| !(c.is_whitespace() || c.is_control()))).map(on_plain);
            let lf = char('\n').map(|_| None);
            let noeol = eof().map(|_| Some(Word::Noeol));
            many::<Vec<_>, _>(choice((spc, tab, cr, codepoints, plain)))
                .and(choice((lf, noeol)))
                .map(|(mut words, end)| {
                    let n = words.len();
                    emph_spc(&mut words, 0);
                    emph_spc(&mut words, cmp::max(n, 1) - 1);
                    words.extend(end);
                    Line { words }
                })
        };
        let size = s.len();
        let (mut lines, mut s) = (vec![], s);
        loop {
            let (line, rest_s) = line_parser.parse(s).unwrap();
            lines.push(line);
            s = rest_s;
            if s.is_empty() {
                break Self { size, lines };
            }
        }
    }

    pub(crate) fn size(&self) -> usize {
        self.size
    }

    pub(crate) fn lines(&self) -> &[Line<Word>] {
        &self.lines
    }

    pub(crate) fn is_empty(&self) -> bool {
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
#[derive(Clone, PartialEq, new)]
pub(super) struct Line<W> {
    words: Vec<W>,
}

impl<W> Line<W> {
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
    LeadTrailU0020(usize),
    Tab(usize),
    Cr(usize),
    CodePoints(Arc<String>),
    FloatLeft {
        value: f64,
        string: Arc<String>, // `str::parse<f64>` is not injective
        relative_error: Option<PositiveFinite<f64>>,
        absolute_error: Option<PositiveFinite<f64>>,
    },
    FloatRight {
        value: f64,
        string: Arc<String>,
    },
    Noeol,
}

impl Word {
    pub fn print_as_common(&self, mut out: impl TermOut) -> io::Result<()> {
        fn write_ntimes(mut out: impl WriteAnsi, n: usize, s: &str) -> io::Result<()> {
            out.with_reset(|o| {
                o.fg(11)?.bold()?;
                (0..n).try_for_each(|_| o.write_str(s))
            })
        }
        match self {
            Word::Plain(s) => out.write_str(s.as_str()),
            Word::U0020(n) => out.write_spaces(*n),
            Word::LeadTrailU0020(n) => out.with_reset(|o| o.bg(11)?.write_spaces(*n)),
            Word::Tab(n) => write_ntimes(out, *n, "\\t"),
            Word::Cr(n) => write_ntimes(out, *n, "\\r"),
            Word::CodePoints(s) => out.with_reset(|o| o.fg(11)?.bold()?.write_str(s.as_str())),
            Word::FloatLeft { string, .. } | Word::FloatRight { string, .. } => {
                out.with_reset(|o| o.fg(6)?.bold()?.write_str(string.as_str()))
            }
            Word::Noeol => out.with_reset(|o| o.fg(11)?.bold()?.write_str("<noeol>")),
        }
    }

    pub fn print_as_difference(&self, mut out: impl TermOut) -> io::Result<()> {
        fn write_ntimes(mut out: impl WriteAnsi, n: usize, s: &str) -> io::Result<()> {
            out.with_reset(|o| {
                o.fg(9)?.bold()?.underline()?;
                (0..n).try_for_each(|_| o.write_str(s))
            })
        }
        match self {
            Word::Plain(s) => out.with_reset(|o| o.fg(9)?.underline()?.write_str(s.as_str())),
            Word::U0020(n) => out.with_reset(|o| o.fg(9)?.underline()?.write_spaces(*n)),
            Word::LeadTrailU0020(n) => {
                out.with_reset(|o| o.fg(9)?.bg(11)?.underline()?.write_spaces(*n))
            }
            Word::Tab(n) => write_ntimes(out, *n, "\\t"),
            Word::Cr(n) => write_ntimes(out, *n, "\\r"),
            Word::CodePoints(s)
            | Word::FloatLeft { string: s, .. }
            | Word::FloatRight { string: s, .. } => {
                out.with_reset(|o| o.fg(9)?.bold()?.underline()?.write_str(s.as_str()))
            }
            Word::Noeol => out.with_reset(|o| o.fg(9)?.bold()?.underline()?.write_str("<noeol>")),
        }
    }
}

impl Width for Word {
    fn width(&self, f: fn(&str) -> usize) -> usize {
        match self {
            Word::Plain(s)
            | Word::CodePoints(s)
            | Word::FloatLeft { string: s, .. }
            | Word::FloatRight { string: s, .. } => f(s),
            Word::U0020(n) | Word::LeadTrailU0020(n) => *n,
            Word::Tab(n) | Word::Cr(n) => 2 * *n,
            Word::Noeol => 7,
        }
    }
}

impl PartialEq for Word {
    fn eq(&self, other: &Self) -> bool {
        // not transitive
        match (self, other) {
            (Word::Plain(s1), Word::Plain(s2)) | (Word::CodePoints(s1), Word::CodePoints(s2)) => {
                s1 == s2
            }
            (Word::U0020(n1), Word::U0020(n2))
            | (Word::LeadTrailU0020(n1), Word::LeadTrailU0020(n2))
            | (Word::Tab(n1), Word::Tab(n2))
            | (Word::Cr(n1), Word::Cr(n2)) => n1 == n2,
            (
                Word::FloatLeft {
                    value: v1,
                    relative_error: r,
                    absolute_error: d,
                    ..
                },
                Word::FloatRight { value: v2, .. },
            )
            | (
                Word::FloatRight { value: v2, .. },
                Word::FloatLeft {
                    value: v1,
                    relative_error: r,
                    absolute_error: d,
                    ..
                },
            ) => {
                let r = r.map(Into::into).unwrap_or(f64::NAN);
                let d = d.map(Into::into).unwrap_or(f64::NAN);
                ((v1 - v2).abs() <= d || ((v1 - v2) / v2).abs() <= r)
            }
            (
                Word::FloatLeft {
                    string: s1,
                    relative_error: r1,
                    absolute_error: d1,
                    ..
                },
                Word::FloatLeft {
                    string: s2,
                    relative_error: r2,
                    absolute_error: d2,
                    ..
                },
            ) => {
                let r1 = r1.map(Into::into).unwrap_or(f64::NAN);
                let d1 = d1.map(Into::into).unwrap_or(f64::NAN);
                let r2 = r2.map(Into::into).unwrap_or(f64::NAN);
                let d2 = d2.map(Into::into).unwrap_or(f64::NAN);
                s1 == s2 && d1 == d2 && r1 == r2
            }
            (Word::FloatRight { string: s1, .. }, Word::FloatRight { string: s2, .. }) => s1 == s2,
            (Word::Noeol, Word::Noeol) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::judging::text::{Line, Text, Word};

    use std::sync::Arc;

    #[test]
    fn it_parses_command_output() {
        let _ = env_logger::try_init();
        static S: &str = "a b 1\n\
                          ccc 2\n  \
                          \t  \n";
        assert_eq!(
            Text::exact(S).lines(),
            text(
                18,
                &[
                    &with_spaces(&[plain("a"), plain("b"), plain("1")]),
                    &with_spaces(&[plain("ccc"), plain("2")]),
                    &[lead_trail(2), tab(1), lead_trail(2)],
                ]
            )
            .lines(),
        );
        assert_eq!(Text::exact("").lines(), text(0, &[&[Word::Noeol]]).lines());
    }

    fn text(size: usize, words: &[&[Word]]) -> Text {
        let lines = words.iter().map(|ws| Line { words: ws.to_vec() }).collect();
        Text { size, lines }
    }

    fn with_spaces(words: &[Word]) -> Vec<Word> {
        let mut r = vec![words[0].clone()];
        for w in words.iter().skip(1) {
            r.extend(vec![Word::U0020(1), w.clone()]);
        }
        r
    }

    fn plain(s: &str) -> Word {
        Word::Plain(Arc::new(s.to_owned()))
    }

    fn lead_trail(n: usize) -> Word {
        Word::LeadTrailU0020(n)
    }

    fn tab(n: usize) -> Word {
        Word::Tab(n)
    }
}
