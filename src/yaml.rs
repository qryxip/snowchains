use derive_more::From;
use itertools::Itertools;
use maplit::{btreeset, hashmap};
use yaml_rust::parser::{Event, Parser};
use yaml_rust::scanner::{ScanError, Scanner, TScalarStyle, Token, TokenType};
use yaml_rust::{Yaml, YamlEmitter};

use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::{cmp, str};

pub(crate) fn escape_string(s: &str) -> Cow<str> {
    if s.parse::<i64>().is_ok() || s.parse::<f64>().is_ok() {
        return Cow::from(s);
    }
    let mut r = String::with_capacity(s.len() + 1);
    {
        let mut emitter = YamlEmitter::new(&mut r);
        emitter.dump(&Yaml::String(s.to_owned())).unwrap();
    }
    Cow::Owned(r.chars().skip(4).collect())
}

pub(crate) fn replace_scalars(
    yaml: &str,
    to: &HashMap<&'static str, impl Borrow<str>>,
) -> std::result::Result<String, ReplaceYamlScalarWarning> {
    #[derive(PartialEq, Debug)]
    enum Nest {
        Document,
        Sequence,
        Mapping,
        Key(String),
    }

    #[derive(Debug)]
    struct Replacement<'a>(&'a str, (usize, usize), (usize, usize), bool);

    let ranges = {
        let mut ranges = hashmap!();
        let mut scanner = Scanner::new(yaml.chars());
        while let Some(Token(start, token)) = scanner.next_token()? {
            if let TokenType::Scalar(style, value) = token {
                let (i, j) = if style == TScalarStyle::Plain && !value.contains('\n') {
                    let i = start.line() - 1;
                    let j = yaml.lines().nth(i).unwrap().chars().count() + 1;
                    (i, j)
                } else {
                    let mut i = scanner.mark().line() - 1;
                    let mut j = scanner.mark().col();
                    if i > 0 && j == 0 {
                        i -= 1;
                        j = yaml.lines().nth(i).unwrap().chars().count() + 1;
                    }
                    let d = match style {
                        TScalarStyle::Plain => 1,
                        _ => 0,
                    };
                    (i, cmp::max(j, d) - d)
                };
                ranges.insert(start.index(), (i, j));
            }
        }
        ranges
    };
    let mut parser = Parser::new(yaml.chars());
    let mut nests = vec![];
    let mut replacements = vec![];
    let mut indices_to_remove = btreeset!();
    loop {
        let (event, marker) = parser.next()?;
        match event {
            Event::StreamStart => {}
            Event::StreamEnd => {
                if !nests.is_empty() {
                    break Err(ReplaceYamlScalarWarning::UnexpectedElement);
                }
                let mut lines = yaml.lines().map(Cow::from).collect::<Vec<_>>();
                for Replacement(to, (i1, j1), (i2, j2), raw) in replacements {
                    let to = if raw {
                        Cow::from(to)
                    } else {
                        escape_string(to)
                    };
                    if i1 == i2 {
                        let left = lines[i1].chars().take(j1).collect::<String>();
                        let right = lines[i1].chars().skip(j2).collect::<String>();
                        lines[i1] = Cow::from(format!("{}{}{}", left, to, right));
                    } else if i1 < i2 {
                        let left = lines[i1].chars().take(j1).collect::<String>();
                        let right = lines[i2].chars().skip(j2).collect::<String>();
                        lines[i1] = Cow::from(format!("{}{}{}", left, to, right));
                        for i in i1 + 1..=i2 {
                            indices_to_remove.insert(i);
                        }
                    } else {
                        unreachable!();
                    }
                }
                let mut filtered = Vec::with_capacity(lines.len() - indices_to_remove.len());
                for (i, line) in lines.into_iter().enumerate() {
                    if !indices_to_remove.contains(&i) {
                        filtered.push(line);
                    }
                }
                filtered.push(Cow::from(""));
                break Ok(filtered.iter().join("\n"));
            }
            Event::DocumentStart => nests.push(Nest::Document),
            Event::DocumentEnd => {
                if nests.pop() != Some(Nest::Document) {
                    break Err(ReplaceYamlScalarWarning::UnexpectedElement);
                }
            }
            Event::Scalar(s, t, 0, None) => match nests.pop() {
                None | Some(Nest::Document) => {
                    break Err(ReplaceYamlScalarWarning::UnexpectedElement);
                }
                Some(Nest::Sequence) => nests.push(Nest::Sequence),
                Some(Nest::Mapping) => nests.push(Nest::Key(s)),
                Some(Nest::Key(k)) => {
                    if let Some(v) = to.get(k.as_str()) {
                        let start = (marker.line() - 1, marker.col());
                        let end = ranges[&marker.index()];
                        let raw = [TScalarStyle::Literal, TScalarStyle::Foled].contains(&t);
                        replacements.push(Replacement(v.borrow(), start, end, raw));
                    }
                    nests.push(Nest::Mapping);
                }
            },
            Event::SequenceStart(0) => nests.push(Nest::Sequence),
            Event::SequenceEnd => {
                if nests.pop() == Some(Nest::Sequence) {
                    if let Some(Nest::Key(_)) = nests.last() {
                        nests.pop();
                        nests.push(Nest::Mapping);
                    }
                } else {
                    break Err(ReplaceYamlScalarWarning::UnexpectedElement);
                }
            }
            Event::MappingStart(0) => nests.push(Nest::Mapping),
            Event::MappingEnd => {
                if nests.pop() == Some(Nest::Mapping) {
                    if let Some(Nest::Key(_)) = nests.last() {
                        nests.pop();
                        nests.push(Nest::Mapping);
                    }
                } else {
                    break Err(ReplaceYamlScalarWarning::UnexpectedElement);
                }
            }
            Event::Nothing
            | Event::Alias(_)
            | Event::Scalar(..)
            | Event::SequenceStart(_)
            | Event::MappingStart(_) => {
                break Err(ReplaceYamlScalarWarning::AnchorAndAliasNotSupported);
            }
        }
    }
}

#[derive(From, Debug, derive_more::Display)]
pub(crate) enum ReplaceYamlScalarWarning {
    #[display(fmt = "{}", _0)]
    Deserialize(serde_yaml::Error),
    #[display(fmt = "{}", _0)]
    Scan(ScanError),
    #[display(fmt = "Anchor and alias not supported")]
    AnchorAndAliasNotSupported,
    #[display(fmt = "Unexpected element")]
    UnexpectedElement,
}
