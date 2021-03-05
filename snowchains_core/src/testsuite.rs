use anyhow::{bail, ensure, Context as _};
use camino::Utf8PathBuf;
use humantime_serde::Serde;
use ignore::{overrides::OverrideBuilder, WalkBuilder};
use itertools::{EitherOrBoth, Itertools as _};
use maplit::hashmap;
use serde::{de::Error as _, Deserialize, Deserializer, Serialize};
use std::{
    borrow::Borrow,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fs,
    hash::Hash,
    path::Path,
    str::FromStr,
    sync::Arc,
    time::Duration,
};
use url::Url;

#[derive(Deserialize, Serialize, Debug, PartialEq)]
#[serde(tag = "type")]
pub enum TestSuite {
    Batch(BatchTestSuite),
    Interactive(InteractiveTestSuite),
    Unsubmittable,
}

impl TestSuite {
    pub fn to_yaml_pretty(&self) -> String {
        return if let Self::Batch(suite) = self {
            (|| -> _ {
                let mut yaml = "---\n".to_owned();

                yaml += &key_value("type", "Batch").ok()?;
                yaml += &key_value("timelimit", Serde::from(suite.timelimit)).ok()?;
                yaml += &key_value("match", &suite.r#match).ok()?;

                yaml += if suite.cases.is_empty() {
                    "\ncases: []\n"
                } else {
                    "\ncases:\n"
                };

                for case in &suite.cases {
                    let mut part = "".to_owned();

                    if let Some(name) = &case.name {
                        part += &key_value("name", name).ok()?;
                    }

                    part += &key_value_in_literal_style("in", &case.r#in).ok()?;

                    if let Some(out) = &case.out {
                        part += &key_value_in_literal_style("out", out).ok()?;
                    }

                    if let Some(timelimit) = case.timelimit {
                        part += &key_value("timelimit", Serde::from(timelimit)).ok()?;
                    }

                    if let Some(r#match) = &case.r#match {
                        part += &key_value("match", r#match).ok()?;
                    }

                    for (i, line) in part.lines().enumerate() {
                        yaml += match i {
                            0 => "  - ",
                            _ => "    ",
                        };
                        yaml += line;
                        yaml += "\n";
                    }
                }

                if suite.extend.is_empty() {
                    yaml += "\nextend: []\n";
                } else {
                    yaml += "\nextend:\n";

                    for line in serde_yaml::to_string(&suite.extend)
                        .ok()?
                        .trim_start_matches("---\n")
                        .lines()
                    {
                        yaml += "  ";
                        yaml += line;
                        yaml += "\n";
                    }
                }

                if serde_yaml::from_str::<Self>(&yaml).ok()? != *self {
                    return None;
                }

                Some(Ok(yaml))
            })()
            .unwrap_or_else(|| serde_yaml::to_string(self))
        } else {
            serde_yaml::to_string(self)
        }
        .unwrap_or_else(|e| panic!("failed to serialize: {}", e));

        fn key_value(key: impl Serialize, value: impl Serialize) -> serde_yaml::Result<String> {
            let key = serde_yaml::to_value(key)?;
            let mut acc = serde_yaml::to_string(&hashmap!(key => value))?;
            debug_assert!(acc.starts_with("---\n") && acc.ends_with('\n'));
            Ok(acc.split_off(4))
        }

        fn key_value_in_literal_style(
            key: impl Serialize,
            value: &str,
        ) -> serde_yaml::Result<String> {
            (|| -> _ {
                if !value
                    .chars()
                    .all(|c| c == ' ' || c == '\n' || !(c.is_whitespace() || c.is_control()))
                {
                    return None;
                }

                let key = serde_yaml::to_value(&key).ok()?;

                let mut acc = serde_yaml::to_string(&hashmap!(&key => serde_yaml::Value::Null))
                    .ok()?
                    .trim_start_matches("---\n")
                    .trim_end_matches('\n')
                    .trim_end_matches('~')
                    .to_owned();

                acc += if value.ends_with('\n') { "|\n" } else { ">\n" };

                for line in value.lines() {
                    acc += "  ";
                    acc += line;
                    acc += "\n";
                }

                if serde_yaml::from_str::<HashMap<serde_yaml::Value, String>>(&acc).ok()?
                    != hashmap!(key => value.to_owned())
                {
                    return None;
                }

                Some(Ok(acc))
            })()
            .unwrap_or_else(|| key_value(key, value))
        }
    }
}

#[derive(Deserialize, Serialize, Debug, PartialEq)]
pub struct BatchTestSuite {
    #[serde(default, with = "humantime_serde")]
    pub timelimit: Option<Duration>,
    pub r#match: Match,
    #[serde(default)]
    pub cases: Vec<PartialBatchTestCase>,
    #[serde(default)]
    pub extend: Vec<Additional>,
}

impl BatchTestSuite {
    pub fn load_test_cases<
        S: Borrow<str> + Eq + Hash,
        F: FnMut(Option<&Url>) -> anyhow::Result<Vec<PartialBatchTestCase>>,
    >(
        &self,
        parent_dir: &Path,
        mut names: Option<HashSet<S>>,
        mut prepare_system_test_cases: F,
    ) -> anyhow::Result<Vec<BatchTestCase>> {
        let mut cases = self.cases.clone();
        for extend in &self.extend {
            cases.extend(extend.load_test_cases(parent_dir, &mut prepare_system_test_cases)?);
        }

        let cases = cases
            .into_iter()
            .filter(
                |PartialBatchTestCase { name, .. }| match (names.as_mut(), name.as_ref()) {
                    (Some(names), Some(name)) => names.remove(name),
                    _ => true,
                },
            )
            .map(|case| BatchTestCase::new(case, self.timelimit, &self.r#match))
            .collect();

        if let Some(names) = names {
            if !names.is_empty() {
                bail!(
                    "No such test cases: {:?}",
                    names.iter().map(Borrow::borrow).collect::<BTreeSet<_>>(),
                );
            }
        }

        Ok(cases)
    }
}

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq)]
pub struct PartialBatchTestCase {
    pub name: Option<String>,
    #[serde(with = "serde_fn::arc_str")]
    pub r#in: Arc<str>,
    #[serde(default, with = "serde_fn::option_arc_str")]
    pub out: Option<Arc<str>>,
    #[serde(default, with = "humantime_serde")]
    pub timelimit: Option<Duration>,
    pub r#match: Option<Match>,
}

#[derive(Deserialize, Serialize, Debug, PartialEq)]
#[serde(tag = "type")]
pub enum Additional {
    Text {
        path: Utf8PathBuf,
        r#in: String,
        out: String,
        #[serde(
            default,
            with = "humantime_serde",
            skip_serializing_if = "Option::is_none"
        )]
        timelimit: Option<Duration>,
        #[serde(skip_serializing_if = "Option::is_none")]
        r#match: Option<Match>,
    },
    SystemTestCases {
        #[serde(skip_serializing_if = "Option::is_none")]
        problem: Option<Url>,
    },
}

impl Additional {
    fn load_test_cases(
        &self,
        parent_dir: &Path,
        mut prepare_system_test_cases: impl FnMut(
            Option<&Url>,
        ) -> anyhow::Result<Vec<PartialBatchTestCase>>,
    ) -> anyhow::Result<Vec<PartialBatchTestCase>> {
        match self {
            Self::Text {
                path: base,
                r#in,
                out,
                r#match,
                timelimit,
            } => {
                let base = Path::new(base);
                let base = parent_dir.join(base.strip_prefix(".").unwrap_or(base));
                let base = base.strip_prefix(".").unwrap_or(&base);

                let mut cases = BTreeMap::<_, (Option<_>, Option<_>)>::new();

                let walk = |overrides| -> _ {
                    WalkBuilder::new(base)
                        .max_depth(Some(128))
                        .overrides(overrides)
                        .standard_filters(false)
                        .build()
                        .map::<anyhow::Result<_>, _>(|entry| {
                            let path = entry?.into_path();

                            if path.is_dir() {
                                return Ok(None);
                            }

                            let name = path
                                .file_stem()
                                .unwrap_or_default()
                                .to_string_lossy()
                                .into_owned();

                            let content = fs::read_to_string(&path)
                                .with_context(|| format!("Could not read {}", path.display()))?
                                .into();

                            Ok(Some((name, content)))
                        })
                        .flat_map(Result::transpose)
                };

                for result in walk(OverrideBuilder::new(base).add(r#in)?.build()?) {
                    let (name, content) = result?;
                    let (entry, _) = cases.entry(name.clone()).or_default();
                    ensure!(entry.is_none(), "Duplicated name: {:?}", name);
                    *entry = Some(content);
                }

                for result in walk(OverrideBuilder::new(base).add(out)?.build()?) {
                    let (name, content) = result?;
                    let (_, entry) = cases.entry(name.clone()).or_default();
                    ensure!(entry.is_none(), "Duplicated name: {:?}", name);
                    *entry = Some(content);
                }

                cases
                    .into_iter()
                    .map(|kv| {
                        let (name, r#in, out) = match kv {
                            (_, (None, None)) => unreachable!(),
                            (name, (None, Some(_))) => bail!("No input file for {:?}", name),
                            (name, (Some(r#in), out)) => (name, r#in, out),
                        };

                        Ok(PartialBatchTestCase {
                            name: Some(name),
                            r#in,
                            out,
                            timelimit: *timelimit,
                            r#match: r#match.clone(),
                        })
                    })
                    .collect()
            }
            Self::SystemTestCases { problem } => prepare_system_test_cases(problem.as_ref()),
        }
    }
}

#[derive(Deserialize, Serialize, Debug, Clone, PartialEq)]
pub enum Match {
    Exact,
    SplitWhitespace,
    Lines,
    Float {
        relative_error: Option<PositiveFinite<f64>>,
        absolute_error: Option<PositiveFinite<f64>>,
    },
    Checker {
        cmd: String,
        shell: CheckerShell,
    },
}

#[derive(Deserialize, Serialize, Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub enum CheckerShell {
    Bash,
}

#[derive(Deserialize, Serialize, Debug, PartialEq)]
pub struct InteractiveTestSuite {
    #[serde(default, with = "humantime_serde")]
    pub timelimit: Option<Duration>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BatchTestCase {
    pub name: Option<String>,
    pub timelimit: Option<Duration>,
    pub input: Arc<str>,
    pub output: ExpectedOutput,
}

impl BatchTestCase {
    fn new(case: PartialBatchTestCase, timelimit: Option<Duration>, matching: &Match) -> Self {
        BatchTestCase {
            name: case.name,
            timelimit: case.timelimit.or(timelimit),
            input: case.r#in,
            output: ExpectedOutput::new(case.out, case.r#match.unwrap_or_else(|| matching.clone())),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpectedOutput {
    Deterministic(DeterministicExpectedOutput),
    Checker {
        text: Option<Arc<str>>,
        cmd: String,
        shell: CheckerShell,
    },
}

impl ExpectedOutput {
    fn new(text: Option<Arc<str>>, matching: Match) -> Self {
        match (text, matching) {
            (text, Match::Checker { cmd, shell }) => Self::Checker { text, cmd, shell },
            (Some(text), Match::Exact) => {
                Self::Deterministic(DeterministicExpectedOutput::Exact { text })
            }
            (Some(text), Match::SplitWhitespace) => {
                Self::Deterministic(DeterministicExpectedOutput::SplitWhitespace { text })
            }
            (Some(text), Match::Lines) => {
                Self::Deterministic(DeterministicExpectedOutput::Lines { text })
            }
            (
                Some(text),
                Match::Float {
                    relative_error,
                    absolute_error,
                },
            ) => Self::Deterministic(DeterministicExpectedOutput::Float {
                text,
                relative_error,
                absolute_error,
            }),
            (None, _) => Self::Deterministic(DeterministicExpectedOutput::Pass),
        }
    }

    pub(crate) fn is_float(&self) -> bool {
        matches!(
            self,
            Self::Deterministic(DeterministicExpectedOutput::Float { .. })
        )
    }

    pub(crate) fn expected_stdout(&self) -> Option<&str> {
        match self {
            Self::Deterministic(DeterministicExpectedOutput::Exact { text })
            | Self::Deterministic(DeterministicExpectedOutput::SplitWhitespace { text })
            | Self::Deterministic(DeterministicExpectedOutput::Lines { text })
            | Self::Deterministic(DeterministicExpectedOutput::Float { text, .. }) => Some(text),
            Self::Deterministic(DeterministicExpectedOutput::Pass) | Self::Checker { .. } => None,
        }
    }

    pub(crate) fn example(&self) -> Option<&str> {
        match self {
            Self::Checker { text, .. } => text.as_deref(),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeterministicExpectedOutput {
    Pass,
    Exact {
        text: Arc<str>,
    },
    SplitWhitespace {
        text: Arc<str>,
    },
    Lines {
        text: Arc<str>,
    },
    Float {
        text: Arc<str>,
        relative_error: Option<PositiveFinite<f64>>,
        absolute_error: Option<PositiveFinite<f64>>,
    },
}

impl DeterministicExpectedOutput {
    pub(crate) fn accepts(&self, actual: &str) -> bool {
        match self {
            Self::Pass => true,
            Self::Exact { text } => &**text == actual,
            Self::SplitWhitespace { text } => text.split_whitespace().eq(actual.split_whitespace()),
            Self::Lines { text } => text.lines().eq(actual.lines()),
            Self::Float {
                text,
                relative_error,
                absolute_error,
            } => {
                let (text, actual) = (text.lines(), actual.lines());
                let relative_error = relative_error.map(PositiveFinite::get).unwrap_or(0.0);
                let absolute_error = absolute_error.map(PositiveFinite::get).unwrap_or(0.0);

                text.zip_longest(actual).all(|zip| {
                    if let EitherOrBoth::Both(line1, line2) = zip {
                        let (words1, words2) = (line1.split_whitespace(), line2.split_whitespace());
                        words1.zip_longest(words2).all(|zip| match zip {
                            EitherOrBoth::Both(s1, s2) => {
                                match (s1.parse::<f64>(), s2.parse::<f64>()) {
                                    (Ok(v1), Ok(v2)) => {
                                        (v1 - v2).abs() <= absolute_error
                                            || ((v1 - v2) / v2).abs() <= relative_error
                                    }
                                    _ => s1 == s2,
                                }
                            }
                            EitherOrBoth::Left(_) | EitherOrBoth::Right(_) => false,
                        })
                    } else {
                        false
                    }
                })
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize)]
#[serde(transparent)]
pub struct PositiveFinite<F>(F);

impl PositiveFinite<f64> {
    fn new(value: f64) -> Option<Self> {
        if value.is_sign_positive() && value.is_finite() {
            Some(Self(value))
        } else {
            None
        }
    }

    pub fn get(self) -> f64 {
        self.0
    }
}

impl FromStr for PositiveFinite<f64> {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        Self::new(s.parse()?).with_context(|| "must be positive and finite")
    }
}

impl<'de> Deserialize<'de> for PositiveFinite<f64> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Self::new(f64::deserialize(deserializer)?)
            .ok_or_else(|| D::Error::custom("must be positive and finite"))
    }
}

mod serde_fn {
    pub(super) mod arc_str {
        use serde::{Deserialize, Deserializer, Serializer};
        use std::sync::Arc;

        pub(crate) fn serialize<S>(this: &Arc<str>, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.serialize_str(this)
        }

        pub(crate) fn deserialize<'de, D>(deserializer: D) -> Result<Arc<str>, D::Error>
        where
            D: Deserializer<'de>,
        {
            String::deserialize(deserializer).map(Into::into)
        }
    }

    pub(super) mod option_arc_str {
        use serde::{Deserialize, Deserializer, Serializer};
        use std::sync::Arc;

        pub(crate) fn serialize<S>(
            this: &Option<Arc<str>>,
            serializer: S,
        ) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            if let Some(s) = this {
                serializer.serialize_some(&**s)
            } else {
                serializer.serialize_none()
            }
        }

        pub(crate) fn deserialize<'de, D>(deserializer: D) -> Result<Option<Arc<str>>, D::Error>
        where
            D: Deserializer<'de>,
        {
            Option::<String>::deserialize(deserializer).map(|s| s.map(Into::into))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::testsuite::{
        Additional, BatchTestSuite, DeterministicExpectedOutput, Match, PartialBatchTestCase,
        PositiveFinite, TestSuite,
    };
    use difference::assert_diff;
    use pretty_assertions::assert_eq;
    use std::time::Duration;

    #[test]
    fn atcoder_abc162_a() {
        test_serialize_deserialize(
            r#"---
type: Batch
timelimit: 2s
match: Lines

cases:
  - name: Sample 1
    in: |
      117
    out: |
      Yes
  - name: Sample 2
    in: |
      123
    out: |
      No
  - name: Sample 3
    in: |
      777
    out: |
      Yes

extend: []
"#,
            &TestSuite::Batch(BatchTestSuite {
                timelimit: Some(Duration::from_secs(2)),
                r#match: Match::Lines,
                cases: vec![
                    PartialBatchTestCase {
                        name: Some("Sample 1".to_owned()),
                        r#in: "117\n".into(),
                        out: Some("Yes\n".into()),
                        timelimit: None,
                        r#match: None,
                    },
                    PartialBatchTestCase {
                        name: Some("Sample 2".to_owned()),
                        r#in: "123\n".into(),
                        out: Some("No\n".into()),
                        timelimit: None,
                        r#match: None,
                    },
                    PartialBatchTestCase {
                        name: Some("Sample 3".to_owned()),
                        r#in: "777\n".into(),
                        out: Some("Yes\n".into()),
                        timelimit: None,
                        r#match: None,
                    },
                ],
                extend: vec![],
            }),
        );

        test_serialize_deserialize(
            r#"---
type: Batch
timelimit: 2s
match: Lines

cases: []

extend:
  - type: Text
    path: "./a"
    in: /in/*.txt
    out: /out/*.txt
"#,
            &TestSuite::Batch(BatchTestSuite {
                timelimit: Some(Duration::from_secs(2)),
                r#match: Match::Lines,
                cases: vec![],
                extend: vec![Additional::Text {
                    path: "./a".into(),
                    r#in: "/in/*.txt".into(),
                    out: "/out/*.txt".into(),
                    timelimit: None,
                    r#match: None,
                }],
            }),
        );
    }

    #[test]
    fn atcoder_abc163_a() {
        test_serialize_deserialize(
            r#"---
type: Batch
timelimit: 2s
match:
  Float:
    relative_error: 0.01
    absolute_error: 0.01

cases:
  - name: Sample 1
    in: |
      1
    out: |
      6.28318530717958623200
  - name: Sample 2
    in: |
      73
    out: |
      458.67252742410977361942

extend: []
"#,
            &TestSuite::Batch(BatchTestSuite {
                timelimit: Some(Duration::from_secs(2)),
                r#match: Match::Float {
                    relative_error: Some(PositiveFinite(0.01)),
                    absolute_error: Some(PositiveFinite(0.01)),
                },
                cases: vec![
                    PartialBatchTestCase {
                        name: Some("Sample 1".to_owned()),
                        r#in: "1\n".into(),
                        out: Some("6.28318530717958623200\n".into()),
                        timelimit: None,
                        r#match: None,
                    },
                    PartialBatchTestCase {
                        name: Some("Sample 2".to_owned()),
                        r#in: "73\n".into(),
                        out: Some("458.67252742410977361942\n".into()),
                        timelimit: None,
                        r#match: None,
                    },
                ],
                extend: vec![],
            }),
        );
    }

    #[test]
    fn atcoder_arc071_c() {
        test_serialize_deserialize(
            r#"---
type: Batch
timelimit: 2s
match: Lines

cases:
  - name: Sample 1
    in: |
      3
      cbaa
      daacc
      acacac
    out: |
      aac
  - name: Sample 2
    in: |
      3
      a
      aa
      b
    out: "\n"

extend: []
"#,
            &TestSuite::Batch(BatchTestSuite {
                timelimit: Some(Duration::from_secs(2)),
                r#match: Match::Lines,
                cases: vec![
                    PartialBatchTestCase {
                        name: Some("Sample 1".to_owned()),
                        r#in: "3\ncbaa\ndaacc\nacacac\n".into(),
                        out: Some("aac\n".into()),
                        timelimit: None,
                        r#match: None,
                    },
                    PartialBatchTestCase {
                        name: Some("Sample 2".to_owned()),
                        r#in: "3\na\naa\nb\n".into(),
                        out: Some("\n".into()),
                        timelimit: None,
                        r#match: None,
                    },
                ],
                extend: vec![],
            }),
        );
    }

    fn test_serialize_deserialize(yaml: &str, expected: &TestSuite) {
        let actual = serde_yaml::from_str::<TestSuite>(yaml).unwrap();
        assert_eq!(*expected, actual);
        assert_diff!(yaml, &actual.to_yaml_pretty(), "\n", 0);
    }

    #[test]
    fn expected_output_accepts() {
        assert!(DeterministicExpectedOutput::Pass.accepts("ミ゙"));

        assert!(DeterministicExpectedOutput::Exact {
            text: "1 2\n".into()
        }
        .accepts("1 2\n"));

        assert!(!DeterministicExpectedOutput::Exact {
            text: "1  2\n".into()
        }
        .accepts("1 2\n"));

        assert!(!DeterministicExpectedOutput::Exact {
            text: "1 2\n".into()
        }
        .accepts("1\n2\n"));

        assert!(DeterministicExpectedOutput::SplitWhitespace { text: "".into() }.accepts(""));

        assert!(DeterministicExpectedOutput::SplitWhitespace { text: "\n".into() }.accepts(""));

        assert!(DeterministicExpectedOutput::SplitWhitespace { text: "".into() }.accepts("\n"));

        assert!(DeterministicExpectedOutput::SplitWhitespace {
            text: "1 2\n".into()
        }
        .accepts("1 2\n"));

        assert!(
            DeterministicExpectedOutput::SplitWhitespace { text: "1 2".into() }.accepts("1 2\n")
        );

        assert!(DeterministicExpectedOutput::SplitWhitespace {
            text: " 1    2 \n".into()
        }
        .accepts("1 2\n"));

        assert!(DeterministicExpectedOutput::Lines {
            text: "1 2\n".into()
        }
        .accepts("1 2\n"));

        assert!(!DeterministicExpectedOutput::Lines {
            text: "1  2\n".into()
        }
        .accepts("1 2\n"));

        assert!(!DeterministicExpectedOutput::Lines {
            text: "1 2\n".into()
        }
        .accepts("1\n2\n"));

        assert!(DeterministicExpectedOutput::Float {
            text: "10000.0\n".into(),
            relative_error: Some(PositiveFinite(0.01)),
            absolute_error: None,
        }
        .accepts("10001.0\n"));

        assert!(!DeterministicExpectedOutput::Float {
            text: "10000.0\n".into(),
            relative_error: Some(PositiveFinite(0.01)),
            absolute_error: None,
        }
        .accepts("0\n"));
    }
}
