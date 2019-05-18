use crate::command::{CompilationCommand, JudgingCommand, TranspilationCommand};
use crate::errors::{
    ConfigError, ConfigErrorKind, ExpandTemplateResult, FileResult, StdError, TestSuiteErrorKind,
    TestSuiteResult,
};
use crate::path::{AbsPath, AbsPathBuf};
use crate::template::Template;
use crate::util::collections::NonEmptyVec;
use crate::util::num::PositiveFinite;
use crate::{time, util};

use derive_more::From;
use derive_new::new;
use failure::Fail as _;
use itertools::{EitherOrBoth, Itertools as _};
use maplit::{hashmap, hashset};
use serde::{Deserialize, Serialize};
use yaml_rust::{Yaml, YamlEmitter};

#[cfg(test)]
use failure::Fallible;

use std::borrow::Cow;
use std::collections::{BTreeSet, HashSet, VecDeque};
use std::fmt::{self, Write as _};
use std::iter::FromIterator;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;
use std::{f64, str, vec};

#[derive(
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Debug,
    strum_macros::Display,
    Serialize,
    Deserialize,
)]
#[strum(serialize_all = "snake_case")]
#[serde(rename_all = "snake_case")]
pub enum SuiteFileExtension {
    Json,
    Toml,
    Yaml,
    Yml,
}

impl FromStr for SuiteFileExtension {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, String> {
        match s {
            "json" => Ok(SuiteFileExtension::Json),
            "toml" => Ok(SuiteFileExtension::Toml),
            "yaml" => Ok(SuiteFileExtension::Yaml),
            "yml" => Ok(SuiteFileExtension::Yml),
            _ => Err(format!(
                r#"Unsupported extension: {:?}, expected "json", "toml", "yaml", or "yml""#,
                s
            )),
        }
    }
}

#[derive(Debug, new)]
pub(crate) struct TestCaseLoader<'a> {
    template: Template<AbsPathBuf>,
    extensions: &'a BTreeSet<SuiteFileExtension>,
    tester_transpilation: Option<Template<TranspilationCommand>>,
    tester_compilation: Option<Template<CompilationCommand>>,
    tester_command: Option<Template<JudgingCommand>>,
}

impl TestCaseLoader<'_> {
    pub fn load_merging(&self, problem: &str) -> TestSuiteResult<(TestCases, String)> {
        fn format_paths(paths: &[impl AsRef<str>]) -> String {
            let mut pref_common = "".to_owned();
            let mut suf_common_rev = vec![];
            let mut css = paths
                .iter()
                .map(|s| s.as_ref().chars().collect::<VecDeque<_>>())
                .collect::<Vec<_>>();
            while same_nexts(&css, VecDeque::front) {
                let mut css = css.iter_mut();
                pref_common.extend(css.next().and_then(VecDeque::pop_front));
                for cs in css {
                    cs.pop_front();
                }
            }
            while same_nexts(&css, VecDeque::back) {
                let mut css = css.iter_mut().rev();
                suf_common_rev.extend(css.next().and_then(VecDeque::pop_back));
                for cs in css {
                    cs.pop_back();
                }
            }
            let mut outcome = pref_common;
            let css = css
                .into_iter()
                .map(|cs| cs.into_iter().collect::<String>())
                .filter(|s| !s.is_empty())
                .collect::<Vec<_>>();
            let n = css.len();
            for (i, s) in css.into_iter().enumerate() {
                match i {
                    0 => outcome.push('{'),
                    _ => outcome.push_str(", "),
                }
                outcome.push_str(&s);
                if i == n - 1 {
                    outcome.push('}');
                }
            }
            outcome.extend(suf_common_rev.into_iter().rev());
            outcome
        }

        fn same_nexts<T: PartialEq + Copy>(
            xss: &[VecDeque<T>],
            f: fn(&VecDeque<T>) -> Option<&T>,
        ) -> bool {
            !xss.is_empty() && xss.iter().all(|xs| f(xs).is_some()) && {
                let mut xss = xss.iter();
                let x0 = f(xss.next().unwrap()).unwrap();
                xss.all(|xs| f(xs).unwrap() == x0)
            }
        }

        let all_paths = self
            .extensions
            .iter()
            .map(|&ext| {
                self.template
                    .clone()
                    .extension(ext)
                    .expand(Some(problem))
                    .map(|path| (path, ext))
            })
            .collect::<ExpandTemplateResult<Vec<_>>>()?;

        let existing_paths = all_paths
            .iter()
            .cloned()
            .filter(|(path, _)| path.exists())
            .collect::<Vec<_>>();

        let mut batch_cases = vec![];
        let mut interactive_cases = vec![];
        let mut filepaths = vec![];

        for (path, extension) in existing_paths {
            let filename = path
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
                .into_owned();
            let path = SuiteFilePath { path, extension };
            match TestSuite::load(&path)? {
                TestSuite::Batch(suite) => batch_cases.extend(suite.cases_named(&path.path)?),
                TestSuite::Interactive(suite) => {
                    interactive_cases.extend(suite.cases_named(
                        self.tester_transpilation.as_ref(),
                        self.tester_compilation.as_ref(),
                        self.tester_command.as_ref(),
                        &filename,
                        problem,
                    )?);
                }
                TestSuite::Unsubmittable => {
                    return Err(TestSuiteErrorKind::Unsubmittable(path.path.clone()).into());
                }
            }
            filepaths.push(path.path.display().to_string());
        }

        let paths_as_text = format_paths(&filepaths);

        match (
            NonEmptyVec::try_new(batch_cases),
            NonEmptyVec::try_new(interactive_cases),
        ) {
            (None, None) => {
                let all_paths = all_paths
                    .into_iter()
                    .map(|(path, _)| path.display().to_string())
                    .collect::<Vec<_>>();
                Err(TestSuiteErrorKind::NoTestcase(format_paths(&all_paths)).into())
            }
            (Some(_), Some(_)) => Err(TestSuiteErrorKind::DifferentTypesOfSuites.into()),
            (Some(batch_cases), None) => Ok((TestCases::Batch(batch_cases), paths_as_text)),
            (None, Some(interactive_cases)) => {
                Ok((TestCases::Interactive(interactive_cases), paths_as_text))
            }
        }
    }
}

#[derive(Debug)]
pub(crate) struct Destinations {
    scraped: Template<AbsPathBuf>,
    text_file_dir: Template<AbsPathBuf>,
    extension: SuiteFileExtension,
}

impl Destinations {
    pub(crate) fn new(
        scraped: Template<AbsPathBuf>,
        text_file_dir: Template<AbsPathBuf>,
        extension: SuiteFileExtension,
    ) -> Self {
        Destinations {
            scraped,
            text_file_dir,
            extension,
        }
    }

    pub(crate) fn expand(&self, problem: &str) -> ExpandTemplateResult<SuiteFilePath> {
        let path = self
            .scraped
            .clone()
            .extension(self.extension)
            .expand(Some(problem))?;
        Ok(SuiteFilePath::new(&path, self.extension))
    }

    pub(crate) fn text_file_dir(&self, problem: &str) -> ExpandTemplateResult<AbsPathBuf> {
        self.text_file_dir.expand(Some(problem))
    }
}

/// File path which extension is 'json', 'toml', 'yaml', or 'yml'.
#[derive(Debug)]
pub(crate) struct SuiteFilePath {
    path: AbsPathBuf,
    extension: SuiteFileExtension,
}

impl SuiteFilePath {
    pub(crate) fn new(path: &AbsPath, extension: SuiteFileExtension) -> Self {
        let path = path.to_owned();
        Self { path, extension }
    }

    pub(crate) fn display(&self) -> std::path::Display {
        self.path.display()
    }
}

impl AsRef<Path> for SuiteFilePath {
    fn as_ref(&self) -> &Path {
        self.path.as_ref()
    }
}

impl AsRef<AbsPath> for SuiteFilePath {
    fn as_ref(&self) -> &AbsPath {
        &self.path
    }
}

/// `SimpelSuite` or `InteractiveSuite`.
#[derive(Clone, Debug, From, Serialize, Deserialize)]
#[cfg_attr(test, derive(PartialEq))]
#[serde(tag = "type", rename_all = "snake_case")]
pub(crate) enum TestSuite {
    Batch(BatchSuite),
    Interactive(InteractiveSuite),
    Unsubmittable,
}

impl TestSuite {
    fn load(path: &SuiteFilePath) -> FileResult<Self> {
        let (path, ext) = (&path.path, path.extension);
        match ext {
            SuiteFileExtension::Json => crate::fs::read_json(path),
            SuiteFileExtension::Toml => crate::fs::read_toml(path),
            SuiteFileExtension::Yaml | SuiteFileExtension::Yml => crate::fs::read_yaml(path),
        }
    }

    /// Serializes `self` and save it to given path.
    pub fn save(&self, path: &SuiteFilePath) -> TestSuiteResult<()> {
        let (path, extension) = (&path.path, path.extension);
        let serialized = self.to_string_pretty(extension)?;
        crate::fs::write(path, serialized.as_bytes()).map_err(Into::into)
    }

    pub(crate) fn timelimit(&self) -> Option<Duration> {
        match self {
            TestSuite::Batch(s) => s.head.timelimit,
            TestSuite::Interactive(s) => s.timelimit,
            TestSuite::Unsubmittable => None,
        }
    }

    fn to_string_pretty(&self, ext: SuiteFileExtension) -> TestSuiteResult<String> {
        match self {
            TestSuite::Batch(this) => this.to_string_pretty(ext),
            TestSuite::Interactive(_) | TestSuite::Unsubmittable => match ext {
                SuiteFileExtension::Json => serde_json::to_string_pretty(self).ser_context(),
                SuiteFileExtension::Toml => toml::to_string_pretty(self).ser_context(),
                SuiteFileExtension::Yaml | SuiteFileExtension::Yml => {
                    serde_yaml::to_string(self).ser_context()
                }
            },
        }
    }

    #[cfg(test)]
    pub(crate) fn md5(&self) -> TestSuiteResult<md5::Digest> {
        self.to_string_pretty(SuiteFileExtension::Json)
            .map(md5::compute)
    }

    #[cfg(test)]
    pub(crate) fn assert_serialize_correctly(&self) -> Fallible<()> {
        use pretty_assertions::assert_eq;

        let serialized = self.to_string_pretty(SuiteFileExtension::Json)?;
        let deserialized = serde_json::from_str(&serialized)?;
        assert_eq!(*self, deserialized);
        Ok(())
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct BatchSuite {
    #[serde(flatten)]
    head: BatchSuiteSchemaHead,
    cases: Vec<BatchSuiteSchemaCase>,
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Default, Clone, Debug, Serialize, Deserialize)]
struct BatchSuiteSchemaHead {
    #[serde(
        serialize_with = "time::ser_millis",
        skip_serializing_if = "Option::is_none",
        deserialize_with = "time::de_secs",
        default
    )]
    timelimit: Option<Duration>,
    #[serde(rename = "match", default)]
    output_match: Match,
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug, Serialize, Deserialize)]
struct BatchSuiteSchemaCase {
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,
    #[serde(rename = "in")]
    input: BatchSuiteText,
    #[serde(rename = "out", skip_serializing_if = "Option::is_none")]
    output: Option<BatchSuiteText>,
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(untagged)]
enum BatchSuiteText {
    String(
        #[serde(
            serialize_with = "util::serde::ser_arc",
            deserialize_with = "util::serde::de_to_arc"
        )]
        Arc<String>,
    ),
    Path {
        path: PathBuf,
    },
}

impl BatchSuite {
    pub(crate) fn new(timelimit: impl Into<Option<Duration>>) -> Self {
        Self {
            head: BatchSuiteSchemaHead {
                timelimit: timelimit.into(),
                output_match: Match::default(),
            },
            cases: vec![],
        }
    }

    pub(crate) fn matching(mut self, matching: Match) -> Self {
        self.head.output_match = matching;
        self
    }

    pub(crate) fn num_cases(&self) -> usize {
        self.cases.len()
    }

    pub(crate) fn sample_cases<
        S: Into<String>,
        O: Into<Option<S>>,
        I: ExactSizeIterator<Item = (S, O)>,
    >(
        mut self,
        cases: I,
        name: fn(usize) -> String,
    ) -> Self {
        self.cases
            .extend(cases.enumerate().map(|(i, (input, output))| {
                BatchSuiteSchemaCase {
                    name: Some(name(i)),
                    input: BatchSuiteText::String(Arc::new(input.into())),
                    output: output
                        .into()
                        .map(|o| BatchSuiteText::String(Arc::new(o.into()))),
                }
            }));
        self
    }

    pub(crate) fn push_path(&mut self, name: String, in_path: AbsPathBuf, out_path: AbsPathBuf) {
        self.cases.push(BatchSuiteSchemaCase {
            name: Some(name),
            input: BatchSuiteText::Path {
                path: in_path.into(),
            },
            output: Some(BatchSuiteText::Path {
                path: out_path.into(),
            }),
        });
    }

    pub(crate) fn paths(mut self, paths: Vec<(String, AbsPathBuf, AbsPathBuf)>) -> Self {
        for (name, in_path, out_path) in paths {
            self.push_path(name, in_path, out_path);
        }
        self
    }

    pub(crate) fn clear_cases(&mut self) {
        self.cases.clear();
    }

    pub(crate) fn without_cases(mut self) -> Self {
        self.cases.clear();
        self
    }

    fn cases_named(self, path: &AbsPath) -> FileResult<Vec<BatchCase>> {
        let filename = path.file_name().unwrap_or_default().to_string_lossy();
        let dir = path.parent().unwrap_or(path);
        let dir = &dir;
        let (output_match, timelimit) = (self.head.output_match, self.head.timelimit);
        self.cases
            .into_iter()
            .enumerate()
            .map(move |(i, case)| {
                let name = match case.name {
                    None => format!("{}[{}]", filename, i),
                    Some(name) => format!("{}[{}]: {:?}", filename, i, name),
                };
                let input = match case.input {
                    BatchSuiteText::String(input) => input.clone(),
                    BatchSuiteText::Path { path } => {
                        Arc::new(crate::fs::read_to_string(&dir.join(path))?)
                    }
                };
                let output = match case.output {
                    None => None,
                    Some(BatchSuiteText::String(output)) => Some(output.clone()),
                    Some(BatchSuiteText::Path { path }) => {
                        Some(Arc::new(crate::fs::read_to_string(&dir.join(path))?))
                    }
                };
                Ok(BatchCase::new(
                    &name,
                    timelimit,
                    input,
                    output,
                    output_match,
                ))
            })
            .collect::<FileResult<Vec<_>>>()
    }

    fn to_string_pretty(&self, ext: SuiteFileExtension) -> TestSuiteResult<String> {
        #[derive(Serialize, new)]
        struct WithType<T: Serialize> {
            #[new(value = "\"batch\"")]
            r#type: &'static str,
            #[serde(flatten)]
            repr: T,
        }

        match ext {
            SuiteFileExtension::Json => {
                serde_json::to_string_pretty(&WithType::new(self)).ser_context()
            }
            SuiteFileExtension::Toml => match self.head.output_match {
                Match::Any | Match::Exact => toml::to_string_pretty(&WithType::new(self)),
                Match::Float {
                    relative_error,
                    absolute_error,
                } => {
                    #[derive(Serialize)]
                    struct Repr<'a> {
                        r#type: &'static str,
                        #[serde(serialize_with = "time::ser_millis")]
                        timelimit: Option<Duration>,
                        r#match: Float,
                        cases: &'a [BatchSuiteSchemaCase],
                    }

                    #[derive(Serialize)]
                    struct Float {
                        relative_error: Option<PositiveFinite<f64>>,
                        absolute_error: Option<PositiveFinite<f64>>,
                    }

                    let repr = Repr {
                        r#type: "batch",
                        timelimit: self.head.timelimit,
                        r#match: Float {
                            relative_error,
                            absolute_error,
                        },
                        cases: &self.cases,
                    };
                    toml::to_string_pretty(&repr)
                }
            }
            .ser_context(),
            SuiteFileExtension::Yaml | SuiteFileExtension::Yml => {
                fn quote_if_necessary(s: &str) -> Cow<str> {
                    if s.parse::<i64>().is_ok() || s.parse::<f64>().is_ok() {
                        Cow::from(s)
                    } else {
                        let mut buf = "".to_owned();
                        {
                            let mut emitter = YamlEmitter::new(&mut buf);
                            emitter.dump(&Yaml::String(s.to_owned())).unwrap();
                        }
                        if buf.starts_with("---\n") {
                            buf = buf[4..].to_owned();
                        } else {
                            buf = format!("{:?}", s);
                        }
                        Cow::from(buf)
                    }
                }

                fn is_valid(s: &str) -> bool {
                    s.ends_with('\n')
                        && s != "\n"
                        && s.chars().all(|c| {
                            [' ', '\n'].contains(&c) || (!c.is_whitespace() && !c.is_control())
                        })
                }

                let mut r;
                let cases = &self.cases;
                let none_is_path =
                    cases
                        .iter()
                        .all(
                            |BatchSuiteSchemaCase { input, output, .. }| match (input, output) {
                                (BatchSuiteText::String(_), Some(BatchSuiteText::String(_))) => {
                                    true
                                }
                                (BatchSuiteText::String(_), None) => true,
                                _ => false,
                            },
                        );
                if !cases.is_empty() && none_is_path {
                    r = serde_yaml::to_string(&WithType::new(&self.head)).ser_context()?;
                    r += "\ncases:\n";
                    for BatchSuiteSchemaCase {
                        name,
                        input,
                        output,
                    } in cases
                    {
                        if let Some(name) = name {
                            write!(r, "  - name: {}\n    ", quote_if_necessary(name)).unwrap();
                        } else {
                            r += "  - ";
                        }
                        match input {
                            BatchSuiteText::String(input) if is_valid(input) => {
                                r += "in: |\n";
                                for l in input.lines() {
                                    writeln!(r, "      {}", l).unwrap();
                                }
                            }
                            BatchSuiteText::String(input) => {
                                let input = quote_if_necessary(input);
                                writeln!(r, "in: {}", input).unwrap();
                            }
                            BatchSuiteText::Path { .. } => unreachable!(),
                        }
                        if let Some(output) = output {
                            match output {
                                BatchSuiteText::String(output) if is_valid(output) => {
                                    r += "    out: |\n";
                                    for l in output.lines() {
                                        writeln!(r, "      {}", l).unwrap();
                                    }
                                }
                                BatchSuiteText::String(output) => {
                                    let output = quote_if_necessary(output);
                                    writeln!(r, "    out: {}", output).unwrap();
                                }
                                BatchSuiteText::Path { .. } => unreachable!(),
                            }
                        }
                    }
                } else {
                    r = serde_yaml::to_string(&WithType::new(self)).ser_context()?;
                }
                Ok(r)
            }
        }
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub(crate) struct InteractiveSuite {
    #[serde(
        serialize_with = "time::ser_millis",
        skip_serializing_if = "Option::is_none",
        deserialize_with = "time::de_secs",
        default
    )]
    timelimit: Option<Duration>,
    each_args: Vec<Vec<String>>,
}

impl InteractiveSuite {
    pub fn new(timelimit: impl Into<Option<Duration>>) -> Self {
        Self {
            timelimit: timelimit.into(),
            each_args: vec![],
        }
    }

    fn cases_named(
        &self,
        tester_transpilation: Option<&Template<TranspilationCommand>>,
        tester_compilation: Option<&Template<CompilationCommand>>,
        tester_command: Option<&Template<JudgingCommand>>,
        filename: &str,
        problem: &str,
    ) -> TestSuiteResult<vec::IntoIter<InteractiveCase>> {
        fn number_arg(i: usize, arg: &str) -> Vec<(String, String)> {
            vec![
                (format!("arg{}", i), arg.to_string()),
                (format!("SNOWCHAINS_ARG{}", i), arg.to_string()),
            ]
        }

        let mut cases = Vec::with_capacity(self.each_args.len());
        for (i, args) in self.each_args.iter().enumerate() {
            let mut m = hashmap!(
                "arg_joined".to_owned() => args.join(" "),
                "SNOWCHAINS_ARG_JOINED".to_owned() => args.join(" "),
            );
            m.extend(
                args.iter()
                    .enumerate()
                    .zip_longest(0..=9)
                    .flat_map(|p| match p {
                        EitherOrBoth::Both((_, arg), i) => number_arg(i, arg),
                        EitherOrBoth::Left((i, arg)) => number_arg(i, arg),
                        EitherOrBoth::Right(i) => number_arg(i, ""),
                    }),
            );
            let tester_transpilation = match tester_transpilation {
                None => None,
                Some(t) => Some(Arc::new(t.expand(problem)?)),
            };
            let tester_compilation = match tester_compilation {
                None => None,
                Some(t) => Some(Arc::new(t.expand(problem)?)),
            };
            let tester = tester_command
                .ok_or_else(|| ConfigError::from(ConfigErrorKind::TesterNotSpecified))?
                .clone()
                .envs(m)
                .expand(problem)?;
            cases.push(InteractiveCase {
                name: Arc::new(format!("{}[{}]", filename, i)),
                tester: Arc::new(tester),
                tester_transpilation,
                tester_compilation,
                timelimit: self.timelimit,
            });
        }
        Ok(cases.into_iter())
    }
}

trait SerContext {
    type Item;
    fn ser_context(self) -> TestSuiteResult<Self::Item>;
}

impl<T, E: std::error::Error + Send + Sync + 'static> SerContext for std::result::Result<T, E> {
    type Item = T;

    fn ser_context(self) -> TestSuiteResult<T> {
        self.map_err(|e| {
            StdError::from(e)
                .context(TestSuiteErrorKind::Serialize)
                .into()
        })
    }
}

/// `Vec<BatchCase>` or `Vec<ReducibleCase>`.
#[derive(Debug)]
pub(crate) enum TestCases {
    Batch(NonEmptyVec<BatchCase>),
    Interactive(NonEmptyVec<InteractiveCase>),
}

impl TestCases {
    pub(crate) fn interactive_tester_transpilations(&self) -> HashSet<Arc<TranspilationCommand>> {
        match self {
            TestCases::Batch(_) => hashset!(),
            TestCases::Interactive(cases) => cases
                .iter()
                .filter_map(|case| case.tester_transpilation.clone())
                .collect(),
        }
    }

    pub(crate) fn interactive_tester_compilations(&self) -> HashSet<Arc<CompilationCommand>> {
        match self {
            TestCases::Batch(_) => hashset!(),
            TestCases::Interactive(cases) => {
                let compilations = cases
                    .iter()
                    .filter_map(|case| case.tester_compilation.clone());
                HashSet::from_iter(compilations)
            }
        }
    }
}

pub(crate) trait TestCase: fmt::Debug + Clone + Serialize {
    /// Gets `name`.
    fn name(&self) -> Arc<String>;
}

/// Pair of `input` and `expected`.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct BatchCase {
    #[serde(serialize_with = "util::serde::ser_arc")]
    name: Arc<String>,
    #[serde(serialize_with = "util::serde::ser_arc")]
    input: Arc<String>,
    expected: ExpectedStdout,
    timelimit: Option<Duration>,
}

impl TestCase for BatchCase {
    fn name(&self) -> Arc<String> {
        self.name.clone()
    }
}

impl BatchCase {
    fn new(
        name: &str,
        timelimit: Option<Duration>,
        input: Arc<String>,
        output: Option<Arc<String>>,
        output_match: Match,
    ) -> Self {
        let expected = match (output_match, output) {
            (Match::Any, example) => ExpectedStdout::Any { example },
            (Match::Exact, None) | (Match::Float { .. }, None) => {
                ExpectedStdout::Any { example: None }
            }
            (Match::Exact, Some(output)) => ExpectedStdout::Exact(output),
            (
                Match::Float {
                    relative_error,
                    absolute_error,
                },
                Some(string),
            ) => ExpectedStdout::Float {
                string,
                errors: FloatErrors {
                    relative: relative_error,
                    absolute: absolute_error,
                },
            },
        };
        Self {
            name: Arc::new(name.to_owned()),
            input,
            expected,
            timelimit,
        }
    }

    pub(crate) fn input(&self) -> Arc<String> {
        self.input.clone()
    }

    pub(crate) fn expected(&self) -> &ExpectedStdout {
        &self.expected
    }

    pub(crate) fn timelimit(&self) -> Option<Duration> {
        self.timelimit
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ExpectedStdout {
    Any {
        #[serde(serialize_with = "util::serde::ser_option_arc")]
        example: Option<Arc<String>>,
    },
    Exact(#[serde(serialize_with = "util::serde::ser_arc")] Arc<String>),
    Float {
        #[serde(serialize_with = "util::serde::ser_arc")]
        string: Arc<String>,
        errors: FloatErrors,
    },
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Match {
    Any,
    Exact,
    Float {
        relative_error: Option<PositiveFinite<f64>>,
        absolute_error: Option<PositiveFinite<f64>>,
    },
}

impl Default for Match {
    fn default() -> Self {
        Match::Exact
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Copy, Debug, Serialize)]
pub(crate) struct FloatErrors {
    pub(crate) relative: Option<PositiveFinite<f64>>,
    pub(crate) absolute: Option<PositiveFinite<f64>>,
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug, Serialize)]
pub(crate) struct InteractiveCase {
    #[serde(serialize_with = "util::serde::ser_arc")]
    name: Arc<String>,
    timelimit: Option<Duration>,
    #[serde(serialize_with = "util::serde::ser_arc")]
    tester: Arc<JudgingCommand>,
    #[serde(serialize_with = "util::serde::ser_option_arc")]
    tester_transpilation: Option<Arc<TranspilationCommand>>,
    #[serde(serialize_with = "util::serde::ser_option_arc")]
    tester_compilation: Option<Arc<CompilationCommand>>,
}

impl TestCase for InteractiveCase {
    fn name(&self) -> Arc<String> {
        self.name.clone()
    }
}

impl InteractiveCase {
    pub fn tester(&self) -> Arc<JudgingCommand> {
        self.tester.clone()
    }

    pub fn timelimit(&self) -> Option<Duration> {
        self.timelimit
    }
}

#[cfg(test)]
mod tests {
    use crate::config;
    use crate::errors::{ConfigErrorKind, TestSuiteError, TestSuiteErrorKind, TestSuiteResult};
    use crate::path::AbsPathBuf;
    use crate::service::ServiceKind;
    use crate::template::{AbsPathBufRequirements, JudgingCommandRequirements, TemplateBuilder};
    use crate::testsuite::{
        BatchSuite, BatchSuiteSchemaCase, BatchSuiteSchemaHead, BatchSuiteText, InteractiveSuite,
        Match, SuiteFileExtension, TestCaseLoader, TestCases, TestSuite,
    };
    use crate::util::num::PositiveFinite;

    use difference::assert_diff;
    use failure::Fallible;
    use if_chain::if_chain;
    use maplit::{btreeset, hashmap};
    use pretty_assertions::assert_eq;
    use tempdir::TempDir;

    use std::convert::TryFrom as _;
    use std::env;
    use std::path::Path;
    use std::sync::Arc;
    use std::time::Duration;

    #[test]
    fn test_suite_file_extension_from_str() -> std::result::Result<(), String> {
        "json".parse::<SuiteFileExtension>()?;
        "toml".parse::<SuiteFileExtension>()?;
        "yaml".parse::<SuiteFileExtension>()?;
        "yml".parse::<SuiteFileExtension>()?;
        "".parse::<SuiteFileExtension>().unwrap_err();
        Ok(())
    }

    #[test]
    fn test_test_case_loader() -> Fallible<()> {
        static ATCODER_ABC117_A_YML: &str = r#"---
type: batch
timelimit: 2000ms
match:
  float:
    relative_error: 0.001
    absolute_error: 0.001
cases:
  - name: Sample 1
    in: |
      8 3
    out: |
      2.6666666667
  - name: Sample 2
    in: |
      99 1
    out: |
      99.0000000000
  - name: Sample 3
    in: |
      1 100
    out: |
      0.0100000000
"#;
        static ATCODER_ABC117_B_JSON: &str = r#"{
  "timelimit": "2000ms",
  "type": "batch",
  "match": "exact",
  "cases": []
}
"#;
        static ATCODER_ABC117_B_TOML: &str = r#"type = "batch"
timelimit = "2000ms"
match = "exact"

[[cases]]
name = "Sample 1"
in = '''
4
3 8 5 1
'''
out = '''
Yes
'''
"#;
        static ATCODER_ABC117_B_YML: &str = r#"---
type: batch
timelimit: 2000ms
match: exact
cases:
  - name: Sample 2
    in: |
      4
      3 8 4 1
    out: |
      No
  - name: Sample 3
    in: |
      10
      1 8 10 5 8 12 34 100 11 3
    out: |
      No
"#;
        static ATCODER_ARC078_E_YML: &str = r#"---
type: interactive
timelimit: 2000ms
each_args:
  - ["12345678"]
"#;
        static ATCODER_APG4B_B_YML: &str = r#"---
type: unsubmittable
"#;

        let tempdir = dunce::canonicalize(&env::temp_dir())?;
        let tempdir = TempDir::new_in(&tempdir, "test_test_case_loader")?;
        let template_builder =
            "${service}/${snake_case(contest)}/tests/${snake_case(problem)}.${extension}"
                .parse::<TemplateBuilder<AbsPathBuf>>()?;

        let template = template_builder.build(AbsPathBufRequirements {
            base_dir: AbsPathBuf::try_new(tempdir.path()).unwrap(),
            service: ServiceKind::Atcoder,
            contest: "abc117".to_owned(),
            mode: None,
        });
        let test_dir = tempdir.path().join("atcoder").join("abc117").join("tests");
        std::fs::create_dir_all(&test_dir)?;
        std::fs::write(test_dir.join("a.yml"), ATCODER_ABC117_A_YML)?;
        std::fs::write(test_dir.join("b.json"), ATCODER_ABC117_B_JSON)?;
        std::fs::write(test_dir.join("b.toml"), ATCODER_ABC117_B_TOML)?;
        std::fs::write(test_dir.join("b.yml"), ATCODER_ABC117_B_YML)?;

        let extensions = btreeset![SuiteFileExtension::Yml];
        let loader = TestCaseLoader {
            template: template.clone(),
            extensions: &extensions,
            tester_transpilation: None,
            tester_compilation: None,
            tester_command: None,
        };
        let (cases, joined_paths) = loader.load_merging("a")?;
        match cases {
            TestCases::Batch(cases) => assert_eq!(cases.len().get(), 3),
            TestCases::Interactive(_) => panic!("{:?}", cases),
        }
        assert_eq!(Path::new(&joined_paths), test_dir.join("a.yml"));

        let extensions = btreeset![
            SuiteFileExtension::Json,
            SuiteFileExtension::Toml,
            SuiteFileExtension::Yml,
        ];
        let loader = TestCaseLoader {
            template: template.clone(),
            extensions: &extensions,
            tester_transpilation: None,
            tester_compilation: None,
            tester_command: None,
        };
        let (cases, joined_paths) = loader.load_merging("b")?;
        match cases {
            TestCases::Batch(cases) => assert_eq!(cases.len().get(), 3),
            TestCases::Interactive(_) => panic!("{:?}", cases),
        }
        assert_eq!(
            Path::new(&joined_paths),
            test_dir.join("b.{json, toml, yml}"),
        );

        let extensions = btreeset![SuiteFileExtension::Json];
        let loader = TestCaseLoader {
            template,
            extensions: &extensions,
            tester_transpilation: None,
            tester_compilation: None,
            tester_command: None,
        };
        for problem in &["b", "nonexisting"] {
            if_chain! {
                let err = loader.load_merging(problem).unwrap_err();
                if let TestSuiteError::Context(ctx) = &err;
                if let TestSuiteErrorKind::NoTestcase(joined_paths) = ctx.get_context();
                then {
                    assert_eq!(
                        Path::new(joined_paths),
                        test_dir.join(problem).with_extension("json"),
                    );
                } else {
                    return Err(err.into());
                }
            }
        }

        let template = template_builder.build(AbsPathBufRequirements {
            base_dir: AbsPathBuf::try_new(tempdir.path()).unwrap(),
            service: ServiceKind::Atcoder,
            contest: "arc078".to_owned(),
            mode: None,
        });
        let tester_command = TemplateBuilder::bash().build(JudgingCommandRequirements {
            base_dir: AbsPathBuf::try_new(tempdir.path()).unwrap(),
            service: ServiceKind::Atcoder,
            contest: "arc078".to_owned(),
            mode: config::Mode::Debug,
            shell: hashmap!(),
            working_dir: ".".parse()?,
            src: "tester".parse()?,
            transpiled: None,
            bin: None,
            crlf_to_lf: false,
        });
        let test_dir = tempdir.path().join("atcoder").join("arc078").join("tests");
        std::fs::create_dir_all(&test_dir)?;
        std::fs::write(test_dir.join("e.yml"), ATCODER_ARC078_E_YML)?;
        std::fs::write(test_dir.join("e.toml"), ATCODER_ABC117_B_TOML)?;

        let extensions = btreeset![SuiteFileExtension::Yml];
        let loader = TestCaseLoader {
            template: template.clone(),
            extensions: &extensions,
            tester_transpilation: None,
            tester_compilation: None,
            tester_command: Some(tester_command.clone()),
        };
        let (cases, joined_paths) = loader.load_merging("e")?;
        if let TestCases::Batch(_) = cases {
            panic!("{:?}", cases);
        }
        assert_eq!(Path::new(&joined_paths), test_dir.join("e.yml"));

        let extensions = btreeset![SuiteFileExtension::Toml, SuiteFileExtension::Yml];
        let loader = TestCaseLoader {
            template: template.clone(),
            extensions: &extensions,
            tester_transpilation: None,
            tester_compilation: None,
            tester_command: Some(tester_command),
        };
        if_chain! {
            let err = loader.load_merging("e").unwrap_err();
            if let TestSuiteError::Context(ctx) = &err;
            if let TestSuiteErrorKind::DifferentTypesOfSuites = ctx.get_context();
            then {} else { return Err(err.into()) }
        }

        let extensions = btreeset![SuiteFileExtension::Yml];
        let loader = TestCaseLoader {
            template,
            extensions: &extensions,
            tester_transpilation: None,
            tester_compilation: None,
            tester_command: None,
        };
        if_chain! {
            let err = loader.load_merging("e").unwrap_err();
            if let TestSuiteError::Config(config_err) = &err;
            if let ConfigErrorKind::TesterNotSpecified = config_err.kind();
            then {} else { return Err(err.into()) }
        }

        let template = template_builder.build(AbsPathBufRequirements {
            base_dir: AbsPathBuf::try_new(tempdir.path()).unwrap(),
            service: ServiceKind::Atcoder,
            contest: "apg4b".to_owned(),
            mode: None,
        });
        let test_dir = tempdir.path().join("atcoder").join("apg4b").join("tests");
        std::fs::create_dir_all(&test_dir)?;
        std::fs::write(test_dir.join("b.yml"), ATCODER_APG4B_B_YML)?;

        let extensions = btreeset![SuiteFileExtension::Yml];
        let loader = TestCaseLoader {
            template,
            extensions: &extensions,
            tester_transpilation: None,
            tester_compilation: None,
            tester_command: None,
        };
        if_chain! {
            let err = loader.load_merging("b").unwrap_err();
            if let TestSuiteError::Context(ctx) = &err;
            if let TestSuiteErrorKind::Unsubmittable(path) = ctx.get_context();
            then {
                assert_eq!(*path, test_dir.join("b.yml"));
                Ok(())
            } else {
                Err(err.into())
            }
        }
    }

    #[test]
    fn test_to_string_pretty() -> TestSuiteResult<()> {
        let atcoder_arc100_c = TestSuite::Batch(BatchSuite {
            head: BatchSuiteSchemaHead {
                timelimit: Some(Duration::from_secs(2)),
                output_match: Match::Exact,
            },
            cases: vec![
                BatchSuiteSchemaCase {
                    name: Some("Sample 1".to_owned()),
                    input: BatchSuiteText::string("5\n2 2 3 5 5\n"),
                    output: Some(BatchSuiteText::string("2\n")),
                },
                BatchSuiteSchemaCase {
                    name: Some("Sample 2".to_owned()),
                    input: BatchSuiteText::string("9\n1 2 3 4 5 6 7 8 9\n"),
                    output: Some(BatchSuiteText::string("0\n")),
                },
                BatchSuiteSchemaCase {
                    name: Some("Sample 3".to_owned()),
                    input: BatchSuiteText::string("6\n6 5 4 3 2 1\n"),
                    output: Some(BatchSuiteText::string("18\n")),
                },
                BatchSuiteSchemaCase {
                    name: Some("Sample 4".to_owned()),
                    input: BatchSuiteText::string("7\n1 1 1 1 2 3 4\n"),
                    output: Some(BatchSuiteText::string("6\n")),
                },
            ],
        });
        assert_diff!(
            &atcoder_arc100_c.to_string_pretty(SuiteFileExtension::Json)?,
            r#"{
  "type": "batch",
  "timelimit": "2000ms",
  "match": "exact",
  "cases": [
    {
      "name": "Sample 1",
      "in": "5\n2 2 3 5 5\n",
      "out": "2\n"
    },
    {
      "name": "Sample 2",
      "in": "9\n1 2 3 4 5 6 7 8 9\n",
      "out": "0\n"
    },
    {
      "name": "Sample 3",
      "in": "6\n6 5 4 3 2 1\n",
      "out": "18\n"
    },
    {
      "name": "Sample 4",
      "in": "7\n1 1 1 1 2 3 4\n",
      "out": "6\n"
    }
  ]
}"#,
            "\n",
            0
        );
        assert_diff!(
            &atcoder_arc100_c.to_string_pretty(SuiteFileExtension::Toml)?,
            r#"type = 'batch'
timelimit = '2000ms'
match = 'exact'

[[cases]]
name = 'Sample 1'
in = '''
5
2 2 3 5 5
'''
out = '''
2
'''

[[cases]]
name = 'Sample 2'
in = '''
9
1 2 3 4 5 6 7 8 9
'''
out = '''
0
'''

[[cases]]
name = 'Sample 3'
in = '''
6
6 5 4 3 2 1
'''
out = '''
18
'''

[[cases]]
name = 'Sample 4'
in = '''
7
1 1 1 1 2 3 4
'''
out = '''
6
'''
"#,
            "\n",
            0
        );
        assert_diff!(
            &atcoder_arc100_c.to_string_pretty(SuiteFileExtension::Yml)?,
            r#"---
type: batch
timelimit: 2000ms
match: exact
cases:
  - name: Sample 1
    in: |
      5
      2 2 3 5 5
    out: |
      2
  - name: Sample 2
    in: |
      9
      1 2 3 4 5 6 7 8 9
    out: |
      0
  - name: Sample 3
    in: |
      6
      6 5 4 3 2 1
    out: |
      18
  - name: Sample 4
    in: |
      7
      1 1 1 1 2 3 4
    out: |
      6
"#,
            "\n",
            0
        );

        let atcoder_abc117_a = TestSuite::Batch(BatchSuite {
            head: BatchSuiteSchemaHead {
                timelimit: Some(Duration::from_secs(2)),
                output_match: Match::Float {
                    relative_error: Some(PositiveFinite::try_from(0.001).unwrap()),
                    absolute_error: Some(PositiveFinite::try_from(0.001).unwrap()),
                },
            },
            cases: vec![
                BatchSuiteSchemaCase {
                    name: Some("Sample 1".to_owned()),
                    input: BatchSuiteText::string("8 3\n"),
                    output: Some(BatchSuiteText::string("2.6666666667\n")),
                },
                BatchSuiteSchemaCase {
                    name: Some("Sample 2".to_owned()),
                    input: BatchSuiteText::string("99 1\n"),
                    output: Some(BatchSuiteText::string("99.0000000000\n")),
                },
                BatchSuiteSchemaCase {
                    name: Some("Sample 3".to_owned()),
                    input: BatchSuiteText::string("1 100\n"),
                    output: Some(BatchSuiteText::string("0.0100000000\n")),
                },
            ],
        });
        assert_diff!(
            &atcoder_abc117_a.to_string_pretty(SuiteFileExtension::Json)?,
            r#"{
  "type": "batch",
  "timelimit": "2000ms",
  "match": {
    "float": {
      "relative_error": 0.001,
      "absolute_error": 0.001
    }
  },
  "cases": [
    {
      "name": "Sample 1",
      "in": "8 3\n",
      "out": "2.6666666667\n"
    },
    {
      "name": "Sample 2",
      "in": "99 1\n",
      "out": "99.0000000000\n"
    },
    {
      "name": "Sample 3",
      "in": "1 100\n",
      "out": "0.0100000000\n"
    }
  ]
}"#,
            "\n",
            0
        );
        assert_diff!(
            &atcoder_abc117_a.to_string_pretty(SuiteFileExtension::Toml)?,
            r#"type = 'batch'
timelimit = '2000ms'

[match]
relative_error = 0.001
absolute_error = 0.001

[[cases]]
name = 'Sample 1'
in = '''
8 3
'''
out = '''
2.6666666667
'''

[[cases]]
name = 'Sample 2'
in = '''
99 1
'''
out = '''
99.0000000000
'''

[[cases]]
name = 'Sample 3'
in = '''
1 100
'''
out = '''
0.0100000000
'''
"#,
            "\n",
            0
        );
        assert_diff!(
            &atcoder_abc117_a.to_string_pretty(SuiteFileExtension::Yml)?,
            r#"---
type: batch
timelimit: 2000ms
match:
  float:
    relative_error: 0.001
    absolute_error: 0.001
cases:
  - name: Sample 1
    in: |
      8 3
    out: |
      2.6666666667
  - name: Sample 2
    in: |
      99 1
    out: |
      99.0000000000
  - name: Sample 3
    in: |
      1 100
    out: |
      0.0100000000
"#,
            "\n",
            0
        );

        let atcoder_arc078_e = TestSuite::Interactive(InteractiveSuite {
            timelimit: Some(Duration::from_secs(2)),
            each_args: vec![vec![]],
        });
        assert_diff!(
            &atcoder_arc078_e.to_string_pretty(SuiteFileExtension::Json)?,
            r#"{
  "type": "interactive",
  "timelimit": "2000ms",
  "each_args": [
    []
  ]
}"#,
            "\n",
            0
        );
        assert_diff!(
            &atcoder_arc078_e.to_string_pretty(SuiteFileExtension::Toml)?,
            r#"type = 'interactive'
timelimit = '2000ms'
each_args = [[]]
"#,
            "\n",
            0
        );
        assert_diff!(
            &atcoder_arc078_e.to_string_pretty(SuiteFileExtension::Yml)?,
            r#"---
type: interactive
timelimit: 2000ms
each_args:
  - []"#,
            "\n",
            0
        );

        let atcoder_apg4b_b = TestSuite::Unsubmittable;
        assert_diff!(
            &atcoder_apg4b_b.to_string_pretty(SuiteFileExtension::Json)?,
            r#"{
  "type": "unsubmittable"
}"#,
            "\n",
            0
        );
        assert_diff!(
            &atcoder_apg4b_b.to_string_pretty(SuiteFileExtension::Toml)?,
            r#"type = 'unsubmittable'
"#,
            "\n",
            0
        );
        assert_diff!(
            &atcoder_apg4b_b.to_string_pretty(SuiteFileExtension::Yml)?,
            r#"---
type: unsubmittable"#,
            "\n",
            0
        );

        Ok(())
    }

    impl BatchSuiteText {
        fn string(s: &str) -> Self {
            BatchSuiteText::String(Arc::new(s.to_owned()))
        }
    }
}
