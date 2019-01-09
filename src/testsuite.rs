use crate::command::{CompilationCommand, JudgingCommand, TranspilationCommand};
use crate::errors::{
    ConfigError, ConfigErrorKind, ExpandTemplateResult, FileResult, StdError, TestSuiteErrorKind,
    TestSuiteResult,
};
use crate::path::{AbsPath, AbsPathBuf};
use crate::template::Template;
use crate::terminal::WriteAnsi;
use crate::{time, yaml};

use derive_more::From;
use derive_new::new;
use failure::Fail;
use itertools::{EitherOrBoth, Itertools};
use maplit::{hashmap, hashset};
use serde::Serialize;
use serde_derive::{Deserialize, Serialize};

use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use std::fmt::Write;
use std::iter::FromIterator;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;
use std::{f64, str, vec};

pub(crate) fn modify_timelimit(
    stdout: impl WriteAnsi,
    name: &str,
    path: &SuiteFilePath,
    timelimit: Option<Duration>,
) -> TestSuiteResult<()> {
    let mut suite = TestSuite::load(path)?;
    suite.modify_timelimit(&path.path, timelimit)?;
    suite.save(name, path, stdout)
}

pub(crate) fn modify_append(
    name: &str,
    path: &SuiteFilePath,
    input: &str,
    output: Option<&str>,
    stdout: impl WriteAnsi,
) -> TestSuiteResult<()> {
    let mut suite = TestSuite::load(path)?;
    suite.append(input, output)?;
    suite.save(name, path, stdout)
}

pub(crate) fn modify_match(
    stdout: impl WriteAnsi,
    name: &str,
    path: &SuiteFilePath,
    output_match: Match,
) -> TestSuiteResult<()> {
    let mut suite = TestSuite::load(path)?;
    suite.modify_match(output_match)?;
    suite.save(name, path, stdout)
}

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
            _ => Err(format!("Unsupported extension: {:?}", s)),
        }
    }
}

#[derive(new)]
pub(crate) struct TestCaseLoader<'a> {
    template: Template<AbsPathBuf>,
    extensions: &'a BTreeSet<SuiteFileExtension>,
    tester_transpilations: HashMap<String, Template<TranspilationCommand>>,
    tester_compilacions: HashMap<String, Template<CompilationCommand>>,
    tester_commands: HashMap<String, Template<JudgingCommand>>,
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
                pref_common.extend(css.next().and_then(|cs| cs.pop_front()));
                for cs in css {
                    cs.pop_front();
                }
            }
            while same_nexts(&css, VecDeque::back) {
                let mut css = css.iter_mut().rev();
                suf_common_rev.extend(css.next().and_then(|cs| cs.pop_back()));
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
                    .expand(problem)
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
                        &self.tester_transpilations,
                        &self.tester_compilacions,
                        &self.tester_commands,
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

        if batch_cases.is_empty() && interactive_cases.is_empty() {
            let all_paths = all_paths
                .into_iter()
                .map(|(path, _)| path.display().to_string())
                .collect::<Vec<_>>();
            Err(TestSuiteErrorKind::NoFile(format_paths(&all_paths)).into())
        } else if interactive_cases.is_empty() {
            Ok((TestCases::Batch(batch_cases), paths_as_text))
        } else if batch_cases.is_empty() {
            Ok((TestCases::Interactive(interactive_cases), paths_as_text))
        } else {
            Err(TestSuiteErrorKind::DifferentTypesOfSuites.into())
        }
    }
}

pub(crate) struct DownloadDestinations {
    scraped: Template<AbsPathBuf>,
    text_file_dir: Template<AbsPathBuf>,
    extension: SuiteFileExtension,
}

impl DownloadDestinations {
    pub(crate) fn new(
        scraped: Template<AbsPathBuf>,
        text_file_dir: Template<AbsPathBuf>,
        extension: SuiteFileExtension,
    ) -> Self {
        Self {
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
            .expand(problem)?;
        Ok(SuiteFilePath::new(&path, self.extension))
    }

    pub(crate) fn text_file_dir(&self, problem: &str) -> ExpandTemplateResult<AbsPathBuf> {
        self.text_file_dir.expand(problem)
    }
}

/// File path which extension is 'json', 'toml', 'yaml', or 'yml'.
pub(crate) struct SuiteFilePath {
    path: AbsPathBuf,
    extension: SuiteFileExtension,
}

impl SuiteFilePath {
    pub(crate) fn new(path: &AbsPath, extension: SuiteFileExtension) -> Self {
        let path = path.to_owned();
        Self { path, extension }
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
    pub fn save(
        &self,
        name: &str,
        path: &SuiteFilePath,
        mut out: impl WriteAnsi,
    ) -> TestSuiteResult<()> {
        let (path, extension) = (&path.path, path.extension);
        let serialized = self.to_string_pretty(extension)?;
        crate::fs::write(path, serialized.as_bytes())?;
        out.with_reset(|o| o.bold()?.write_str(name))?;
        write!(out, ": Saved to {} ", path.display())?;
        match self {
            TestSuite::Batch(s) => match s.cases.len() {
                0 => out.with_reset(|o| o.fg(11)?.write_str("(no test case)\n")),
                1 => out.with_reset(|o| o.fg(10)?.write_str("(1 test case)\n")),
                n => out.with_reset(|o| writeln!(o.fg(10)?, "({} test cases)", n)),
            },
            TestSuite::Interactive(_) => {
                out.with_reset(|o| o.fg(10)?.write_str("(interactive problem)\n"))
            }
            TestSuite::Unsubmittable => {
                out.with_reset(|o| o.fg(10)?.write_str("(unsubmittable problem)\n"))
            }
        }
        .map_err(Into::into)
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

    fn modify_timelimit(
        &mut self,
        path: &AbsPath,
        timelimit: Option<Duration>,
    ) -> TestSuiteResult<()> {
        match self {
            TestSuite::Batch(suite) => {
                suite.head.timelimit = timelimit;
                Ok(())
            }
            TestSuite::Interactive(suite) => {
                suite.timelimit = timelimit;
                Ok(())
            }
            TestSuite::Unsubmittable => {
                Err(TestSuiteErrorKind::Unsubmittable(path.to_owned()).into())
            }
        }
    }

    fn append(&mut self, input: &str, output: Option<&str>) -> TestSuiteResult<()> {
        match self {
            TestSuite::Batch(suite) => {
                suite.append(input, output);
                Ok(())
            }
            _ => Err(TestSuiteErrorKind::SuiteIsNotBatch.into()),
        }
    }

    fn modify_match(&mut self, output_match: Match) -> TestSuiteResult<()> {
        match self {
            TestSuite::Batch(suite) => {
                suite.head.output_match = output_match;
                Ok(())
            }
            _ => Err(TestSuiteErrorKind::SuiteIsNotBatch.into()),
        }
    }

    #[cfg(test)]
    pub(crate) fn md5(&self) -> serde_json::Result<md5::Digest> {
        serde_json::to_string_pretty(self).map(md5::compute)
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
    String(String),
    Path { path: PathBuf },
}

impl BatchSuiteText {
    /// # Panics
    ///
    /// Panics if `self` is `Path`.
    fn unwrap_str(&self) -> &str {
        match self {
            BatchSuiteText::String(s) => s,
            BatchSuiteText::Path { .. } => panic!("called `BatchSuiteText::unwrap_str` on Path"),
        }
    }
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

    pub(crate) fn sample_cases<
        S: Into<String>,
        O: Into<Option<S>>,
        I: ExactSizeIterator<Item = (S, O)>,
    >(
        mut self,
        cases: I,
        name: fn(usize) -> String,
        name_single: Option<&str>,
    ) -> Self {
        let cases_len = cases.len();
        self.cases.extend(
            cases
                .enumerate()
                .map(|(i, (input, output))| BatchSuiteSchemaCase {
                    name: Some(match (name_single, cases_len) {
                        (Some(name_single), 0) => name_single.to_owned(),
                        _ => name(i),
                    }),
                    input: BatchSuiteText::String(input.into()),
                    output: output.into().map(|o| BatchSuiteText::String(o.into())),
                }),
        );
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
                    BatchSuiteText::String(input) => input,
                    BatchSuiteText::Path { path } => crate::fs::read_to_string(&dir.join(path))?,
                };
                let output = match case.output {
                    None => None,
                    Some(BatchSuiteText::String(output)) => Some(output),
                    Some(BatchSuiteText::Path { path }) => {
                        Some(crate::fs::read_to_string(&dir.join(path))?)
                    }
                };
                Ok(BatchCase::new(
                    &name,
                    timelimit,
                    &input,
                    output.as_ref().map(AsRef::as_ref),
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

        fn is_valid(s: &str) -> bool {
            s.ends_with('\n')
                && s.chars()
                    .all(|c| [' ', '\n'].contains(&c) || (!c.is_whitespace() && !c.is_control()))
        }

        match ext {
            SuiteFileExtension::Json => {
                serde_json::to_string_pretty(&WithType::new(self)).ser_context()
            }
            SuiteFileExtension::Toml => toml::to_string_pretty(&WithType::new(self)).ser_context(),
            SuiteFileExtension::Yaml | SuiteFileExtension::Yml => {
                let mut r;
                let cases = &self.cases;
                let all_valid =
                    cases
                        .iter()
                        .all(
                            |BatchSuiteSchemaCase { input, output, .. }| match (input, output) {
                                (BatchSuiteText::String(i), None) => is_valid(i),
                                (BatchSuiteText::String(i), Some(BatchSuiteText::String(o))) => {
                                    is_valid(i) && is_valid(o)
                                }
                                _ => false,
                            },
                        );
                if !cases.is_empty() && all_valid {
                    r = serde_yaml::to_string(&WithType::new(&self.head)).ser_context()?;
                    r += "\ncases:\n";
                    for BatchSuiteSchemaCase {
                        name,
                        input,
                        output,
                    } in cases
                    {
                        if let Some(name) = name {
                            write!(r, "  - name: {}\n    ", yaml::escape_string(name)).unwrap();
                        } else {
                            r += "  - ";
                        }
                        r += "in: |\n";
                        for l in input.unwrap_str().lines() {
                            writeln!(r, "      {}", l).unwrap();
                        }
                        if let Some(output) = output {
                            r += "    out: |\n";
                            for l in output.unwrap_str().lines() {
                                writeln!(r, "      {}", l).unwrap();
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

    fn append(&mut self, input: &str, output: Option<&str>) {
        self.cases.push(BatchSuiteSchemaCase {
            name: None,
            input: BatchSuiteText::String(input.to_owned()),
            output: output.map(|o| BatchSuiteText::String(o.to_owned())),
        });
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
    tester: Option<String>,
    each_args: Vec<Vec<String>>,
}

impl InteractiveSuite {
    pub fn new(timelimit: impl Into<Option<Duration>>) -> Self {
        Self {
            timelimit: timelimit.into(),
            tester: None,
            each_args: vec![],
        }
    }

    fn cases_named(
        &self,
        tester_transpilations: &HashMap<String, Template<TranspilationCommand>>,
        tester_compilations: &HashMap<String, Template<CompilationCommand>>,
        tester_commands: &HashMap<String, Template<JudgingCommand>>,
        filename: &str,
        problem: &str,
    ) -> TestSuiteResult<vec::IntoIter<InteractiveCase>> {
        let mut cases = Vec::with_capacity(self.each_args.len());
        for (i, args) in self.each_args.iter().enumerate() {
            let lang = self
                .tester
                .as_ref()
                .ok_or_else(|| ConfigError::from(ConfigErrorKind::LanguageNotSpecified))?;
            let mut m = hashmap!("*".to_owned() => args.join(" "));
            m.extend(args.iter().enumerate().zip_longest(1..=9).map(|p| match p {
                EitherOrBoth::Both((_, arg), i) => (i.to_string(), arg.clone()),
                EitherOrBoth::Left((j, arg)) => ((j + i).to_string(), arg.clone()),
                EitherOrBoth::Right(i) => (i.to_string(), "".to_owned()),
            }));
            let tester_transpilation = match tester_transpilations
                .get(lang)
                .map(|t| t.clone().expand(&problem))
            {
                None => Ok(None),
                Some(Err(err)) => Err(err),
                Some(Ok(comp)) => Ok(Some(Arc::new(comp))),
            }?;
            let tester_compilation = match tester_compilations
                .get(lang)
                .map(|t| t.clone().expand(&problem))
            {
                None => Ok(None),
                Some(Err(err)) => Err(err),
                Some(Ok(comp)) => Ok(Some(Arc::new(comp))),
            }?;
            let tester = tester_commands
                .get(lang)
                .map(|template| template.clone().clone())
                .ok_or_else(|| ConfigError::from(ConfigErrorKind::NoSuchLanguage(lang.to_owned())))?
                .envs(Some(&m))
                .expand(&problem)?;
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
pub(crate) enum TestCases {
    Batch(Vec<BatchCase>),
    Interactive(Vec<InteractiveCase>),
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

pub(crate) trait TestCase {
    /// Gets `name`.
    fn name(&self) -> Arc<String>;
}

/// Pair of `input` and `expected`.
#[derive(Clone)]
pub(crate) struct BatchCase {
    name: Arc<String>,
    input: Arc<String>,
    expected: Arc<ExpectedStdout>,
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
        input: &str,
        output: Option<&str>,
        output_match: Match,
    ) -> Self {
        let expected = match (output_match, output.map(ToOwned::to_owned)) {
            (Match::Any, example) => ExpectedStdout::Any { example },
            (Match::Exact, None) | (Match::Float { .. }, None) => {
                ExpectedStdout::Any { example: None }
            }
            (Match::Exact, Some(output)) => ExpectedStdout::Exact(output),
            (
                Match::Float {
                    absolute_error,
                    relative_error,
                },
                Some(string),
            ) => ExpectedStdout::Float {
                string,
                absolute_error,
                relative_error,
            },
        };
        Self {
            name: Arc::new(name.to_owned()),
            input: Arc::new(input.to_owned()),
            expected: Arc::new(expected),
            timelimit,
        }
    }

    pub(crate) fn input(&self) -> Arc<String> {
        self.input.clone()
    }

    pub(crate) fn expected(&self) -> Arc<ExpectedStdout> {
        self.expected.clone()
    }

    pub(crate) fn timelimit(&self) -> Option<Duration> {
        self.timelimit
    }
}

#[derive(Clone)]
#[cfg_attr(test, derive(Debug, PartialEq))]
pub(crate) enum ExpectedStdout {
    Any {
        example: Option<String>,
    },
    Exact(String),
    Float {
        string: String,
        absolute_error: f64,
        relative_error: f64,
    },
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum Match {
    Any,
    Exact,
    Float {
        #[serde(default = "nan")]
        relative_error: f64,
        #[serde(default = "nan")]
        absolute_error: f64,
    },
}

fn nan() -> f64 {
    f64::NAN
}

impl Default for Match {
    fn default() -> Self {
        Match::Exact
    }
}

#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Clone)]
pub(crate) struct InteractiveCase {
    name: Arc<String>,
    timelimit: Option<Duration>,
    tester: Arc<JudgingCommand>,
    tester_transpilation: Option<Arc<TranspilationCommand>>,
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
