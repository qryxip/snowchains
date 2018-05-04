use command::{CompilationCommand, JudgingCommand};
use config::Config;
use errors::{SuiteFileErrorKind, SuiteFileResult};
use template::{BaseDirSome, PathTemplate};
use terminal::Color;
use util::{self, ScalarOrArray};

use {serde_json, serde_yaml, toml};
use itertools::Itertools as _Itertools;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use std::{self, io, iter, slice, vec, f64};
use std::collections::HashSet;
use std::ffi::OsStr;
use std::fmt::{self, Write as _Write};
use std::iter::FromIterator as _FromIterator;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;

/// Appends `input` and `output` to a test suite read from `path`.
pub(crate) fn append(
    path: &SuiteFilePath,
    input: &str,
    output: Option<&str>,
) -> SuiteFileResult<()> {
    let mut suite = TestSuite::load(path)?;
    suite.append(input, output)?;
    suite.save(path, false)
}

/// Extension of a test suite file.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum SuiteFileExtension {
    Json,
    Toml,
    Yaml,
    Yml,
}

impl Default for SuiteFileExtension {
    fn default() -> Self {
        SuiteFileExtension::Yaml
    }
}

impl fmt::Display for SuiteFileExtension {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SuiteFileExtension::Json => write!(f, "json"),
            SuiteFileExtension::Toml => write!(f, "toml"),
            SuiteFileExtension::Yaml => write!(f, "yaml"),
            SuiteFileExtension::Yml => write!(f, "yml"),
        }
    }
}

impl FromStr for SuiteFileExtension {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, String> {
        match s.to_lowercase() {
            ref s if s == "json" => Ok(SuiteFileExtension::Json),
            ref s if s == "toml" => Ok(SuiteFileExtension::Toml),
            ref s if s == "yaml" => Ok(SuiteFileExtension::Yaml),
            ref s if s == "yml" => Ok(SuiteFileExtension::Yml),
            ref s => Err(format!("Unsupported extension: {:?}", s)),
        }
    }
}

impl SuiteFileExtension {
    pub(crate) fn all() -> iter::Cloned<slice::Iter<'static, Self>> {
        use testsuite::SuiteFileExtension::{Json, Toml, Yaml, Yml};
        [Json, Toml, Yaml, Yml].iter().cloned()
    }
}

pub(crate) struct SuiteFilePaths<'a> {
    directory: PathTemplate<BaseDirSome<'a>>,
    stem: &'a str,
    extensions: Vec<SuiteFileExtension>,
}

impl<'a> SuiteFilePaths<'a> {
    pub fn new(
        directory: PathTemplate<BaseDirSome<'a>>,
        stem: &'a str,
        extensions: Vec<SuiteFileExtension>,
    ) -> Self {
        Self {
            directory,
            stem,
            extensions,
        }
    }

    /// Merge test suites in `dir` which filename stem equals `stem` and
    /// extension is in `extensions`.
    pub fn load_merging(
        &self,
        config: &Config,
        allow_missing: bool,
    ) -> SuiteFileResult<(TestCases, String)> {
        let dir = self.directory.expand(self.stem)?;
        if !dir.exists() && allow_missing {
            let paths_as_text = dir.join("{}").display().to_string();
            return Ok((TestCases::Simple(vec![]), paths_as_text));
        }
        if !dir.exists() {
            bail!(SuiteFileErrorKind::DirNotExist(dir.to_owned()));
        }
        let (existing_suites, paths_as_text) = {
            let existing_filenames = {
                let stem_lowercase = self.stem.to_lowercase();
                let mut filenames = util::fs::read_dir(&dir)?
                    .filter_map(|entry| match entry {
                        Ok(entry) => {
                            let path = entry.path();
                            let stem = path.file_stem()?.to_str()?.to_owned();
                            let ext = path.extension()?.to_str()?;
                            let ext = SuiteFileExtension::from_str(ext).ok()?;
                            ensure_opt!(stem.to_lowercase() == stem_lowercase);
                            ensure_opt!(self.extensions.contains(&ext));
                            Some(Ok((stem, ext)))
                        }
                        Err(e) => Some(Err::<_, io::Error>(e)),
                    })
                    .collect::<io::Result<Vec<_>>>()?;
                filenames.sort_by_key(|&(ref s, _)| s.clone());
                filenames.sort_by_key(|&(_, x)| x);
                filenames
            };
            let (mut suites, mut names) = (vec![], vec![]);
            for &(ref stem, ext) in &existing_filenames {
                let path = SuiteFilePath::new(&dir, stem.as_str(), ext);
                let suite = TestSuite::load(&path)?;
                suites.push((suite, path.joined));
                names.push(format!("{}.{}", stem, ext));
            }
            let paths_as_text = dir.join(format!("{{{}}}", names.iter().join(", ")))
                .display()
                .to_string();
            (suites, paths_as_text)
        };

        if existing_suites.is_empty() && allow_missing {
            Ok((TestCases::Simple(vec![]), paths_as_text))
        } else if existing_suites.is_empty() {
            bail!(SuiteFileErrorKind::NoFile(dir.clone()));
        } else if existing_suites.iter().all(|&(ref s, _)| s.is_simple()) {
            let cases = existing_suites
                .into_iter()
                .flat_map(|(suite, path)| {
                    let suite = suite.unwrap_to_simple();
                    let timelimit = suite.timelimit;
                    let output_match = suite.output_match;
                    suite
                        .cases
                        .into_iter()
                        .map(move |(input, out)| {
                            let out = out.as_ref().map(|s| s.as_str());
                            let expected = Arc::new(ExpectedStdout::new(out, output_match));
                            SimpleCase {
                                path: path.clone(),
                                input,
                                expected,
                                timelimit,
                            }
                        })
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>();
            Ok((TestCases::Simple(cases), paths_as_text))
        } else if existing_suites.iter().all(|&(ref s, _)| s.is_interactive()) {
            let mut cases = Vec::with_capacity(existing_suites.len());
            for (suite, path) in existing_suites {
                cases.extend(suite.unwrap_to_interactive().cases(&config, &path)?);
            }
            Ok((TestCases::Interactive(cases), paths_as_text))
        } else if existing_suites
            .iter()
            .all(|&(ref s, _)| s.is_unsubmittable())
        {
            bail!(SuiteFileErrorKind::Unsubmittable(self.stem.to_owned()))
        } else {
            bail!(SuiteFileErrorKind::DifferentTypesOfSuites);
        }
    }
}

/// File path which extension is 'json', 'toml', 'yaml', or 'yml'.
pub(crate) struct SuiteFilePath {
    extension: SuiteFileExtension,
    joined: Arc<PathBuf>,
}

impl<'a> SuiteFilePath {
    pub fn new(directory: &Path, stem: &str, extension: SuiteFileExtension) -> Self {
        let joined = directory.join(stem).with_extension(&extension.to_string());
        Self {
            extension,
            joined: Arc::new(joined),
        }
    }
}

/// `SimpelSuite` or `InteractiveSuite`.
#[derive(Clone, Serialize, Deserialize)]
#[cfg_attr(test, derive(Debug, PartialEq))]
#[serde(tag = "type", rename_all = "lowercase")]
pub(crate) enum TestSuite {
    Simple(SimpleSuite),
    Interactive(InteractiveSuite),
    Unsubmittable,
}

impl TestSuite {
    /// Constructs a `TestSuite::Simple` with `timelimit` and `samples`.
    ///
    /// Make sure the order is (<outout>, <inout>).
    pub fn simple<T: Into<Option<u64>>>(
        timelimit: T,
        absolute_error: Option<f64>,
        relative_error: Option<f64>,
        samples: Vec<(String, String)>,
    ) -> Self {
        TestSuite::Simple(SimpleSuite::from_samples(
            timelimit.into(),
            absolute_error,
            relative_error,
            samples,
        ))
    }

    /// Constructs a `TestSuite::Interactive` with `timelimit`.
    pub fn interactive(timelimit: u64) -> Self {
        TestSuite::Interactive(InteractiveSuite::empty(Some(timelimit)))
    }

    fn load(path: &SuiteFilePath) -> SuiteFileResult<Self> {
        let (path, extension) = (&path.joined, path.extension);
        let text = util::fs::string_from_path(path)?;
        let suite: Self = match extension {
            SuiteFileExtension::Json => serde_json::from_str(&text)?,
            SuiteFileExtension::Toml => toml::from_str(&text)?,
            SuiteFileExtension::Yaml | SuiteFileExtension::Yml => serde_yaml::from_str(&text)?,
        };
        Ok(suite)
    }

    /// Serializes `self` and save it to given path.
    pub fn save(&self, path: &SuiteFilePath, prints_num_cases: bool) -> SuiteFileResult<()> {
        let (path, extension) = (&path.joined, path.extension);
        let serialized = match extension {
            SuiteFileExtension::Json => serde_json::to_string(self)?,
            SuiteFileExtension::Toml => toml::to_string(self)?,
            SuiteFileExtension::Yaml | SuiteFileExtension::Yml => serde_yaml::to_string(self)?,
        };
        util::fs::write(path, serialized.as_bytes())?;
        print!("Saved to {}", path.display());
        if prints_num_cases {
            match *self {
                TestSuite::Simple(ref s) => match s.cases.len() {
                    0 => print_bold!(Color::Warning, " (no sample case extracted)"),
                    1 => print_bold!(Color::Success, " (1 sample case extracted)"),
                    n => print_bold!(Color::Success, " ({} sample cases extracted)", n),
                },
                TestSuite::Interactive(_) => print_bold!(Color::Success, " (interactive problem)"),
                TestSuite::Unsubmittable => print_bold!(Color::Success, " (unsubmittable problem)"),
            }
        }
        println!();
        Ok(())
    }

    fn is_simple(&self) -> bool {
        match *self {
            TestSuite::Simple(_) => true,
            _ => false,
        }
    }

    fn is_interactive(&self) -> bool {
        match *self {
            TestSuite::Interactive(_) => true,
            _ => false,
        }
    }

    fn is_unsubmittable(&self) -> bool {
        match *self {
            TestSuite::Unsubmittable => true,
            _ => false,
        }
    }

    fn unwrap_to_simple(self) -> SimpleSuite {
        match self {
            TestSuite::Simple(suite) => suite,
            _ => unreachable!(),
        }
    }

    fn unwrap_to_interactive(self) -> InteractiveSuite {
        match self {
            TestSuite::Interactive(suite) => suite,
            _ => unreachable!(),
        }
    }

    fn append(&mut self, input: &str, output: Option<&str>) -> SuiteFileResult<()> {
        match *self {
            TestSuite::Simple(ref mut suite) => {
                suite.append(input, output);
                Ok(())
            }
            _ => bail!(SuiteFileErrorKind::SuiteIsNotSimple),
        }
    }
}

/// Set of the timelimit and test cases.
#[derive(Clone, Default)]
#[cfg_attr(test, derive(Debug, PartialEq))]
pub(crate) struct SimpleSuite {
    timelimit: Option<u64>,
    output_match: Match,
    cases: Vec<(Arc<String>, Option<Arc<String>>)>,
    raw_cases: Option<Vec<SimpleCaseRaw>>,
}

impl SimpleSuite {
    fn from_samples<S: IntoIterator<Item = (String, String)>>(
        timelimit: Option<u64>,
        absolute_error: Option<f64>,
        relative_error: Option<f64>,
        samples: S,
    ) -> Self {
        let output_match = if absolute_error.is_none() && relative_error.is_none() {
            Match::default()
        } else {
            Match::Float {
                relative_error: relative_error.unwrap_or(f64::NAN),
                absolute_error: absolute_error.unwrap_or(f64::NAN),
            }
        };
        Self {
            timelimit,
            output_match,
            cases: samples
                .into_iter()
                .map(|(o, i)| (Arc::new(i), Some(Arc::new(o))))
                .collect(),
            raw_cases: None,
        }
    }

    fn append(&mut self, input: &str, output: Option<&str>) {
        let input = Arc::new(input.to_owned());
        let output = output.map(str::to_owned).map(Arc::new);
        self.cases.push((input, output))
    }
}

impl Serialize for SimpleSuite {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        fn single_string(s: String) -> ScalarOrArray<NonArrayValue> {
            ScalarOrArray::Scalar(NonArrayValue::String(s))
        }
        let cases: Vec<SimpleCaseRaw> = if let Some(ref raw_cases) = self.raw_cases {
            raw_cases.clone()
        } else {
            self.cases
                .iter()
                .map(|&(ref i, ref o)| SimpleCaseRaw {
                    input: single_string(i.as_ref().clone()),
                    output: o.as_ref().map(|o| single_string(o.as_ref().clone())),
                })
                .collect()
        };
        SimpleSuiteRaw {
            timelimit: self.timelimit,
            output_match: self.output_match,
            cases,
        }.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for SimpleSuite {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        fn to_string(x: &ScalarOrArray<NonArrayValue>) -> Arc<String> {
            Arc::new(match *x {
                ScalarOrArray::Scalar(ref x) => x.to_string(),
                ScalarOrArray::Array(ref xs) => xs.iter().fold("".to_owned(), |mut r, x| {
                    write!(r, "{}", x).unwrap();
                    r
                }),
            })
        }
        let raw = SimpleSuiteRaw::deserialize(deserializer)?;
        Ok(Self {
            timelimit: raw.timelimit,
            output_match: raw.output_match,
            cases: raw.cases
                .iter()
                .map(|case| {
                    let input = to_string(&case.input);
                    let output = case.output.as_ref().map(to_string);
                    (input, output)
                })
                .collect(),
            raw_cases: Some(raw.cases),
        })
    }
}

#[derive(Serialize, Deserialize)]
struct SimpleSuiteRaw {
    #[serde(skip_serializing_if = "Option::is_none")]
    timelimit: Option<u64>,
    #[serde(rename = "match", default)]
    output_match: Match,
    cases: Vec<SimpleCaseRaw>,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Clone, Serialize, Deserialize)]
struct SimpleCaseRaw {
    #[serde(rename = "in")]
    input: ScalarOrArray<NonArrayValue>,
    #[serde(rename = "out", skip_serializing_if = "Option::is_none")]
    output: Option<ScalarOrArray<NonArrayValue>>,
}

#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Clone, Serialize, Deserialize)]
#[serde(untagged)]
enum NonArrayValue {
    Integer(i64),
    Float(f64),
    String(String),
}

impl fmt::Display for NonArrayValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            NonArrayValue::Integer(n) => writeln!(f, "{}", n),
            NonArrayValue::Float(v) => writeln!(f, "{}", v),
            NonArrayValue::String(ref s) if s.ends_with('\n') => write!(f, "{}", s),
            NonArrayValue::String(ref s) => writeln!(f, "{}", s),
        }
    }
}

#[derive(Clone, Default, Serialize, Deserialize)]
#[cfg_attr(test, derive(Debug, PartialEq))]
pub(crate) struct InteractiveSuite {
    timelimit: Option<u64>,
    tester: Option<String>,
    each_args: Vec<Vec<String>>,
}

impl InteractiveSuite {
    fn empty(timelimit: Option<u64>) -> Self {
        Self {
            timelimit,
            tester: None,
            each_args: vec![],
        }
    }

    fn cases(
        &self,
        config: &Config,
        path: &Arc<PathBuf>,
    ) -> SuiteFileResult<vec::IntoIter<InteractiveCase>> {
        let target = path.file_stem()
            .map(OsStr::to_string_lossy)
            .unwrap_or_default();
        let mut cases = Vec::with_capacity(self.each_args.len());
        for args in &self.each_args {
            let lang = self.tester.as_ref().map(String::as_str);
            let mut m = hashmap!("*".to_owned() => args.join(" "));
            for (i, arg) in args.iter().enumerate() {
                m.insert((i + 1).to_string(), arg.clone());
            }
            let tester = config
                .interactive_tester(lang)?
                .embed_strings(&m)
                .expand(&target)?;
            let tester_compilation = match config.interactive_tester_compilation(lang)? {
                Some(compilation) => Some(Arc::new(compilation.expand(&target)?)),
                None => None,
            };
            cases.push(InteractiveCase {
                tester: Arc::new(tester),
                tester_compilation,
                timelimit: self.timelimit.map(Duration::from_millis),
                path: path.clone(),
            });
        }
        Ok(cases.into_iter())
    }
}

/// `Vec<SimpleCase>` or `Vec<ReducibleCase>`.
pub(crate) enum TestCases {
    Simple(Vec<SimpleCase>),
    Interactive(Vec<InteractiveCase>),
}

impl TestCases {
    pub fn interactive_tester_compilations(&self) -> HashSet<Arc<CompilationCommand>> {
        match *self {
            TestCases::Simple(_) => hashset!(),
            TestCases::Interactive(ref cases) => {
                let compilations = cases
                    .iter()
                    .filter_map(|case| case.tester_compilation.clone());
                HashSet::from_iter(compilations)
            }
        }
    }
}

pub(crate) trait TestCase {
    /// Gets `path`.
    fn path(&self) -> Arc<PathBuf>;
}

/// Pair of `input` and `expected`.
///
/// `expected` is empty IFF omitted.
#[derive(Clone)]
pub(crate) struct SimpleCase {
    path: Arc<PathBuf>,
    input: Arc<String>,
    expected: Arc<ExpectedStdout>,
    timelimit: Option<u64>,
}

impl TestCase for SimpleCase {
    fn path(&self) -> Arc<PathBuf> {
        self.path.clone()
    }
}

impl SimpleCase {
    #[cfg(test)]
    pub fn default_matching<T: Into<Option<u64>>>(
        input: &str,
        expected: &str,
        timelimit: T,
    ) -> Self {
        let expected = Arc::new(ExpectedStdout::new(Some(expected), Match::default()));
        Self {
            path: Arc::default(),
            input: Arc::new(input.to_owned()),
            expected,
            timelimit: timelimit.into(),
        }
    }

    #[cfg(test)]
    pub fn float_matching<T: Into<Option<u64>>>(
        input: &str,
        expected: &str,
        timelimit: T,
        absolute_error: f64,
        relative_error: f64,
    ) -> Self {
        let output_match = Match::Float {
            absolute_error: absolute_error,
            relative_error: relative_error,
        };
        let expected = Arc::new(ExpectedStdout::new(Some(expected), output_match));
        Self {
            path: Arc::default(),
            input: Arc::new(input.to_owned()),
            expected,
            timelimit: timelimit.into(),
        }
    }

    /// # Example
    ///
    /// ```no-run
    /// let (input, expected, timelimit) = self.values()
    /// ```
    pub fn values(&self) -> (Arc<String>, Arc<ExpectedStdout>, Option<Duration>) {
        let timelimit = self.timelimit.map(Duration::from_millis);
        (self.input.clone(), self.expected.clone(), timelimit)
    }
}

#[derive(Clone)]
#[cfg_attr(test, derive(Debug, PartialEq))]
pub(crate) enum ExpectedStdout {
    AcceptAny,
    Exact(String),
    Lines(String),
    Float {
        lines: String,
        absolute_error: f64,
        relative_error: f64,
    },
}

impl ExpectedStdout {
    fn new(text: Option<&str>, output_match: Match) -> Self {
        match (text, output_match) {
            (None, _) => ExpectedStdout::AcceptAny,
            (Some(s), Match::Exact) => ExpectedStdout::Exact(s.to_owned()),
            (Some(s), Match::Lines) => ExpectedStdout::Lines(s.to_owned()),
            (
                Some(s),
                Match::Float {
                    absolute_error,
                    relative_error,
                },
            ) => ExpectedStdout::Float {
                lines: s.to_owned(),
                absolute_error,
                relative_error,
            },
        }
    }
}

#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
enum Match {
    Exact,
    Lines,
    Float {
        #[serde(default = "nan", skip_serializing_if = "is_nan")]
        relative_error: f64,
        #[serde(default = "nan", skip_serializing_if = "is_nan")]
        absolute_error: f64,
    },
}

fn nan() -> f64 {
    f64::NAN
}

fn is_nan(v: &f64) -> bool {
    v.is_nan()
}

impl Default for Match {
    #[cfg(windows)]
    fn default() -> Self {
        Match::Lines
    }

    #[cfg(not(windows))]
    fn default() -> Self {
        Match::Exact
    }
}

#[derive(Clone)]
#[cfg_attr(test, derive(Debug, PartialEq))]
pub(crate) struct InteractiveCase {
    tester: Arc<JudgingCommand>,
    tester_compilation: Option<Arc<CompilationCommand>>,
    timelimit: Option<Duration>,
    path: Arc<PathBuf>,
}

impl TestCase for InteractiveCase {
    fn path(&self) -> Arc<PathBuf> {
        self.path.clone()
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
