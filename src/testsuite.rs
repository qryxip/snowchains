use errors::{FileIoErrorKind, FileIoResultExt, SuiteFileErrorKind, SuiteFileResult};
use terminal::Color;
use util;

use {serde_json, serde_yaml, toml};

use std::fmt;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;

/// Appends `input` and `output` to a test suite read from `path`.
pub fn append(path: &SuiteFilePath, input: &str, output: Option<&str>) -> SuiteFileResult<()> {
    let mut suite = TestSuite::load(path)?;
    suite.append(input, output)?;
    suite.save(path, false)
}

/// `SimpelSuite` or `InteractiveSuite`.
#[derive(Clone, Serialize, Deserialize)]
#[cfg_attr(test, derive(Debug, PartialEq))]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum TestSuite {
    Simple(SimpleSuite),
    Interactive(InteractiveSuite),
    Unsubmittable,
}

impl Default for TestSuite {
    fn default() -> Self {
        TestSuite::Interactive(InteractiveSuite::default())
    }
}

impl TestSuite {
    /// Constructs a `TestSuite::Simple` with `timelimit` and `samples`.
    ///
    /// Make sure the order is (<outout>, <inout>).
    pub fn simple<T: Into<Option<u64>>>(timelimit: T, samples: Vec<(String, String)>) -> Self {
        TestSuite::Simple(SimpleSuite::from_samples(timelimit.into(), samples))
    }

    /// Constructs a `TestSuite::Interactive` with `timelimit`.
    pub fn interactive(timelimit: u64) -> Self {
        TestSuite::Interactive(InteractiveSuite::without_cases(Some(timelimit)))
    }

    fn load(path: &SuiteFilePath) -> SuiteFileResult<Self> {
        let (path, extension) = (path.build(), path.extension);
        let text = util::string_from_file_path(&path)?;
        let mut suite: Self = match extension {
            SuiteFileExtension::Json => serde_json::from_str(&text)?,
            SuiteFileExtension::Toml => toml::from_str(&text)?,
            SuiteFileExtension::Yaml | SuiteFileExtension::Yml => serde_yaml::from_str(&text)?,
        };
        match suite {
            TestSuite::Simple(ref mut suite) => suite.path = path,
            TestSuite::Interactive(ref mut suite) => suite.path = Arc::new(path),
            TestSuite::Unsubmittable => {}
        }
        Ok(suite)
    }

    /// Serializes `self` and save it to given path.
    pub fn save(&self, path: &SuiteFilePath, prints_num_cases: bool) -> SuiteFileResult<()> {
        let (path, extension) = (path.build(), path.extension);
        let mut file = util::create_file_and_dirs(&path)?;
        let serialized = match extension {
            SuiteFileExtension::Json => serde_json::to_string(self)?,
            SuiteFileExtension::Toml => toml::to_string(self)?,
            SuiteFileExtension::Yaml | SuiteFileExtension::Yml => serde_yaml::to_string(self)?,
        };
        file.write_all(serialized.as_bytes())
            .chain_err(|| FileIoErrorKind::Write(path.clone()))?;
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
#[derive(Clone, Default, Serialize, Deserialize)]
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct SimpleSuite {
    timelimit: Option<u64>,
    cases: Vec<ReducibleCase>,
    #[serde(skip)]
    path: PathBuf,
}

impl SimpleSuite {
    fn from_samples<S: IntoIterator<Item = (String, String)>>(
        timelimit: Option<u64>,
        samples: S,
    ) -> Self {
        Self {
            timelimit,
            cases: samples
                .into_iter()
                .map(|(output, input)| ReducibleCase::from_strings(input, Some(output)))
                .collect(),
            path: PathBuf::default(),
        }
    }

    fn append(&mut self, input: &str, output: Option<&str>) {
        self.cases.push(ReducibleCase::from_strings(input, output));
    }
}

#[derive(Clone, Default, Serialize, Deserialize)]
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct InteractiveSuite {
    timelimit: Option<u64>,
    cases: Vec<InteractiveCase>,
    #[serde(skip)]
    path: Arc<PathBuf>,
}

impl InteractiveSuite {
    fn without_cases(timelimit: Option<u64>) -> Self {
        Self {
            timelimit,
            cases: vec![],
            path: Arc::new(PathBuf::new()),
        }
    }
}

/// `Vec<SimpleCase>` or `Vec<ReducibleCase>`.
pub enum TestCases {
    Simple(Vec<SimpleCase>),
    Interactive(Vec<InteractiveCase>),
}

pub trait TestCase {
    /// Gets `path`.
    fn path(&self) -> Arc<PathBuf>;
}

/// Pair of `input` and `expected`.
///
/// `expected` is empty IFF omitted.
#[derive(Clone)]
pub struct SimpleCase {
    path: Arc<PathBuf>,
    input: Arc<String>,
    expected: Arc<String>,
    timelimit: Option<u64>,
}

impl TestCase for SimpleCase {
    fn path(&self) -> Arc<PathBuf> {
        self.path.clone()
    }
}

impl SimpleCase {
    #[cfg(test)]
    pub fn new(input: &str, expected: &str, timelimit: u64) -> Self {
        Self {
            path: Arc::new(PathBuf::new()),
            input: Arc::new(input.to_owned()),
            expected: Arc::new(expected.to_owned()),
            timelimit: Some(timelimit),
        }
    }

    /// # Example
    ///
    /// ```
    /// let (input, expected, timelimit) = self.values()
    /// ```
    pub fn values(&self) -> (Arc<String>, Arc<String>, Option<Duration>) {
        let timelimit = self.timelimit.map(Duration::from_millis);
        (self.input.clone(), self.expected.clone(), timelimit)
    }
}

#[derive(Clone, Serialize, Deserialize)]
#[cfg_attr(test, derive(Debug, PartialEq))]
pub struct InteractiveCase {
    tester: String,
    timelimit: Option<u64>,
    #[serde(skip)]
    path: Arc<PathBuf>,
}

impl TestCase for InteractiveCase {
    fn path(&self) -> Arc<PathBuf> {
        self.path.clone()
    }
}

impl InteractiveCase {
    /// Gets `self.tester` as `&str`.
    pub fn get_tester(&self) -> &str {
        &self.tester
    }

    /// Gets the timelimit as `Option<Duration>`.
    pub fn get_timelimit(&self) -> Option<Duration> {
        self.timelimit.map(Duration::from_millis)
    }

    fn appended(mut self, path: Arc<PathBuf>, timelimit: Option<u64>) -> Self {
        self.path = path;
        self.timelimit = self.timelimit.or(timelimit);
        self
    }
}

pub struct SuiteFilePaths {
    directory: PathBuf,
    stem: String,
    extensions: Vec<SuiteFileExtension>,
}

impl SuiteFilePaths {
    pub fn new(directory: &Path, stem: &str, extensions: &[SuiteFileExtension]) -> Self {
        Self {
            directory: directory.to_owned(),
            stem: stem.to_owned(),
            extensions: extensions.to_vec(),
        }
    }

    /// Merge test suites in `dir` which filename stem equals `stem` and
    /// extension is in `extensions`.
    pub fn load_merging(&self, allow_missing: bool) -> SuiteFileResult<TestCases> {
        let existing_suites = self.extensions
            .iter()
            .filter_map(|&ext| {
                let path = SuiteFilePath::new(&self.directory, self.stem.clone(), ext);
                if path.build().exists() {
                    Some(TestSuite::load(&path))
                } else {
                    None
                }
            })
            .collect::<SuiteFileResult<Vec<_>>>()?;

        if existing_suites.is_empty() {
            if allow_missing {
                Ok(TestCases::Simple(vec![]))
            } else {
                bail!(SuiteFileErrorKind::NoFile(self.directory.clone()));
            }
        } else if existing_suites.iter().all(TestSuite::is_simple) {
            Ok(TestCases::Simple(
                existing_suites
                    .into_iter()
                    .map(TestSuite::unwrap_to_simple)
                    .flat_map(|suite| {
                        let (timelimit, cases, path) = (suite.timelimit, suite.cases, suite.path);
                        let path = Arc::new(path);
                        cases
                            .into_iter()
                            .map(move |case| case.reduce(path.clone(), timelimit))
                    })
                    .collect(),
            ))
        } else if existing_suites.iter().all(TestSuite::is_interactive) {
            Ok(TestCases::Interactive(
                existing_suites
                    .into_iter()
                    .map(TestSuite::unwrap_to_interactive)
                    .flat_map(|suite| {
                        let (timelimit, cases, path) = (suite.timelimit, suite.cases, suite.path);
                        cases
                            .into_iter()
                            .map(move |case| case.appended(path.clone(), timelimit))
                    })
                    .collect(),
            ))
        } else if existing_suites.iter().all(TestSuite::is_unsubmittable) {
            bail!(SuiteFileErrorKind::Unsubmittable(self.stem.clone()))
        } else {
            bail!(SuiteFileErrorKind::DifferentTypesOfSuites);
        }
    }
}

/// File path which extension is 'json', 'toml', 'yaml', or 'yml'.
pub struct SuiteFilePath {
    directory: PathBuf,
    stem: String,
    extension: SuiteFileExtension,
}

impl SuiteFilePath {
    pub fn new<S: Into<String>>(directory: &Path, stem: S, extension: SuiteFileExtension) -> Self {
        Self {
            directory: PathBuf::from(directory),
            stem: stem.into(),
            extension,
        }
    }

    pub fn build(&self) -> PathBuf {
        let mut path = PathBuf::from(&self.directory);
        path.push(&self.stem);
        path.set_extension(&self.extension.to_string());
        path
    }
}

/// Extension of a test suite file.
#[derive(Clone, Copy, Serialize, Deserialize)]
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
        let s = match *self {
            SuiteFileExtension::Json => "json",
            SuiteFileExtension::Toml => "toml",
            SuiteFileExtension::Yaml => "yaml",
            SuiteFileExtension::Yml => "yml",
        };
        write!(f, "{}", s)
    }
}

impl FromStr for SuiteFileExtension {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, String> {
        let s = s.to_lowercase();
        if s == "json" {
            Ok(SuiteFileExtension::Json)
        } else if s == "toml" {
            Ok(SuiteFileExtension::Toml)
        } else if s == "yaml" {
            Ok(SuiteFileExtension::Yaml)
        } else if s == "yml" {
            Ok(SuiteFileExtension::Yml)
        } else {
            Err(format!("Unsupported extension: {:?}", s))
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
#[cfg_attr(test, derive(Debug, PartialEq))]
struct ReducibleCase {
    input: NonNestedValue,
    #[serde(skip_serializing_if = "Option::is_none")]
    expected: Option<NonNestedValue>,
    #[serde(skip_serializing_if = "Option::is_none")]
    timelimit: Option<u64>,
}

impl ReducibleCase {
    fn from_strings<S1: Into<String>, S2: Into<String>>(input: S1, output: Option<S2>) -> Self {
        Self {
            input: NonNestedValue::string(input.into()),
            expected: output.map(|s| NonNestedValue::string(s.into())),
            timelimit: None,
        }
    }

    fn reduce(self, path: Arc<PathBuf>, timelimit: Option<u64>) -> SimpleCase {
        SimpleCase {
            path,
            input: Arc::new(self.input.reduce()),
            expected: Arc::new(self.expected.map(|v| v.reduce()).unwrap_or_default()),
            timelimit: self.timelimit.or(timelimit),
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
#[cfg_attr(test, derive(Debug, PartialEq))]
#[serde(untagged)]
enum NonNestedValue {
    Array(Vec<NonArrayValue>),
    NonArray(NonArrayValue),
}

impl NonNestedValue {
    fn string<S: Into<String>>(s: S) -> Self {
        NonNestedValue::NonArray(NonArrayValue::String(s.into()))
    }

    fn reduce(self) -> String {
        use std::fmt::Write;

        fn as_lines<T: fmt::Display>(a: &[T]) -> String {
            let mut result = String::new();
            for x in a {
                writeln!(result, "{}", x).unwrap();
            }
            result
        }

        match self {
            NonNestedValue::Array(a) => as_lines(&a),
            NonNestedValue::NonArray(NonArrayValue::Integer(n)) => format!("{}\n", n),
            NonNestedValue::NonArray(NonArrayValue::Float(v)) => format!("{}\n", v),
            NonNestedValue::NonArray(NonArrayValue::String(mut s)) => {
                if !s.ends_with('\n') {
                    s.push('\n');
                }
                s
            }
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
#[cfg_attr(test, derive(Debug, PartialEq))]
#[serde(untagged)]
enum NonArrayValue {
    Integer(i64),
    Float(f64),
    String(String),
}

impl fmt::Display for NonArrayValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            NonArrayValue::Integer(n) => write!(f, "{}", n),
            NonArrayValue::Float(v) => write!(f, "{}", v),
            NonArrayValue::String(ref s) => write!(f, "{}", s),
        }
    }
}
