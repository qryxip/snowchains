use errors::{SuiteFileErrorKind, SuiteFileResult};
use util;

use {serde_json, serde_yaml, toml};

use std::{fmt, vec};
use std::fs::{self, File};
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
#[serde(tag = "type", rename_all = "lowercase")]
pub enum TestSuite {
    Simple(SimpleSuite),
    Interactive(InteractiveSuite),
}

impl Default for TestSuite {
    fn default() -> Self {
        TestSuite::Interactive(InteractiveSuite::default())
    }
}

impl TestSuite {
    /// Constructs a `TestSuite` with `timelimit` and `cases`.
    ///
    /// Returns `InteractiveSuite` if `cases` is empty, otherwise returns `SimpleSuite`.
    pub fn from_samples(timelimit: Option<u64>, cases: Vec<(String, String)>) -> Self {
        if cases.is_empty() {
            TestSuite::Interactive(InteractiveSuite::without_cases(timelimit))
        } else {
            TestSuite::Simple(SimpleSuite::from_samples(timelimit, cases.into_iter()))
        }
    }

    /// Loads from `path` and deserializes it into `TestSuite`.
    pub fn load(path: &SuiteFilePath) -> SuiteFileResult<Self> {
        let (path, extension) = (path.build(), path.extension);
        let text = util::string_from_file_path(&path)?;
        let mut suite: Self = match extension {
            SuiteFileExtension::Json => serde_json::from_str(&text)?,
            SuiteFileExtension::Toml => toml::from_str(&text)?,
            SuiteFileExtension::Yaml | SuiteFileExtension::Yml => serde_yaml::from_str(&text)?,
        };
        match suite {
            TestSuite::Simple(ref mut suite) => suite.path = path,
            TestSuite::Interactive(ref mut suite) => suite.path = path,
        }
        Ok(suite)
    }

    /// Whether `self` has no test case.
    pub fn is_empty(&self) -> bool {
        match *self {
            TestSuite::Simple(ref suite) => suite.is_empty(),
            TestSuite::Interactive(ref suite) => suite.is_empty(),
        }
    }

    /// Returns the number of test cases of `self`.
    pub fn num_cases(&self) -> usize {
        match *self {
            TestSuite::Simple(ref suite) => suite.num_cases(),
            TestSuite::Interactive(ref suite) => suite.num_cases(),
        }
    }

    /// Serializes `self` and save it to given path.
    pub fn save(&self, path: &SuiteFilePath, prints_num_cases: bool) -> SuiteFileResult<()> {
        fs::create_dir_all(&path.directory)?;
        let (path, extension) = (path.build(), path.extension);
        let mut file = File::create(&path)?;
        let serialized = match extension {
            SuiteFileExtension::Json => serde_json::to_string(self)?,
            SuiteFileExtension::Toml => toml::to_string(self)?,
            SuiteFileExtension::Yaml | SuiteFileExtension::Yml => serde_yaml::to_string(self)?,
        };
        file.write_all(serialized.as_bytes())?;
        print!("Saved to {}", path.display());
        if prints_num_cases {
            match self.num_cases() {
                0 => print!(" (no sample case extracted)"),
                1 => print!(" (1 sample case extracted)"),
                n => print!(" ({} sample cases extracted)", n),
            }
        }
        Ok(println!(""))
    }

    fn is_simple(&self) -> bool {
        match *self {
            TestSuite::Simple(_) => true,
            TestSuite::Interactive(_) => false,
        }
    }

    fn is_interactive(&self) -> bool {
        !self.is_simple()
    }

    fn unwrap_to_simple(self) -> SimpleSuite {
        match self {
            TestSuite::Simple(suite) => suite,
            TestSuite::Interactive(_) => unreachable!(),
        }
    }

    fn unwrap_to_interactive(self) -> InteractiveSuite {
        match self {
            TestSuite::Simple(_) => unreachable!(),
            TestSuite::Interactive(suite) => suite,
        }
    }

    fn append(&mut self, input: &str, output: Option<&str>) -> SuiteFileResult<()> {
        match *self {
            TestSuite::Simple(ref mut suite) => Ok(suite.append(input, output)),
            TestSuite::Interactive(_) => bail!(SuiteFileErrorKind::SuiteIsInteractive),
        }
    }
}

/// Set of the timelimit and test cases.
#[derive(Clone, Default, Serialize, Deserialize)]
pub struct SimpleSuite {
    timelimit: Option<u64>,
    cases: Vec<ReducibleCase>,
    #[serde(skip)]
    path: PathBuf,
}

impl SimpleSuite {
    /// Constructs a `Cases` with a timelimit value and pairs of input/output samples.
    fn from_samples(timelimit: Option<u64>, cases: vec::IntoIter<(String, String)>) -> Self {
        Self {
            timelimit: timelimit,
            cases: cases
                .map(|(output, input)| ReducibleCase::from_strings(input, Some(output)))
                .collect(),
            path: PathBuf::default(),
        }
    }

    fn is_empty(&self) -> bool {
        self.cases.is_empty()
    }

    fn num_cases(&self) -> usize {
        self.cases.len()
    }

    fn append(&mut self, input: &str, output: Option<&str>) {
        self.cases.push(ReducibleCase::from_strings(input, output));
    }
}

#[derive(Clone, Default, Serialize, Deserialize)]
pub struct InteractiveSuite {
    timelimit: Option<u64>,
    cases: Vec<InteractiveCase>,
    #[serde(skip)]
    path: PathBuf,
}

impl InteractiveSuite {
    fn without_cases(timelimit: Option<u64>) -> Self {
        Self {
            timelimit: timelimit,
            cases: vec![],
            path: PathBuf::new(),
        }
    }

    fn is_empty(&self) -> bool {
        self.cases.is_empty()
    }

    fn num_cases(&self) -> usize {
        self.cases.len()
    }
}

/// `Vec<SimpleCase>` or `Vec<ReducibleCase>`.
pub enum TestCases {
    Simple(Vec<SimpleCase>),
    Interactive(Vec<InteractiveCase>),
}

impl TestCases {
    /// Returns the length of its content.
    pub fn len(&self) -> usize {
        match *self {
            TestCases::Simple(ref cases) => cases.len(),
            TestCases::Interactive(ref cases) => cases.len(),
        }
    }
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

impl SimpleCase {
    /// # Example
    ///
    /// ```
    /// let (input, expected, timelimit) = self.values()
    /// ```
    pub fn values(&self) -> (Arc<String>, Arc<String>, Option<Duration>) {
        let timelimit = self.timelimit.map(Duration::from_millis);
        (self.input.clone(), self.expected.clone(), timelimit)
    }

    /// Gets `self::path`.
    pub fn get_path(&self) -> Arc<PathBuf> {
        self.path.clone()
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct InteractiveCase {
    tester: String,
    timelimit: Option<u64>,
    #[serde(skip)]
    path: PathBuf,
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

    /// Gets `self::path` as `Arc`.
    pub fn get_path(&self) -> Arc<PathBuf> {
        Arc::new(self.path.clone())
    }

    fn appended<P: Into<PathBuf>>(mut self, path: P, timelimit: Option<u64>) -> Self {
        self.path = path.into();
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
            extensions: extensions.iter().cloned().collect(),
        }
    }

    /// Merge test suites in `dir` which filename stem equals `stem` and extension is in `extensions`.
    pub fn load_and_merge_all(&self) -> SuiteFileResult<TestCases> {
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

        if existing_suites.iter().all(TestSuite::is_simple) {
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
            extension: extension,
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
        SuiteFileExtension::Yml
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
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
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
            Err(())
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
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
            path: path,
            input: Arc::new(self.input.reduce()),
            expected: Arc::new(self.expected.map(|v| v.reduce()).unwrap_or_default()),
            timelimit: self.timelimit.or(timelimit),
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
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
                if s.chars().last() != Some('\n') {
                    s.push('\n');
                }
                s
            }
        }
    }
}

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
            NonArrayValue::Integer(n) => write!(f, "{}", n),
            NonArrayValue::Float(v) => write!(f, "{}", v),
            NonArrayValue::String(ref s) => write!(f, "{}", s),
        }
    }
}
