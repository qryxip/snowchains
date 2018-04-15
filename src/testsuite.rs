use errors::{FileIoErrorKind, FileIoResultExt, SuiteFileErrorKind, SuiteFileResult};
use template::PathTemplate;
use terminal::Color;
use util;

use {serde_json, serde_yaml, toml};
use decimal::d128;

use std::{self, iter, slice};
use std::borrow::Cow;
use std::fmt::{self, Write as _FmtWrite};
use std::io::Write as _IoWrite;
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

pub struct SuiteFilePaths<'a> {
    directory: PathTemplate<'a>,
    stem: &'a str,
    extensions: Vec<SuiteFileExtension>,
}

impl<'a> SuiteFilePaths<'a> {
    pub fn new(
        directory: PathTemplate<'a>,
        stem: &'a str,
        extensions: Vec<SuiteFileExtension>,
    ) -> Self {
        Self {
            directory,
            stem,
            extensions,
        }
    }

    pub fn existing_paths_as_text(&self) -> SuiteFileResult<String> {
        let dir = self.directory.expand(self.stem)?;
        let delim = if cfg!(windows) && !dir.display().to_string().ends_with('\\') {
            "\\"
        } else if !dir.display().to_string().ends_with('/') {
            "/"
        } else {
            ""
        };
        let mut r = format!("{}{}{{", dir.display(), delim);
        let mut p = false;
        for &ext in &self.extensions {
            if SuiteFilePath::new(&dir, self.stem, ext).build().exists() {
                if p {
                    r += ", ";
                } else {
                    p = true;
                }
                write!(r, "{}.{}", self.stem, ext).unwrap();
            }
        }
        r += "}";
        Ok(r)
    }

    /// Merge test suites in `dir` which filename stem equals `stem` and
    /// extension is in `extensions`.
    pub fn load_merging(&self, allow_missing: bool) -> SuiteFileResult<TestCases> {
        let dir = self.directory.expand(self.stem)?;
        let existing_suites = self.extensions
            .iter()
            .filter_map(|&ext| {
                let path = SuiteFilePath::new(&dir, self.stem, ext);
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
                bail!(SuiteFileErrorKind::NoFile(dir.clone()));
            }
        } else if existing_suites.iter().all(TestSuite::is_simple) {
            let cases = existing_suites
                .into_iter()
                .map(TestSuite::unwrap_to_simple)
                .flat_map(|suite| {
                    let (timelimit, trim_crlf, cases, path) =
                        (suite.timelimit, suite.trim_crlf, suite.cases, suite.path);
                    let absolute = suite.absolute_error;
                    let relative = suite.relative_error;
                    let path = Arc::new(path);
                    cases
                        .into_iter()
                        .map(|case| {
                            case.reduce(path.clone(), trim_crlf, timelimit, absolute, relative)
                        })
                        .collect::<Vec<_>>()
                })
                .collect::<SuiteFileResult<Vec<_>>>()?;
            Ok(TestCases::Simple(cases))
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
            bail!(SuiteFileErrorKind::Unsubmittable(self.stem.to_owned()))
        } else {
            bail!(SuiteFileErrorKind::DifferentTypesOfSuites);
        }
    }
}

/// File path which extension is 'json', 'toml', 'yaml', or 'yml'.
pub struct SuiteFilePath<'a> {
    directory: &'a Path,
    stem: Cow<'a, str>,
    extension: SuiteFileExtension,
}

impl<'a> SuiteFilePath<'a> {
    pub fn new<S: Into<Cow<'a, str>>>(
        directory: &'a Path,
        stem: S,
        extension: SuiteFileExtension,
    ) -> Self {
        Self {
            directory,
            stem: stem.into(),
            extension,
        }
    }

    pub fn build(&self) -> PathBuf {
        self.directory
            .join(self.stem.as_ref())
            .with_extension(&self.extension.to_string())
    }
}

/// Extension of a test suite file.
#[derive(Clone, Copy, PartialEq, Serialize, Deserialize)]
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

/// `SimpelSuite` or `InteractiveSuite`.
#[derive(Clone, Serialize, Deserialize)]
#[cfg_attr(test, derive(Debug, PartialEq))]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum TestSuite {
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
    #[serde(default = "util::cfg_windows", skip_serializing_if = "util::is_default")]
    trim_crlf: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    timelimit: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    absolute_error: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    relative_error: Option<f64>,
    cases: Vec<ReducibleCase>,
    #[serde(skip)]
    path: PathBuf,
}

impl SimpleSuite {
    fn from_samples<S: IntoIterator<Item = (String, String)>>(
        timelimit: Option<u64>,
        absolute_error: Option<f64>,
        relative_error: Option<f64>,
        samples: S,
    ) -> Self {
        Self {
            trim_crlf: cfg!(windows),
            timelimit,
            absolute_error,
            relative_error,
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
    pub fn new<T: Into<Option<u64>>, E: Into<Option<d128>>>(
        input: &str,
        expected: &str,
        timelimit: T,
        absolute_error: E,
        relative_error: E,
    ) -> Self {
        let abs = absolute_error.into();
        let rel = relative_error.into();
        let expected = Arc::new(ExpectedStdout::new(Some(expected), cfg!(windows), abs, rel));
        Self {
            path: Arc::new(PathBuf::new()),
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
pub enum ExpectedStdout {
    AcceptAny,
    Exact(String),
    Lines(String),
    Float {
        lines: String,
        absolute_error: d128,
        relative_error: d128,
    },
}

impl ExpectedStdout {
    fn new(
        text: Option<&str>,
        trim_crlf: bool,
        absolute: Option<d128>,
        relative: Option<d128>,
    ) -> Self {
        if let Some(text) = text {
            let mut text = text.to_owned();
            if absolute.is_none() && relative.is_none() && trim_crlf {
                ExpectedStdout::Lines(text)
            } else if absolute.is_none() && relative.is_none() {
                if !text.ends_with('\n') {
                    text += "\n";
                }
                ExpectedStdout::Exact(text)
            } else {
                ExpectedStdout::Float {
                    lines: text,
                    absolute_error: absolute.unwrap_or_default(),
                    relative_error: relative.unwrap_or_default(),
                }
            }
        } else {
            ExpectedStdout::AcceptAny
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
#[cfg_attr(test, derive(Debug, PartialEq))]
struct ReducibleCase {
    #[serde(rename = "in")]
    input: NonNestedValue,
    #[serde(rename = "out", skip_serializing_if = "Option::is_none")]
    output: Option<NonNestedValue>,
    #[serde(default = "util::cfg_windows", skip_serializing_if = "util::is_default")]
    trim_crlf: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    timelimit: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    absolute_error: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    relative_error: Option<f64>,
}

impl ReducibleCase {
    fn from_strings<S1: Into<String>, S2: Into<String>>(input: S1, output: Option<S2>) -> Self {
        Self {
            input: NonNestedValue::single_string(input.into()),
            output: output.map(Into::into).map(NonNestedValue::single_string),
            trim_crlf: false,
            timelimit: None,
            absolute_error: None,
            relative_error: None,
        }
    }

    fn reduce(
        &self,
        path: Arc<PathBuf>,
        trim_crlf: bool,
        timelimit: Option<u64>,
        absolute_error: Option<f64>,
        relative_error: Option<f64>,
    ) -> SuiteFileResult<SimpleCase> {
        fn parse_floating_error(e1: Option<f64>, e2: Option<f64>) -> SuiteFileResult<Option<d128>> {
            match e1.or(e2).map(|e| d128::from_str(&e.to_string()).unwrap()) {
                Some(x) if x.is_nan() => bail!(SuiteFileErrorKind::Nan),
                Some(x) => Ok(Some(x)),
                None => Ok(None),
            }
        }

        let output = self.output.as_ref().map(ToString::to_string);
        let absolute_error = parse_floating_error(self.absolute_error, absolute_error)?;
        let relative_error = parse_floating_error(self.relative_error, relative_error)?;
        Ok(SimpleCase {
            path,
            input: Arc::new(self.input.to_string()),
            expected: Arc::new(ExpectedStdout::new(
                output.as_ref().map(String::as_str),
                self.trim_crlf || trim_crlf,
                absolute_error,
                relative_error,
            )),
            timelimit: self.timelimit.or(timelimit),
        })
    }
}

#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Clone, Serialize, Deserialize)]
#[serde(untagged)]
enum NonNestedValue {
    Single(NonArrayValue),
    Multiple(Vec<NonArrayValue>),
}

impl NonNestedValue {
    fn single_string(s: String) -> Self {
        NonNestedValue::Single(NonArrayValue::String(s))
    }
}

impl Default for NonNestedValue {
    fn default() -> Self {
        NonNestedValue::Single(NonArrayValue::String("".to_owned()))
    }
}

impl fmt::Display for NonNestedValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            NonNestedValue::Single(NonArrayValue::Integer(n)) => writeln!(f, "{}", n),
            NonNestedValue::Single(NonArrayValue::Float(v)) => writeln!(f, "{}", v),
            NonNestedValue::Single(NonArrayValue::String(ref s)) => if s.ends_with('\n') {
                write!(f, "{}", s)
            } else {
                writeln!(f, "{}", s)
            },
            NonNestedValue::Multiple(ref xs) => {
                if xs.is_empty() {
                    writeln!(f)
                } else {
                    for x in xs {
                        match *x {
                            NonArrayValue::Integer(n) => writeln!(f, "{}", n),
                            NonArrayValue::Float(v) => writeln!(f, "{}", v),
                            NonArrayValue::String(ref s) => writeln!(f, "{}", s),
                        }?;
                    }
                    Ok(())
                }
            }
        }
    }
}

#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Clone, Serialize, Deserialize)]
#[serde(untagged)]
enum NonArrayValue {
    Integer(i64),
    Float(f64),
    String(String),
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
