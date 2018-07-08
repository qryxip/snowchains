use command::{CompilationCommand, JudgingCommand};
use config::Config;
use errors::{FileIoError, FileIoErrorKind, SerializeError, SuiteFileError, SuiteFileResult};
use palette::Palette;
use path::{AbsPath, AbsPathBuf};
use template::{BaseDirSome, PathTemplate};
use util::{self, ScalarOrArray};
use yaml;

use itertools::Itertools as _Itertools;
use regex::Regex;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use zip::ZipArchive;
use {serde_json, serde_yaml, toml};

use std::cmp::Ordering;
use std::collections::{BTreeSet, HashSet};
use std::fmt::{self, Write as _Write};
use std::iter::FromIterator as _FromIterator;
use std::path::Path;
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;
use std::{self, f64, io, vec};

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
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum SerializableExtension {
    Json,
    Toml,
    Yaml,
    Yml,
}

impl Default for SerializableExtension {
    fn default() -> Self {
        SerializableExtension::Yaml
    }
}

impl fmt::Display for SerializableExtension {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SerializableExtension::Json => write!(f, "json"),
            SerializableExtension::Toml => write!(f, "toml"),
            SerializableExtension::Yaml => write!(f, "yaml"),
            SerializableExtension::Yml => write!(f, "yml"),
        }
    }
}

impl FromStr for SerializableExtension {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, String> {
        match s {
            s if s.eq_ignore_ascii_case("json") => Ok(SerializableExtension::Json),
            s if s.eq_ignore_ascii_case("toml") => Ok(SerializableExtension::Toml),
            s if s.eq_ignore_ascii_case("yaml") => Ok(SerializableExtension::Yaml),
            s if s.eq_ignore_ascii_case("yml") => Ok(SerializableExtension::Yml),
            s => Err(format!("Unsupported extension: {:?}", s)),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum SuiteFileExtension {
    Json,
    Toml,
    Yaml,
    Yml,
    Zip,
}

impl SuiteFileExtension {
    fn serializable(self) -> Option<SerializableExtension> {
        match self {
            SuiteFileExtension::Json => Some(SerializableExtension::Json),
            SuiteFileExtension::Toml => Some(SerializableExtension::Toml),
            SuiteFileExtension::Yaml => Some(SerializableExtension::Yaml),
            SuiteFileExtension::Yml => Some(SerializableExtension::Yml),
            SuiteFileExtension::Zip => None,
        }
    }
}

impl FromStr for SuiteFileExtension {
    type Err = ();

    fn from_str(s: &str) -> std::result::Result<Self, ()> {
        match s {
            s if s.eq_ignore_ascii_case("json") => Ok(SuiteFileExtension::Json),
            s if s.eq_ignore_ascii_case("toml") => Ok(SuiteFileExtension::Toml),
            s if s.eq_ignore_ascii_case("yaml") => Ok(SuiteFileExtension::Yaml),
            s if s.eq_ignore_ascii_case("yml") => Ok(SuiteFileExtension::Yml),
            s if s.eq_ignore_ascii_case("zip") => Ok(SuiteFileExtension::Zip),
            _ => Err(()),
        }
    }
}

impl fmt::Display for SuiteFileExtension {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SuiteFileExtension::Json => write!(f, "json"),
            SuiteFileExtension::Toml => write!(f, "toml"),
            SuiteFileExtension::Yaml => write!(f, "yaml"),
            SuiteFileExtension::Yml => write!(f, "yml"),
            SuiteFileExtension::Zip => write!(f, "zip"),
        }
    }
}

pub(crate) struct SuiteFilePathsTemplate<'a> {
    directory: PathTemplate<BaseDirSome<'a>>,
    extensions: &'a BTreeSet<SuiteFileExtension>,
    zip: &'a ZipConfig,
}

impl<'a> SuiteFilePathsTemplate<'a> {
    pub fn new(
        directory: PathTemplate<BaseDirSome<'a>>,
        extensions: &'a BTreeSet<SuiteFileExtension>,
        zip: &'a ZipConfig,
    ) -> Self {
        Self {
            directory,
            extensions,
            zip,
        }
    }

    /// Merge test suites in `dir` which filename stem equals `stem` and
    /// extension is in `extensions`.
    pub fn load_merging(
        &self,
        config: &Config,
        problem: &str,
    ) -> SuiteFileResult<(TestCases, String)> {
        let dir = self.directory.expand(problem)?;
        if !dir.exists() {
            return Err(SuiteFileError::DirNotExist(dir.to_owned()));
        }
        let existing_filenames = {
            let mut filenames = ::fs::read_dir(&dir)?
                .filter_map(|entry| match entry {
                    Ok(entry) => {
                        let path = entry.path();
                        let stem = path.file_stem()?.to_str()?.to_owned();
                        let ext = path.extension()?.to_str()?;
                        let ext = SuiteFileExtension::from_str(ext).ok()?;
                        ensure_opt!(problem.eq_ignore_ascii_case(&stem));
                        ensure_opt!(self.extensions.contains(&ext));
                        Some(Ok((stem, ext)))
                    }
                    Err(e) => Some(Err::<_, io::Error>(e)),
                })
                .collect::<io::Result<Vec<_>>>()?;
            filenames.sort_by_key(|(s, _)| s.clone());
            filenames.sort_by_key(|&(_, e)| e);
            filenames
        };
        let mut simple_cases = vec![];
        let mut interactive_cases = vec![];
        let mut filenames = vec![];
        for (stem, ext) in &existing_filenames {
            let filename = format!("{}.{}", stem, ext);
            if let Some(ext) = ext.serializable() {
                let path = SuiteFilePath::new(&dir, stem.as_str(), ext);
                match TestSuite::load(&path)? {
                    TestSuite::Simple(suite) => simple_cases.extend(suite.with_filename(&filename)),
                    TestSuite::Interactive(suite) => {
                        interactive_cases.extend(suite.cases(&config, &filename, problem)?);
                    }
                    TestSuite::Unsubmittable => {
                        return Err(SuiteFileError::Unsubmittable(problem.to_owned()))
                    }
                }
                filenames.push(filename);
            } else {
                let cases = self.zip.load(&filename, &dir)?;
                if !cases.is_empty() {
                    simple_cases.extend(cases);
                    filenames.push(filename);
                }
            }
        }
        let paths_as_text = dir
            .join(format!("{{{}}}", filenames.iter().join(", ")))
            .display()
            .to_string();
        if simple_cases.is_empty() && interactive_cases.is_empty() {
            Err(SuiteFileError::NoFile(dir.clone()))
        } else if interactive_cases.is_empty() {
            Ok((TestCases::Simple(simple_cases), paths_as_text))
        } else if simple_cases.is_empty() {
            Ok((TestCases::Interactive(interactive_cases), paths_as_text))
        } else {
            Err(SuiteFileError::DifferentTypesOfSuites)
        }
    }
}

/// File path which extension is 'json', 'toml', 'yaml', or 'yml'.
pub(crate) struct SuiteFilePath {
    extension: SerializableExtension,
    joined: Arc<AbsPathBuf>,
}

impl<'a> SuiteFilePath {
    pub fn new(directory: AbsPath, stem: &str, extension: SerializableExtension) -> Self {
        let joined = directory.join(stem).with_extension(&extension.to_string());
        Self {
            extension,
            joined: Arc::new(joined),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub(crate) struct ZipConfig {
    #[serde(
        serialize_with = "util::ser::millis",
        deserialize_with = "util::de::millis",
        skip_serializing_if = "Option::is_none"
    )]
    timelimit: Option<Duration>,
    #[serde(rename = "match")]
    output_match: Match,
    entries: Vec<ZipEntries>,
}

impl ZipConfig {
    fn load(&self, filename: &str, dir: AbsPath) -> SuiteFileResult<Vec<SimpleCase>> {
        let (timelimit, output_match) = (self.timelimit, self.output_match);
        let mut r = vec![];
        for e in &self.entries {
            r.extend(e.load(&dir.join(filename), filename, timelimit, output_match)?);
        }
        Ok(r)
    }
}

#[derive(Serialize, Deserialize)]
struct ZipEntries {
    sort: Vec<ZipEntrySorting>,
    #[serde(rename = "in")]
    input: ZipEntry,
    #[serde(rename = "out")]
    output: ZipEntry,
}

impl ZipEntries {
    fn load(
        &self,
        path: AbsPath,
        filename: &str,
        timelimit: Option<Duration>,
        output_match: Match,
    ) -> SuiteFileResult<Vec<SimpleCase>> {
        if !path.exists() {
            return Ok(vec![]);
        }
        let mut zip =
            ZipArchive::new(::fs::open(path)?).map_err(|e| FileIoError::read_zip(path, e))?;
        let mut pairs = hashmap!();
        for i in 0..zip.len() {
            let (filename, content) = {
                let file = zip.by_index(i).map_err(|e| FileIoError::read_zip(path, e))?;
                let filename = file.name().to_owned();
                let content = util::string_from_read(file, 0)
                    .map_err(|e| FileIoError::read_zip(path, e.into()))?;
                (filename, content)
            };
            if let Some(caps) = self.input.entry.captures(&filename) {
                let name = caps
                    .get(self.input.match_group)
                    .ok_or_else(|| SuiteFileError::RegexGroupOutOfBounds(self.input.match_group))?
                    .as_str()
                    .to_owned();
                if let Some((_, output)) = pairs.remove(&name) {
                    pairs.insert(name, (Some(content.clone()), output));
                } else {
                    pairs.insert(name, (Some(content.clone()), None));
                }
            }
            if let Some(caps) = self.output.entry.captures(&filename) {
                let name = caps
                    .get(self.output.match_group)
                    .ok_or_else(|| SuiteFileError::RegexGroupOutOfBounds(self.output.match_group))?
                    .as_str()
                    .to_owned();
                if let Some((input, _)) = pairs.remove(&name) {
                    pairs.insert(name, (input, Some(content)));
                } else {
                    pairs.insert(name, (None, Some(content)));
                }
            }
        }
        let mut cases = pairs
            .into_iter()
            .filter_map(|(name, (input, output))| {
                if let (Some(input), Some(output)) = (input, output) {
                    Some((name, input, output))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        for sorting in &self.sort {
            match sorting {
                ZipEntrySorting::Dictionary => cases.sort_by(|(s1, _, _), (s2, _, _)| s1.cmp(s2)),
                ZipEntrySorting::Number => cases.sort_by(|(s1, _, _), (s2, _, _)| {
                    match (s1.parse::<usize>(), s2.parse::<usize>()) {
                        (Ok(n1), Ok(n2)) => n1.cmp(&n2),
                        (Ok(_), Err(_)) => Ordering::Less,
                        (Err(_), Ok(_)) => Ordering::Greater,
                        (Err(_), Err(_)) => Ordering::Equal,
                    }
                }),
            }
        }
        let cases = cases
            .into_iter()
            .map(|(name, input, output)| SimpleCase {
                name: Arc::new(format!("{}:{}", filename, name)),
                input: Arc::new(input),
                expected: Arc::new(ExpectedStdout::new(Some(&output), output_match)),
                timelimit,
            })
            .collect();
        Ok(cases)
    }
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
enum ZipEntrySorting {
    Dictionary,
    Number,
}

#[derive(Serialize, Deserialize)]
struct ZipEntry {
    #[serde(
        serialize_with = "yaml::serialize_regex", deserialize_with = "yaml::deserialize_regex"
    )]
    entry: Regex,
    match_group: usize,
}

/// `SimpelSuite` or `InteractiveSuite`.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[cfg_attr(test, derive(PartialEq))]
#[serde(tag = "type", rename_all = "lowercase")]
pub(crate) enum TestSuite {
    Simple(SimpleSuite),
    Interactive(InteractiveSuite),
    Unsubmittable,
}

impl TestSuite {
    /// Constructs a `TestSuite::Simple` with `timelimit` and `samples`.
    pub fn simple(
        timelimit: impl Into<Option<Duration>>,
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
    pub fn interactive(timelimit: Duration) -> Self {
        TestSuite::Interactive(InteractiveSuite::empty(Some(timelimit)))
    }

    fn load(path: &SuiteFilePath) -> SuiteFileResult<Self> {
        fn chain_err<E: std::error::Error>(err: E, path: &Path) -> FileIoError {
            FileIoError::chaining(FileIoErrorKind::Deserialize, path, err)
        }
        let (path, extension) = (&path.joined, path.extension);
        let content = ::fs::read_to_string(&path)?;
        match extension {
            SerializableExtension::Json => {
                serde_json::from_str(&content).map_err(|e| chain_err(e, &path))
            }
            SerializableExtension::Toml => {
                toml::from_str(&content).map_err(|e| chain_err(e, &path))
            }
            SerializableExtension::Yaml | SerializableExtension::Yml => {
                serde_yaml::from_str(&content).map_err(|e| chain_err(e, &path))
            }
        }.map_err(Into::into)
    }

    /// Serializes `self` and save it to given path.
    pub fn save(&self, path: &SuiteFilePath, prints_num_cases: bool) -> SuiteFileResult<()> {
        fn chain_err<E: std::error::Error>(err: E, content: &TestSuite) -> SerializeError {
            SerializeError::new(content, err)
        }
        let (path, extension) = (&path.joined, path.extension);
        let serialized = match extension {
            SerializableExtension::Json => {
                serde_json::to_string(self).map_err(|e| chain_err(e, self))
            }
            SerializableExtension::Toml => toml::to_string(self).map_err(|e| chain_err(e, self)),
            SerializableExtension::Yaml | SerializableExtension::Yml => {
                serde_yaml::to_string(self).map_err(|e| chain_err(e, self))
            }
        }?;
        ::fs::write(path.as_ref(), serialized.as_bytes())?;
        print!("Saved to {}", path.display());
        if prints_num_cases {
            let msg = match self {
                TestSuite::Simple(s) => match s.cases.len() {
                    0 => Palette::Warning.paint(" (no sample case extracted)"),
                    1 => Palette::Success.paint(" (1 sample case extracted)"),
                    n => Palette::Success.paint(format!(" ({} sample cases extracted)", n)),
                },
                TestSuite::Interactive(_) => Palette::Success.paint(" (interactive problem)"),
                TestSuite::Unsubmittable => Palette::Success.paint(" (unsubmittable problem)"),
            };
            println!("{}", msg);
        }
        Ok(())
    }

    fn append(&mut self, input: &str, output: Option<&str>) -> SuiteFileResult<()> {
        match self {
            TestSuite::Simple(suite) => {
                suite.append(input, output);
                Ok(())
            }
            _ => Err(SuiteFileError::SuiteIsNotSimple),
        }
    }
}

trait WrapInIoError {
    type Problem;
    fn wrap_in_io_error(self) -> io::Result<Self::Problem>;
}

impl<T, E: 'static + std::error::Error + Send + Sync> WrapInIoError for std::result::Result<T, E> {
    type Problem = T;

    fn wrap_in_io_error(self) -> io::Result<T> {
        self.map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
    }
}

/// Set of the timelimit and test cases.
#[derive(Clone, Debug, Default)]
#[cfg_attr(test, derive(PartialEq))]
pub(crate) struct SimpleSuite {
    timelimit: Option<Duration>,
    output_match: Match,
    cases: Vec<(Arc<String>, Option<Arc<String>>)>,
    raw_cases: Option<Vec<SimpleCaseRaw>>,
}

impl SimpleSuite {
    fn from_samples(
        timelimit: Option<Duration>,
        absolute_error: Option<f64>,
        relative_error: Option<f64>,
        samples: impl IntoIterator<Item = (String, String)>,
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
                .map(|(i, o)| (Arc::new(i), Some(Arc::new(o))))
                .collect(),
            raw_cases: None,
        }
    }

    fn with_filename(self, filename: &str) -> Vec<SimpleCase> {
        let timelimit = self.timelimit;
        let output_match = self.output_match;
        self.cases
            .into_iter()
            .enumerate()
            .map(move |(i, (input, out))| {
                let out = out.as_ref().map(|s| s.as_str());
                let expected = Arc::new(ExpectedStdout::new(out, output_match));
                let name = Arc::new(format!("{}[{}]", filename, i));
                SimpleCase {
                    name,
                    input,
                    expected,
                    timelimit,
                }
            })
            .collect::<Vec<_>>()
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
        let cases = if let Some(raw_cases) = &self.raw_cases {
            raw_cases.clone()
        } else {
            self.cases
                .iter()
                .map(|(i, o)| SimpleCaseRaw {
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
            Arc::new(match x {
                ScalarOrArray::Scalar(x) => x.to_string(),
                ScalarOrArray::Array(xs) => xs.iter().fold("".to_owned(), |mut r, x| {
                    write!(r, "{}", x).unwrap();
                    r
                }),
            })
        }
        let raw = SimpleSuiteRaw::deserialize(deserializer)?;
        Ok(Self {
            timelimit: raw.timelimit,
            output_match: raw.output_match,
            cases: raw
                .cases
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

#[derive(Debug, Serialize, Deserialize)]
struct SimpleSuiteRaw {
    #[serde(
        serialize_with = "util::ser::millis",
        deserialize_with = "util::de::millis",
        skip_serializing_if = "Option::is_none"
    )]
    timelimit: Option<Duration>,
    #[serde(rename = "match", default)]
    output_match: Match,
    cases: Vec<SimpleCaseRaw>,
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug, Serialize, Deserialize)]
struct SimpleCaseRaw {
    #[serde(rename = "in")]
    input: ScalarOrArray<NonArrayValue>,
    #[serde(rename = "out", skip_serializing_if = "Option::is_none")]
    output: Option<ScalarOrArray<NonArrayValue>>,
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(untagged)]
enum NonArrayValue {
    Integer(i64),
    Float(f64),
    String(String),
}

impl fmt::Display for NonArrayValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NonArrayValue::Integer(n) => writeln!(f, "{}", n),
            NonArrayValue::Float(v) => writeln!(f, "{}", v),
            NonArrayValue::String(s) if s.ends_with('\n') => write!(f, "{}", s),
            NonArrayValue::String(s) => writeln!(f, "{}", s),
        }
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub(crate) struct InteractiveSuite {
    #[serde(
        serialize_with = "util::ser::millis",
        deserialize_with = "util::de::millis",
        skip_serializing_if = "Option::is_none"
    )]
    timelimit: Option<Duration>,
    tester: Option<String>,
    each_args: Vec<Vec<String>>,
}

impl InteractiveSuite {
    fn empty(timelimit: Option<Duration>) -> Self {
        Self {
            timelimit,
            tester: None,
            each_args: vec![],
        }
    }

    fn cases(
        &self,
        config: &Config,
        filename: &str,
        problem: &str,
    ) -> SuiteFileResult<vec::IntoIter<InteractiveCase>> {
        let mut cases = Vec::with_capacity(self.each_args.len());
        for (i, args) in self.each_args.iter().enumerate() {
            let lang = self.tester.as_ref().map(String::as_str);
            let mut m = hashmap!("*".to_owned() => args.join(" "));
            for (i, arg) in args.iter().enumerate() {
                m.insert((i + 1).to_string(), arg.clone());
            }
            let tester = config
                .interactive_tester(lang)?
                .embed_strings(&m)
                .expand(&problem)?;
            let tester_compilation = match config.interactive_tester_compilation(lang)? {
                Some(compilation) => Some(Arc::new(compilation.expand(&problem)?)),
                None => None,
            };
            cases.push(InteractiveCase {
                name: Arc::new(format!("{}[{}]", filename, i)),
                tester: Arc::new(tester),
                tester_compilation,
                timelimit: self.timelimit,
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
        match self {
            TestCases::Simple(_) => hashset!(),
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
///
/// `expected` is empty IFF omitted.
#[derive(Clone)]
pub(crate) struct SimpleCase {
    name: Arc<String>,
    input: Arc<String>,
    expected: Arc<ExpectedStdout>,
    timelimit: Option<Duration>,
}

impl TestCase for SimpleCase {
    fn name(&self) -> Arc<String> {
        self.name.clone()
    }
}

impl SimpleCase {
    #[cfg(test)]
    pub fn default_matching(input: &str, expected: &str, timelimit: Duration) -> Self {
        let expected = Arc::new(ExpectedStdout::new(Some(expected), Match::default()));
        Self {
            name: Arc::default(),
            input: Arc::new(input.to_owned()),
            expected,
            timelimit: Some(timelimit),
        }
    }

    #[cfg(test)]
    pub fn float_matching(
        input: &str,
        expected: &str,
        timelimit: Duration,
        absolute_error: f64,
        relative_error: f64,
    ) -> Self {
        let output_match = Match::Float {
            absolute_error: absolute_error,
            relative_error: relative_error,
        };
        let expected = Arc::new(ExpectedStdout::new(Some(expected), output_match));
        Self {
            name: Arc::default(),
            input: Arc::new(input.to_owned()),
            expected,
            timelimit: Some(timelimit),
        }
    }

    pub fn values(&self) -> (Arc<String>, Arc<ExpectedStdout>, Option<Duration>) {
        let timelimit = self.timelimit;
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

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
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

#[cfg_attr(feature = "cargo-clippy", allow(trivially_copy_pass_by_ref))]
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
    name: Arc<String>,
    timelimit: Option<Duration>,
    tester: Arc<JudgingCommand>,
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
