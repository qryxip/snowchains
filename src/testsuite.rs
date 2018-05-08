use command::{CompilationCommand, JudgingCommand};
use config::Config;
use errors::{SuiteFileErrorKind, SuiteFileResult};
use template::{BaseDirSome, PathTemplate};
use terminal::Color;
use util::{self, ScalarOrArray};

use itertools::Itertools as _Itertools;
use regex::Regex;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use zip::ZipArchive;
use {serde_json, serde_yaml, toml};

use std::cmp::Ordering;
use std::collections::{BTreeSet, HashSet};
use std::fmt::{self, Write as _Write};
use std::iter::FromIterator as _FromIterator;
use std::path::{Path, PathBuf};
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
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
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
        match *self {
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
            ref s if s.eq_ignore_ascii_case("json") => Ok(SerializableExtension::Json),
            ref s if s.eq_ignore_ascii_case("toml") => Ok(SerializableExtension::Toml),
            ref s if s.eq_ignore_ascii_case("yaml") => Ok(SerializableExtension::Yaml),
            ref s if s.eq_ignore_ascii_case("yml") => Ok(SerializableExtension::Yml),
            ref s => Err(format!("Unsupported extension: {:?}", s)),
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
            ref s if s.eq_ignore_ascii_case("json") => Ok(SuiteFileExtension::Json),
            ref s if s.eq_ignore_ascii_case("toml") => Ok(SuiteFileExtension::Toml),
            ref s if s.eq_ignore_ascii_case("yaml") => Ok(SuiteFileExtension::Yaml),
            ref s if s.eq_ignore_ascii_case("yml") => Ok(SuiteFileExtension::Yml),
            ref s if s.eq_ignore_ascii_case("zip") => Ok(SuiteFileExtension::Zip),
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
        target: &str,
    ) -> SuiteFileResult<(TestCases, String)> {
        let dir = self.directory.expand(target)?;
        if !dir.exists() {
            bail!(SuiteFileErrorKind::DirNotExist(dir.to_owned()));
        }
        let existing_filenames = {
            let mut filenames = util::fs::read_dir(&dir)?
                .filter_map(|entry| match entry {
                    Ok(entry) => {
                        let path = entry.path();
                        let stem = path.file_stem()?.to_str()?.to_owned();
                        let ext = path.extension()?.to_str()?;
                        let ext = SuiteFileExtension::from_str(ext).ok()?;
                        ensure_opt!(target.eq_ignore_ascii_case(&stem));
                        ensure_opt!(self.extensions.contains(&ext));
                        Some(Ok((stem, ext)))
                    }
                    Err(e) => Some(Err::<_, io::Error>(e)),
                })
                .collect::<io::Result<Vec<_>>>()?;
            filenames.sort_by_key(|&(ref s, _)| s.clone());
            filenames.sort_by_key(|&(_, e)| e);
            filenames
        };
        let mut simple_cases = vec![];
        let mut interactive_cases = vec![];
        let mut filenames = vec![];
        for &(ref stem, ext) in &existing_filenames {
            let filename = format!("{}.{}", stem, ext);
            if let Some(ext) = ext.serializable() {
                let path = SuiteFilePath::new(&dir, stem.as_str(), ext);
                match TestSuite::load(&path)? {
                    TestSuite::Simple(suite) => simple_cases.extend(suite.with_filename(&filename)),
                    TestSuite::Interactive(suite) => {
                        interactive_cases.extend(suite.cases(&config, &filename, target)?);
                    }
                    TestSuite::Unsubmittable => {
                        bail!(SuiteFileErrorKind::Unsubmittable(target.to_owned()))
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
        let paths_as_text = dir.join(format!("{{{}}}", filenames.iter().join(", ")))
            .display()
            .to_string();
        if simple_cases.is_empty() && interactive_cases.is_empty() {
            bail!(SuiteFileErrorKind::NoFile(dir.clone()))
        } else if interactive_cases.is_empty() {
            Ok((TestCases::Simple(simple_cases), paths_as_text))
        } else if simple_cases.is_empty() {
            Ok((TestCases::Interactive(interactive_cases), paths_as_text))
        } else {
            bail!(SuiteFileErrorKind::DifferentTypesOfSuites);
        }
    }
}

/// File path which extension is 'json', 'toml', 'yaml', or 'yml'.
pub(crate) struct SuiteFilePath {
    extension: SerializableExtension,
    joined: Arc<PathBuf>,
}

impl<'a> SuiteFilePath {
    pub fn new(directory: &Path, stem: &str, extension: SerializableExtension) -> Self {
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
    fn load(&self, filename: &str, dir: &Path) -> SuiteFileResult<Vec<SimpleCase>> {
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
        path: &Path,
        filename: &str,
        timelimit: Option<Duration>,
        output_match: Match,
    ) -> SuiteFileResult<Vec<SimpleCase>> {
        if !path.exists() {
            return Ok(vec![]);
        }
        let mut zip = ZipArchive::new(util::fs::open(path)?)?;
        let mut pairs = hashmap!();
        for i in 0..zip.len() {
            let (filename, content) = {
                let file = zip.by_index(i)?;
                (file.name().to_owned(), util::string_from_read(file, 0)?)
            };
            if let Some(caps) = self.input.entry.captures(&filename) {
                let name = caps.get(self.input.match_group)
                    .ok_or_else(|| {
                        SuiteFileErrorKind::RegexGroupOutOfBounds(self.input.match_group)
                    })?
                    .as_str()
                    .to_owned();
                if let Some((_, output)) = pairs.remove(&name) {
                    pairs.insert(name, (Some(content.clone()), output));
                } else {
                    pairs.insert(name, (Some(content.clone()), None));
                }
            }
            if let Some(caps) = self.output.entry.captures(&filename) {
                let name = caps.get(self.output.match_group)
                    .ok_or_else(|| {
                        SuiteFileErrorKind::RegexGroupOutOfBounds(self.output.match_group)
                    })?
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
            match *sorting {
                ZipEntrySorting::Dictionary => {
                    cases.sort_by(|&(ref s1, _, _), &(ref s2, _, _)| s1.cmp(s2))
                }
                ZipEntrySorting::Number => {
                    cases.sort_by(|&(ref s1, _, _), &(ref s2, _, _)| {
                        match (s1.parse::<usize>(), s2.parse::<usize>()) {
                            (Ok(n1), Ok(n2)) => n1.cmp(&n2),
                            (Ok(_), Err(_)) => Ordering::Less,
                            (Err(_), Ok(_)) => Ordering::Greater,
                            (Err(_), Err(_)) => Ordering::Equal,
                        }
                    })
                }
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
        serialize_with = "util::yaml::serialize_regex",
        deserialize_with = "util::yaml::deserialize_regex"
    )]
    entry: Regex,
    match_group: usize,
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
    pub fn simple<T: Into<Option<Duration>>>(
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
    pub fn interactive(timelimit: Duration) -> Self {
        TestSuite::Interactive(InteractiveSuite::empty(Some(timelimit)))
    }

    fn load(path: &SuiteFilePath) -> SuiteFileResult<Self> {
        let (path, extension) = (&path.joined, path.extension);
        let text = util::fs::string_from_path(path)?;
        let suite: Self = match extension {
            SerializableExtension::Json => serde_json::from_str(&text)?,
            SerializableExtension::Toml => toml::from_str(&text)?,
            SerializableExtension::Yaml | SerializableExtension::Yml => {
                serde_yaml::from_str(&text)?
            }
        };
        Ok(suite)
    }

    /// Serializes `self` and save it to given path.
    pub fn save(&self, path: &SuiteFilePath, prints_num_cases: bool) -> SuiteFileResult<()> {
        let (path, extension) = (&path.joined, path.extension);
        let serialized = match extension {
            SerializableExtension::Json => serde_json::to_string(self)?,
            SerializableExtension::Toml => toml::to_string(self)?,
            SerializableExtension::Yaml | SerializableExtension::Yml => {
                serde_yaml::to_string(self)?
            }
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
    timelimit: Option<Duration>,
    output_match: Match,
    cases: Vec<(Arc<String>, Option<Arc<String>>)>,
    raw_cases: Option<Vec<SimpleCaseRaw>>,
}

impl SimpleSuite {
    fn from_samples<S: IntoIterator<Item = (String, String)>>(
        timelimit: Option<Duration>,
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
        target: &str,
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
                .expand(&target)?;
            let tester_compilation = match config.interactive_tester_compilation(lang)? {
                Some(compilation) => Some(Arc::new(compilation.expand(&target)?)),
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
