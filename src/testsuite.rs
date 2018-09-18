use command::{CompilationCommand, JudgingCommand};
use console::{ConsoleWrite, Palette};
use errors::{
    ExpandTemplateResult, FileIoError, FileIoErrorCause, FileIoErrorKind, LoadConfigError,
    SuiteFileError, SuiteFileResult,
};
use path::{AbsPath, AbsPathBuf};
use template::Template;
use {util, yaml};

use regex::Regex;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use zip::ZipArchive;
use {serde_json, serde_yaml, toml};

use std::cmp::Ordering;
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use std::io::{self, Write as _Write};
use std::iter::FromIterator as _FromIterator;
use std::ops::Deref;
use std::path::Path;
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;
use std::{self, f64, fmt, str, vec};

/// Appends `input` and `output` to a test suite read from `path`.
pub(crate) fn append(
    path: &SuiteFilePath,
    input: &str,
    output: Option<&str>,
    stdout: impl ConsoleWrite,
) -> SuiteFileResult<()> {
    let mut suite = TestSuite::load(path)?;
    suite.append(input, output)?;
    suite.save(path, stdout)
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
            "json" => Ok(SerializableExtension::Json),
            "toml" => Ok(SerializableExtension::Toml),
            "yaml" => Ok(SerializableExtension::Yaml),
            "yml" => Ok(SerializableExtension::Yml),
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
            "json" => Ok(SuiteFileExtension::Json),
            "toml" => Ok(SuiteFileExtension::Toml),
            "yaml" => Ok(SuiteFileExtension::Yaml),
            "yml" => Ok(SuiteFileExtension::Yml),
            "zip" => Ok(SuiteFileExtension::Zip),
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

pub(crate) struct TestCaseLoader<'a> {
    template: Template<AbsPathBuf>,
    extensions: &'a BTreeSet<SuiteFileExtension>,
    zip_conf: &'a ZipConfig,
    tester_compilacions: HashMap<String, Template<CompilationCommand>>,
    tester_commands: HashMap<String, Template<JudgingCommand>>,
}

impl<'a> TestCaseLoader<'a> {
    pub fn new(
        template: Template<AbsPathBuf>,
        extensions: &'a BTreeSet<SuiteFileExtension>,
        zip_conf: &'a ZipConfig,
        tester_compilacions: HashMap<String, Template<CompilationCommand>>,
        tester_commands: HashMap<String, Template<JudgingCommand>>,
    ) -> Self {
        Self {
            template,
            extensions,
            zip_conf,
            tester_compilacions,
            tester_commands,
        }
    }

    pub fn load_merging(&self, problem: &str) -> SuiteFileResult<(TestCases, String)> {
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
            .map(|ext| {
                self.template
                    .clone()
                    .insert_string("extension", ext.to_string())
                    .expand(problem)
                    .map(|path| (path, ext))
            }).collect::<ExpandTemplateResult<Vec<_>>>()?;

        let existing_paths = all_paths
            .iter()
            .cloned()
            .filter(|(path, _)| path.exists())
            .collect::<Vec<_>>();

        let mut simple_cases = vec![];
        let mut interactive_cases = vec![];
        let mut filepaths = vec![];

        for (path, extension) in existing_paths {
            let filename = path
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
                .into_owned();
            if let Some(extension) = extension.serializable() {
                let path = SuiteFilePath { path, extension };
                match TestSuite::load(&path)? {
                    TestSuite::Simple(suite) => simple_cases.extend(suite.cases_named(&filename)),
                    TestSuite::Interactive(suite) => {
                        interactive_cases.extend(suite.cases_named(
                            &self.tester_compilacions,
                            &self.tester_commands,
                            &filename,
                            problem,
                        )?);
                    }
                    TestSuite::Unsubmittable => {
                        return Err(SuiteFileError::Unsubmittable(problem.to_owned()))
                    }
                }
                filepaths.push(path.path.display().to_string());
            } else {
                let cases = self.zip_conf.load(&path, &filename)?;
                if !cases.is_empty() {
                    simple_cases.extend(cases);
                    filepaths.push(path.display().to_string());
                }
            }
        }

        let paths_as_text = format_paths(&filepaths);

        if simple_cases.is_empty() && interactive_cases.is_empty() {
            let all_paths = all_paths
                .into_iter()
                .map(|(path, _)| path.display().to_string())
                .collect::<Vec<_>>();
            Err(SuiteFileError::NoFile(format_paths(&all_paths)))
        } else if interactive_cases.is_empty() {
            Ok((TestCases::Simple(simple_cases), paths_as_text))
        } else if simple_cases.is_empty() {
            Ok((TestCases::Interactive(interactive_cases), paths_as_text))
        } else {
            Err(SuiteFileError::DifferentTypesOfSuites)
        }
    }
}

pub(crate) struct DownloadDestinations {
    template: Template<AbsPathBuf>,
    scraping_ext: SerializableExtension,
}

impl DownloadDestinations {
    pub fn new(template: Template<AbsPathBuf>, scraping_ext: SerializableExtension) -> Self {
        Self {
            template,
            scraping_ext,
        }
    }

    pub fn scraping(&self, problem: &str) -> ExpandTemplateResult<SuiteFilePath> {
        let path = self
            .template
            .clone()
            .insert_string("extension", self.scraping_ext.to_string())
            .expand(problem)?;
        Ok(SuiteFilePath::new(&path, self.scraping_ext))
    }

    pub fn zip(&self, problem: &str) -> ExpandTemplateResult<AbsPathBuf> {
        self.template
            .clone()
            .insert_string("extension", "zip")
            .expand(problem)
    }
}

/// File path which extension is 'json', 'toml', 'yaml', or 'yml'.
pub(crate) struct SuiteFilePath {
    path: AbsPathBuf,
    extension: SerializableExtension,
}

impl SuiteFilePath {
    pub fn new(path: AbsPath, extension: SerializableExtension) -> Self {
        let path = path.to_owned();
        Self { path, extension }
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
    fn load(&self, path: AbsPath, filename: &str) -> SuiteFileResult<Vec<SimpleCase>> {
        let (timelimit, output_match) = (self.timelimit, self.output_match);
        let mut cases = vec![];
        for entry in &self.entries {
            cases.extend(entry.load(path, filename, timelimit, output_match)?);
        }
        Ok(cases)
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
                let file = zip
                    .by_index(i)
                    .map_err(|e| FileIoError::read_zip(path, e))?;
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
            }).collect::<Vec<_>>();
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
            .map(|(name, input, output)| {
                let (input, expected) = process_pair(&input, Some(&output), output_match);
                SimpleCase {
                    name: Arc::new(format!("{}:{}", filename, name)),
                    input,
                    expected: Arc::new(expected),
                    timelimit,
                }
            }).collect();
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
        serialize_with = "yaml::serialize_regex",
        deserialize_with = "yaml::deserialize_regex"
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
    fn load(path: &SuiteFilePath) -> SuiteFileResult<Self> {
        fn chain_err<E: Into<FileIoErrorCause>>(err: E, path: &Path) -> FileIoError {
            FileIoError::new(FileIoErrorKind::Deserialize, path).with(err)
        }

        let (path, extension) = (&path.path, path.extension);
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
    pub fn save(&self, path: &SuiteFilePath, mut stdout: impl ConsoleWrite) -> SuiteFileResult<()> {
        let (path, extension) = (&path.path, path.extension);
        let serialized = match extension {
            SerializableExtension::Json => serde_json::to_string(self)?,
            SerializableExtension::Toml => toml::to_string(self)?,
            SerializableExtension::Yaml | SerializableExtension::Yml => {
                serde_yaml::to_string(self)?
            }
        };
        ::fs::write(path, serialized.as_bytes())?;
        write!(stdout, "Saved to {}", path.display())?;
        match self {
            TestSuite::Simple(s) => match s.cases.len() {
                0 => writeln!(stdout.plain(Palette::Warning), " (no test case)"),
                1 => writeln!(stdout.plain(Palette::Success), " (1 test case)"),
                n => writeln!(stdout.plain(Palette::Success), " ({} test cases)", n),
            },
            TestSuite::Interactive(_) => {
                writeln!(stdout.plain(Palette::Success), " (interactive problem)")
            }
            TestSuite::Unsubmittable => {
                writeln!(stdout.plain(Palette::Success), " (unsubmittable problem)")
            }
        }?;
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

impl From<SimpleSuite> for TestSuite {
    fn from(from: SimpleSuite) -> Self {
        TestSuite::Simple(from)
    }
}

impl From<InteractiveSuite> for TestSuite {
    fn from(from: InteractiveSuite) -> Self {
        TestSuite::Interactive(from)
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
}

impl SimpleSuite {
    pub fn new(timelimit: impl Into<Option<Duration>>) -> Self {
        Self {
            timelimit: timelimit.into(),
            output_match: Match::default(),
            cases: vec![],
        }
    }

    pub fn cases<S: Into<String>, O: Into<Option<S>>, I: IntoIterator<Item = (S, O)>>(
        mut self,
        cases: I,
    ) -> Self {
        self.cases.extend(
            cases
                .into_iter()
                .map(|(i, o)| (Arc::new(i.into()), o.into().map(|o| Arc::new(o.into())))),
        );
        self
    }

    fn cases_named(self, filename: &str) -> Vec<SimpleCase> {
        let timelimit = self.timelimit;
        let output_match = self.output_match;
        self.cases
            .into_iter()
            .enumerate()
            .map(move |(i, (input, output))| {
                let output = output.as_ref().map(|s| s.as_str());
                let (input, expected) = process_pair(&input, output, output_match);
                let name = Arc::new(format!("{}[{}]", filename, i));
                SimpleCase {
                    name,
                    input,
                    expected: Arc::new(expected),
                    timelimit,
                }
            }).collect::<Vec<_>>()
    }

    fn append(&mut self, input: &str, output: Option<&str>) {
        let input = Arc::new(input.to_owned());
        let output = output.map(str::to_owned).map(Arc::new);
        self.cases.push((input, output))
    }
}

impl Serialize for SimpleSuite {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        let cases = self
            .cases
            .iter()
            .map(|(input, output)| SimpleCaseSchema {
                input: input.deref().clone(),
                output: output.as_ref().map(|o| o.deref().clone()),
            }).collect();
        SimpleSuiteSchema {
            timelimit: self.timelimit,
            output_match: self.output_match,
            cases,
        }.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for SimpleSuite {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        let raw = SimpleSuiteSchema::deserialize(deserializer)?;
        Ok(Self {
            timelimit: raw.timelimit,
            output_match: raw.output_match,
            cases: raw
                .cases
                .into_iter()
                .map(|case| (Arc::new(case.input), case.output.map(Arc::new)))
                .collect(),
        })
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct SimpleSuiteSchema {
    #[serde(
        serialize_with = "util::ser::millis",
        deserialize_with = "util::de::millis",
        skip_serializing_if = "Option::is_none"
    )]
    timelimit: Option<Duration>,
    #[serde(rename = "match", default)]
    output_match: Match,
    cases: Vec<SimpleCaseSchema>,
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug, Serialize, Deserialize)]
struct SimpleCaseSchema {
    #[serde(rename = "in")]
    input: String,
    #[serde(rename = "out", skip_serializing_if = "Option::is_none")]
    output: Option<String>,
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
    pub fn new(timelimit: impl Into<Option<Duration>>) -> Self {
        Self {
            timelimit: timelimit.into(),
            tester: None,
            each_args: vec![],
        }
    }

    fn cases_named(
        &self,
        tester_compilations: &HashMap<String, Template<CompilationCommand>>,
        tester_commands: &HashMap<String, Template<JudgingCommand>>,
        filename: &str,
        problem: &str,
    ) -> SuiteFileResult<vec::IntoIter<InteractiveCase>> {
        let mut cases = Vec::with_capacity(self.each_args.len());
        for (i, args) in self.each_args.iter().enumerate() {
            let lang = self
                .tester
                .as_ref()
                .ok_or_else(|| LoadConfigError::LanguageNotSpecified)?;
            let mut m = hashmap!("*".to_owned() => args.join(" "));
            for (i, arg) in args.iter().enumerate() {
                m.insert((i + 1).to_string(), arg.clone());
            }
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
                .ok_or_else(|| LoadConfigError::NoSuchLanguage(lang.to_owned()))?
                .insert_strings(&m)
                .expand(&problem)?;
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
    pub fn input(&self) -> Arc<String> {
        self.input.clone()
    }

    pub fn expected(&self) -> Arc<ExpectedStdout> {
        self.expected.clone()
    }

    pub fn timelimit(&self) -> Option<Duration> {
        self.timelimit
    }
}

fn process_pair(
    input: &str,
    output: Option<&str>,
    output_match: Match,
) -> (Arc<String>, ExpectedStdout) {
    fn add_eols(ss: &mut [&mut String]) {
        for s in ss {
            if !s.ends_with('\n') {
                s.push('\n');
            }
        }
    }

    match (output, output_match) {
        (None, _) => (Arc::new(input.to_owned()), ExpectedStdout::AcceptAny),
        (Some(output), Match::Exact { add_eols_to_cases }) => {
            let (mut input, mut output) = (input.to_owned(), output.to_owned());
            if add_eols_to_cases {
                add_eols(&mut [&mut input, &mut output]);
            }
            (Arc::new(input), ExpectedStdout::Exact(output))
        }
        (
            Some(output),
            Match::Float {
                add_eols_to_cases,
                absolute_error,
                relative_error,
            },
        ) => {
            let (mut input, mut string) = (input.to_owned(), output.to_owned());
            if add_eols_to_cases {
                add_eols(&mut [&mut input, &mut string]);
            }
            let expected = ExpectedStdout::Float {
                lines: string,
                absolute_error,
                relative_error,
            };
            (Arc::new(input), expected)
        }
    }
}

#[derive(Clone)]
#[cfg_attr(test, derive(Debug, PartialEq))]
pub(crate) enum ExpectedStdout {
    AcceptAny,
    Exact(String),
    Float {
        lines: String,
        absolute_error: f64,
        relative_error: f64,
    },
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
enum Match {
    Exact {
        add_eols_to_cases: bool,
    },
    Float {
        add_eols_to_cases: bool,
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
        Match::Exact {
            add_eols_to_cases: true,
        }
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
