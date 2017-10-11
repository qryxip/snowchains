use error::SuiteFileResult;
use util;

use serde_json;
use serde_yaml;
use std::fmt;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::vec;
use toml;


pub fn append(path: &SuiteFilePath, input: &str, output: Option<&str>) -> SuiteFileResult<()> {
    TestSuite::load(&path)?.append(input, output).save(&path)
}


/// Set of the timelimit and test cases.
#[derive(Clone, Serialize, Deserialize)]
pub struct TestSuite {
    timelimit: Option<u64>,
    cases: Vec<TestCase>,
}

impl Default for TestSuite {
    fn default() -> Self {
        Self {
            timelimit: None,
            cases: vec![],
        }
    }
}

impl IntoIterator for TestSuite {
    type Item = TestCase;
    type IntoIter = vec::IntoIter<TestCase>;

    fn into_iter(mut self) -> vec::IntoIter<TestCase> {
        let timelimit = self.timelimit;
        if let Some(timelimit) = timelimit {
            for case in &mut self.cases {
                if case.timelimit.is_none() {
                    case.timelimit = Some(timelimit);
                }
            }
        }
        self.cases.into_iter()
    }
}

impl TestSuite {
    /// Constructs a `Cases` with a timelimit value and pairs of input/output samples.
    pub fn from_text<I: IntoIterator<Item = (String, String)>>(
        timelimit: Option<u64>,
        cases: I,
    ) -> Self {
        Self {
            timelimit: timelimit,
            cases: cases
                .into_iter()
                .map(|(output, input)| {
                    TestCase::from_strings(input, Some(output))
                })
                .collect(),
        }
    }

    /// Loads from given path and deserializes it into `TestSuite`.
    pub fn load(path: &SuiteFilePath) -> SuiteFileResult<Self> {
        let (path, extension) = (path.build(), path.extension);
        let text = util::string_from_file_path(&path)?;
        match extension {
            SuiteFileExtension::Json => Ok(serde_json::from_str(&text)?),
            SuiteFileExtension::Toml => Ok(toml::from_str(&text)?),
            SuiteFileExtension::Yaml | SuiteFileExtension::Yml => Ok(serde_yaml::from_str(&text)?),
        }
    }

    /// Whether `self` has no test case.
    pub fn is_empty(&self) -> bool {
        self.cases.is_empty()
    }

    /// Returns the number of test cases of `self`.
    pub fn num_cases(&self) -> usize {
        self.cases.len()
    }

    /// Serializes `self` and save it to given path.
    pub fn save(&self, path: &SuiteFilePath) -> SuiteFileResult<()> {
        fs::create_dir_all(&path.dir)?;
        let (path, extension) = (path.build(), path.extension);
        let mut file = File::create(&path)?;
        let serialized = match extension {
            SuiteFileExtension::Json => serde_json::to_string(self)?,
            SuiteFileExtension::Toml => toml::to_string(self)?,
            SuiteFileExtension::Yaml | SuiteFileExtension::Yml => serde_yaml::to_string(self)?,
        };
        file.write_all(serialized.as_bytes())?;
        Ok(if self.is_empty() {
            println!("Saved to {} (no sample extracted)", path.display());
        } else {
            println!("Saved to {}", path.display());
        })
    }

    fn append(mut self, input: &str, output: Option<&str>) -> Self {
        self.cases.push(TestCase::from_strings(input, output));
        self
    }
}


/// Set of input/output strings and timelimit.
#[derive(Clone, Serialize, Deserialize)]
pub struct TestCase {
    input: NonNestedValue,
    #[serde(skip_serializing_if = "Option::is_none")]
    expected: Option<NonNestedValue>,
    #[serde(skip_serializing_if = "Option::is_none")]
    timelimit: Option<u64>,
}

impl Into<(String, String, Option<u64>)> for TestCase {
    fn into(self) -> (String, String, Option<u64>) {
        (
            self.input.into(),
            self.expected.map(|x| x.into()).unwrap_or_default(),
            self.timelimit,
        )
    }
}

impl TestCase {
    fn from_strings<S1: Into<String>, S2: Into<String>>(input: S1, output: Option<S2>) -> Self {
        Self {
            input: NonNestedValue::string(input.into()),
            expected: output.map(|s| NonNestedValue::string(s.into())),
            timelimit: None,
        }
    }
}


/// File path which extension is 'json', 'toml', 'yaml', or 'yml'.
pub struct SuiteFilePath {
    dir: PathBuf,
    name: String,
    extension: SuiteFileExtension,
}

impl SuiteFilePath {
    pub fn new<S: Into<String>>(dir: &Path, name: S, extension: SuiteFileExtension) -> Self {
        Self {
            dir: PathBuf::from(dir),
            name: name.into(),
            extension: extension,
        }
    }

    pub fn build(&self) -> PathBuf {
        let mut path = PathBuf::from(&self.dir);
        path.push(&self.name);
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

impl SuiteFileExtension {
    pub fn from_str(s: &str) -> Option<Self> {
        let s = s.to_lowercase();
        if s == "json" {
            Some(SuiteFileExtension::Json)
        } else if s == "toml" {
            Some(SuiteFileExtension::Toml)
        } else if s == "yaml" {
            Some(SuiteFileExtension::Yaml)
        } else if s == "yml" {
            Some(SuiteFileExtension::Yml)
        } else {
            None
        }
    }
}


#[derive(Clone, Serialize, Deserialize)]
#[serde(untagged)]
enum NonNestedValue {
    Array(Vec<NonArrayValue>),
    NonArray(NonArrayValue),
}


impl Into<String> for NonNestedValue {
    fn into(self) -> String {
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

impl NonNestedValue {
    fn string<S: Into<String>>(s: S) -> Self {
        NonNestedValue::NonArray(NonArrayValue::String(s.into()))
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
