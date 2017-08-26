use super::error::{TestCaseErrorKind, TestCaseResult};
use super::util;
use serde_json;
use serde_yaml;
use std::fmt::{self, Display, Formatter};
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;
use std::vec;
use toml;


#[derive(Serialize, Deserialize)]
pub struct Cases {
    #[serde(skip_serializing_if = "Option::is_none")]
    timelimit: Option<u64>,
    cases: Vec<Case>,
}

impl IntoIterator for Cases {
    type Item = Case;
    type IntoIter = vec::IntoIter<Case>;

    fn into_iter(self) -> vec::IntoIter<Case> {
        let mut this = self;
        let timelimit = this.timelimit;
        if let Some(timelimit) = timelimit {
            for case in &mut this.cases {
                if case.timelimit.is_none() {
                    case.timelimit = Some(timelimit);
                }
            }
        }
        this.cases.into_iter()
    }
}

impl Cases {
    pub fn from_text(timelimit: u64, cases: Vec<(String, String)>) -> Self {
        Self {
            timelimit: Some(timelimit),
            cases: cases
                .into_iter()
                .map(|(expected, input)| {
                    Case {
                        timelimit: None,
                        expected: Some(NonNestedValue::NonArray(NonArrayValue::String(expected))),
                        input: NonNestedValue::NonArray(NonArrayValue::String(input)),
                    }
                })
                .collect(),
        }
    }

    pub fn num_cases(&self) -> usize {
        self.cases.len()
    }

    pub fn load(path: &Path) -> TestCaseResult<Self> {
        let text = util::string_from_read(File::open(path)?)?;
        match path.extension() {
            Some(ref ext) if *ext == "json" => Ok(serde_json::from_str(&text)?),
            Some(ref ext) if *ext == "toml" => Ok(toml::from_str(&text)?),
            Some(ref ext) if *ext == "yaml" || *ext == "yml" => Ok(serde_yaml::from_str(&text)?),
            Some(ref ext) => {
                bail!(TestCaseErrorKind::UnsupportedExtension(
                    format!("{:?}", ext),
                ))
            }
            _ => {
                bail!(TestCaseErrorKind::UnsupportedExtension(
                    format!("no extension"),
                ))
            }
        }
    }

    pub fn save(&self, path: &Path) -> TestCaseResult<()> {
        fs::create_dir_all(path.parent().unwrap())?;
        let mut file = File::create(path)?;
        let serialized = match path.extension() {
            Some(ref ext) if *ext == "json" => serde_json::to_string(self)?,
            Some(ref ext) if *ext == "toml" => toml::to_string(self)?,
            Some(ref ext) if *ext == "yaml" || *ext == "yml" => serde_yaml::to_string(self)?,
            Some(ref ext) => {
                bail!(TestCaseErrorKind::UnsupportedExtension(
                    format!("{:?}", ext),
                ))
            }
            _ => {
                bail!(TestCaseErrorKind::UnsupportedExtension(
                    format!("no extension"),
                ))
            }
        };
        Ok(file.write_all(serialized.as_bytes())?)
    }
}


#[derive(Clone, Serialize, Deserialize)]
pub struct Case {
    #[serde(skip_serializing_if = "Option::is_none")]
    timelimit: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    expected: Option<NonNestedValue>,
    input: NonNestedValue,
}

impl Into<(Option<u64>, String, String)> for Case {
    fn into(self) -> (Option<u64>, String, String) {
        (
            self.timelimit,
            self.expected.map(|x| x.into()).unwrap_or_default(),
            self.input.into(),
        )
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

        fn as_lines<T: Display>(a: &[T]) -> String {
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

impl Display for NonArrayValue {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            NonArrayValue::Integer(n) => write!(f, "{}", n),
            NonArrayValue::Float(v) => write!(f, "{}", v),
            NonArrayValue::String(ref s) => write!(f, "{}", s),
        }
    }
}
