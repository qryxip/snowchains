use error::{ConfigError, ConfigErrorKind, ConfigResult};
use judge::CommandParameters;
use testcase::{TestCaseFileExtension, TestCaseFilePath};
use util::{self, CapitalizeFirst};

use serde_yaml;
use std::cmp::PartialEq;
use std::env;
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::str::FromStr;


type InputPath = String;


/// Creates `snowchains.yml` in `dir`.
pub fn create_config_file(lang: &str, dir: &str) -> ConfigResult<()> {
    let config = Config {
        service: Some(ServiceName::AtCoderBeta),
        contest: Some("chokudai_s001".to_owned()),
        testcases: "snowchains/".to_owned(),
        testcase_extension: TestCaseFileExtension::Yml,
        default_lang: lang.to_owned(),
        languages: vec![
            Project::Build(BuildProject {
                name: "c".to_owned(),
                src: "c/".to_owned(),
                bin: "c/build/".to_owned(),
                extension: "c".to_owned(),
                capitalize_src: false,
                capitalize_bin: false,
                src_prefix: "".to_owned(),
                bin_prefix: "".to_owned(),
                build: Some(ScalarOrVec::Scalar("ninja".to_owned())),
                atcoder_lang_id: Some(3002),
            }),
            Project::Build(BuildProject {
                name: "c++".to_owned(),
                src: "cc/".to_owned(),
                bin: "cc/build/".to_owned(),
                extension: "cc".to_owned(),
                capitalize_src: false,
                capitalize_bin: false,
                src_prefix: "".to_owned(),
                bin_prefix: "".to_owned(),
                build: Some(ScalarOrVec::Scalar("ninja".to_owned())),
                atcoder_lang_id: Some(3003),
            }),
            Project::Build(BuildProject {
                name: "rust".to_owned(),
                src: "rust/src/bin/".to_owned(),
                bin: "rust/target/release".to_owned(),
                extension: "rs".to_owned(),
                capitalize_src: false,
                capitalize_bin: false,
                src_prefix: "".to_owned(),
                bin_prefix: "".to_owned(),
                build: Some(ScalarOrVec::Scalar("cargo build --release".to_owned())),
                atcoder_lang_id: Some(3504),
            }),
            Project::Build(BuildProject {
                name: "haskell".to_owned(),
                src: "haskell/src/".to_owned(),
                bin: "~/.local/bin/".to_owned(),
                extension: "hs".to_owned(),
                capitalize_src: true,
                capitalize_bin: false,
                src_prefix: "".to_owned(),
                bin_prefix: "problem-".to_owned(),
                build: Some(ScalarOrVec::Scalar("stack install".to_owned())),
                atcoder_lang_id: Some(3014),
            }),
            Project::Java(JavaProject {
                name: "java".to_owned(),
                src: "java/src/main/java/".to_owned(),
                bin: "java/build/classes/java/main/".to_owned(),
                extension: "java".to_owned(),
                prefix: "".to_owned(),
                build: Some(ScalarOrVec::Scalar(
                    "gradle --daemon -p ../../../ compileJava".to_owned(),
                )),
                runtime: ScalarOrVec::Scalar("java".to_owned()),
                atcoder_lang_id: Some(3016),
            }),
            Project::Java(JavaProject {
                name: "scala".to_owned(),
                src: "scala/src/main/scala/".to_owned(),
                bin: "scala/target/scala-2.12/classes/".to_owned(),
                extension: "scala".to_owned(),
                prefix: "".to_owned(),
                build: Some(ScalarOrVec::Scalar("sbt compile".to_owned())),
                runtime: ScalarOrVec::Scalar("scala".to_owned()),
                atcoder_lang_id: Some(3025),
            }),
            Project::Script(ScriptProject {
                name: "python3".to_owned(),
                src: "python/".to_owned(),
                extension: "py".to_owned(),
                capitalize: false,
                prefix: "".to_owned(),
                runtime: Some(ScalarOrVec::Scalar("python3".to_owned())),
                atcoder_lang_id: Some(3023),
            }),
        ],
        base_dir: PathBuf::new(),
    };
    let config = serde_yaml::to_string(&config)?;
    let mut path = PathBuf::from(dir);
    path.push("snowchains.yml");
    Ok(File::create(path)?.write_all(config.as_bytes())?)
}


/// Sets a property in `snowchains.yml`.
pub fn set_property(key: PropertyKey, value: &str) -> ConfigResult<()> {
    let mut config = Config::load_from_file()?;
    match key {
        PropertyKey::Service => config.service = Some(serde_yaml::from_str(value)?),
        PropertyKey::Contest => config.contest = Some(value.to_owned()),
        PropertyKey::Testcases => config.testcases = value.to_owned(),
        PropertyKey::TestcaseExtension => {
            if let Some(extension) = TestCaseFileExtension::from_str(value) {
                config.testcase_extension = extension;
            } else {
                bail!(ConfigErrorKind::UnsupportedExtension(value.to_owned()));
            }
        }
        PropertyKey::DefaultLang => config.default_lang = value.to_owned(),
    }
    let config = serde_yaml::to_string(&config)?;
    File::create(find_base()?.1)?.write_all(config.as_bytes())?;
    Ok(())
}


/// Config data.
#[derive(Serialize, Deserialize)]
pub struct Config {
    #[serde(skip_serializing_if = "Option::is_none")]
    service: Option<ServiceName>,
    #[serde(skip_serializing_if = "Option::is_none")]
    contest: Option<String>,
    #[serde(default = "default_testcases_path")]
    testcases: InputPath,
    #[serde(default)]
    testcase_extension: TestCaseFileExtension,
    default_lang: String,
    languages: Vec<Project>,
    #[serde(skip)]
    base_dir: PathBuf,
}

impl Config {
    /// Loads and deserializes from the nearest `snowchains.yml`
    pub fn load_from_file() -> ConfigResult<Self> {
        let (base, path) = find_base()?;
        let mut config = serde_yaml::from_str::<Self>(&util::string_from_file_path(&path)?)?;
        config.base_dir = base;
        Ok(config)
    }

    /// Get `service`.
    pub fn service_name(&self) -> ConfigResult<ServiceName> {
        match self.service.clone() {
            Some(service) => Ok(service),
            None => bail!(ConfigErrorKind::PropertyNotSet("service")),
        }
    }

    /// Get `contest`.
    pub fn contest_name(&self) -> ConfigResult<String> {
        match self.contest.clone() {
            Some(contest) => Ok(contest),
            None => bail!(ConfigErrorKind::PropertyNotSet("contest")),
        }
    }

    /// Get `testcase_extension`.
    pub fn testcase_extension(&self) -> TestCaseFileExtension {
        self.testcase_extension
    }

    /// Get the absolute path of the test case files directory
    pub fn testcase_dir(&self) -> ConfigResult<PathBuf> {
        Ok(resolve_path(&self.base_dir, &self.testcases)?)
    }

    /// Returns the absolute path of test case file.
    pub fn testcase_path(&self, target_name: &str) -> ConfigResult<TestCaseFilePath> {
        Ok(TestCaseFilePath::new(
            &self.testcase_dir()?,
            target_name,
            self.testcase_extension,
        ))
    }

    /// Returns the path of the source file.
    pub fn src_path(&self, target_name: &str, lang: Option<&str>) -> ConfigResult<PathBuf> {
        let project = self.project(lang)?;
        Ok(project.src(&self.base_dir, target_name)?)
    }

    /// Returns the `lang_id` of given or default language
    pub fn atcoder_lang_id(&self, lang: Option<&str>) -> ConfigResult<u32> {
        let project = self.project(lang)?;
        project.atcoder_lang_id().ok_or_else(|| {
            ConfigError::from(ConfigErrorKind::PropertyNotSet("atcoder_lang_id"))
        })
    }

    /// Constructs arguments of build command for given or default language.
    pub fn construct_build_command(
        &self,
        lang: Option<&str>,
    ) -> ConfigResult<Option<CommandParameters>> {
        let project = self.project(lang)?;
        Ok(project.construct_build_command(&self.base_dir)?)
    }

    /// Constructs arguments of build executionfor given or default language.
    pub fn construct_run_command(
        &self,
        target_name: &str,
        lang: Option<&str>,
    ) -> ConfigResult<CommandParameters> {
        let project = self.project(lang)?;
        Ok(project.construct_run_command(&self.base_dir, target_name)?)
    }

    fn project(&self, lang: Option<&str>) -> ConfigResult<&Project> {
        let lang = lang.unwrap_or(&self.default_lang);
        for project in &self.languages {
            if project.name() == lang {
                return Ok(project);
            }
        }
        bail!(ConfigErrorKind::NoSuchLanguage(lang.to_owned()))
    }
}


/// Property names of `snowchains.yml`.
pub enum PropertyKey {
    Service,
    Contest,
    Testcases,
    TestcaseExtension,
    DefaultLang,
}

impl FromStr for PropertyKey {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        let s = s.to_lowercase();
        return match &s {
            s if s == "service" => Ok(PropertyKey::Service),
            s if s == "contest" => Ok(PropertyKey::Contest),
            s if s == "testcases" => Ok(PropertyKey::Testcases),
            s if s == "testcase_extension" => Ok(PropertyKey::TestcaseExtension),
            s if s == "default_lang" => Ok(PropertyKey::DefaultLang),
            _ => Err(()),
        };
    }
}


/// Names of programming contest services.
#[derive(Clone, Serialize, Deserialize)]
pub enum ServiceName {
    #[serde(rename = "atcoder")]
    AtCoder,
    #[serde(rename = "atcoder-beta")]
    AtCoderBeta,
}

impl FromStr for ServiceName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        let s = s.to_lowercase();
        return match &s {
            s if s == "atcoder" => Ok(ServiceName::AtCoder),
            s if s == "atcoder-beta" => Ok(ServiceName::AtCoderBeta),
            _ => Err(()),
        };
    }
}


fn is_default<T: Default + PartialEq>(x: &T) -> bool {
    *x == T::default()
}


fn default_testcases_path() -> InputPath {
    if cfg!(target_os = "windows") {
        ".\\snowchains\\".to_owned()
    } else {
        "./snowchains/".to_owned()
    }
}


fn default_java_extension() -> String {
    "java".to_owned()
}


fn default_java_runtime() -> ScalarOrVec<String> {
    ScalarOrVec::Scalar("java".to_owned())
}


fn is_default_java_extension(extension: &String) -> bool {
    extension == "java"
}


fn is_default_java_runtime(runtime: &ScalarOrVec<String>) -> bool {
    match *runtime {
        ScalarOrVec::Scalar(ref x) => x == "java",
        ScalarOrVec::Vec(ref xs) => xs.len() == 1 && xs[0] == "java",
    }
}


fn find_base() -> ConfigResult<(PathBuf, PathBuf)> {
    fn snowchain_yml_exists<P: AsRef<Path>>(dir: P) -> io::Result<bool> {
        for entry in fs::read_dir(dir)? {
            let path = entry?.path();
            if path.is_file() && path.file_name().unwrap() == "snowchains.yml" {
                return Ok(true);
            }
        }
        Ok(false)
    }

    let mut dir = env::current_dir()?;
    loop {
        if let Ok(true) = snowchain_yml_exists(&dir) {
            let mut path = dir.clone();
            path.push("snowchains.yml");
            return Ok((dir, path));
        } else if !dir.pop() {
            bail!(ConfigErrorKind::ConfigFileNotFound);
        }
    }
}


fn resolve_path(base: &Path, path: &str) -> io::Result<PathBuf> {
    if path.chars().next() == Some('~') {
        let mut pathbuf = util::home_dir_as_io_result()?;
        pathbuf.push(path.chars().skip(2).collect::<String>());
        return Ok(pathbuf);
    }
    let path = PathBuf::from(path);
    if path.is_absolute() {
        Ok(path)
    } else {
        let mut pathbuf = PathBuf::from(base);
        pathbuf.push(path);
        Ok(pathbuf)
    }
}


#[derive(Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
enum Project {
    Script(ScriptProject),
    Build(BuildProject),
    Java(JavaProject),
}

impl Project {
    fn name(&self) -> &str {
        match *self {
            Project::Script(ScriptProject { ref name, .. }) => &name,
            Project::Build(BuildProject { ref name, .. }) => &name,
            Project::Java(JavaProject { ref name, .. }) => &name,
        }
    }

    fn src(&self, base: &Path, target_name: &str) -> io::Result<PathBuf> {
        let mut pathbuf = resolve_path(
            base,
            match *self {
                Project::Script(ScriptProject { ref src, .. }) => src,
                Project::Build(BuildProject { ref src, .. }) => src,
                Project::Java(JavaProject { ref src, .. }) => src,
            },
        )?;
        let prefix = match *self {
            Project::Script(ScriptProject { ref prefix, .. }) => prefix,
            Project::Build(BuildProject { ref src_prefix, .. }) => src_prefix,
            Project::Java(JavaProject { ref prefix, .. }) => prefix,
        };
        pathbuf.push(if match *self {
            Project::Script(ScriptProject { capitalize, .. }) => capitalize,
            Project::Build(BuildProject { capitalize_src, .. }) => capitalize_src,
            Project::Java(_) => true,
        }
        {
            format!("{}{}", prefix, target_name.capitalize_first())
        } else {
            format!("{}{}", prefix, target_name)
        });
        pathbuf.set_extension(match *self {
            Project::Script(ScriptProject { ref extension, .. }) => extension,
            Project::Build(BuildProject { ref extension, .. }) => extension,
            Project::Java(JavaProject { ref extension, .. }) => extension,
        });
        Ok(pathbuf)
    }

    fn atcoder_lang_id(&self) -> Option<u32> {
        match *self {
            Project::Script(ScriptProject { ref atcoder_lang_id, .. }) => atcoder_lang_id,
            Project::Build(BuildProject { ref atcoder_lang_id, .. }) => atcoder_lang_id,
            Project::Java(JavaProject { ref atcoder_lang_id, .. }) => atcoder_lang_id,
        }.clone()
    }

    fn construct_build_command(&self, base: &Path) -> ConfigResult<Option<CommandParameters>> {
        fn construct(
            base: &Path,
            build: &ScalarOrVec<String>,
            dir: &str,
        ) -> ConfigResult<CommandParameters> {
            let working_dir = resolve_path(base, dir)?;
            Ok(build.to_command(working_dir, None))
        }

        match *self {
            Project::Build(BuildProject {
                               ref src,
                               build: Some(ref build),
                               ..
                           }) => Ok(Some(construct(base, build, src)?)),
            Project::Java(JavaProject {
                              ref src,
                              build: Some(ref build),
                              ..
                          }) => Ok(Some(construct(base, build, src)?)),
            _ => Ok(None),
        }
    }

    fn construct_run_command(
        &self,
        base: &Path,
        target_name: &str,
    ) -> io::Result<CommandParameters> {
        match *self {
            Project::Script(ScriptProject {
                                ref src,
                                ref runtime,
                                ..
                            }) => {
                let working_dir = resolve_path(base, src)?;
                let src_path = self.src(base, target_name)?;
                if let Some(ref runtime) = *runtime {
                    let o = src_path.to_string_lossy().to_string();
                    Ok(runtime.to_command(working_dir, Some(o)))
                } else {
                    Ok(CommandParameters::new(
                        src_path.to_string_lossy().to_string(),
                        vec![],
                        working_dir,
                    ))
                }
            }
            Project::Build(BuildProject {
                               ref bin,
                               capitalize_bin,
                               ref bin_prefix,
                               ..
                           }) => {
                let mut bin_path = resolve_path(base, bin)?;
                let working_dir = bin_path.clone();
                bin_path.push(if capitalize_bin {
                    format!("{}{}", bin_prefix, target_name.capitalize_first())
                } else {
                    format!("{}{}", bin_prefix, target_name)
                });
                if cfg!(target_os = "windows") {
                    bin_path.set_extension("exe");
                }
                Ok(CommandParameters::new(
                    bin_path.to_string_lossy().to_string(),
                    vec![],
                    working_dir,
                ))
            }
            Project::Java(JavaProject {
                              ref bin,
                              ref runtime,
                              ..
                          }) => {
                Ok(runtime.to_command(
                    resolve_path(base, &bin)?,
                    Some(target_name.capitalize_first()),
                ))
            }
        }
    }
}


#[derive(Serialize, Deserialize)]
struct ScriptProject {
    name: String,
    src: InputPath,
    extension: String,
    #[serde(default, skip_serializing_if = "is_default")]
    capitalize: bool,
    #[serde(default, skip_serializing_if = "is_default")]
    prefix: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    runtime: Option<ScalarOrVec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    atcoder_lang_id: Option<u32>,
}


#[derive(Serialize, Deserialize)]
struct BuildProject {
    name: String,
    src: InputPath,
    bin: InputPath,
    extension: String,
    #[serde(default, skip_serializing_if = "is_default")]
    capitalize_src: bool,
    #[serde(default, skip_serializing_if = "is_default")]
    capitalize_bin: bool,
    #[serde(default, skip_serializing_if = "is_default")]
    src_prefix: String,
    #[serde(default, skip_serializing_if = "is_default")]
    bin_prefix: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    build: Option<ScalarOrVec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    atcoder_lang_id: Option<u32>,
}


#[derive(Serialize, Deserialize)]
struct JavaProject {
    name: String,
    src: InputPath,
    bin: InputPath,
    #[serde(default = "default_java_extension", skip_serializing_if = "is_default_java_extension")]
    extension: String,
    #[serde(default, skip_serializing_if = "is_default")]
    prefix: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    build: Option<ScalarOrVec<String>>,
    #[serde(default = "default_java_runtime", skip_serializing_if = "is_default_java_runtime")]
    runtime: ScalarOrVec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    atcoder_lang_id: Option<u32>,
}


#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum ScalarOrVec<T> {
    Scalar(T),
    Vec(Vec<T>),
}

impl ScalarOrVec<String> {
    fn to_command(&self, working_dir: PathBuf, o: Option<String>) -> CommandParameters {
        match *self {
            ScalarOrVec::Scalar(ref arg0) => {
                let arg0 = if let Some(o) = o {
                    format!("{} {}", arg0, o)
                } else {
                    arg0.clone()
                };
                CommandParameters::wrap_in_sh_or_cmd_if_necessary(arg0, working_dir)
            }
            ScalarOrVec::Vec(ref values) => {
                let mut it = values.iter();
                let arg0 = it.next().cloned().unwrap_or_default();
                let mut rest_args = it.cloned().collect::<Vec<_>>();
                if let Some(o) = o {
                    rest_args.push(o);
                }
                CommandParameters::new(arg0, rest_args, working_dir)
            }
        }
    }
}
