use error::{ConfigError, ConfigErrorKind, ConfigResult};
use judge::CommandParameters;
use testcase::{TestCaseFileExtension, TestCaseFilePath};
use util::{self, ToCamlCase};

use serde_yaml;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::str::FromStr;


/// Creates `snowchains.yml` in `dir`.
pub fn create_config_file(lang: &str, dir: &str) -> ConfigResult<()> {
    fn append_exe_if_windows(path: &str) -> String {
        if cfg!(target_os = "windows") {
            format!("{}.exe", path)
        } else {
            path.to_owned()
        }
    }

    let csharp_or_mono = if cfg!(target_os = "windows") {
        Project::Build(BuildProject {
            name: "c#".to_owned(),
            camelize_src: true,
            camelize_bin: true,
            src: PercentFormat("csharp/%/%.cs".to_owned()),
            bin: PercentFormat("csharp/%/bin/Release/%.exe".to_owned()),
            working_dir: InputPath("csharp/".to_owned()),
            build: Some(ScalarOrVec::Scalar(
                "msbuild .\\csharp.sln /p:Configuration=Release".to_owned(),
            )),
            atcoder_lang_id: Some(3006),
        })
    } else {
        Project::Vm(VmProject {
            name: "mono".to_owned(),
            camelize_src: true,
            camelize_bin: true,
            src: PercentFormat("csharp/%/%.cs".to_owned()),
            build_working_dir: InputPath("csharp/".to_owned()),
            runtime_working_dir: InputPath("csharp/".to_owned()),
            build: Some(ScalarOrVec::Scalar(
                "msbuild.exe ./csharp.sln /p:Configuration=Release"
                    .to_owned(),
            )),
            runtime: PercentFormat("mono ./%/bin/Release/%.exe".to_owned()),
            atcoder_lang_id: Some(3006),
        })
    };

    let config = Config {
        service: Some(ServiceName::AtCoderBeta),
        contest: Some("chokudai_s001".to_owned()),
        testcases: InputPath("snowchains/".to_owned()),
        testcase_extension: TestCaseFileExtension::Yml,
        default_lang: lang.to_owned(),
        languages: vec![
            Project::Build(BuildProject {
                name: "c".to_owned(),
                camelize_src: false,
                camelize_bin: false,
                src: PercentFormat("c/%.c".to_owned()),
                bin: PercentFormat(append_exe_if_windows("c/build/%")),
                working_dir: InputPath("c/".to_owned()),
                build: Some(ScalarOrVec::Scalar("ninja".to_owned())),
                atcoder_lang_id: Some(3002),
            }),
            Project::Build(BuildProject {
                name: "c++".to_owned(),
                camelize_src: false,
                camelize_bin: false,
                src: PercentFormat("cc/%.cc".to_owned()),
                bin: PercentFormat(append_exe_if_windows("cc/build/%")),
                working_dir: InputPath("cc/".to_owned()),
                build: Some(ScalarOrVec::Scalar("ninja".to_owned())),
                atcoder_lang_id: Some(3003),
            }),
            Project::Build(BuildProject {
                name: "rust".to_owned(),
                camelize_src: false,
                camelize_bin: false,
                src: PercentFormat("rust/src/bin/%.rs".to_owned()),
                bin: PercentFormat(append_exe_if_windows("rust/target/release/%.exe")),
                working_dir: InputPath("rust/".to_owned()),
                build: Some(ScalarOrVec::Scalar("cargo build --release".to_owned())),
                atcoder_lang_id: Some(3504),
            }),
            Project::Vm(VmProject {
                name: "haskell".to_owned(),
                camelize_src: true,
                camelize_bin: false,
                src: PercentFormat("haskell/src/%.hs".to_owned()),
                build_working_dir: InputPath("haskell/src/".to_owned()),
                runtime_working_dir: InputPath("haskell/".to_owned()),
                build: Some(ScalarOrVec::Scalar("stack build".to_owned())),
                runtime: PercentFormat("stack run %".to_owned()),
                atcoder_lang_id: Some(3014),
            }),
            csharp_or_mono,
            Project::Vm(VmProject {
                name: "java".to_owned(),
                camelize_src: true,
                camelize_bin: true,
                src: PercentFormat("java/src/main/java/%.java".to_owned()),
                build_working_dir: InputPath("java/".to_owned()),
                runtime_working_dir: InputPath("java/build/classes/java/main/".to_owned()),
                build: Some(ScalarOrVec::Scalar(
                    "gradle --daemon compileJava".to_owned(),
                )),
                runtime: PercentFormat("java %".to_owned()),
                atcoder_lang_id: Some(3016),
            }),
            Project::Vm(VmProject {
                name: "scala".to_owned(),
                camelize_src: true,
                camelize_bin: true,
                src: PercentFormat("scala/src/main/scala/%.scala".to_owned()),
                build_working_dir: InputPath("scala/".to_owned()),
                runtime_working_dir: InputPath("scala/target/scala-2.12/classes/".to_owned()),
                build: Some(ScalarOrVec::Scalar("sbt compile".to_owned())),
                runtime: PercentFormat("scala %".to_owned()),
                atcoder_lang_id: Some(3025),
            }),
            Project::Script(ScriptProject {
                name: "python3".to_owned(),
                camelize: false,
                src: PercentFormat("python/%.py".to_owned()),
                working_dir: InputPath("python/".to_owned()),
                runtime: Some(PercentFormat("python3 %".to_owned())),
                atcoder_lang_id: Some(3023),
            }),
        ],
        base_dir: PathBuf::new(),
    };
    let config = serde_yaml::to_string(&config)?;
    let mut path = PathBuf::from(dir);
    path.push("snowchains.yml");
    Ok(util::create_file_and_dirs(&path)?.write_all(
        config.as_bytes(),
    )?)
}


/// Sets a property in `snowchains.yml`.
pub fn set_property(key: PropertyKey, value: &str) -> ConfigResult<()> {
    let mut config = Config::load_from_file()?;
    match key {
        PropertyKey::Service => config.service = Some(serde_yaml::from_str(value)?),
        PropertyKey::Contest => config.contest = Some(value.to_owned()),
        PropertyKey::Testcases => config.testcases = InputPath(value.to_owned()),
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
    Ok(util::create_file_and_dirs(&find_base()?.1)?.write_all(
        config
            .as_bytes(),
    )?)
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
        Ok(self.testcases.resolve(&self.base_dir)?)
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
    #[serde(rename = "hackerrank")]
    HackerRank,
}

impl FromStr for ServiceName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        let s = s.to_lowercase();
        return match &s {
            s if s == "atcoder" => Ok(ServiceName::AtCoder),
            s if s == "atcoder-beta" => Ok(ServiceName::AtCoderBeta),
            s if s == "hackerrank" => Ok(ServiceName::HackerRank),
            _ => Err(()),
        };
    }
}


fn default_testcases_path() -> InputPath {
    InputPath(
        if cfg!(target_os = "windows") {
            ".\\snowchains\\"
        } else {
            "./snowchains/"
        }.to_owned(),
    )
}


fn always_true() -> bool {
    true
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


#[derive(Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
enum Project {
    Script(ScriptProject),
    Build(BuildProject),
    Vm(VmProject),
}

impl Project {
    fn name(&self) -> &str {
        match *self {
            Project::Script(ScriptProject { ref name, .. }) => &name,
            Project::Build(BuildProject { ref name, .. }) => &name,
            Project::Vm(VmProject { ref name, .. }) => &name,
        }
    }

    fn src(&self, base: &Path, target_name: &str) -> io::Result<PathBuf> {
        let camelize = match *self {
            Project::Script(ScriptProject { camelize, .. }) => camelize,
            Project::Build(BuildProject { camelize_src, .. }) => camelize_src,
            Project::Vm(VmProject { camelize_src, .. }) => camelize_src,
        };
        let src = match *self {
            Project::Script(ScriptProject { ref src, .. }) => src,
            Project::Build(BuildProject { ref src, .. }) => src,
            Project::Vm(VmProject { ref src, .. }) => src,
        };
        src.resolve_as_path(base, target_name, camelize)
    }

    fn atcoder_lang_id(&self) -> Option<u32> {
        match *self {
            Project::Script(ScriptProject { ref atcoder_lang_id, .. }) => atcoder_lang_id,
            Project::Build(BuildProject { ref atcoder_lang_id, .. }) => atcoder_lang_id,
            Project::Vm(VmProject { ref atcoder_lang_id, .. }) => atcoder_lang_id,
        }.clone()
    }

    fn construct_build_command(&self, base: &Path) -> ConfigResult<Option<CommandParameters>> {
        match *self {
            Project::Build(BuildProject {
                               ref working_dir,
                               build: Some(ref build),
                               ..
                           }) => Ok(Some(build.to_command(working_dir.resolve(base)?))),
            Project::Vm(VmProject {
                            ref build_working_dir,
                            build: Some(ref build),
                            ..
                        }) => Ok(Some(build.to_command(build_working_dir.resolve(base)?))),
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
                                camelize,
                                ref working_dir,
                                ref runtime,
                                ..
                            }) => {
                let working_dir = working_dir.resolve(base)?;
                let o = self.src(base, target_name)?.to_string_lossy().to_string();
                Ok(if let Some(ref runtime) = *runtime {
                    runtime.to_run_command(&o, camelize, working_dir)
                } else {
                    CommandParameters::new(o, vec![], working_dir)
                })
            }
            Project::Build(BuildProject {
                               camelize_bin,
                               ref bin,
                               ref working_dir,
                               ..
                           }) => {
                let working_dir = working_dir.resolve(base)?;
                let bin_path = bin.resolve_as_path(base, target_name, camelize_bin)?;
                Ok(CommandParameters::new(
                    bin_path.to_string_lossy().to_string(),
                    vec![],
                    working_dir,
                ))
            }
            Project::Vm(VmProject {
                            camelize_bin,
                            ref runtime_working_dir,
                            ref runtime,
                            ..
                        }) => {
                Ok(runtime.to_run_command(
                    target_name,
                    camelize_bin,
                    runtime_working_dir.resolve(base)?,
                ))
            }
        }
    }
}


#[derive(Serialize, Deserialize)]
struct ScriptProject {
    name: String,
    #[serde(default)]
    camelize: bool,
    src: PercentFormat,
    working_dir: InputPath,
    runtime: Option<PercentFormat>,
    #[serde(skip_serializing_if = "Option::is_none")]
    atcoder_lang_id: Option<u32>,
}


#[derive(Serialize, Deserialize)]
struct BuildProject {
    name: String,
    #[serde(default)]
    camelize_src: bool,
    #[serde(default)]
    camelize_bin: bool,
    src: PercentFormat,
    bin: PercentFormat,
    working_dir: InputPath,
    build: Option<ScalarOrVec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    atcoder_lang_id: Option<u32>,
}


#[derive(Serialize, Deserialize)]
struct VmProject {
    name: String,
    #[serde(default = "always_true")]
    camelize_src: bool,
    #[serde(default = "always_true")]
    camelize_bin: bool,
    src: PercentFormat,
    build_working_dir: InputPath,
    runtime_working_dir: InputPath,
    build: Option<ScalarOrVec<String>>,
    runtime: PercentFormat,
    #[serde(skip_serializing_if = "Option::is_none")]
    atcoder_lang_id: Option<u32>,
}


#[derive(Serialize, Deserialize)]
struct InputPath(String);
impl InputPath {
    fn resolve(&self, base: &Path) -> io::Result<PathBuf> {
        if self.0.chars().next() == Some('~') {
            let mut pathbuf = util::home_dir_as_io_result()?;
            pathbuf.push(self.0.chars().skip(2).collect::<String>());
            return Ok(pathbuf);
        }
        let path = PathBuf::from(&self.0);
        if path.is_absolute() {
            Ok(path)
        } else {
            let mut pathbuf = PathBuf::from(base);
            pathbuf.push(path);
            Ok(pathbuf)
        }
    }
}


#[derive(Serialize, Deserialize)]
struct PercentFormat(String);

impl PercentFormat {
    fn resolve_as_path(
        &self,
        base: &Path,
        target_name: &str,
        camelize: bool,
    ) -> io::Result<PathBuf> {
        let rel_path = InputPath(self.format(&target_name, camelize));
        rel_path.resolve(base)
    }

    fn to_run_command(&self, arg: &str, camelize: bool, working_dir: PathBuf) -> CommandParameters {
        CommandParameters::wrap_in_sh_or_cmd_if_necessary(self.format(arg, camelize), working_dir)
    }

    fn format(&self, arg: &str, camelize: bool) -> String {
        // e.g.
        // `PercentFormat("cc/%.cc".to_owned()).format("a", false) == "cc/a.cc"`
        // `PercentFormat("csharp/%/%.cs".to_owned()).format("a", true) == "csharp/A/A.cs"`
        let arg = if camelize {
            arg.to_caml_case()
        } else {
            arg.to_owned()
        };
        let mut formatted = String::new();
        for c in self.0.chars() {
            if c == '%' {
                formatted.push_str(&arg);
            } else {
                formatted.push(c);
            }
        }
        formatted
    }
}


#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum ScalarOrVec<T> {
    Scalar(T),
    Vec(Vec<T>),
}

impl ScalarOrVec<String> {
    fn to_command(&self, working_dir: PathBuf) -> CommandParameters {
        match *self {
            ScalarOrVec::Scalar(ref arg0) => {
                CommandParameters::wrap_in_sh_or_cmd_if_necessary(arg0.clone(), working_dir)
            }
            ScalarOrVec::Vec(ref values) => {
                let mut it = values.iter();
                let arg0 = it.next().cloned().unwrap_or_default();
                let rest_args = it.cloned().collect();
                CommandParameters::new(arg0, rest_args, working_dir)
            }
        }
    }
}
