use super::error::{ConfigError, ConfigErrorKind, ConfigResult};
use super::judge::CommandParameters;
use super::testcase::{TestCaseFileExtension, TestCaseFilePath};
use super::util::{self, CapitalizeFirst};
use serde_yaml;
use std::env;
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::{Path, PathBuf};


type InputPath = String;


pub fn create_config_file(lang: &str, dir: &str) -> ConfigResult<()> {
    let config = Config {
        service: Some(ServiceName::AtCoderBeta),
        contest: Some("agc001".to_owned()),
        testcases: "./snowchains/".to_owned(),
        testcase_extension: TestCaseFileExtension::Yml,
        default_lang: lang.to_owned(),
        targets: vec![],
        languages: vec![
            Project::Build(BuildProject {
                name: "c".to_owned(),
                src: "./c/".to_owned(),
                bin: "./c/build/".to_owned(),
                extension: "c".to_owned(),
                capitalize: false,
                build: Some(ScalarOrVec::Scalar("ninja".to_owned())),
                atcoder_lang_id: Some(3002),
            }),
            Project::Build(BuildProject {
                name: "c++".to_owned(),
                src: "./cc/".to_owned(),
                bin: "./cc/build/".to_owned(),
                extension: "cc".to_owned(),
                capitalize: false,
                build: Some(ScalarOrVec::Scalar("ninja".to_owned())),
                atcoder_lang_id: Some(3003),
            }),
            Project::Build(BuildProject {
                name: "rust".to_owned(),
                src: "./rust/src/bin/".to_owned(),
                bin: "./rust/target/release".to_owned(),
                extension: "rs".to_owned(),
                capitalize: false,
                build: Some(ScalarOrVec::Vec(vec![
                    "cargo".to_owned(),
                    "build".to_owned(),
                    "--release".to_owned(),
                ])),
                atcoder_lang_id: Some(3504),
            }),
            Project::Java(JavaProject {
                name: "java".to_owned(),
                src: "./java/serc/main/java/".to_owned(),
                bin: "./java/build/classes/java/main/".to_owned(),
                extension: "java".to_owned(),
                build: Some(ScalarOrVec::Vec(vec![
                    "gradle".to_owned(),
                    "--daemon".to_owned(),
                    "build".to_owned(),
                ])),
                atcoder_lang_id: Some(3016),
            }),
            Project::Script(ScriptProject {
                name: "python3".to_owned(),
                src: "./python/".to_owned(),
                extension: "py".to_owned(),
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


pub fn set_property(key: &str, value: &str) -> ConfigResult<()> {
    let mut config = Config::load_from_file()?;
    if key == "service" {
        config.service = Some(serde_yaml::from_str(value)?);
    } else if key == "contest" {
        config.contest = Some(value.to_owned());
    } else if key == "testcases" {
        config.testcases = value.to_owned();
    } else if key == "testcase_extension" {
        if let Some(extension) = TestCaseFileExtension::from_str(value) {
            config.testcase_extension = extension;
        } else {
            bail!(ConfigErrorKind::UnsupportedExtension(value.to_owned()));
        }
    } else if key == "default_lang" {
        config.default_lang = value.to_owned();
    } else {
        config.set_target_lang(key, value)?;
    }
    let config = serde_yaml::to_string(&config)?;
    File::create(find_base()?.1)?.write_all(config.as_bytes())?;
    Ok(())
}


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
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    targets: Vec<Target>,
    languages: Vec<Project>,
    #[serde(skip)]
    base_dir: PathBuf,
}

impl Config {
    pub fn load_from_file() -> ConfigResult<Self> {
        let (base, path) = find_base()?;
        let mut config =
            serde_yaml::from_str::<Self>(&util::string_from_read(File::open(&path)?)?)?;
        config.base_dir = base;
        Ok(config)
    }

    pub fn service(&self) -> ConfigResult<ServiceName> {
        match self.service.clone() {
            Some(service) => Ok(service),
            None => bail!(ConfigErrorKind::PropertyNotSet("service")),
        }
    }

    pub fn contest(&self) -> ConfigResult<String> {
        match self.contest.clone() {
            Some(contest) => Ok(contest),
            None => bail!(ConfigErrorKind::PropertyNotSet("contest")),
        }
    }

    pub fn testcase_extension(&self) -> TestCaseFileExtension {
        self.testcase_extension
    }

    pub fn testcase_dir(&self) -> ConfigResult<PathBuf> {
        Ok(resolve_path(&self.base_dir, &self.testcases)?)
    }

    pub fn testcase_path(&self, target_name: &str) -> ConfigResult<TestCaseFilePath> {
        Ok(TestCaseFilePath::new(
            &self.testcase_dir()?,
            target_name,
            self.testcase_extension,
        ))
    }

    pub fn src_path(&self, target_name: &str) -> ConfigResult<PathBuf> {
        let project = self.project(target_name)?;
        Ok(project.src(&self.base_dir, target_name)?)
    }

    pub fn atcoder_lang_id(&self, target_name: &str) -> ConfigResult<u32> {
        let project = self.project(target_name)?;
        project.atcoder_lang_id().ok_or_else(|| {
            ConfigError::from(ConfigErrorKind::PropertyNotSet("atcoder_lang_id"))
        })
    }

    pub fn construct_build_command(
        &self,
        target_name: &str,
    ) -> ConfigResult<Option<CommandParameters>> {
        let project = self.project(target_name)?;
        Ok(project.construct_build_command(&self.base_dir)?)
    }

    pub fn construct_run_command(&self, target_name: &str) -> ConfigResult<CommandParameters> {
        let project = self.project(target_name)?;
        Ok(project.construct_run_command(&self.base_dir, target_name)?)
    }

    fn project(&self, target_name: &str) -> ConfigResult<&Project> {
        fn search<'a>(
            targets: &'a [Target],
            projects: &'a [Project],
            target_name: &str,
        ) -> Option<&'a Project> {
            let project_name = try_opt!(
                targets
                    .iter()
                    .filter(|ref target| target.name == target_name)
                    .map(|ref target| target.lang.clone())
                    .next()
            );
            projects
                .iter()
                .filter(|ref project| project.name() == project_name)
                .next()
        }

        if let Some(project) = search(&self.targets, &self.languages, target_name) {
            Ok(project)
        } else {
            let default_lang = self.default_lang.clone();
            for project in &self.languages {
                if project.name() == default_lang {
                    return Ok(project);
                }
            }
            bail!(ConfigErrorKind::NoSuchLanguage(default_lang))
        }
    }

    fn set_target_lang(&mut self, target_name: &str, lang: &str) -> ConfigResult<()> {
        for mut target in self.targets.iter_mut() {
            if target.name == target_name {
                return Ok(target.lang = serde_yaml::from_str(lang)?);
            }
        }
        Ok(self.targets.push(Target::new(target_name, lang)))
    }
}


#[derive(Clone, Serialize, Deserialize)]
pub enum ServiceName {
    #[serde(rename = "atcoder")]
    AtCoder,
    #[serde(rename = "atcoder-beta")]
    AtCoderBeta,
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
        let mut pathbuf = PathBuf::from(base);
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
struct Target {
    name: String,
    lang: String,
}

impl Target {
    fn new(name: &str, lang: &str) -> Self {
        Self {
            name: name.to_owned(),
            lang: lang.to_owned(),
        }
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

    fn src(&self, base: &Path, filename: &str) -> io::Result<PathBuf> {
        let mut pathbuf = PathBuf::from(base);
        pathbuf.push(PathBuf::from(match *self {
            Project::Script(ScriptProject { ref src, .. }) => src,
            Project::Build(BuildProject { ref src, .. }) => src,
            Project::Java(JavaProject { ref src, .. }) => src,
        }));
        pathbuf.push(filename);
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
            let (command, args) = build.split();
            let working_dir = resolve_path(base, dir)?;
            Ok(CommandParameters::new(command, args, working_dir))
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
                                ref extension,
                                ref runtime,
                                ..
                            }) => {
                let mut srcpath = resolve_path(base, src)?;
                let working_dir = srcpath.clone();
                srcpath.push(target_name);
                srcpath.set_extension(extension);
                if let Some(ref runtime) = *runtime {
                    let (command, mut args) = runtime.split();
                    args.push(srcpath.to_string_lossy().to_string());
                    Ok(CommandParameters::new(command, args, working_dir))
                } else {
                    Ok(CommandParameters::new(
                        srcpath.to_string_lossy().to_string(),
                        vec![],
                        working_dir,
                    ))
                }
            }
            Project::Build(BuildProject {
                               ref bin,
                               capitalize,
                               ..
                           }) => {
                let mut binpath = resolve_path(base, bin)?;
                let working_dir = binpath.clone();
                if capitalize {
                    binpath.push(target_name.capitalize_first());
                } else {
                    binpath.push(target_name);
                }
                if cfg!(target_os = "windows") {
                    binpath.set_extension("exe");
                }
                Ok(CommandParameters::new(
                    binpath.to_string_lossy().to_string(),
                    vec![],
                    working_dir,
                ))
            }
            Project::Java(JavaProject { ref bin, .. }) => {
                Ok(CommandParameters::new(
                    "java".to_owned(),
                    vec![target_name.capitalize_first()],
                    resolve_path(base, &bin)?,
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
    #[serde(default)]
    capitalize: bool,
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
    #[serde(default = "default_java_extension")]
    extension: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    build: Option<ScalarOrVec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    atcoder_lang_id: Option<u32>,
}


#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum ScalarOrVec<T> {
    Scalar(T),
    Vec(Vec<T>),
}

impl<T: Clone + Default> ScalarOrVec<T> {
    fn split(&self) -> (T, Vec<T>) {
        match *self {
            ScalarOrVec::Scalar(ref x) => (x.clone(), vec![]),
            ScalarOrVec::Vec(ref xs) => {
                let mut it = xs.iter();
                (
                    it.next().map(|x| x.clone()).unwrap_or_default(),
                    it.map(|x| x.clone()).collect(),
                )
            }
        }
    }
}
