use super::error::{ConfigErrorKind, ConfigResult};
use super::judge::CommandParameters;
use super::util::{self, CapitalizeFirst};
use serde_yaml;
use std::env;
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::{Path, PathBuf};


type InputPath = String;


pub fn create_template(lang: &str, dir: &str) -> ConfigResult<()> {
    let template = format!(
        "service: \"atcoder-beta\" # [\"atcoder\", \"atcoder-beta\"]\n\
         contest: \"agc001\"\n\
         testcases: \"./snowchains/\"\n\
         testcase_extension: \"yml\"\n\
         default_lang: \"{}\"\n\
         languages:\n  \
           -\n    \
             name: \"c\"\n    \
             type: \"build\"\n    \
             src: \"./c/\"\n    \
             bin: \"./c/build/\"\n    \
             extension: \"c\"\n    \
             build: \"ninja\"\n  \
           -\n    \
             name: \"c++\"\n    \
             type: \"build\"\n    \
             src: \"./cc/\"\n    \
             bin: \"./cc/build/\"\n    \
             extension: \"cc\"\n    \
             build: \"ninja\"\n  \
           -\n    \
             name: \"rust\"\n    \
             type: \"build\"\n    \
             src: \"./rust/src/bin/\"\n    \
             bin: \"./rust/target/release/\"\n    \
             extension: \"rs\"\n    \
             build: [\"cargo\", \"build\", \"--release\"]\n  \
           -\n    \
             name: \"java\"\n    \
             type: \"java\"\n    \
             src: \"./java/src/main/java/\"\n    \
             bin: \"./java/build/classes/java/main/\"\n    \
             build: [\"gradle\", \"--daemon\", \"build\"] # optional\n  \
           -\n    \
             name: \"python3\"\n    \
             type: \"script\"\n    \
             src: \"./python/\"\n    \
             extension: \"py\"\n    \
             runtime: \"python3\" # or shebang\n",
        lang
    );
    let mut path = PathBuf::from(dir);
    path.push("snowchains.yml");
    Ok(File::create(path)?.write_all(template.as_bytes())?)
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
        config.testcase_extension = value.to_owned();
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
    #[serde(default = "default_testcase_extension")]
    testcase_extension: String,
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

    pub fn testcase_extension(&self) -> &str {
        &self.testcase_extension
    }

    pub fn testcase_dir(&self) -> ConfigResult<PathBuf> {
        Ok(resolve_path(&self.base_dir, &self.testcases)?)
    }

    pub fn testcase_path(&self, target_name: &str) -> ConfigResult<PathBuf> {
        let mut pathbuf = self.testcase_dir()?;
        pathbuf.push(target_name);
        pathbuf.set_extension(&self.testcase_extension);
        Ok(pathbuf)
    }

    #[allow(dead_code)]
    pub fn src_path(&self, target_name: &str) -> ConfigResult<PathBuf> {
        let project = self.project(target_name)?;
        Ok(project.src(&self.base_dir, target_name)?)
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


fn default_testcase_extension() -> String {
    "yml".to_owned()
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
}


#[derive(Serialize, Deserialize)]
struct BuildProject {
    name: String,
    src: InputPath,
    bin: InputPath,
    extension: String,
    #[serde(default)]
    capitalize: bool,
    build: Option<ScalarOrVec<String>>,
}


#[derive(Serialize, Deserialize)]
struct JavaProject {
    name: String,
    src: InputPath,
    bin: InputPath,
    #[serde(default = "default_java_extension")]
    extension: String,
    build: Option<ScalarOrVec<String>>,
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
