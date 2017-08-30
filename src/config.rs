use super::error::{ConfigErrorKind, ConfigResult};
use super::util::{self, CapitalizeFirst};
use serde_yaml;
use std::env;
use std::fs::{self, File};
use std::io;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};


#[derive(Serialize, Deserialize)]
pub struct Config {
    #[serde(skip_serializing_if = "Option::is_none")]
    service: Option<ServiceName>,
    #[serde(skip_serializing_if = "Option::is_none")]
    contest: Option<String>,
    #[serde(default = "default_testcases_path")]
    testcases: PathBuf,
    #[serde(default = "default_testcase_extension")]
    testcase_extension: String,
    targets: Vec<Target>,
    projects: Vec<Project>,
}

impl Config {
    pub fn load_from_file() -> ConfigResult<Self> {
        let mut pathbuf = snowchains_working_dir()?;
        pathbuf.push("snowchains.yml");
        Ok(serde_yaml::from_str(
            &util::string_from_read(File::open(pathbuf)?)?,
        )?)
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
        let mut pathbuf = snowchains_working_dir()?;
        pathbuf.push(&self.testcases);
        Ok(pathbuf)
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
        project.src(target_name)
    }

    pub fn construct_run_command(&self, target_name: &str) -> ConfigResult<CommandParameters> {
        let project = self.project(target_name)?;
        Ok(project.construct_run_command(target_name)?)
    }

    pub fn build_if_needed(&self, target_name: &str) -> ConfigResult<()> {
        let project = self.project(target_name)?;
        project.build_if_needed()
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
                    .map(|ref target| target.project.clone())
                    .next()
            );
            projects
                .iter()
                .filter(|ref project| project.name() == project_name)
                .next()
        }

        if let Some(project) = search(&self.targets, &self.projects, target_name) {
            Ok(project)
        } else {
            bail!(ConfigErrorKind::NoSuchTarget(target_name.to_owned()))
        }
    }
}


#[derive(Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ServiceName {
    AtCoder,
}


#[derive(Clone)]
pub struct CommandParameters {
    command: String,
    args: Vec<String>,
    working_dir: PathBuf,
}

impl CommandParameters {
    pub fn build_and_spawn(self) -> io::Result<Child> {
        Command::new(&self.command)
            .args(self.args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .current_dir(self.working_dir)
            .spawn()
    }

    fn new(command: String, args: Vec<String>, working_dir: PathBuf) -> Self {
        Self {
            command: command,
            args: args,
            working_dir: working_dir,
        }
    }
}


fn default_testcases_path() -> PathBuf {
    if cfg!(target_os = "windows") {
        PathBuf::from(".\\snowchains\\")
    } else {
        PathBuf::from("./snowchains/")
    }
}


fn default_testcase_extension() -> String {
    "yml".to_owned()
}


fn default_java_extension() -> String {
    "java".to_owned()
}


fn snowchains_working_dir() -> ConfigResult<PathBuf> {
    fn find_snowchain_yml<P: AsRef<Path>>(dir: P) -> ConfigResult<bool> {
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
        if find_snowchain_yml(&dir)? {
            return Ok(dir);
        } else if !dir.pop() {
            bail!(ConfigErrorKind::ConfigFileNotFound);
        }
    }
}


#[derive(Serialize, Deserialize)]
struct Target {
    name: String,
    project: String,
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

    fn src(&self, filename: &str) -> ConfigResult<PathBuf> {
        let mut pathbuf = snowchains_working_dir()?;
        pathbuf.push(match *self {
            Project::Script(ScriptProject { ref src, .. }) => PathBuf::from(src),
            Project::Build(BuildProject { ref src, .. }) => PathBuf::from(src),
            Project::Java(JavaProject { ref src, .. }) => PathBuf::from(src),
        });
        pathbuf.push(filename);
        pathbuf.set_extension(match *self {
            Project::Script(ScriptProject { ref extension, .. }) => extension,
            Project::Build(BuildProject { ref extension, .. }) => extension,
            Project::Java(JavaProject { ref extension, .. }) => extension,
        });
        Ok(pathbuf.canonicalize()?)
    }

    fn build_if_needed(&self) -> ConfigResult<()> {
        fn do_build(build: &ScalarOrVec<String>, dir: &PathBuf) -> ConfigResult<()> {
            let mut pathbuf = snowchains_working_dir()?;
            pathbuf.push(dir);
            let (command, args) = build.split();
            let status = Command::new(command)
                .args(args)
                .current_dir(pathbuf)
                .status()?;
            if status.success() {
                Ok(())
            } else {
                bail!(io::Error::from_raw_os_error(status.code().unwrap_or(1)));
            }
        }

        match *self {
            Project::Script(_) => Ok(()),
            Project::Build(BuildProject { ref src, ref build, .. }) => do_build(build, src),
            Project::Java(JavaProject { ref src, ref build, .. }) => do_build(build, src),
        }
    }

    fn construct_run_command(&self, target_name: &str) -> ConfigResult<CommandParameters> {
        fn resolve(path: &Path) -> ConfigResult<PathBuf> {
            if path.is_relative() {
                let mut pathbuf = snowchains_working_dir()?;
                pathbuf.push(path);
                Ok(pathbuf.canonicalize()?)
            } else {
                Ok(path.canonicalize()?)
            }
        }

        fn resolve_to_string(path: &Path) -> ConfigResult<String> {
            Ok(resolve(path)?.to_string_lossy().to_string())
        }

        match *self {
            Project::Script(ScriptProject {
                                ref src,
                                ref extension,
                                ref runtime,
                                ..
                            }) => {
                let mut pathbuf = src.clone();
                pathbuf.push(target_name);
                pathbuf.set_extension(extension);
                if let &Some(ref runtime) = runtime {
                    let (command, mut args) = runtime.split();
                    args.push(resolve_to_string(&pathbuf)?);
                    Ok(CommandParameters::new(
                        command,
                        args,
                        snowchains_working_dir()?,
                    ))
                } else {
                    Ok(CommandParameters::new(
                        resolve_to_string(&pathbuf)?,
                        vec![],
                        snowchains_working_dir()?,
                    ))
                }
            }
            Project::Build(BuildProject {
                               ref bin,
                               capitalize,
                               ..
                           }) => {
                let mut pathbuf = bin.clone();
                if capitalize {
                    pathbuf.push(target_name.capitalize_first());
                } else {
                    pathbuf.push(target_name);
                }
                if cfg!(target_os = "windows") {
                    pathbuf.set_extension("exe");
                }
                Ok(CommandParameters::new(
                    resolve_to_string(&pathbuf)?,
                    vec![],
                    snowchains_working_dir()?,
                ))
            }
            Project::Java(JavaProject { ref bin, .. }) => {
                Ok(CommandParameters::new(
                    "java".to_owned(),
                    vec![target_name.capitalize_first()],
                    resolve(&bin)?,
                ))
            }
        }
    }
}


#[derive(Serialize, Deserialize)]
struct ScriptProject {
    name: String,
    src: PathBuf,
    extension: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    runtime: Option<ScalarOrVec<String>>,
}


#[derive(Serialize, Deserialize)]
struct BuildProject {
    name: String,
    src: PathBuf,
    bin: PathBuf,
    extension: String,
    #[serde(default)]
    capitalize: bool,
    build: ScalarOrVec<String>,
}


#[derive(Serialize, Deserialize)]
struct JavaProject {
    name: String,
    src: PathBuf,
    bin: PathBuf,
    #[serde(default = "default_java_extension")]
    extension: String,
    build: ScalarOrVec<String>,
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
