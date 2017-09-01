use super::error::{ConfigErrorKind, ConfigResult};
use super::util::{self, CapitalizeFirst};
use serde_yaml;
use std::env;
use std::fs::{self, File};
use std::io;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};


type InputPath = String;


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
    targets: Vec<Target>,
    projects: Vec<Project>,
    #[serde(skip)]
    base_dir: PathBuf,
}

impl Config {
    pub fn load_from_file() -> ConfigResult<Self> {
        fn find_base_dir() -> ConfigResult<PathBuf> {
            fn find_snowchain_yml<P: AsRef<Path>>(dir: P) -> io::Result<bool> {
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

        let mut pathbuf = find_base_dir()?;
        let base_dir = pathbuf.clone();
        pathbuf.push("snowchains.yml");
        let mut config =
            serde_yaml::from_str::<Self>(&util::string_from_read(File::open(&pathbuf)?)?)?;
        config.base_dir = base_dir;
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

    pub fn construct_run_command(&self, target_name: &str) -> ConfigResult<CommandParameters> {
        let project = self.project(target_name)?;
        Ok(project.construct_run_command(&self.base_dir, target_name)?)
    }

    pub fn build_if_needed(&self, target_name: &str) -> ConfigResult<()> {
        let project = self.project(target_name)?;
        Ok(project.build_if_needed(&self.base_dir)?)
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


fn resolve_path(base: &Path, path: &str) -> io::Result<PathBuf> {
    if path.chars().next() == Some('~') {
        let mut pathbuf = PathBuf::from(base);
        pathbuf.push(path.chars().skip(2).collect::<String>());
        return Ok(pathbuf.canonicalize()?);
    }
    let path = PathBuf::from(path);
    if path.is_absolute() {
        Ok(path.canonicalize()?)
    } else {
        let mut pathbuf = PathBuf::from(base);
        pathbuf.push(path);
        Ok(pathbuf.canonicalize()?)
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
        Ok(pathbuf.canonicalize()?)
    }

    fn build_if_needed(&self, base: &Path) -> io::Result<()> {
        fn do_build(base: &Path, build: &ScalarOrVec<String>, dir: &str) -> io::Result<()> {
            let (command, args) = build.split();
            let output = Command::new(command)
                .args(args)
                .current_dir(resolve_path(base, dir)?)
                .stdin(Stdio::inherit())
                .stdout(Stdio::inherit())
                .stderr(Stdio::inherit())
                .output()?;
            if output.status.success() {
                Ok(())
            } else {
                Err(io::Error::new(
                    io::ErrorKind::Other,
                    if let Some(code) = output.status.code() {
                        format!("Build failed with code {}", code)
                    } else {
                        format!("Build failed")
                    },
                ))
            }
        }

        match *self {
            Project::Build(BuildProject {
                               ref src,
                               build: Some(ref build),
                               ..
                           }) => do_build(base, build, src),
            Project::Java(JavaProject {
                              ref src,
                              build: Some(ref build),
                              ..
                          }) => do_build(base, build, src),
            _ => Ok(()),
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
