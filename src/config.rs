use command::{CompilationCommand, JudgingCommand};
use errors::{CodeReplaceResult, ConfigError, ConfigErrorKind, ConfigResult, FileIoErrorKind,
             FileIoResult, FileIoResultExt};
use replacer::CodeReplacer;
use template::{PathTemplate, TemplateString};
use testsuite::{SuiteFileExtension, SuiteFilePaths};
use util;

use {rprompt, serde_yaml};
use regex::Regex;

use std::{cmp, env, fmt, fs};
use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap};
use std::io::{self, Write};
use std::iter::FromIterator;
use std::path::{Path, PathBuf};
use std::str::FromStr;

/// Creates `snowchains.yaml` in `directory`.
pub fn init(directory: PathBuf) -> ConfigResult<()> {
    const LANGS: [&str; 8] = [
        "c++", "rust", "haskell", "bash", "python3", "java", "scala", "c#"
    ];

    println!("Choose or input:");
    for (i, lang) in LANGS.iter().enumerate() {
        println!("{}) {}", i + 1, lang);
    }

    let ask = |prompt: &str| -> io::Result<Cow<'static, str>> {
        let input = rprompt::prompt_reply_stderr(prompt)?;
        if let Ok(i) = input.parse::<usize>() {
            if 0 < i && i <= 8 {
                return Ok(LANGS[i - 1].into());
            }
        }
        Ok(input.into())
    };

    let atcoder_default_lang = ask("Atcoder/Atcoder(Beta): ")?;
    let hackerrank_default_lang = ask("HackerRank: ")?;

    let config = format!(
        r#"---
service: "atcoderbeta"
contest: "chokudai_s001"
testsuites: "snowchains/$service/$contest/"
extension_on_downloading: "yaml"
extensions_on_judging: ["json", "toml", "yaml", "yml"]

atcoder:
  default_language: {atcoder_default_lang:?}
  variables:
    "cxx_flags": "-std=c++14 -O2 -Wall -Wextra"
    "rust_version": "1.15.1"

hackerrank:
  default_language: {hackerrank_default_lang:?}
  variables:
    "cxx_flags": "-std=c++14 -O2 -Wall -Wextra -lm"
    "rust_version": "1.21.0"

languages:
  - name: "c++"
    src: "cc/{{}}.cc"
    compile:
      bin: "cc/build/{{}}{exe}"
      command: "g++ $cxx_flags -o $bin $src"
      working_directory: "cc/"
    run:
      command: "$bin"
      working_directory: "cc/"
    language_ids:
      atcoder: 3003
  - name: "rust"
    src: "rs/src/bin/{{}}.rs"
    compile:
      bin: "rs/target/release/{{}}{exe}"
      command: "rustc +$rust_version -o $bin $src"
      working_directory: "rs/"
    run:
      command: "$bin"
      working_directory: "rs/"
    language_ids:
      atcoder: 3504
  - name: "haskell"
    src: "hs/src/{{C}}.hs"
    compile:
      bin: "hs/target/{{C}}{exe}"
      command: "stack ghc -- -O2 -o $bin $src"
      working_directory: "hs/"
    run:
      command: "$bin"
      working_directory: "hs/"
    language_ids:
      atcoder: 3014
  - name: "bash"
    src: "bash/{{}}.bash"
    run:
      command: "bash $src"
      working_directory: "bash/"
    language_ids:
      atcoder: 3001
  - name: "python3"
    src: "py/{{}}.py"
    run:
      command: "./venv/bin/python3 $src"
      working_directory: "py/"
    language_ids:
      atcoder: 3023
  - name: "java"
    src: "java/src/main/java/{{C}}.java"
    compile:
      bin: "java/build/classes/java/main/{{C}}.class"
      command: "javac -d ./build/classes/java/main/ $src"
      working_directory: "java/"
    run:
      command: "java -classpath ./build/classes/java/main/{{C}}"
      working_directory: "java/"
    replace:
      regex: "^\\s*public(\\s+final)?\\s+class\\s+([A-Z][a-zA-Z0-9_]*).*$"
      regex_group: 2
      local: "{{C}}"
      atcoder: "Main"
      once: true
    language_ids:
      atcoder: 3016
  - name: "scala"
    src: "scala/src/main/scala/{{C}}.scala"
    compile:
      bin: "scala/target/scala-2.12/classes/{{C}}.class"
      command: "scalac -optimise -d ./target/scala-2.12/classes/ $src"
      working_directory: "scala/"
    run:
      command: "scala -classpath ./target/scala-2.12/classes/ {{C}}"
      working_directory: "scala/"
    replace:
      regex: "^\\s*object\\s+([A-Z][a-zA-Z0-9_]*).*$"
      regex_group: 1
      local: "{{C}}"
      atcoder: "Main"
      once: true
    language_ids:
      atcoder: 3025
{csharp}
"#,
        atcoder_default_lang = atcoder_default_lang,
        hackerrank_default_lang = hackerrank_default_lang,
        exe = if cfg!(target_os = "windows") {
            ".exe"
        } else {
            ""
        },
        csharp = if cfg!(target_os = "windows") {
            r#"  - name: "c#"
    src: "cs/{C}/{C}.cs"
    compile:
      bin: "cs/{C}/bin/Release/{C}.exe"
      command: "csc /o+ /r:System.Numerics /out:$bin $src"
      working_directory: "cs/"
    run:
      command: "$bin"
      working_directory: "cs/"
    language_ids:
      atcoder: 3006"#
        } else {
            r#"  - name: "c#"
    src: "cs/{C}/{C}.cs"
    compile:
      bin: "cs/{C}/bin/Release/{C}.exe"
      command: "mcs -o+ -r:System.Numerics -out:$bin $src"
      working_directory: "cs/"
    run:
      command: "mono $bin"
      working_directory: "cs/"
    language_ids:
      atcoder: 3006"#
        }
    );

    let mut path = directory;
    path.push("snowchains.yaml");
    util::create_file_and_dirs(&path)?
        .write_all(config.as_bytes())
        .chain_err(|| FileIoErrorKind::Write(path.to_owned()))?;
    Ok(())
}

/// Changes <service> and <contest>.
pub fn switch(service: ServiceName, contest: &str) -> ConfigResult<()> {
    fn str_from_opt<T: fmt::Display>(x: &Option<T>) -> Cow<'static, str> {
        match *x {
            Some(ref x) => Cow::Owned(format!("{:?}", x.to_string())),
            None => Cow::Borrowed("None"),
        }
    }

    fn print_change(n: usize, prev: &str, new: &str) {
        print!("{}", prev);
        for _ in 0..n - prev.len() {
            print!(" ");
        }
        println!(" -> {:?}", new);
    }

    let (_, path) = find_base()?;
    let text = util::string_from_file_path(&path)?;
    println!("Loaded {}", path.display());
    let (replaced, prev_service, prev_contest) = {
        let mut replaced = "".to_owned();
        let (mut prev_service, mut prev_contest) = (None, None);
        for line in text.lines() {
            lazy_static! {
                static ref SERVICE: Regex = Regex::new(r"^service\s*:\s*(\S.*)$").unwrap();
                static ref CONTEST: Regex = Regex::new(r"^contest\s*:\s*(\S.*)$").unwrap();
            }
            if let Some(caps) = SERVICE.captures(line) {
                prev_service = Some(caps[1].to_owned());
                replaced += &format!("service: {:?}", service.to_string());
            } else if let Some(caps) = CONTEST.captures(line) {
                prev_contest = Some(caps[1].to_owned());
                replaced += &format!("contest: {:?}", contest);
            } else {
                replaced += line;
            }
            replaced.push('\n');
        }
        if prev_service.is_some() && prev_contest.is_some()
            && serde_yaml::from_str::<Config>(&replaced).is_ok()
        {
            let (prev_service, prev_contest) = (prev_service.unwrap(), prev_contest.unwrap());
            (replaced, Cow::Owned(prev_service), Cow::Owned(prev_contest))
        } else {
            let mut config = serde_yaml::from_str::<Config>(&text)?;
            let prev_service = str_from_opt(&config.service);
            let prev_contest = str_from_opt(&config.contest);
            config.service = Some(service);
            config.contest = Some(contest.to_owned());
            (serde_yaml::to_string(&config)?, prev_service, prev_contest)
        }
    };
    let n = cmp::max(prev_service.len(), prev_contest.len());
    print_change(n, &prev_service, &service.to_string());
    print_change(n, &prev_contest, contest);
    let mut file = util::create_file_and_dirs(&path)?;
    file.write_all(replaced.as_bytes())?;
    println!("Saved.");
    Ok(())
}

/// Config.
#[derive(Serialize, Deserialize)]
pub struct Config {
    service: Option<ServiceName>,
    contest: Option<String>,
    #[serde(default = "default_testsuites")]
    testsuites: TemplateString,
    #[serde(default)]
    extension_on_downloading: SuiteFileExtension,
    #[serde(default = "default_extensions")]
    extensions_on_judging: Vec<SuiteFileExtension>,
    #[serde(skip_serializing_if = "Option::is_none")]
    atcoder: Option<Service>,
    #[serde(skip_serializing_if = "Option::is_none")]
    hackerrank: Option<Service>,
    languages: Vec<Language>,
    #[serde(skip)]
    base_dir: PathBuf,
}

impl Config {
    /// Loads and deserializes from the nearest `snowchains.yaml`.
    pub fn load_from_file(
        service: Option<ServiceName>,
        contest: Option<String>,
    ) -> ConfigResult<Self> {
        let (base, path) = find_base()?;
        let mut config = serde_yaml::from_reader::<_, Self>(util::open_file(&path)?)?;
        config.base_dir = base;
        config.service = service.or(config.service);
        config.contest = contest.or(config.contest);
        println!("Loaded {}", path.display());
        Ok(config)
    }

    /// Gets `service`.
    pub fn service_name(&self) -> ConfigResult<ServiceName> {
        match self.service {
            Some(service) => Ok(service),
            None => bail!(ConfigErrorKind::PropertyNotSet("service")),
        }
    }

    /// Gets `contest`.
    pub fn contest_name(&self) -> ConfigResult<&str> {
        match self.contest {
            Some(ref contest) => Ok(contest),
            None => bail!(ConfigErrorKind::PropertyNotSet("contest")),
        }
    }

    /// Gets `extension_on_downloading`.
    pub fn get_extension_on_downloading(&self) -> SuiteFileExtension {
        self.extension_on_downloading
    }

    pub fn src_paths_on_atcoder(&self) -> ConfigResult<BTreeMap<u32, PathTemplate>> {
        let mut templates = BTreeMap::new();
        for lang in &self.languages {
            if let Some(lang_id) = lang.language_ids.atcoder {
                let mut vars = HashMap::new();
                if let Some(ref atcoder) = self.atcoder {
                    for (k, v) in &atcoder.variables {
                        vars.insert(k.as_str(), v.as_str());
                    }
                }
                let template = lang.src.embed_vars(&vars)?.with_base_dir(&self.base_dir);
                templates.insert(lang_id, template);
            }
        }
        Ok(templates)
    }

    /// Gets the absolute path of the test suite files directory
    pub fn suite_dir(&self) -> ConfigResult<PathBuf> {
        let service = self.service.map(|s| s.to_string()).unwrap_or_default();
        let contest = self.contest.clone().unwrap_or_default();
        let vars = vec![("service", service.as_str()), ("contest", contest.as_str())];
        let vars = HashMap::from_iter(vars);
        let dir = self.testsuites.resolve_as_path(&self.base_dir, "", &vars)?;
        Ok(dir)
    }

    pub fn suite_paths(&self, target: &str) -> ConfigResult<SuiteFilePaths> {
        let dir = self.suite_dir()?;
        Ok(SuiteFilePaths::new(
            &dir,
            target,
            &self.extensions_on_judging,
        ))
    }

    /// Returns the path of the source file.
    pub fn src_path(&self, target: &str, lang_name: Option<&str>) -> ConfigResult<PathBuf> {
        let (lang, vars) = self.lang_property(lang_name)?;
        Ok(lang.resolve_src(&self.base_dir, target, vars)?)
    }

    pub fn code_replacer(&self, lang_name: Option<&str>) -> ConfigResult<Option<CodeReplacer>> {
        let (lang, vars) = self.lang_property(lang_name)?;
        let replacer = match lang.replace.as_ref() {
            Some(replacer) => Some(replacer.build(vars)?),
            None => None,
        };
        Ok(replacer)
    }

    pub fn code_replacers_on_atcoder(&self) -> ConfigResult<BTreeMap<u32, CodeReplacer>> {
        let mut replacers = BTreeMap::new();
        for lang in &self.languages {
            if let Some(lang_id) = lang.language_ids.atcoder {
                if let Some(ref replacer_prop) = lang.replace {
                    let replacer =
                        replacer_prop.build(self.atcoder.as_ref().map(|a| &a.variables))?;
                    replacers.insert(lang_id, replacer);
                }
            }
        }
        Ok(replacers)
    }

    /// Returns the `lang_id` of `lang_name` or a default language
    pub fn atcoder_lang_id(&self, lang_name: Option<&str>) -> ConfigResult<u32> {
        let (lang, _) = self.lang_property(lang_name)?;
        lang.language_ids.atcoder.ok_or_else(|| {
            ConfigError::from(ConfigErrorKind::PropertyNotSet("language_ids.atcoder"))
        })
    }

    /// Constructs arguments of compilation command for given or default
    /// language.
    pub fn construct_compilation_command(
        &self,
        target: &str,
        lang_name: Option<&str>,
    ) -> ConfigResult<Option<CompilationCommand>> {
        let (lang, vars) = self.lang_property(lang_name)?;
        Ok(lang.construct_compilation_command(&self.base_dir, target, vars)?)
    }

    /// Constructs arguments of execution command for given or default language.
    pub fn construct_solver(
        &self,
        target: &str,
        lang_name: Option<&str>,
    ) -> ConfigResult<JudgingCommand> {
        let (lang, vars) = self.lang_property(lang_name)?;
        Ok(lang.construct_solver(&self.base_dir, target, vars)?)
    }

    fn lang_property(
        &self,
        lang_name: Option<&str>,
    ) -> ConfigResult<(&Language, Option<&HashMap<String, String>>)> {
        let service_prop = self.service.and_then(|service| match service {
            ServiceName::AtCoder | ServiceName::AtCoderBeta => self.atcoder.as_ref(),
            ServiceName::HackerRank => self.hackerrank.as_ref(),
        });
        let lang_name = lang_name
            .or_else(|| service_prop.map(|p| p.default_language.as_ref()))
            .ok_or_else::<ConfigError, _>(|| ConfigErrorKind::LanguageNotSpecified.into())?;
        let lang = self.languages
            .iter()
            .find(|lang| lang.name == lang_name)
            .ok_or_else(|| {
                ConfigError::from(ConfigErrorKind::NoSuchLanguage(lang_name.to_owned()))
            })?;
        let vars = service_prop.map(|s| &s.variables);
        Ok((lang, vars))
    }
}

/// Names of programming contest services.
#[derive(Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ServiceName {
    AtCoder,
    AtCoderBeta,
    HackerRank,
}

impl fmt::Display for ServiceName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ServiceName::AtCoder => write!(f, "atcoder"),
            ServiceName::AtCoderBeta => write!(f, "atcoderbeta"),
            ServiceName::HackerRank => write!(f, "hackerrank"),
        }
    }
}

impl FromStr for ServiceName {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, String> {
        match s.to_lowercase().as_str() {
            "atcoder" => Ok(ServiceName::AtCoder),
            "atcoderbeta" => Ok(ServiceName::AtCoderBeta),
            "hackerrank" => Ok(ServiceName::HackerRank),
            _ => Err(format!("Unsupported service name: {:?}", s)),
        }
    }
}

fn find_base() -> ConfigResult<(PathBuf, PathBuf)> {
    fn snowchain_yaml_exists(dir: &Path) -> io::Result<bool> {
        for entry in fs::read_dir(dir)? {
            let path = entry?.path();
            if path.is_file() && path.file_name().unwrap() == "snowchains.yaml" {
                return Ok(true);
            }
        }
        Ok(false)
    }

    let mut dir = env::current_dir()?;
    loop {
        if let Ok(true) = snowchain_yaml_exists(&dir) {
            let mut path = dir.clone();
            path.push("snowchains.yaml");
            return Ok((dir, path));
        } else if !dir.pop() {
            bail!(ConfigErrorKind::ConfigFileNotFound);
        }
    }
}

fn default_extensions() -> Vec<SuiteFileExtension> {
    use testsuite::SuiteFileExtension::{Json, Toml, Yaml, Yml};
    vec![Json, Toml, Yaml, Yml]
}

fn default_testsuites() -> TemplateString {
    TemplateString::new("snowchains/$service/$contest/")
}

#[derive(Serialize, Deserialize)]
struct Service {
    default_language: String,
    variables: HashMap<String, String>,
}

#[derive(Serialize, Deserialize)]
struct Language {
    name: String,
    src: TemplateString,
    #[serde(skip_serializing_if = "Option::is_none")]
    compile: Option<Compile>,
    run: Run,
    replace: Option<CodeReplacerProp>,
    #[serde(default, skip_serializing_if = "LanguageIds::is_empty")]
    language_ids: LanguageIds,
}

impl Language {
    fn resolve_src(
        &self,
        base: &Path,
        target: &str,
        extra_vars: Option<&HashMap<String, String>>,
    ) -> ConfigResult<PathBuf> {
        let mut vars = HashMap::new();
        if let Some(extra_vars) = extra_vars {
            for (k, v) in extra_vars.iter() {
                vars.insert(k.as_str(), v.as_str());
            }
        }
        let src = self.src.resolve_as_path(base, target, &vars)?;
        Ok(src)
    }

    fn construct_compilation_command(
        &self,
        base: &Path,
        target: &str,
        extra_variables: Option<&HashMap<String, String>>,
    ) -> ConfigResult<Option<CompilationCommand>> {
        match self.compile {
            None => Ok(None),
            Some(ref compile) => {
                let wd = compile.working_directory.resolve(base)?;
                let src = self.src.resolve_as_path(base, target, &HashMap::new())?;
                let bin = compile.bin.resolve_as_path(base, target, &HashMap::new())?;
                let (src_s, bin_s) = (src.display().to_string(), bin.display().to_string());
                let vars = {
                    let mut vars = HashMap::new();
                    vars.insert("src", src_s.as_str());
                    vars.insert("bin", bin_s.as_str());
                    if let Some(extra_variables) = extra_variables {
                        for (k, v) in extra_variables.iter() {
                            vars.insert(k, v);
                        }
                    }
                    vars
                };
                let cmd = compile.command.format(target, &vars)?;
                Ok(Some(CompilationCommand::new(cmd, wd, src, bin)))
            }
        }
    }

    fn construct_solver(
        &self,
        base: &Path,
        target: &str,
        extra_vars: Option<&HashMap<String, String>>,
    ) -> ConfigResult<JudgingCommand> {
        let wd = self.run.working_directory.resolve(base)?;
        let (src, bin) = self.resolve_src_and_bin(base, target, extra_vars)?;
        let src = src.display().to_string();
        let bin = bin.map(|p| p.display().to_string()).unwrap_or_default();
        let vars = {
            let mut vars = HashMap::new();
            vars.insert("src", src.as_str());
            vars.insert("bin", bin.as_str());
            if let Some(extra_vars) = extra_vars {
                for (k, v) in extra_vars.iter() {
                    vars.insert(k, v);
                }
            }
            vars
        };
        let cmd = self.run.command.format(target, &vars)?;
        Ok(JudgingCommand::new(cmd, wd))
    }

    fn resolve_src_and_bin(
        &self,
        base: &Path,
        target: &str,
        extra_vars: Option<&HashMap<String, String>>,
    ) -> ConfigResult<(PathBuf, Option<PathBuf>)> {
        let mut vars = HashMap::new();
        if let Some(extra_vars) = extra_vars {
            for (k, v) in extra_vars.iter() {
                vars.insert(k.as_str(), v.as_str());
            }
        }
        let src = self.src.resolve_as_path(base, target, &vars)?;
        let bin = match self.compile {
            Some(ref compile) => Some(compile.bin.resolve_as_path(base, target, &vars)?),
            None => None,
        };
        Ok((src, bin))
    }
}

#[derive(Serialize, Deserialize)]
struct Compile {
    bin: TemplateString,
    command: TemplateString,
    working_directory: InputPath,
}

#[derive(Serialize, Deserialize)]
struct Run {
    command: TemplateString,
    working_directory: InputPath,
}

impl Default for Run {
    fn default() -> Self {
        Self {
            command: TemplateString::new("$bin"),
            working_directory: Default::default(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct CodeReplacerProp {
    regex: String,
    regex_group: usize,
    local: TemplateString,
    #[serde(default = "atcoder_class_name")]
    atcoder: Cow<'static, str>,
    #[serde(default = "always_true")]
    once: bool,
}

impl CodeReplacerProp {
    fn build(
        &self,
        variables: Option<&HashMap<String, String>>,
    ) -> CodeReplaceResult<CodeReplacer> {
        CodeReplacer::new(
            &self.regex,
            self.regex_group,
            &self.local,
            variables,
            self.atcoder.clone(),
            self.once,
        )
    }
}

fn atcoder_class_name() -> Cow<'static, str> {
    "Main".into()
}

fn always_true() -> bool {
    true
}

#[derive(Default, Serialize, Deserialize)]
struct LanguageIds {
    #[serde(skip_serializing_if = "Option::is_none")]
    atcoder: Option<u32>,
}

impl LanguageIds {
    fn is_empty(&self) -> bool {
        self.atcoder.is_none()
    }
}

#[derive(Default, Serialize, Deserialize)]
struct InputPath(String);

impl InputPath {
    fn resolve(&self, base: &Path) -> FileIoResult<PathBuf> {
        util::expand_path(&self.0, base)
    }
}
