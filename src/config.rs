use ServiceName;
use command::{CompilationCommand, JudgingCommand};
use errors::{ConfigError, ConfigErrorKind, ConfigResult, FileIoErrorKind, FileIoResult,
             FileIoResultExt};
use replacer::CodeReplacer;
use template::{PathTemplate, TemplateString};
use testsuite::{SuiteFileExtension, SuiteFilePaths};
use util;

use {rprompt, serde_yaml};
use regex::Regex;

use std::{cmp, fmt, fs, iter, slice, str};
use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap};
use std::io::{self, Write as _Write};
use std::path::{Path, PathBuf};

static CONFIG_FILE_NAME: &str = "snowchains.yaml";

/// Creates `snowchains.yaml` in `directory`.
pub fn init(
    directory: PathBuf,
    atcoder_default_lang: Option<&'static str>,
    hackerrank_default_lang: Option<&'static str>,
) -> ConfigResult<()> {
    const LANGS: [&str; 8] = [
        "c++", "rust", "haskell", "bash", "python3", "java", "scala", "c#"
    ];

    if atcoder_default_lang.is_none() && hackerrank_default_lang.is_none() {
        println!("Choose or input:");
        for (i, lang) in LANGS.iter().enumerate() {
            println!("{}) {}", i + 1, lang);
        }
    }

    let ask = |prompt: &str| -> io::Result<Cow<'static, str>> {
        let input = rprompt::prompt_reply_stderr(prompt)?;
        if let Ok(i) = input.parse::<usize>() {
            if 0 < i && i <= 8 {
                return Ok(LANGS[i - 1].into());
            }
        }
        if input.is_empty() || ":->|{[ ".chars().any(|c| input.starts_with(c)) {
            Ok(format!("{:?}", input).into())
        } else {
            Ok(input.into())
        }
    };

    let atcoder_default_lang = match atcoder_default_lang {
        Some(atcoder_default_lang) => atcoder_default_lang.into(),
        None => ask("Atcoder/Atcoder(Beta): ")?,
    };
    let hackerrank_default_lang = match hackerrank_default_lang {
        Some(hackerrank_default_lang) => hackerrank_default_lang.into(),
        None => ask("HackerRank: ")?,
    };

    let config = format!(
        r#"---
service: atcoderbeta
contest: chokudai_s001

testfiles:
  directory: snowchains/$service/$contest/
  download: yaml
  exclude: []

{shell}

atcoder:
  default_language: {atcoder_default_lang}
  variables:
    cxx_flags: -std=c++14 -O2 -Wall -Wextra
    rust_version: 1.15.1

hackerrank:
  default_language: {hackerrank_default_lang}
  variables:
    cxx_flags: -std=c++14 -O2 -Wall -Wextra -lm
    rust_version: 1.21.0

languages:
  - name: c++
    src: cc/{{}}.cc
    compile:
      bin: cc/build/{{}}{exe}
      command: g++ $cxx_flags -o $bin $src
      working_directory: cc/
    run:
      command: $bin
      working_directory: cc/
    language_ids:
      atcoder: 3003
  - name: rust
    src: rs/src/bin/{{}}.rs
    compile:
      bin: rs/target/release/{{}}{exe}
      command: rustc +$rust_version -o $bin $src
      working_directory: rs/
    run:
      command: $bin
      working_directory: rs/
    language_ids:
      atcoder: 3504
  - name: haskell
    src: hs/src/{{C}}.hs
    compile:
      bin: hs/target/{{C}}{exe}
      command: stack ghc -- -O2 -o $bin $src
      working_directory: hs/
    run:
      command: $bin
      working_directory: hs/
    language_ids:
      atcoder: 3014
  - name: bash
    src: bash/{{}}.bash
    run:
      command: bash $src
      working_directory: bash/
    language_ids:
      atcoder: 3001
  - name: python3
    src: py/{{}}.py
    run:
      command: ./venv/bin/python3 $src
      working_directory: py/
    language_ids:
      atcoder: 3023
  - name: java
    src: java/src/main/java/{{C}}.java
    compile:
      bin: java/build/classes/java/main/{{C}}.class
      command: javac -d ./build/classes/java/main/ $src
      working_directory: java/
    run:
      command: java -classpath ./build/classes/java/main/ {{C}}
      working_directory: java/
    replace:
      regex: /^\s*public(\s+final)?\s+class\s+([A-Z][a-zA-Z0-9_]*).*$/
      regex_group: 2
      local: "{{C}}"
      atcoder: Main
      once: true
    language_ids:
      atcoder: 3016
  - name: scala
    src: scala/src/main/scala/{{C}}.scala
    compile:
      bin: scala/target/scala-2.12/classes/{{C}}.class
      command: scalac -optimise -d ./target/scala-2.12/classes/ $src
      working_directory: scala/
    run:
      command: scala -classpath ./target/scala-2.12/classes/ {{C}}
      working_directory: scala/
    replace:
      regex: /^\s*object\s+([A-Z][a-zA-Z0-9_]*).*$/
      regex_group: 1
      local: "{{C}}"
      atcoder: Main
      once: true
    language_ids:
      atcoder: 3025
{csharp}
"#,
        atcoder_default_lang = atcoder_default_lang,
        hackerrank_default_lang = hackerrank_default_lang,
        shell = if cfg!(windows) {
            r#"shell:
  args: [C:\Windows\cmd.exe, /C]
  on: '%@#$^&*;|?<>()[]{}''"'"#
        } else {
            r#"shell:
  args: [/bin/sh, -c]
  on: '\@#$^&*;|?<>()[]{}''"'"#
        },
        exe = if cfg!(target_os = "windows") {
            ".exe"
        } else {
            ""
        },
        csharp = if cfg!(target_os = "windows") {
            r#"  - name: c#
    src: cs/{C}/{C}.cs
    compile:
      bin: cs/{C}/bin/Release/{C}.exe
      command: csc /o+ /r:System.Numerics /out:$bin $src
      working_directory: cs/
    run:
      command: $bin
      working_directory: cs/
    language_ids:
      atcoder: 3006"#
        } else {
            r#"  - name: c#
    src: cs/{C}/{C}.cs
    compile:
      bin: cs/{C}/bin/Release/{C}.exe
      command: mcs -o+ -r:System.Numerics -out:$bin $src
      working_directory: cs/
    run:
      command: mono $bin
      working_directory: cs/
    language_ids:
      atcoder: 3006"#
        }
    );

    let mut path = directory;
    path.push(CONFIG_FILE_NAME);
    util::create_file_and_dirs(&path)?
        .write_all(config.as_bytes())
        .chain_err(|| FileIoErrorKind::Write(path.to_owned()))?;
    Ok(())
}

/// Changes <service> and <contest>.
pub fn switch(service: ServiceName, contest: &str, dir: &Path) -> ConfigResult<()> {
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

    let path = find_base(dir)?.join(CONFIG_FILE_NAME);
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
    testfiles: TestFiles,
    shell: Shell,
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
        dir: &Path,
    ) -> ConfigResult<Self> {
        let base = find_base(dir)?;
        let path = base.join(CONFIG_FILE_NAME);
        let mut config = serde_yaml::from_reader::<_, Self>(util::open_file(&path)?)?;
        config.base_dir = base;
        config.service = service.or(config.service);
        config.contest = contest.or(config.contest);
        println!("Loaded {}", path.display());
        Ok(config)
    }

    /// Gets `service`.
    pub fn service(&self) -> ConfigResult<ServiceName> {
        match self.service {
            Some(service) => Ok(service),
            None => bail!(ConfigErrorKind::PropertyNotSet("service")),
        }
    }

    /// Gets `contest`.
    pub fn contest(&self) -> ConfigResult<&str> {
        match self.contest {
            Some(ref contest) => Ok(contest),
            None => bail!(ConfigErrorKind::PropertyNotSet("contest")),
        }
    }

    /// Gets `testfiles/directory` as path.
    pub fn testfiles_dir(&self) -> ConfigResult<PathTemplate> {
        let mut vars = hashmap!();
        if let Some(service) = self.service {
            vars.insert("service", service.as_str());
        }
        if let Some(ref contest) = self.contest {
            vars.insert("contest", &contest);
        }
        self.testfiles
            .directory
            .as_path_template(&self.base_dir, &vars)
            .map_err(Into::into)
    }

    pub fn suite_paths<'a>(&'a self, target: &'a str) -> ConfigResult<SuiteFilePaths<'a>> {
        let dir = self.testfiles_dir()?;
        let exts_on_judge = SuiteFileExtension::all()
            .filter(|e| !self.testfiles.exclude.contains(e))
            .collect::<Vec<_>>();
        Ok(SuiteFilePaths::new(dir, target, exts_on_judge))
    }

    /// Gets `testfiles/download`.
    pub fn extension_on_downloading(&self) -> SuiteFileExtension {
        self.testfiles.download
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

    pub(crate) fn src_to_submit(&self, language: Option<&str>) -> ConfigResult<PathTemplate> {
        let lang = self.language(language)?;
        let vars = self.vars_on_service(None);
        lang.src
            .as_path_template(&self.base_dir, &vars)
            .map_err(Into::into)
    }

    pub fn code_replacer(&self, lang_name: Option<&str>) -> ConfigResult<Option<CodeReplacer>> {
        let lang = self.language(lang_name)?;
        let vars = self.vars_on_service(None);
        let replacer = match lang.replace.as_ref() {
            Some(replacer) => Some(replacer.build(&vars)?),
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
                        replacer_prop.build(&self.vars_on_service(Some(ServiceName::AtCoder)))?;
                    replacers.insert(lang_id, replacer);
                }
            }
        }
        Ok(replacers)
    }

    /// Returns the `lang_id` of `lang_name` or a default language
    pub fn atcoder_lang_id(&self, lang_name: Option<&str>) -> ConfigResult<u32> {
        let lang = self.language(lang_name)?;
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
        let lang = self.language(lang_name)?;
        let vars_on_service = self.vars_on_service(None);
        match lang.compile {
            None => Ok(None),
            Some(ref compile) => {
                let wd = compile.working_directory.resolve(&self.base_dir)?;
                let src = lang.src
                    .resolve_as_path(&self.base_dir, target, &HashMap::new())?;
                let bin = compile
                    .bin
                    .resolve_as_path(&self.base_dir, target, &HashMap::new())?;
                let (src_s, bin_s) = (src.display().to_string(), bin.display().to_string());
                let vars = {
                    let mut vars = hashmap!("src" => src_s.as_str(), "bin" => bin_s.as_str());
                    for (k, v) in &vars_on_service {
                        vars.insert(k, v);
                    }
                    vars
                };
                let cmd = compile.command.format(target, &vars)?;
                let shell = &self.shell;
                Ok(Some(CompilationCommand::new(cmd, wd, shell, src, bin)))
            }
        }
    }

    /// Constructs arguments of execution command for given or default language.
    pub fn construct_solver(
        &self,
        target: &str,
        lang_name: Option<&str>,
    ) -> ConfigResult<JudgingCommand> {
        let lang = self.language(lang_name)?;
        let vars_on_service = self.vars_on_service(None);
        let base = &self.base_dir;
        let wd = lang.run.working_directory.resolve(base)?;
        let (src, bin) = {
            let mut vars = HashMap::new();
            for (&k, &v) in &vars_on_service {
                vars.insert(k, v);
            }
            let src = lang.src.resolve_as_path(base, target, &vars)?;
            let bin = match lang.compile {
                Some(ref compile) => Some(compile.bin.resolve_as_path(base, target, &vars)?),
                None => None,
            };
            (src, bin)
        };
        let src = src.display().to_string();
        let bin = bin.map(|p| p.display().to_string()).unwrap_or_default();
        let vars = {
            let mut vars = hashmap!("src" => src.as_str(), "bin" => bin.as_str());
            for (&k, &v) in &vars_on_service {
                vars.insert(k, v);
            }
            vars
        };
        let cmd = lang.run.command.format(target, &vars)?;
        Ok(JudgingCommand::new(cmd, wd, &self.shell))
    }

    fn language(&self, name: Option<&str>) -> ConfigResult<&Language> {
        let service = self.service.and_then(|service| match service {
            ServiceName::AtCoder | ServiceName::AtCoderBeta => self.atcoder.as_ref(),
            ServiceName::HackerRank => self.hackerrank.as_ref(),
        });
        let name = name.or_else(|| service.map(|s| s.default_language.as_ref()))
            .ok_or_else(|| ConfigError::from(ConfigErrorKind::LanguageNotSpecified))?;
        self.languages
            .iter()
            .find(|lang| lang.name == name)
            .ok_or_else(|| ConfigError::from(ConfigErrorKind::NoSuchLanguage(name.to_owned())))
    }

    fn vars_on_service(&self, service: Option<ServiceName>) -> HashMap<&str, &str> {
        let service = service.or(self.service).and_then(|service| match service {
            ServiceName::AtCoder | ServiceName::AtCoderBeta => self.atcoder.as_ref(),
            ServiceName::HackerRank => self.hackerrank.as_ref(),
        });
        let mut vars = HashMap::<&str, &str>::new();
        let vs = service.map(|s| &s.variables);
        if let Some(vs) = vs {
            for (k, v) in vs.iter() {
                vars.insert(k, v);
            }
        }
        vars
    }
}

fn find_base(start: &Path) -> FileIoResult<PathBuf> {
    fn target_exists(dir: &Path) -> io::Result<bool> {
        for entry in fs::read_dir(dir)? {
            let path = entry?.path();
            if path.is_file() && path.file_name().unwrap() == CONFIG_FILE_NAME {
                return Ok(true);
            }
        }
        Ok(false)
    }

    let mut dir = PathBuf::from(start);
    loop {
        if let Ok(true) = target_exists(&dir) {
            return Ok(dir);
        } else if !dir.pop() {
            bail!(FileIoErrorKind::Search(CONFIG_FILE_NAME, start.to_owned()));
        }
    }
}

#[derive(Serialize, Deserialize)]
struct Service {
    default_language: String,
    variables: HashMap<String, String>,
}

#[derive(Serialize, Deserialize)]
struct TestFiles {
    #[serde(default = "default_testsuites")]
    directory: TemplateString,
    download: SuiteFileExtension,
    #[serde(default)]
    exclude: Vec<SuiteFileExtension>,
}

fn default_testsuites() -> TemplateString {
    TemplateString::new("snowchains/$service/$contest/")
}

#[derive(Clone, Serialize, Deserialize)]
pub(crate) struct Shell {
    args: Vec<String>,
    on: String,
}

impl Shell {
    #[cfg(test)]
    pub(crate) fn new(args: &[&str], on: &str) -> Self {
        Self {
            args: args.iter().cloned().map(str::to_owned).collect(),
            on: on.to_owned(),
        }
    }

    pub(crate) fn values(
        &self,
    ) -> (
        &str,
        iter::Map<slice::Iter<String>, for<'r> fn(&'r String) -> &'r str>,
        &str,
    ) {
        let arg0 = self.args.get(0).map(String::as_str).unwrap_or("");
        (arg0, self.args[1..].iter().map(String::as_str), &self.on)
    }
}

#[derive(Serialize, Deserialize)]
struct Language {
    name: String,
    src: TemplateString,
    #[serde(skip_serializing_if = "Option::is_none")]
    compile: Option<Compile>,
    #[serde(default)]
    run: Run,
    replace: Option<CodeReplacerProp>,
    #[serde(default, skip_serializing_if = "LanguageIds::is_empty")]
    language_ids: LanguageIds,
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
    #[serde(default = "default_class_name_on_atcoder")]
    atcoder: Cow<'static, str>,
    #[serde(default = "always_true")]
    once: bool,
}

fn default_class_name_on_atcoder() -> Cow<'static, str> {
    "Main".into()
}

fn always_true() -> bool {
    true
}

impl CodeReplacerProp {
    fn build(&self, variables: &HashMap<&str, &str>) -> ConfigResult<CodeReplacer> {
        let regex = if self.regex.starts_with('/') && self.regex.ends_with('/') {
            let n = self.regex.len();
            String::from_utf8_lossy(&self.regex.as_bytes()[1..n - 1])
        } else {
            self.regex.as_str().into()
        };
        let regex = Regex::new(&regex)?;
        let local = self.local.embed_vars(variables)?;
        Ok(CodeReplacer::new(
            regex,
            self.regex_group,
            local,
            self.atcoder.clone(),
            self.once,
        ))
    }
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
