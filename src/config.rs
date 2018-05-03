use ServiceName;
use command::{CompilationCommand, JudgingCommand};
use errors::{ConfigError, ConfigErrorKind, ConfigResult, FileIoErrorKind, FileIoResult};
use replacer::CodeReplacer;
use template::{BaseDirNone, BaseDirSome, CommandTemplate, CompilationTemplate, JudgeTemplate,
               PathTemplate, StringTemplate};
use testsuite::{SuiteFileExtension, SuiteFilePaths};
use util;

use {rprompt, serde_yaml};
use regex::Regex;

use std::{cmp, fs, io, str};
use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};

static CONFIG_FILE_NAME: &str = "snowchains.yaml";

/// Creates `snowchains.yaml` in `directory`.
pub fn init(
    directory: &Path,
    atcoder_default_lang: Option<&'static str>,
    hackerrank_default_lang: Option<&'static str>,
) -> FileIoResult<()> {
    const LANGS: [&str; 8] = [
        "c++", "rust", "haskell", "bash", "python3", "java", "scala", "c#"
    ];

    let ask_lang = || -> io::Result<Cow<'static, str>> {
        for (i, lang) in LANGS.iter().enumerate() {
            println!("{}) {}", i + 1, lang);
        }
        let input = rprompt::prompt_reply_stderr("Choose or input: ")?;
        if let Ok(i) = input.parse::<usize>() {
            if 0 < i && i <= 8 {
                return Ok(LANGS[i - 1].into());
            }
        }
        Ok(format!("{:?}", input).into())
    };

    let asked_lang = match (atcoder_default_lang, hackerrank_default_lang) {
        (None, None) => Some(ask_lang()?),
        _ => None,
    };

    let atcoder_default_lang = atcoder_default_lang
        .map(Cow::from)
        .unwrap_or(asked_lang.clone().unwrap());
    let hackerrank_default_lang = hackerrank_default_lang
        .map(Cow::from)
        .unwrap_or(asked_lang.unwrap());

    let config = format!(
        r#"---
service: atcoderbeta
contest: arc001

shell: {shell}

testfiles:
  directory: snowchains/$service/$contest/
  extension_on_download: yaml
  excluded_extensions: []

atcoder:
  default_language: {atcoder_default_lang}
  variables:
    cxx_flags: -std=c++14 -O2 -Wall -Wextra
    rust_version: 1.15.1
    java_class: Main

hackerrank:
  default_language: {hackerrank_default_lang}
  variables:
    cxx_flags: -std=c++14 -O2 -Wall -Wextra -lm
    rust_version: 1.21.0
    java_class: Main

interactive:
  python3:
    src: py/{{kebab}}-tester.py
    run:
      command: python3 -- $src $*
      working_directory: py/
  haskell:
    src: hs/src/{{Pascal}}Tester.hs
    compile:
      bin: hs/target/{{Pascal}}Tester
      command: [stack, ghc, --, -O2, -o, $bin, $src]
      working_directory: hs/
    run:
      command: $bin $*
      working_directory: hs/

languages:
  c++:
    src: cc/{{kebab}}.cc
    compile:
      bin: cc/build/{{kebab}}{exe}
      command: g++ $cxx_flags -o $bin $src
      working_directory: cc/
    run:
      command: [$bin]
      working_directory: cc/
    language_ids:
      atcoder: 3003
  rust:
    src: rs/src/bin/{{kebab}}.rs
    compile:
      bin: rs/target/release/{{kebab}}{exe}
      command: [rustc, +$rust_version, -o, $bin, $src]
      working_directory: rs/
    run:
      command: [$bin]
      working_directory: rs/
    language_ids:
      atcoder: 3504
  haskell:
    src: hs/src/{{Pascal}}.hs
    compile:
      bin: hs/target/{{Pascal}}{exe}
      command: [stack, ghc, --, -O2, -o, $bin, $src]
      working_directory: hs/
    run:
      command: [$bin]
      working_directory: hs/
    language_ids:
      atcoder: 3014
  bash:
    src: bash/{{kebab}}.bash
    run:
      command: [bash, $src]
      working_directory: bash/
    language_ids:
      atcoder: 3001
  python3:
    src: py/{{kebab}}.py
    run:
      command: [./venv/bin/python3, $src]
      working_directory: py/
    language_ids:
      atcoder: 3023
  java:
    src: java/src/main/java/{{Pascal}}.java
    compile:
      bin: java/build/classes/java/main/{{Pascal}}.class
      command: [javac, -d, ./build/classes/java/main/, $src]
      working_directory: java/
    run:
      command: [java, -classpath, ./build/classes/java/main/, '{{Pascal}}']
      working_directory: java/
    replace:
      regex: /^\s*public(\s+final)?\s+class\s+([A-Z][a-zA-Z0-9_]*).*$/
      regex_group: 2
      local: '{{Pascal}}'
      submit: $java_class
      all_matched: false
    language_ids:
      atcoder: 3016
  scala:
    src: scala/src/main/scala/{{Pascal}}.scala
    compile:
      bin: scala/target/scala-2.12/classes/{{Pascal}}.class
      command: [scalac, -optimise, -d, ./target/scala-2.12/classes/, $src]
      working_directory: scala/
    run:
      command: [scala, -classpath, ./target/scala-2.12/classes/, '{{Pascal}}']
      working_directory: scala/
    replace:
      regex: /^\s*object\s+([A-Z][a-zA-Z0-9_]*).*$/
      regex_group: 1
      local: '{{Pascal}}'
      submit: $java_class
      all_matched: false
    language_ids:
      atcoder: 3025
{csharp}
"#,
        atcoder_default_lang = atcoder_default_lang,
        hackerrank_default_lang = hackerrank_default_lang,
        shell = if cfg!(windows) {
            r"['C:\Windows\cmd.exe', /C]"
        } else {
            "[/bin/sh, -c]"
        },
        exe = if cfg!(target_os = "windows") {
            ".exe"
        } else {
            ""
        },
        csharp = if cfg!(target_os = "windows") {
            r#"  c#:
    src: cs/{Pascal}/{Pascal}.cs
    compile:
      bin: cs/{Pascal}/bin/Release/{Pascal}.exe
      command: [csc, /o+, '/r:System.Numerics', '/out:$bin', $src]
      working_directory: cs/
    run:
      command: [$bin]
      working_directory: cs/
    language_ids:
      atcoder: 3006"#
        } else {
            r#"  c#:
    src: cs/{Pascal}/{Pascal}.cs
    compile:
      bin: cs/{Pascal}/bin/Release/{Pascal}.exe
      command: [mcs, -o+, '-r:System.Numerics', '-out:$bin', $src]
      working_directory: cs/
    run:
      command: [mono, $bin]
      working_directory: cs/
    language_ids:
      atcoder: 3006"#
        }
    );

    util::fs::write(&directory.join(CONFIG_FILE_NAME), config.as_bytes())
}

/// Changes <service> and <contest>.
pub fn switch(service: ServiceName, contest: &str, dir: &Path) -> FileIoResult<()> {
    fn print_change(n: usize, prev: &str, new: &str) {
        print!("{}", prev);
        for _ in 0..n - prev.len() {
            print!(" ");
        }
        println!(" -> {:?}", new);
    }

    let path = find_base(dir)?.join(CONFIG_FILE_NAME);
    let text = util::fs::string_from_path(&path)?;
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
            (replaced, prev_service, prev_contest)
        } else {
            let mut config = serde_yaml::from_str::<Config>(&text)?;
            let prev_service = config.service.to_string();
            let prev_contest = config.contest;
            config.service = service;
            config.contest = contest.to_owned();
            (serde_yaml::to_string(&config)?, prev_service, prev_contest)
        }
    };
    let n = cmp::max(prev_service.len(), prev_contest.len());
    print_change(n, &prev_service, &service.to_string());
    print_change(n, &prev_contest, contest);
    util::fs::write(&path, replaced.as_bytes())?;
    println!("Saved.");
    Ok(())
}

/// Config.
#[derive(Serialize, Deserialize)]
pub struct Config {
    #[serde(default)]
    service: ServiceName,
    contest: String,
    shell: Vec<StringTemplate>,
    testfiles: TestFiles,
    #[serde(skip_serializing_if = "Option::is_none")]
    atcoder: Option<Service>,
    #[serde(skip_serializing_if = "Option::is_none")]
    hackerrank: Option<Service>,
    interactive: HashMap<String, Language>,
    languages: HashMap<String, Language>,
    #[serde(skip)]
    base_dir: PathBuf,
}

impl Config {
    /// Loads and deserializes from the nearest `snowchains.yaml`.
    pub fn load_from_file(
        service: Option<ServiceName>,
        contest: Option<String>,
        dir: &Path,
    ) -> FileIoResult<Self> {
        let base = find_base(dir)?;
        let path = base.join(CONFIG_FILE_NAME);
        let mut config = serde_yaml::from_reader::<_, Self>(util::fs::open(&path)?)?;
        config.base_dir = base;
        config.service = service.unwrap_or(config.service);
        config.contest = contest.unwrap_or(config.contest);
        println!("Loaded {}", path.display());
        Ok(config)
    }

    /// Gets `service`.
    pub fn service(&self) -> ServiceName {
        self.service
    }

    /// Gets `contest`.
    pub fn contest(&self) -> &str {
        &self.contest
    }

    /// Gets `testfiles/directory` as a `PathTemplate<BaseDirSome>`.
    pub fn testfiles_dir(&self) -> PathTemplate<BaseDirSome> {
        self.testfiles
            .directory
            .base_dir(&self.base_dir)
            .embed_strings(
                &hashmap!("service" => self.service.as_str(), "contest" => &self.contest),
            )
    }

    pub fn suite_paths<'a>(&'a self, target: &'a str) -> SuiteFilePaths<'a> {
        let dir = self.testfiles_dir();
        let exts_on_judge = SuiteFileExtension::all()
            .filter(|e| !self.testfiles.excluded_extensions.contains(e))
            .collect::<Vec<_>>();
        SuiteFilePaths::new(dir, target, exts_on_judge)
    }

    /// Gets `testfiles/extension_on_download`.
    pub fn extension_on_download(&self) -> SuiteFileExtension {
        self.testfiles.extension_on_download
    }

    pub fn src_paths_on_atcoder(&self) -> BTreeMap<u32, PathTemplate<BaseDirSome>> {
        let mut templates = BTreeMap::new();
        for lang in self.languages.values() {
            if let Some(lang_id) = lang.language_ids.atcoder {
                let template = lang.src
                    .base_dir(&self.base_dir)
                    .embed_strings(self.atcoder.as_ref().map(|s| &s.variables));
                templates.insert(lang_id, template);
            }
        }
        templates
    }

    pub(crate) fn src_to_submit(
        &self,
        lang: Option<&str>,
    ) -> ConfigResult<PathTemplate<BaseDirSome>> {
        let lang = find_language(&self.languages, self.default_lang(), lang)?;
        let vars = self.vars_for_langs(None);
        Ok(lang.src.base_dir(&self.base_dir).embed_strings(&vars))
    }

    pub(crate) fn code_replacer(&self, lang: Option<&str>) -> ConfigResult<Option<CodeReplacer>> {
        let lang = find_language(&self.languages, self.default_lang(), lang)?;
        let vars = self.vars_for_langs(None);
        Ok(lang.replace.as_ref().map(|r| r.embed_strings(&vars)))
    }

    pub(crate) fn code_replacers_on_atcoder(&self) -> ConfigResult<BTreeMap<u32, CodeReplacer>> {
        let mut replacers = BTreeMap::new();
        for lang in self.languages.values() {
            if let Some(lang_id) = lang.language_ids.atcoder {
                if let Some(ref replacer) = lang.replace {
                    let vars = self.vars_for_langs(ServiceName::AtCoder);
                    let replacer = replacer.embed_strings(&vars);
                    replacers.insert(lang_id, replacer);
                }
            }
        }
        Ok(replacers)
    }

    /// Returns the `lang_id` of `lang` or a default language
    pub fn atcoder_lang_id(&self, lang: Option<&str>) -> ConfigResult<u32> {
        let lang = find_language(&self.languages, self.default_lang(), lang)?;
        lang.language_ids.atcoder.ok_or_else(|| {
            ConfigError::from(ConfigErrorKind::PropertyNotSet("language_ids.atcoder"))
        })
    }

    pub(crate) fn solver_compilation(
        &self,
        lang: Option<&str>,
    ) -> ConfigResult<Option<CompilationTemplate>> {
        self.compilation_command(find_language(&self.languages, self.default_lang(), lang)?)
    }

    pub(crate) fn interactive_tester_compilation(
        &self,
        lang: Option<&str>,
    ) -> ConfigResult<Option<CompilationTemplate>> {
        self.compilation_command(find_language(&self.interactive, None, lang)?)
    }

    pub(crate) fn solver(&self, lang: Option<&str>) -> ConfigResult<JudgeTemplate> {
        self.judge_command(find_language(&self.languages, self.default_lang(), lang)?)
    }

    pub(crate) fn interactive_tester(&self, lang: Option<&str>) -> ConfigResult<JudgeTemplate> {
        self.judge_command(find_language(&self.interactive, None, lang)?)
    }

    fn compilation_command(&self, lang: &Language) -> ConfigResult<Option<CompilationTemplate>> {
        match lang.compile {
            None => Ok(None),
            Some(ref compile) => {
                let wd = compile.working_directory.base_dir(&self.base_dir);
                let vars = self.vars_for_langs(None);
                let cmd = compile.command.embed_strings(&vars);
                let src = lang.src.base_dir(&self.base_dir).embed_strings(&vars);
                let bin = compile.bin.base_dir(&self.base_dir).embed_strings(&vars);
                Ok(Some(cmd.as_compilation(&self.shell, wd, src, bin)))
            }
        }
    }

    fn judge_command(&self, lang: &Language) -> ConfigResult<JudgeTemplate> {
        let wd = lang.run.working_directory.base_dir(&self.base_dir);
        let vars = self.vars_for_langs(None);
        let cmd = lang.run.command.embed_strings(&vars);
        let src = lang.src.base_dir(&self.base_dir).embed_strings(&vars);
        let bin = lang.compile
            .as_ref()
            .map(|c| c.bin.base_dir(&self.base_dir));
        Ok(cmd.as_judge(&self.shell, wd, &src, bin.as_ref()))
    }

    fn default_lang(&self) -> Option<&str> {
        match self.service {
            ServiceName::AtCoder | ServiceName::AtCoderBeta => self.atcoder.as_ref(),
            ServiceName::HackerRank => self.hackerrank.as_ref(),
            ServiceName::Other => None,
        }.map(|s| s.default_language.as_ref())
    }

    fn vars_for_langs<S: Into<Option<ServiceName>>>(&self, service: S) -> HashMap<&str, &str> {
        let vars_in_service = match service.into().unwrap_or(self.service) {
            ServiceName::AtCoder | ServiceName::AtCoderBeta => self.atcoder.as_ref(),
            ServiceName::HackerRank => self.hackerrank.as_ref(),
            ServiceName::Other => None,
        }.map(|s| &s.variables);
        let mut vars = hashmap!("service" => self.service.as_str(), "contest" => &self.contest);
        if let Some(vars_in_service) = vars_in_service {
            for (k, v) in vars_in_service {
                vars.insert(k, v);
            }
        }
        vars
    }
}

fn find_language<'a>(
    langs: &'a HashMap<String, Language>,
    default_lang: Option<&str>,
    name: Option<&str>,
) -> ConfigResult<&'a Language> {
    let name = name.or_else(|| default_lang)
        .ok_or_else(|| ConfigError::from(ConfigErrorKind::LanguageNotSpecified))?;
    langs
        .get(name)
        .ok_or_else(|| ConfigError::from(ConfigErrorKind::NoSuchLanguage(name.to_owned())))
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
struct TestFiles {
    #[serde(default = "default_testsuites")]
    directory: PathTemplate<BaseDirNone>,
    extension_on_download: SuiteFileExtension,
    #[serde(default)]
    excluded_extensions: Vec<SuiteFileExtension>,
}

fn default_testsuites() -> PathTemplate<BaseDirNone> {
    PathTemplate::from_static_str("snowchains/$service/$contest/")
}

#[derive(Serialize, Deserialize)]
struct Service {
    default_language: String,
    variables: HashMap<String, String>,
}

#[derive(Serialize, Deserialize)]
struct Language {
    src: PathTemplate<BaseDirNone>,
    #[serde(skip_serializing_if = "Option::is_none")]
    compile: Option<Compile>,
    run: Run,
    replace: Option<CodeReplacer>,
    #[serde(default, skip_serializing_if = "LanguageIds::is_empty")]
    language_ids: LanguageIds,
}

#[derive(Serialize, Deserialize)]
struct Compile {
    bin: PathTemplate<BaseDirNone>,
    command: CommandTemplate<CompilationCommand>,
    #[serde(default)]
    working_directory: PathTemplate<BaseDirNone>,
}

#[derive(Serialize, Deserialize)]
struct Run {
    command: CommandTemplate<JudgingCommand>,
    #[serde(default)]
    working_directory: PathTemplate<BaseDirNone>,
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
