use command::{CompilationCommand, JudgingCommand};
use errors::{ConfigError, ConfigErrorKind, ConfigResult, FileIoErrorKind, FileIoResult};
use replacer::CodeReplacer;
use service::SessionConfig;
use template::{
    BaseDirNone, BaseDirSome, CommandTemplate, CompilationTemplate, JudgeTemplate, PathTemplate,
    StringTemplate,
};
use terminal::{self, TerminalMode};
use testsuite::{SerializableExtension, SuiteFileExtension, SuiteFilePathsTemplate, ZipConfig};
use util;
use ServiceName;

use regex::Regex;
use serde_yaml;

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::Write as _Write;
use std::path::{Path, PathBuf};
use std::str;
use std::time::Duration;

static CONFIG_FILE_NAME: &str = "snowchains.yaml";

/// Creates `snowchains.yaml` in `directory`.
pub(crate) fn init(
    directory: &Path,
    terminal: TerminalMode,
    session_cookies: &str,
) -> FileIoResult<()> {
    let config = format!(
        r#"---
service: atcoder
contest: arc001
language: c++

terminal: {terminal}

session:
  timeout: 10
  cookies: {session_cookies}

shell: {shell}

testfiles:
  directory: snowchains/$service/$contest/
  forall: [json, toml, yaml, yml, zip]
  scrape: yaml
  zip:
    timelimit: 2000
    match: {default_match}
    entries:
      - in:
          entry: /\Ain/([a-z0-9_\-]+)\.txt\z/
          match_group: 1
        out:
          entry: /\Aout/([a-z0-9_\-]+)\.txt\z/
          match_group: 1
        sort: [dictionary]
      - in:
          entry: /\Ainput/input([0-9]+)\.txt\z/
          match_group: 1
        out:
          entry: /\Aoutput/output([0-9]+)\.txt\z/
          match_group: 1
        sort: [number]
      - in:
          entry: /\Atest_in/([a-z0-9_]+)\.txt\z/
          match_group: 1
        out:
          entry: /\Atest_out/([a-z0-9_]+)\.txt\z/
          match_group: 1
        sort: [dictionary, number]

services:
  atcoder:
    # language: c++
    variables:
      cxx_flags: -std=c++14 -O2 -Wall -Wextra
      rust_version: 1.15.1
      java_class: Main
  hackerrank:
    # language: c++
    variables:
      cxx_flags: -std=c++14 -O2 -Wall -Wextra -lm
      rust_version: 1.21.0
      java_class: Main
  other:
    variables:
      cxx_flags: -std=c++14 -O2 -Wall -Wextra
      rust_version: stable

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
      match_group: 2
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
      match_group: 1
      local: '{{Pascal}}'
      submit: $java_class
      all_matched: false
    language_ids:
      atcoder: 3025
{csharp}
"#,
        terminal = terminal,
        session_cookies = util::yaml::repr_string(session_cookies),
        shell = if cfg!(windows) {
            r"['C:\Windows\cmd.exe', /C]"
        } else {
            "[/bin/sh, -c]"
        },
        default_match = if cfg!(windows) { "lines" } else { "exact" },
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
    let path = directory.join(CONFIG_FILE_NAME);
    util::fs::write(&path, config.as_bytes())?;
    println!("Wrote to {}", util::path::remove_dots(&path).display());
    Ok(())
}

/// Changes <service> and <contest>.
pub(crate) fn switch(
    directory: &Path,
    service: ServiceName,
    contest: &str,
    language: Option<&str>,
) -> FileIoResult<()> {
    fn print_change(len: usize, prev: &str, new: &str) {
        print!("{}", prev);
        (0..len - prev.len()).for_each(|_| print!(" "));
        println!(" -> {:?}", new);
    }

    let path = find_base(directory)?.join(CONFIG_FILE_NAME);
    let text = util::fs::read_to_string(&path)?;
    println!("Loaded {}", path.display());
    let (replaced, prev_service, prev_contest, prev_lang) = {
        let mut replaced = "".to_owned();
        let (mut prev_service, mut prev_contest, mut prev_lang) = (None, None, None);
        for line in text.lines() {
            lazy_static! {
                static ref SERVICE: Regex = Regex::new(r"^service\s*:\s*(\S.*)$").unwrap();
                static ref CONTEST: Regex = Regex::new(r"^contest\s*:\s*(\S.*)$").unwrap();
                static ref LANG: Regex = Regex::new(r"^language\s*:\s*(\S.*)$").unwrap();
            }
            if let Some(caps) = SERVICE.captures(line) {
                prev_service = Some(format!("{:?}", &caps[1]));
                write!(replaced, "service: {}", service).unwrap();
            } else if let Some(caps) = CONTEST.captures(line) {
                prev_contest = Some(format!("{:?}", &caps[1]));
                write!(replaced, "contest: {}", util::yaml::repr_string(contest)).unwrap();
            } else if let (Some(language), Some(caps)) = (language, LANG.captures(line)) {
                prev_lang = Some(format!("{:?}", &caps[1]));
                write!(replaced, "language: {}", util::yaml::repr_string(language)).unwrap();
            } else {
                replaced += line;
            }
            replaced.push('\n');
        }
        if prev_service.is_some() && prev_contest.is_some()
            && (prev_contest.is_some() == language.is_some())
            && serde_yaml::from_str::<Config>(&replaced).is_ok()
        {
            let (prev_service, prev_contest) = (prev_service.unwrap(), prev_contest.unwrap());
            (replaced, prev_service, prev_contest, prev_lang)
        } else {
            let mut config = serde_yaml::from_str::<Config>(&text)?;
            let prev_service = format!("{:?}", config.service.to_string());
            let prev_contest = format!("{:?}", config.contest);
            config.service = service;
            config.contest = contest.to_owned();
            let prev_lang;
            if let Some(language) = language {
                prev_lang = Some(config.language);
                config.language = format!("{:?}", language);
            } else {
                prev_lang = None;
            }
            (
                serde_yaml::to_string(&config)?,
                prev_service,
                prev_contest,
                prev_lang,
            )
        }
    };
    let len = {
        let l1 = prev_service.len();
        let l2 = prev_contest.len();
        let l3 = prev_lang.as_ref().map(|s| s.len()).unwrap_or(0);
        *[l1, l2, l3].iter().max().unwrap()
    };
    print_change(len, &prev_service, service.as_str());
    print_change(len, &prev_contest, contest);
    if let (Some(prev_lang), Some(language)) = (prev_lang, language) {
        print_change(len, &prev_lang, language);
    }
    util::fs::write(&path, replaced.as_bytes())?;
    println!("Saved.");
    Ok(())
}

/// Config.
#[derive(Serialize, Deserialize)]
pub(crate) struct Config {
    #[serde(default)]
    service: ServiceName,
    contest: String,
    language: String,
    terminal: TerminalMode,
    session: SessionConfig,
    shell: Vec<StringTemplate>,
    testfiles: TestFiles,
    #[serde(default)]
    services: BTreeMap<ServiceName, ServiceConfig>,
    #[serde(default)]
    interactive: HashMap<String, Language>,
    languages: HashMap<String, Language>,
    #[serde(skip)]
    base_dir: PathBuf,
}

impl Config {
    /// Loads and deserializes from the nearest `snowchains.yaml`.
    pub fn load_setting_term_mode(
        service: impl Into<Option<ServiceName>>,
        contest: impl Into<Option<String>>,
        dir: &Path,
    ) -> FileIoResult<Self> {
        let base = find_base(dir)?;
        let path = base.join(CONFIG_FILE_NAME);
        let mut config = serde_yaml::from_reader::<_, Self>(util::fs::open(&path)?)?;
        config.base_dir = base;
        config.service = service.into().unwrap_or(config.service);
        config.contest = contest.into().unwrap_or(config.contest);
        terminal::terminal_mode(config.terminal);
        println!(
            "Loaded {} (terminal mode: {})",
            path.display(),
            config.terminal,
        );
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

    /// Gets `session.timeout`.
    pub fn session_timeout(&self) -> Option<Duration> {
        self.session.timeout()
    }

    /// Gets `session.cookies` embedding "service" and "base_dir".
    pub fn session_cookies(&self) -> PathTemplate<BaseDirSome> {
        self.session.cookies(&self.base_dir, self.service)
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

    pub fn suite_paths(&self) -> SuiteFilePathsTemplate {
        let dir = self.testfiles_dir();
        let exts = &self.testfiles.forall;
        let zip = &self.testfiles.zip;
        SuiteFilePathsTemplate::new(dir, exts, zip)
    }

    /// Gets `testfiles.scrape`.
    pub fn extension_on_scrape(&self) -> SerializableExtension {
        self.testfiles.scrape
    }

    pub fn src_paths(&self) -> BTreeMap<u32, PathTemplate<BaseDirSome>> {
        let vars = self.vars_for_langs(None);
        let mut templates = BTreeMap::new();
        for lang in self.languages.values() {
            if let Some(lang_id) = lang.language_ids.atcoder {
                let template = lang.src.base_dir(&self.base_dir).embed_strings(&vars);
                templates.insert(lang_id, template);
            }
        }
        templates
    }

    pub fn src_to_submit(&self, lang: Option<&str>) -> ConfigResult<PathTemplate<BaseDirSome>> {
        let lang = find_language(&self.languages, self.lang_name(lang))?;
        let vars = self.vars_for_langs(None);
        Ok(lang.src.base_dir(&self.base_dir).embed_strings(&vars))
    }

    pub fn code_replacer(&self, lang: Option<&str>) -> ConfigResult<Option<CodeReplacer>> {
        let lang = find_language(&self.languages, self.lang_name(lang))?;
        let vars = self.vars_for_langs(None);
        Ok(lang.replace.as_ref().map(|r| r.embed_strings(&vars)))
    }

    pub fn code_replacers_on_atcoder(&self) -> ConfigResult<BTreeMap<u32, CodeReplacer>> {
        let mut replacers = BTreeMap::new();
        for lang in self.languages.values() {
            if let Some(lang_id) = lang.language_ids.atcoder {
                if let Some(replacer) = &lang.replace {
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
        let lang = find_language(&self.languages, self.lang_name(lang))?;
        lang.language_ids.atcoder.ok_or_else(|| {
            ConfigError::from(ConfigErrorKind::PropertyNotSet("language_ids.atcoder"))
        })
    }

    pub fn solver_compilation(
        &self,
        lang: Option<&str>,
    ) -> ConfigResult<Option<CompilationTemplate>> {
        self.compilation_command(find_language(&self.languages, self.lang_name(lang))?)
    }

    pub fn interactive_tester_compilation(
        &self,
        lang: Option<&str>,
    ) -> ConfigResult<Option<CompilationTemplate>> {
        self.compilation_command(find_language(&self.interactive, lang)?)
    }

    pub fn solver(&self, lang: Option<&str>) -> ConfigResult<JudgeTemplate> {
        self.judge_command(find_language(&self.languages, self.lang_name(lang))?)
    }

    pub fn interactive_tester(&self, lang: Option<&str>) -> ConfigResult<JudgeTemplate> {
        self.judge_command(find_language(&self.interactive, lang)?)
    }

    fn compilation_command(&self, lang: &Language) -> ConfigResult<Option<CompilationTemplate>> {
        match &lang.compile {
            None => Ok(None),
            Some(compile) => {
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

    fn lang_name<'a>(&'a self, name: Option<&'a str>) -> &'a str {
        name.or_else(|| {
            self.services
                .get(&self.service)
                .and_then(|s| s.language.as_ref())
                .map(String::as_str)
        }).unwrap_or(&self.language)
    }

    fn vars_for_langs(&self, service: impl Into<Option<ServiceName>>) -> HashMap<&str, &str> {
        let vars_in_service = self.services
            .get(&service.into().unwrap_or(self.service))
            .map(|s| &s.variables);
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
    langs: &HashMap<String, Language>,
    default_lang: impl Into<Option<&'a str>>,
) -> ConfigResult<&Language> {
    let name = default_lang
        .into()
        .ok_or_else(|| ConfigError::from(ConfigErrorKind::LanguageNotSpecified))?;
    langs
        .get(name)
        .ok_or_else(|| ConfigError::from(ConfigErrorKind::NoSuchLanguage(name.to_owned())))
}

fn find_base(start: &Path) -> FileIoResult<PathBuf> {
    fn target_exists(dir: &Path) -> FileIoResult<bool> {
        for entry in util::fs::read_dir(dir)? {
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
    directory: PathTemplate<BaseDirNone>,
    forall: BTreeSet<SuiteFileExtension>,
    scrape: SerializableExtension,
    zip: ZipConfig,
}

#[derive(Serialize, Deserialize)]
struct ServiceConfig {
    language: Option<String>,
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
