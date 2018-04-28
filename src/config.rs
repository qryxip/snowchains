use ServiceName;
use command::{CompilationCommand, JudgingCommand};
use errors::{ConfigError, ConfigErrorKind, ConfigResult, FileIoErrorKind, FileIoResult,
             FileIoResultExt};
use replacer::CodeReplacer;
use template::{BaseDirNone, BaseDirSome, CommandTemplate, CompilationTemplate, JudgeTemplate,
               PathTemplate, StringTemplate};
use testsuite::{SuiteFileExtension, SuiteFilePaths};
use util;

use {rprompt, serde_yaml};
use regex::Regex;

use std::{cmp, fs, str};
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

shell: {shell}

testfiles:
  directory: snowchains/$service/$contest/
  download: yaml
  exclude: []

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
      once: true
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
      once: true
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

    let mut path = directory;
    path.push(CONFIG_FILE_NAME);
    util::create_file_and_dirs(&path)?
        .write_all(config.as_bytes())
        .chain_err(|| FileIoErrorKind::Write(path.to_owned()))?;
    Ok(())
}

/// Changes <service> and <contest>.
pub fn switch(service: ServiceName, contest: &str, dir: &Path) -> ConfigResult<()> {
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
    let mut file = util::create_file_and_dirs(&path)?;
    file.write_all(replaced.as_bytes())?;
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
    ) -> ConfigResult<Self> {
        let base = find_base(dir)?;
        let path = base.join(CONFIG_FILE_NAME);
        let mut config = serde_yaml::from_reader::<_, Self>(util::open_file(&path)?)?;
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
            .filter(|e| !self.testfiles.exclude.contains(e))
            .collect::<Vec<_>>();
        SuiteFilePaths::new(dir, target, exts_on_judge)
    }

    /// Gets `testfiles/download`.
    pub fn extension_on_downloading(&self) -> SuiteFileExtension {
        self.testfiles.download
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
        language: Option<&str>,
    ) -> ConfigResult<PathTemplate<BaseDirSome>> {
        let lang = self.language(language)?;
        Ok(lang.src
            .base_dir(&self.base_dir)
            .embed_strings(self.vars_on_service(None)))
    }

    pub fn code_replacer(&self, lang_name: Option<&str>) -> ConfigResult<Option<CodeReplacer>> {
        let lang = self.language(lang_name)?;
        let vars = self.vars_on_service(None);
        Ok(lang.replace.as_ref().map(|r| r.embed_strings(vars)))
    }

    pub fn code_replacers_on_atcoder(&self) -> ConfigResult<BTreeMap<u32, CodeReplacer>> {
        let mut replacers = BTreeMap::new();
        for lang in self.languages.values() {
            if let Some(lang_id) = lang.language_ids.atcoder {
                if let Some(ref replacer) = lang.replace {
                    let replacer =
                        replacer.embed_strings(self.vars_on_service(Some(ServiceName::AtCoder)));
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

    pub(crate) fn compilation(
        &self,
        lang_name: Option<&str>,
    ) -> ConfigResult<Option<(CompilationTemplate)>> {
        let lang = self.language(lang_name)?;
        match lang.compile {
            None => Ok(None),
            Some(ref compile) => {
                let wd = compile.working_directory.base_dir(&self.base_dir);
                let vars = self.vars_on_service(None);
                let cmd = compile.command.embed_strings(vars);
                let src = lang.src.base_dir(&self.base_dir).embed_strings(vars);
                let bin = compile.bin.base_dir(&self.base_dir).embed_strings(vars);
                Ok(Some(cmd.as_compilation(&self.shell, wd, src, bin)))
            }
        }
    }

    pub(crate) fn solver(&self, lang_name: Option<&str>) -> ConfigResult<JudgeTemplate> {
        let lang = self.language(lang_name)?;
        let wd = lang.run.working_directory.base_dir(&self.base_dir);
        let vars = self.vars_on_service(None);
        let cmd = lang.run.command.embed_strings(vars);
        let src = lang.src.base_dir(&self.base_dir).embed_strings(vars);
        let bin = lang.compile
            .as_ref()
            .map(|c| c.bin.base_dir(&self.base_dir));
        Ok(cmd.as_judge(&self.shell, wd, &src, bin.as_ref()))
    }

    fn language(&self, name: Option<&str>) -> ConfigResult<&Language> {
        let service = match self.service {
            ServiceName::AtCoder | ServiceName::AtCoderBeta => self.atcoder.as_ref(),
            ServiceName::HackerRank => self.hackerrank.as_ref(),
            ServiceName::Other => None,
        };
        let name = name.or_else(|| service.map(|s| s.default_language.as_ref()))
            .ok_or_else(|| ConfigError::from(ConfigErrorKind::LanguageNotSpecified))?;
        self.languages
            .get(name)
            .ok_or_else(|| ConfigError::from(ConfigErrorKind::NoSuchLanguage(name.to_owned())))
    }

    fn vars_on_service(&self, service: Option<ServiceName>) -> Option<&HashMap<String, String>> {
        match service.unwrap_or(self.service) {
            ServiceName::AtCoder | ServiceName::AtCoderBeta => self.atcoder.as_ref(),
            ServiceName::HackerRank => self.hackerrank.as_ref(),
            ServiceName::Other => None,
        }.map(|s| &s.variables)
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
    directory: PathTemplate<BaseDirNone>,
    download: SuiteFileExtension,
    #[serde(default)]
    exclude: Vec<SuiteFileExtension>,
}

fn default_testsuites() -> PathTemplate<BaseDirNone> {
    PathTemplate::from_static_str("snowchains/$service/$contest/")
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
