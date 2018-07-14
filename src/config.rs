use command::{CompilationCommand, JudgingCommand};
use errors::{FileIoError, FileIoErrorKind, FileIoResult, LoadConfigError, LoadConfigResult};
use palette::{ColorRange, Palette};
use path::{AbsPath, AbsPathBuf};
use replacer::CodeReplacer;
use service::SessionConfig;
use template::{
    BaseDirNone, BaseDirSome, CommandTemplate, CompilationTemplate, JudgeTemplate, PathTemplate,
    StringTemplate,
};
use testsuite::{SerializableExtension, SuiteFileExtension, SuiteFilePathsTemplate, ZipConfig};
use {yaml, ServiceName};

use serde_yaml;
use unicode_width::UnicodeWidthStr as _UnicodeWidthStr;

use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::ops::Deref as _Deref;
use std::str;
use std::time::Duration;

static CONFIG_FILE_NAME: &str = "snowchains.yaml";

/// Creates `snowchains.yaml` in `directory`.
pub(crate) fn init(directory: AbsPath, session_cookies: &str) -> FileIoResult<()> {
    let config = format!(
        r#"---
service: atcoder
contest: arc001
language: c++

color_range: {color_range}

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
    variables:
      cxx_flags: -std=c++14 -O2 -Wall -Wextra -lm
      rust_version: 1.21.0
      java_class: Main
  yukicoder:
    variables:
      cxx_flags: =std=c++14 -O2 -Wall -Wextra
      rust_version: 1.22.1
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
      yukicoder: cpp14
  rust:
    src: rs$rust_version/src/bin/{{kebab}}.rs
    compile:
      bin: rs$rust_version/target/release/{{kebab}}{exe}
      command: [rustc, +$rust_version, -o, $bin, $src]
      working_directory: rs$rust_version/
    run:
      command: [$bin]
      working_directory: rs$rust_version/
    language_ids:
      atcoder: 3504
      yukicoder: rust
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
      yukicoder: haskell
  bash:
    src: bash/{{kebab}}.bash
    run:
      command: [bash, $src]
      working_directory: bash/
    language_ids:
      atcoder: 3001
      yukicoder: sh
  python3:
    src: py/{{kebab}}.py
    run:
      command: [./venv/bin/python3, $src]
      working_directory: py/
    language_ids:
      atcoder: 3023
      yukicoder: python3
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
      yukicoder: java8
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
      yukicoder: scala
{csharp}
  text:
    src: txt/{{snake}}.txt
    run:
      command: [cat, $src]
      working_directory: txt/
    language_ids:
      atcoder: 3027
      yukicoder: text
"#,
        color_range = ColorRange::default(),
        session_cookies = yaml::escape_string(session_cookies),
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
      atcoder: 3006
      yukicoder: csharp"#
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
      atcoder: 3006
      yukicoder: csharp_mono"#
        }
    );
    let path = directory.join(CONFIG_FILE_NAME);
    ::fs::write(&path, config.as_bytes())?;
    println!("Wrote to {}", path.display());
    Ok(())
}

/// Changes attributes.
pub(crate) fn switch(
    directory: AbsPath,
    service: Option<ServiceName>,
    contest: Option<String>,
    language: Option<String>,
) -> FileIoResult<()> {
    fn print_change(left_width: usize, prev: &Option<String>, new: &Option<String>) {
        let prev = prev.as_ref().map(String::as_str).unwrap_or("~");
        let new = new.as_ref().map(String::as_str).unwrap_or("~");
        print!("{}", prev);
        (0..left_width - prev.len()).for_each(|_| print!(" "));
        println!(" -> {}", new);
    }

    let path = ::fs::find_filepath(directory, CONFIG_FILE_NAME)?;
    let mut old_yaml = ::fs::read_to_string(&path)?;
    let old_config = serde_yaml::from_str::<Config>(&old_yaml)
        .map_err(|err| FileIoError::chaining(FileIoErrorKind::Deserialize, path.deref(), err))?;
    println!("Loaded {}", path.display());

    let mut m = hashmap!();
    if let Some(service) = service {
        m.insert("service", Cow::from(service.as_str()));
    }
    if let Some(contest) = contest.as_ref() {
        m.insert("contest", Cow::from(contest.clone()));
    }
    if let Some(language) = language.as_ref() {
        if old_config.language.is_some() {
            m.insert("language", Cow::from(language.clone()));
        } else {
            let line_to_insert = format!("language: {}", yaml::escape_string(&language));
            old_yaml = {
                let mut lines = old_yaml.lines().collect::<Vec<_>>();
                let index = if lines.get(0) == Some(&"---") { 1 } else { 0 };
                lines.insert(index, &line_to_insert);
                lines.join("\n")
            };
        }
    }

    let (new_yaml, new_config) = yaml::replace_scalars(&old_yaml, &m)
        .and_then(|new_yaml| {
            let new_config = serde_yaml::from_str(&new_yaml)?;
            Ok((new_yaml, new_config))
        })
        .or_else(|warning| {
            eprintln!("{}", Palette::Warning.paint(warning.to_string()));
            let mut new_config = serde_yaml::from_str::<Config>(&old_yaml).map_err(|err| {
                FileIoError::chaining(FileIoErrorKind::Deserialize, path.deref(), err)
            })?;
            new_config.service = service.unwrap_or(new_config.service);
            new_config.contest = contest.unwrap_or(new_config.contest);
            new_config.language = language.or(new_config.language);
            let new_yaml = serde_yaml::to_string(&new_config)
                .map_err(|err| FileIoError::chaining(FileIoErrorKind::Write, path.deref(), err))?;
            Ok((new_yaml, new_config))
        })?;

    let s1 = Some(format!("{:?}", old_config.service.as_str()));
    let s2 = Some(format!("{:?}", new_config.service.as_str()));
    let c1 = Some(format!("{:?}", old_config.contest));
    let c2 = Some(format!("{:?}", new_config.contest));
    let l1 = old_config.language.as_ref().map(|l| format!("{:?}", l));
    let l2 = new_config.language.as_ref().map(|l| format!("{:?}", l));
    let w = [
        s1.as_ref().map(|s| s.width_cjk()).unwrap_or(1),
        c1.as_ref().map(|s| s.width_cjk()).unwrap_or(1),
        l1.as_ref().map(|s| s.width_cjk()).unwrap_or(1),
    ].iter()
        .cloned()
        .max()
        .unwrap();
    print_change(w, &s1, &s2);
    print_change(w, &c1, &c2);
    print_change(w, &l1, &l2);
    ::fs::write(&path, new_yaml.as_bytes())?;
    println!("Saved.");
    Ok(())
}

/// Config.
#[derive(Serialize, Deserialize)]
pub(crate) struct Config {
    #[serde(default)]
    service: ServiceName,
    contest: String,
    language: Option<String>,
    color_range: ColorRange,
    session: SessionConfig,
    shell: Vec<StringTemplate>,
    testfiles: TestFiles,
    #[serde(default)]
    services: BTreeMap<ServiceName, ServiceConfig>,
    #[serde(default)]
    interactive: HashMap<String, Language>,
    languages: HashMap<String, Language>,
    #[serde(skip)]
    base_dir: AbsPathBuf,
}

impl Config {
    pub fn load_setting_color_range(
        service: impl Into<Option<ServiceName>>,
        contest: impl Into<Option<String>>,
        dir: AbsPath,
    ) -> FileIoResult<Self> {
        let path = ::fs::find_filepath(dir, CONFIG_FILE_NAME)?;
        let mut config = serde_yaml::from_reader::<_, Self>(::fs::open(&path)?)
            .map_err(|err| FileIoError::chaining(FileIoErrorKind::Deserialize, path.deref(), err))?;
        config.base_dir = path.parent().unwrap();
        config.service = service.into().unwrap_or(config.service);
        config.contest = contest.into().unwrap_or(config.contest);
        config.color_range.set_globally();
        println!(
            "Loaded {} (color_range: {})",
            path.display(),
            config.color_range
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

    pub fn src_paths(&self) -> HashMap<&str, PathTemplate<BaseDirSome>> {
        let vars = self.vars_for_langs(None);
        let mut templates = hashmap!();
        for lang in self.languages.values() {
            if let Some(lang_id) = lang.language_ids.get(&ServiceName::AtCoder) {
                let template = lang.src.base_dir(&self.base_dir).embed_strings(&vars);
                templates.insert(lang_id.as_str(), template);
            }
        }
        templates
    }

    pub fn src_to_submit(&self, lang: Option<&str>) -> LoadConfigResult<PathTemplate<BaseDirSome>> {
        let lang = find_language(&self.languages, self.lang_name(lang)?)?;
        let vars = self.vars_for_langs(None);
        Ok(lang.src.base_dir(&self.base_dir).embed_strings(&vars))
    }

    pub fn code_replacer(&self, lang: Option<&str>) -> LoadConfigResult<Option<CodeReplacer>> {
        let lang = find_language(&self.languages, self.lang_name(lang)?)?;
        let vars = self.vars_for_langs(None);
        Ok(lang.replace.as_ref().map(|r| r.embed_strings(&vars)))
    }

    pub fn code_replacers_on_atcoder(&self) -> LoadConfigResult<HashMap<&str, CodeReplacer>> {
        let mut replacers = hashmap!();
        for lang in self.languages.values() {
            if let Some(lang_id) = lang.language_ids.get(&ServiceName::AtCoder) {
                if let Some(replacer) = &lang.replace {
                    let vars = self.vars_for_langs(ServiceName::AtCoder);
                    let replacer = replacer.embed_strings(&vars);
                    replacers.insert(lang_id.as_str(), replacer);
                }
            }
        }
        Ok(replacers)
    }

    pub fn lang_id(&self, service: ServiceName, lang: Option<&str>) -> LoadConfigResult<&str> {
        let lang = find_language(&self.languages, self.lang_name(lang)?)?;
        lang.language_ids
            .get(&service)
            .map(String::as_str)
            .ok_or_else(|| LoadConfigError::PropertyNotSet("language_ids.atcoder"))
    }

    pub fn solver_compilation(
        &self,
        lang: Option<&str>,
    ) -> LoadConfigResult<Option<CompilationTemplate>> {
        self.compilation_command(find_language(&self.languages, self.lang_name(lang)?)?)
    }

    pub fn interactive_tester_compilation(
        &self,
        lang: Option<&str>,
    ) -> LoadConfigResult<Option<CompilationTemplate>> {
        self.compilation_command(find_language(&self.interactive, lang)?)
    }

    pub fn solver(&self, lang: Option<&str>) -> LoadConfigResult<JudgeTemplate> {
        self.judge_command(find_language(&self.languages, self.lang_name(lang)?)?)
    }

    pub fn interactive_tester(&self, lang: Option<&str>) -> LoadConfigResult<JudgeTemplate> {
        self.judge_command(find_language(&self.interactive, lang)?)
    }

    fn compilation_command(
        &self,
        lang: &Language,
    ) -> LoadConfigResult<Option<CompilationTemplate>> {
        match &lang.compile {
            None => Ok(None),
            Some(compile) => {
                let vars = self.vars_for_langs(None);
                let wd = compile
                    .working_directory
                    .base_dir(&self.base_dir)
                    .embed_strings(&vars);
                let cmd = compile.command.embed_strings(&vars);
                let src = lang.src.base_dir(&self.base_dir).embed_strings(&vars);
                let bin = compile.bin.base_dir(&self.base_dir).embed_strings(&vars);
                Ok(Some(cmd.as_compilation(&self.shell, wd, src, bin)))
            }
        }
    }

    fn judge_command(&self, lang: &Language) -> LoadConfigResult<JudgeTemplate> {
        let vars = self.vars_for_langs(None);
        let wd = lang
            .run
            .working_directory
            .base_dir(&self.base_dir)
            .embed_strings(&vars);
        let cmd = lang.run.command.embed_strings(&vars);
        let src = lang.src.base_dir(&self.base_dir).embed_strings(&vars);
        let bin = lang
            .compile
            .as_ref()
            .map(|c| c.bin.base_dir(&self.base_dir).embed_strings(&vars));
        Ok(cmd.as_judge(&self.shell, wd, &src, bin.as_ref()))
    }

    fn lang_name<'a>(&'a self, name: Option<&'a str>) -> LoadConfigResult<&'a str> {
        name.or_else(|| {
            self.services
                .get(&self.service)
                .and_then(|s| s.language.as_ref())
                .map(String::as_str)
        }).or_else(|| self.language.as_ref().map(String::as_str))
            .ok_or_else(|| LoadConfigError::PropertyNotSet("language"))
    }

    fn vars_for_langs(&self, service: impl Into<Option<ServiceName>>) -> HashMap<&str, &str> {
        let vars_in_service = self
            .services
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
) -> LoadConfigResult<&Language> {
    let name = default_lang
        .into()
        .ok_or_else(|| LoadConfigError::LanguageNotSpecified)?;
    langs
        .get(name)
        .ok_or_else(|| LoadConfigError::NoSuchLanguage(name.to_owned()))
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
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    language_ids: BTreeMap<ServiceName, String>,
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
