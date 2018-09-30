use command::{CompilationCommand, JudgingCommand};
use console::{self, ConsoleWrite, Palette};
use errors::{FileIoError, FileIoErrorKind, FileIoResult, LoadConfigError, LoadConfigResult};
use path::{AbsPath, AbsPathBuf};
use replacer::{CodeReplacer, CodeReplacerConf};
use service::{ServiceName, SessionConfig};
use template::{Template, TemplateBuilder};
use testsuite::{
    DownloadDestinations, SerializableExtension, SuiteFileExtension, TestCaseLoader, ZipConfig,
};
use yaml;

use serde_yaml;

use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::ffi::OsString;
use std::io::{self, Write};
use std::num::NonZeroUsize;
use std::ops::Deref as _Deref;
use std::str;
use std::time::Duration;

static CONFIG_FILE_NAME: &str = "snowchains.yaml";

/// Creates `snowchains.yaml` in `directory`.
pub(crate) fn init(
    mut stdout: impl ConsoleWrite,
    directory: AbsPath,
    session_cookies: &str,
) -> FileIoResult<()> {
    let config = format!(
        r#"---
service: atcoder
contest: arc100
language: c++

# console:
#   color: 256color
#   cjk: true

session:
  timeout: 60
  cookies: {session_cookies}

shell: {shell} # Used if `languages._.[compile|run].command` is a single string.

judge:
  jobs: 4
  path: snowchains/$service/$contest/{{snake}}.$extension
  forall: [json, toml, yaml, yml, zip]
  scrape: yaml
  zip:
    timelimit: 2000
    match: exact
    modify:
      add_eol: false
      crlf_to_lf: true
    entries:
      # AtCoder
      - in:
          entry: /\Ain/([a-z0-9_\-]+)\.txt\z/
          match_group: 1
        out:
          entry: /\Aout/([a-z0-9_\-]+)\.txt\z/
          match_group: 1
        sort: [dictionary]
      # HackerRank
      - in:
          entry: /\Ainput/input([0-9]+)\.txt\z/
          match_group: 1
        out:
          entry: /\Aoutput/output([0-9]+)\.txt\z/
          match_group: 1
        sort: [number]
      # yukicoder
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
      rust_version: 1.15.1
      java_class: Main
  hackerrank:
    # language: c++
    variables:
      rust_version: 1.21.0
      java_class: Main
  yukicoder:
    # language: c++
    variables:
      rust_version: 1.28.0
      java_class: Main
  other:
    # language: c++
    variables:
      rust_version: stable

interactive:
  python3:
    src: testers/py/test-{{kebab}}.py
    run:
      command: python3 -- $src $*
      working_directory: testers/py
  haskell:
    src: testers/hs/src/Test{{Pascal}}.hs
    compile:
      bin: testers/hs/target/Test{{Pascal}}
      command: [stack, ghc, --, -O2, -o, $bin, $src]
      working_directory: testers/hs
    run:
      command: $bin $*
      working_directory: testers/hs

languages:
  c++:
    src: cpp/{{kebab}}.cpp     # source file to test and to submit
    compile:                 # optional
      bin: cpp/build/{{kebab}}
      command: [g++, -std=c++14, -Wall, -Wextra, -g, -fsanitize=undefined, -D_GLIBCXX_DEBUG, -o, $bin, $src]
      working_directory: cpp # default: "."
    run:
      command: [$bin]
      working_directory: cpp # default: "."
    language_ids:            # optional
      atcoder: 3003          # "C++14 (GCC x.x.x)"
      yukicoder: cpp14       # "C++14 (gcc x.x.x)"
  rust:
    src: rs$rust_version/src/bin/{{kebab}}.rs
    compile:
      bin: rs$rust_version/target/release/{{kebab}}{exe}
      command: [rustc, +$rust_version, -o, $bin, $src]
      working_directory: rs$rust_version
    run:
      command: [$bin]
      working_directory: rs$rust_version
    language_ids:
      atcoder: 3504
      yukicoder: rust
  go:
    src: go/{{kebab}}.go
    compile:
      bin: go/{{kebab}}{exe}
      command: [go, build, -o, $bin, $src]
      working_directory: go
    run:
      command: [$bin]
      working_directory: go
    language_ids:
      atcoder: 3013
      yukicoder: go
  haskell:
    src: hs/src/{{Pascal}}.hs
    compile:
      bin: hs/target/{{Pascal}}{exe}
      command: [stack, ghc, --, -O2, -o, $bin, $src]
      working_directory: hs
    run:
      command: [$bin]
      working_directory: hs
    language_ids:
      atcoder: 3014
      yukicoder: haskell
  bash:
    src: bash/{{kebab}}.bash
    run:
      command: [bash, $src]
      working_directory: bash
    language_ids:
      atcoder: 3001
      yukicoder: sh
  python3:
    src: py/{{kebab}}.py
    run:
      command: [./venv/bin/python3, $src]
      working_directory: py
    language_ids:
      atcoder: 3023      # "Python3 (3.x.x)"
      yukicoder: python3 # "Python3 (3.x.x + numpy x.x.x)"
  java:
    src: java/src/main/java/{{Pascal}}.java
    compile:
      bin: java/build/classes/java/main/{{Pascal}}.class
      command: [javac, -d, ./build/classes/java/main, $src]
      working_directory: java
    run:
      command: [java, -classpath, ./build/classes/java/main, '{{Pascal}}']
      working_directory: java
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
      command: [scalac, -optimise, -d, ./target/scala-2.12/classes, $src]
      working_directory: scala
    run:
      command: [scala, -classpath, ./target/scala-2.12/classes, '{{Pascal}}']
      working_directory: scala
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
      working_directory: txt
    language_ids:
      atcoder: 3027
      yukicoder: text
"#,
        session_cookies = yaml::escape_string(session_cookies),
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
      working_directory: cs
    run:
      command: [$bin]
      working_directory: cs
    language_ids:
      atcoder: 3006     # "C# (Mono x.x.x.x)"
      yukicoder: csharp # "C# (csc x.x.x.x)""#
        } else {
            r#"  c#:
    src: cs/{Pascal}/{Pascal}.cs
    compile:
      bin: cs/{Pascal}/bin/Release/{Pascal}.exe
      command: [mcs, -o+, '-r:System.Numerics', '-out:$bin', $src]
      working_directory: cs
    run:
      command: [mono, $bin]
      working_directory: cs
    language_ids:
      atcoder: 3006          # "C# (Mono x.x.x.x)"
      yukicoder: csharp_mono # "C#(mono) (mono x.x.x.x)""#
        }
    );
    let path = directory.join(CONFIG_FILE_NAME);
    ::fs::write(&path, config.as_bytes())?;
    writeln!(stdout, "Wrote to {}", path.display())?;
    stdout.flush()?;
    Ok(())
}

/// Changes attributes.
pub(crate) fn switch(
    (mut stdout, mut stderr): (impl ConsoleWrite, impl ConsoleWrite),
    directory: AbsPath,
    service: Option<ServiceName>,
    contest: Option<String>,
    language: Option<String>,
) -> FileIoResult<()> {
    fn print_change(
        mut stdout: impl ConsoleWrite,
        left_width: usize,
        prev: &Option<String>,
        new: &Option<String>,
    ) -> io::Result<()> {
        let prev = prev.as_ref().map(String::as_str).unwrap_or("~");
        let new = new.as_ref().map(String::as_str).unwrap_or("~");
        write!(stdout, "{}", prev)?;
        (0..left_width - prev.len()).try_for_each(|_| write!(stdout, " "))?;
        writeln!(stdout, " -> {}", new)
    }

    let path = ::fs::find_filepath(directory, CONFIG_FILE_NAME)?;
    let mut old_yaml = ::fs::read_to_string(&path)?;
    let old_config = serde_yaml::from_str::<Config>(&old_yaml)
        .map_err(|err| FileIoError::new(FileIoErrorKind::Deserialize, path.deref()).with(err))?;
    writeln!(stdout, "Loaded {}", path.display())?;

    let mut m = hashmap!();
    if let Some(service) = service {
        m.insert("service", Cow::from(service.to_str()));
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
        }).or_else::<FileIoError, _>(|warning| {
            writeln!(stderr.plain(Palette::Warning), "{}", warning)?;
            stderr.flush()?;
            let mut new_config = serde_yaml::from_str::<Config>(&old_yaml).map_err(|err| {
                FileIoError::new(FileIoErrorKind::Deserialize, path.deref()).with(err)
            })?;
            new_config.service = service.unwrap_or(new_config.service);
            new_config.contest = contest.unwrap_or(new_config.contest);
            new_config.language = language.or(new_config.language);
            let new_yaml = serde_yaml::to_string(&new_config)
                .map_err(|err| FileIoError::new(FileIoErrorKind::Write, path.deref()).with(err))?;
            Ok((new_yaml, new_config))
        })?;

    let s1 = Some(format!("{:?}", old_config.service.to_str()));
    let s2 = Some(format!("{:?}", new_config.service.to_str()));
    let c1 = Some(format!("{:?}", old_config.contest));
    let c2 = Some(format!("{:?}", new_config.contest));
    let l1 = old_config.language.as_ref().map(|l| format!("{:?}", l));
    let l2 = new_config.language.as_ref().map(|l| format!("{:?}", l));
    let w = [
        s1.as_ref().map(|s| stdout.width(s)).unwrap_or(1),
        c1.as_ref().map(|s| stdout.width(s)).unwrap_or(1),
        l1.as_ref().map(|s| stdout.width(s)).unwrap_or(1),
    ]
        .iter()
        .cloned()
        .max()
        .unwrap();
    print_change(&mut stdout, w, &s1, &s2)?;
    print_change(&mut stdout, w, &c1, &c2)?;
    print_change(&mut stdout, w, &l1, &l2)?;
    ::fs::write(&path, new_yaml.as_bytes())?;

    writeln!(stdout, "Saved.")?;
    stdout.flush()?;
    Ok(())
}

/// Config.
#[derive(Serialize, Deserialize)]
pub(crate) struct Config {
    #[serde(default)]
    service: ServiceName,
    contest: String,
    language: Option<String>,
    #[serde(default)]
    console: console::Conf,
    session: SessionConfig,
    shell: Vec<TemplateBuilder<OsString>>,
    judge: Judge,
    #[serde(default)]
    services: BTreeMap<ServiceName, ServiceConfig>,
    #[serde(default)]
    interactive: HashMap<String, Language>,
    languages: HashMap<String, Language>,
    #[serde(skip)]
    base_dir: AbsPathBuf,
}

impl Config {
    pub fn load(
        mut stdout: impl ConsoleWrite,
        service: impl Into<Option<ServiceName>>,
        contest: impl Into<Option<String>>,
        dir: AbsPath,
    ) -> FileIoResult<Self> {
        let path = ::fs::find_filepath(dir, CONFIG_FILE_NAME)?;
        let mut config = serde_yaml::from_reader::<_, Self>(::fs::open(&path)?).map_err(|err| {
            FileIoError::new(FileIoErrorKind::Deserialize, path.deref()).with(err)
        })?;
        config.base_dir = path.parent().unwrap();
        config.service = service.into().unwrap_or(config.service);
        config.contest = contest.into().unwrap_or(config.contest);
        writeln!(stdout, "Loaded {}", path.display())?;
        stdout.flush()?;
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

    pub fn console(&self) -> &console::Conf {
        &self.console
    }

    /// Gets `session.timeout`.
    pub fn session_timeout(&self) -> Option<Duration> {
        self.session.timeout()
    }

    /// Gets `session.cookies` embedding "service" and "base_dir".
    pub fn session_cookies(&self) -> Template<AbsPathBuf> {
        self.session.cookies(&self.base_dir, self.service)
    }

    pub fn judge_jobs(&self) -> NonZeroUsize {
        self.judge.jobs
    }

    pub fn download_destinations(
        &self,
        ext: Option<SerializableExtension>,
    ) -> DownloadDestinations {
        let template = self
            .judge
            .path
            .build(&self.base_dir)
            .insert_string("service", self.service.to_str())
            .insert_string("contest", &self.contest);
        let ext = ext.unwrap_or(self.judge.scrape);
        DownloadDestinations::new(template, ext)
    }

    pub fn testcase_loader(&self) -> TestCaseLoader {
        let path = self
            .judge
            .path
            .build(&self.base_dir)
            .insert_string("service", self.service.to_str())
            .insert_string("contest", &self.contest);
        TestCaseLoader::new(
            path,
            &self.judge.forall,
            &self.judge.zip,
            self.interactive_tester_compilations(),
            self.interactive_testers(),
        )
    }

    pub fn src_paths(&self) -> HashMap<&str, Template<AbsPathBuf>> {
        let vars = self.vars_for_langs(None);
        let mut templates = hashmap!();
        for lang in self.languages.values() {
            if let Some(lang_id) = lang.language_ids.get(&ServiceName::Atcoder) {
                let template = lang.src.build(&self.base_dir).insert_strings(&vars);
                templates.insert(lang_id.as_str(), template);
            }
        }
        templates
    }

    pub fn src_to_submit(&self, lang: Option<&str>) -> LoadConfigResult<Template<AbsPathBuf>> {
        let lang = find_language(&self.languages, self.lang_name(lang)?)?;
        let vars = self.vars_for_langs(None);
        Ok(lang.src.build(&self.base_dir).insert_strings(&vars))
    }

    pub fn code_replacer(&self, lang: Option<&str>) -> LoadConfigResult<Option<CodeReplacer>> {
        let lang = find_language(&self.languages, self.lang_name(lang)?)?;
        let vars = self.vars_for_langs(None);
        Ok(lang.replace.as_ref().map(|r| r.build(&vars)))
    }

    pub fn code_replacers_on_atcoder(&self) -> LoadConfigResult<HashMap<&str, CodeReplacer>> {
        let mut replacers = hashmap!();
        for lang in self.languages.values() {
            if let Some(lang_id) = lang.language_ids.get(&ServiceName::Atcoder) {
                if let Some(replacer) = &lang.replace {
                    let vars = self.vars_for_langs(ServiceName::Atcoder);
                    let replacer = replacer.build(&vars);
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
    ) -> LoadConfigResult<Option<Template<CompilationCommand>>> {
        let lang = find_language(&self.languages, self.lang_name(lang)?)?;
        Ok(self.compilation_command(lang))
    }

    pub fn solver(&self, lang: Option<&str>) -> LoadConfigResult<Template<JudgingCommand>> {
        let lang = find_language(&self.languages, self.lang_name(lang)?)?;
        Ok(self.judge_command(lang))
    }

    fn interactive_tester_compilations(&self) -> HashMap<String, Template<CompilationCommand>> {
        self.interactive
            .iter()
            .filter_map(|(name, conf)| self.compilation_command(conf).map(|t| (name.to_owned(), t)))
            .collect()
    }

    fn interactive_testers(&self) -> HashMap<String, Template<JudgingCommand>> {
        self.interactive
            .iter()
            .map(|(name, conf)| (name.clone(), self.judge_command(&conf)))
            .collect()
    }

    fn compilation_command(&self, lang: &Language) -> Option<Template<CompilationCommand>> {
        lang.compile.as_ref().map(|compile| {
            compile
                .command
                .build(
                    &self.base_dir,
                    &self.shell,
                    &compile.working_directory,
                    &lang.src,
                    &compile.bin,
                ).insert_strings(&self.vars_for_langs(None))
        })
    }

    fn judge_command(&self, lang: &Language) -> Template<JudgingCommand> {
        lang.run
            .command
            .build(
                &self.base_dir,
                &self.shell,
                &lang.run.working_directory,
                &lang.src,
                lang.compile.as_ref().map(|c| &c.bin),
            ).insert_strings(&self.vars_for_langs(None))
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
        let mut vars = hashmap!("service" => self.service.to_str(), "contest" => &self.contest);
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
struct Judge {
    jobs: NonZeroUsize,
    path: TemplateBuilder<AbsPathBuf>,
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
    src: TemplateBuilder<AbsPathBuf>,
    #[serde(skip_serializing_if = "Option::is_none")]
    compile: Option<Compile>,
    run: Run,
    replace: Option<CodeReplacerConf>,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    language_ids: BTreeMap<ServiceName, String>,
}

#[derive(Serialize, Deserialize)]
struct Compile {
    bin: TemplateBuilder<AbsPathBuf>,
    command: TemplateBuilder<CompilationCommand>,
    #[serde(default)]
    working_directory: TemplateBuilder<AbsPathBuf>,
}

#[derive(Serialize, Deserialize)]
struct Run {
    command: TemplateBuilder<JudgingCommand>,
    #[serde(default)]
    working_directory: TemplateBuilder<AbsPathBuf>,
}
