use crate::command::{CompilationCommand, HookCommands, JudgingCommand, TranspilationCommand};
use crate::errors::{ConfigErrorKind, ConfigResult, FileResult};
use crate::path::{AbsPath, AbsPathBuf};
use crate::service::{DownloadOutcome, ServiceName};
use crate::template::{
    AbsPathBufRequirements, CompilationCommandRequirements, HookCommandsRequirements,
    JudgingCommandRequirements, Template, TemplateBuilder, TranspilationCommandRequirements,
};
use crate::terminal::{TermOut, WriteAnsi, WriteSpaces};
use crate::testsuite::{DownloadDestinations, SuiteFileExtension, TestCaseLoader};
use crate::{time, yaml};

use if_chain::if_chain;
use maplit::hashmap;
use serde::ser::SerializeMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_derive::{Deserialize, Serialize};

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::ffi::OsString;
use std::io::{self, Write};
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;
use std::{env, str};

static CONFIG_FILE_NAME: &str = "snowchains.yaml";

/// Creates "snowchains.yaml" in `directory`.
pub(crate) fn init(mut stdout: impl Write, directory: &AbsPath) -> FileResult<()> {
    let yaml = generate_yaml();
    let path = directory.join(CONFIG_FILE_NAME);
    crate::fs::write(&path, yaml.as_bytes())?;
    writeln!(stdout, "Wrote {}", path.display())?;
    stdout.flush().map_err(Into::into)
}

fn generate_yaml() -> String {
    #[cfg(not(windows))]
    static CONSOLE_ALT_WIDTH: &str = "";
    #[cfg(windows)]
    static CONSOLE_ALT_WIDTH: &str = "\n  # alt_width: 100";

    #[cfg(not(windows))]
    static EXE: &str = "";
    #[cfg(windows)]
    static EXE: &str = ".exe";

    #[cfg(not(windows))]
    static VENV_PYTHON3: &str = "./venv/bin/python3";
    #[cfg(windows)]
    static VENV_PYTHON3: &str = "./venv/Scripts/python.exe";

    #[cfg(not(windows))]
    static CRLF_TO_LF_TRUE_INDENT6: &str = "";
    #[cfg(windows)]
    static CRLF_TO_LF_TRUE_INDENT6: &str = "\n      crlf_to_lf: true";

    #[cfg(not(windows))]
    static CRLF_TO_LF_TRUE_INDENT4: &str = "";
    #[cfg(windows)]
    static CRLF_TO_LF_TRUE_INDENT4: &str = "\n    crlf_to_lf: true";

    #[cfg(not(windows))]
    static CRLF_TO_LF_FALSE: &str = "";
    #[cfg(windows)]
    static CRLF_TO_LF_FALSE: &str = "\n      # crlf_to_lf: false";

    #[cfg(not(windows))]
    static CRLF_TO_LF_FALSE_COMMENTED_OUT: &str = "";
    #[cfg(windows)]
    static CRLF_TO_LF_FALSE_COMMENTED_OUT: &str = "\n  #   # crlf_to_lf: false";

    #[cfg(not(windows))]
    static CSHARP: &str = r#"  c#:
    src: $service/$contest/cs/{Pascal}/{Pascal}.cs
    compile:
      bin: $service/$contest/cs/{Pascal}/bin/Release/{Pascal}.exe
      command: [mcs, -o+, '-r:System.Numerics', '-out:$bin', $src]
    run:
      command: [mono, $bin]
    working_directory: $service/$contest/cs
    language_ids:
      # atcoder: 3006        # "C# (Mono x.x.x.x)"
      yukicoder: csharp_mono # "C#(mono) (mono x.x.x.x)""#;
    #[cfg(windows)]
    static CSHARP: &str = r#"  c#:
    src: $service/$contest/cs/{Pascal}/{Pascal}.cs
    compile:
      bin: $service/$contest/cs/{Pascal}/bin/Release/{Pascal}.exe
      command: [csc, /o+, '/r:System.Numerics', '/out:$bin', $src]
    run:
      command: [$bin]
      crlf_to_lf: true
    working_directory: $service/$contest/cs
    language_ids:
      # atcoder: 3006   # "C# (Mono x.x.x.x)"
      yukicoder: csharp # "C# (csc x.x.x.x)""#;

    fn quote_path(path: &Path) -> Option<String> {
        let separator = if std::path::is_separator('/') {
            '/'
        } else {
            std::path::MAIN_SEPARATOR
        };
        path.to_str().map(|s| {
            yaml::escape_string(&s.replace(std::path::MAIN_SEPARATOR, &separator.to_string()))
                .into_owned()
        })
    }

    let (bash, powershell, cmd, jq, shell, transpile_java, transpile_scala) = {
        trait WithExe: ToOwned {
            fn with_exe(&self, name: &str) -> Self::Owned;
        }

        impl WithExe for Path {
            #[cfg(not(windows))]
            fn with_exe(&self, name: &str) -> PathBuf {
                self.join(name)
            }

            #[cfg(windows)]
            fn with_exe(&self, name: &str) -> PathBuf {
                self.join(name).with_extension("exe")
            }
        }

        let env_path = env::var_os("PATH").unwrap_or_default();
        let bash = env::split_paths(&env_path)
            .chain(if cfg!(windows) {
                vec![
                    PathBuf::from(r"C:\tools\msys64\usr\bin"),
                    PathBuf::from(r"C:\msys64\usr\bin"),
                    PathBuf::from(r"C:\Program Files\Git\usr\bin"),
                ]
            } else {
                vec![]
            })
            .map(|p| p.with_exe("bash"))
            .find(|p| p.exists() && p.to_str().is_some());
        let bash_found = bash.is_some();
        let bash = bash.unwrap_or_else(|| PathBuf::from("bash"));
        let bash = format!(
            "bash: [{}, -c, {}]",
            quote_path(&bash).unwrap(),
            if cfg!(windows) {
                "\"PATH=/usr/bin:$$PATH; $command\""
            } else {
                "$command"
            }
        );
        let powershell = env::split_paths(&env_path)
            .flat_map(|p| vec![p.with_exe("pwsh"), p.with_exe("powershell")])
            .find(|p| cfg!(windows) && p.exists())
            .and_then(|p| quote_path(&p))
            .map(|s| format!("\n  ps: [{}, -Command, $command]", s))
            .unwrap_or_default();
        let cmd = env::split_paths(&env_path)
            .map(|p| p.with_exe("cmd"))
            .find(|p| cfg!(windows) && p.exists())
            .and_then(|p| quote_path(&p))
            .map(|s| format!("\n  cmd: [{}, /C, $command]", s))
            .unwrap_or_default();

        let (jq, shell, transpile_java, transpile_scala);
        if cfg!(windows) && !bash_found {
            jq = "ps: echo \"${Env:SNOWCHAINS_RESULT}\" | jq";
            shell = "ps";
            transpile_java =
                r#"ps: Get-Content "${Env:SNOWCHAINS_SRC}" | ForEach-Object { $_.Replace("class\s+${Env:SNOWCHAINS_PROBLEM_PASCAL}", "class Main") } | sc "${Env:SNOWCHAINS_TRANSPILED}""#;
            transpile_scala =
                r#"ps: Get-Content "${Env:SNOWCHAINS_SRC}" | ForEach-Object { $_.Replace("object\s+${Env:SNOWCHAINS_PROBLEM_PASCAL}", "object Main") } | sc "${Env:SNOWCHAINS_TRANSPILED}""#;
        } else {
            jq = "bash: echo \"$SNOWCHAINS_RESULT\" | jq";
            shell = "bash";
            transpile_java =
                r#"bash: cat "$SNOWCHAINS_SRC" | sed -r "s/class\s+$SNOWCHAINS_PROBLEM_PASCAL/class Main/g" > "$SNOWCHAINS_TRANSPILED""#;
            transpile_scala =
                r#"bash: cat "$SNOWCHAINS_SRC" | sed -r "s/object\s+$SNOWCHAINS_PROBLEM_PASCAL/object Main/g" > "$SNOWCHAINS_TRANSPILED""#;
        };

        (
            bash,
            powershell,
            cmd,
            jq,
            shell,
            transpile_java,
            transpile_scala,
        )
    };
    let (session_cookies, session_dropbox) = {
        let data_local_dir = if_chain! {
            if let (Some(home), Some(local)) = (dirs::home_dir(), dirs::data_local_dir());
            if let Ok(path) = local.strip_prefix(&home);
            if path.to_str().is_some();
            then {
                Path::new("~").join(path).join("snowchains")
            } else {
                Path::new("~").join(".local").join("share").join("snowchains")
            }
        };
        let session_cookies = quote_path(&data_local_dir.join("$service")).unwrap();
        let session_dropbox = quote_path(&data_local_dir.join("dropbox.json")).unwrap();
        (session_cookies, session_dropbox)
    };
    let judge_jobs = num_cpus::get();

    format!(
        r#"---
service: atcoder
contest: arc100
language: c++

console:
  cjk: false{console_alt_width}

shell:
  {bash}{powershell}{cmd}

testfile_path: $service/$contest/tests/{{snake}}.$extension

session:
  timeout: 60s
  silent: false
  cookies: {session_cookies}
  dropbox: false
  # dropbox:
  #   auth: {session_dropbox}
  download:
    extension: yaml
    text_file_dir: $service/$contest/tests/{{snake}}

judge:
  testfile_extensions: [json, toml, yaml, yml]
  # jobs: {judge_jobs}
  display_limit: 1KiB

env:
  atcoder:
    CXXFLAGS: -std=gnu++1y -I/usr/include/boost -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra
    RUST_VERSION: 1.15.1
  yukicoder:
    CXXFLAGS: -std=gnu++14 -lm -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra
    RUST_VERSION: 1.30.1
  other:
    CXXFLAGS: -std=gnu++17 -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra
    RUST_VERSION: stable

# hooks:
#   switch:
#     - {jq}
#   download:
#     - {jq}

tester:
  src: testers/py/{{kebab}}.py
  run:
    command:
      {shell}: {venv_python3} "$SNOWCHAINS_SRC" $SNOWCHAINS_ARGS_JOINED{crlf_to_lf_true_indent4}
  working_directory: testers/py
  # src: testers/hs/app/{{Pascal}}.hs
  # compile:
  #   bin: testers/hs/target/{{Pascal}}
  #   command: [stack, ghc, --, -O2, -o, $bin, $src]
  # run:
  #   command:
  #     {shell}: "$SNOWCHAINS_BIN" $SNOWCHAINS_ARGS_JOINED{crlf_to_lf_false_commented_out}
  # working_directory: testers/hs

languages:
  c++:
    src: $service/$contest/cpp/{{kebab}}.cpp     # source file to test and to submit
    compile:                                   # optional
      bin: $service/$contest/cpp/build/{{kebab}}{exe}
      command:
        bash: g++ $CXXFLAGS -o "$SNOWCHAINS_BIN" "$SNOWCHAINS_SRC"
    run:
      command: [$bin]{crlf_to_lf_true_indent6}
    working_directory: $service/$contest/cpp   # default: "."
    language_ids:                              # optional
      atcoder: 3003                            # "C++14 (GCC x.x.x)"
      yukicoder: cpp14                         # "C++14 (gcc x.x.x)"
  rust:
    src: $service/$contest/rs/src/bin/{{kebab}}.rs
    compile:
      bin: $service/$contest/rs/target/manually/{{kebab}}{exe}
      command: [rustc, +$RUST_VERSION, -o, $bin, $src]
    run:
      command: [$bin]{crlf_to_lf_false}
    working_directory: $service/$contest/rs
    # language_ids:
    #   atcoder: 3504   # "Rust (x.x.x)"
    #   yukicoder: rust # "Rust (x.x.x)"
  go:
    src: $service/$contest/go/{{kebab}}.go
    compile:
      bin: $service/$contest/go/{{kebab}}{exe}
      command: [go, build, -o, $bin, $src]
    run:
      command: [$bin]{crlf_to_lf_false}
    working_directory: $service/$contest/go
    # language_ids:
    #   atcoder: 3013 # "Go (x.x)"
    #   yukicoder: go # "Go (x.x.x)"
  haskell:
    src: $service/$contest/hs/app/{{Pascal}}.hs
    compile:
      bin: $service/$contest/hs/target/{{Pascal}}{exe}
      command: [stack, ghc, --, -O2, -o, $bin, $src]
    run:
      command: [$bin]{crlf_to_lf_false}
    working_directory: $service/$contest/hs
    # language_ids:
    #   atcoder: 3014      # "Haskell (GHC x.x.x)"
    #   yukicoder: haskell # "Haskell (x.x.x)"
  bash:
    src: $service/$contest/bash/{{kebab}}.bash
    run:
      command: [bash, $src]{crlf_to_lf_false}
    working_directory: $service/$contest/bash
    # language_ids:
    #   atcoder: 3001 # "Bash (GNU Bash vx.x.x)"
    #   yukicoder: sh # "Bash (Bash x.x.x)"
  python3:
    src: $service/$contest/py/{{kebab}}.py
    run:
      command: [{venv_python3}, $src]{crlf_to_lf_true_indent6}
    working_directory: $service/$contest/py
    language_ids:
      atcoder: 3023      # "Python3 (3.x.x)"
      yukicoder: python3 # "Python3 (3.x.x + numpy x.x.x + scipy x.x.x)"
  java:
    src: $service/$contest/java/src/main/java/{{Pascal}}.java
    transpile:
      transpiled: $service/$contest/java/build/replaced/{{lower}}/src/Main.java
      command:
        {transpile_java}
    compile:
      bin: $service/$contest/java/build/replaced/{{lower}}/classes/Main.class
      command: [javac, -d, './build/replaced/{{lower}}/classes', $transpiled]
    run:
      command: [java, -classpath, './build/replaced/{{lower}}/classes', Main]{crlf_to_lf_true_indent6}
    working_directory: $service/$contest/java
    language_ids:
      atcoder: 3016      # "Java8 (OpenJDK 1.8.x)"
      # yukicoder: java8 # "Java8 (openjdk 1.8.x.x)"
  scala:
    src: $service/$contest/scala/src/main/scala/{{Pascal}}.scala
    transpile:
      transpiled: $service/$contest/scala/target/replaced/{{lower}}/src/Main.scala
      command:
        {transpile_scala}
    compile:
      bin: $service/$contest/scala/target/replaced/{{lower}}/classes/Main.class
      command: [scalac, -optimise, -d, './target/replaced/{{lower}}/classes', $transpiled]
    run:
      command: [scala, -classpath, './target/replaced/{{lower}}/classes', Main]{crlf_to_lf_true_indent6}
    working_directory: $service/$contest/scala
    # language_ids:
    #   atcoder: 3016    # "Scala (x.x.x)"
    #   yukicoder: scala # "Scala(Beta) (x.x.x)"
{csharp}
  text:
    src: $service/$contest/txt/{{snake}}.txt
    run:
      command: [cat, $src]
      working_directory: $service/$contest/txt{crlf_to_lf_false}
"#,
        console_alt_width = CONSOLE_ALT_WIDTH,
        session_cookies = session_cookies,
        session_dropbox = session_dropbox,
        judge_jobs = judge_jobs,
        bash = bash,
        powershell = powershell,
        cmd = cmd,
        jq = jq,
        shell = shell,
        exe = EXE,
        venv_python3 = VENV_PYTHON3,
        transpile_java = transpile_java,
        transpile_scala = transpile_scala,
        crlf_to_lf_true_indent4 = CRLF_TO_LF_TRUE_INDENT4,
        crlf_to_lf_false_commented_out = CRLF_TO_LF_FALSE_COMMENTED_OUT,
        crlf_to_lf_true_indent6 = CRLF_TO_LF_TRUE_INDENT6,
        crlf_to_lf_false = CRLF_TO_LF_FALSE,
        csharp = CSHARP,
    )
}

#[derive(Serialize)]
pub(crate) struct SwitchOutcome {
    old: SwitchOutcomeAttrs,
    new: SwitchOutcomeAttrs,
}

#[derive(Serialize)]
struct SwitchOutcomeAttrs {
    service: ServiceName,
    contest: String,
    language: String,
}

/// Changes attributes.
pub(crate) fn switch(
    mut stdout: impl TermOut,
    mut stderr: impl TermOut,
    directory: &AbsPath,
    service: Option<ServiceName>,
    contest: Option<String>,
    language: Option<String>,
) -> FileResult<(Config, SwitchOutcome)> {
    let path = crate::fs::find_path(CONFIG_FILE_NAME, directory)?;
    let old_yaml = crate::fs::read_to_string(&path)?;
    let old_config = crate::fs::read_yaml::<Config>(&path)?;
    stdout.apply_conf(&old_config.console);
    stderr.apply_conf(&old_config.console);
    let (new_yaml, mut new_config) = replace_values(
        &old_yaml,
        &old_config,
        service,
        contest,
        language,
        &mut stdout,
        stderr,
    )?;
    crate::fs::write(&path, new_yaml.as_bytes())?;
    writeln!(stdout, "Wrote {}", path.display())?;
    stdout.flush()?;
    let outcome = SwitchOutcome {
        old: SwitchOutcomeAttrs {
            service: old_config.service,
            contest: old_config.contest,
            language: old_config.language,
        },
        new: SwitchOutcomeAttrs {
            service: new_config.service,
            contest: new_config.contest.clone(),
            language: new_config.language.clone(),
        },
    };
    new_config.base_dir = directory.to_owned();
    Ok((new_config, outcome))
}

fn replace_values(
    yaml: &str,
    config: &Config,
    service: Option<ServiceName>,
    contest: Option<String>,
    language: Option<String>,
    mut stdout: impl TermOut,
    mut stderr: impl TermOut,
) -> io::Result<(String, Config)> {
    fn print_change(
        mut stdout: impl WriteAnsi,
        title: &str,
        left_width: usize,
        prev: &Option<String>,
        new: &Option<String>,
    ) -> io::Result<()> {
        let prev = prev.as_ref().map(String::as_str).unwrap_or("~");
        let new = new.as_ref().map(String::as_str).unwrap_or("~");
        stdout.write_str(title)?;
        stdout.with_reset(|o| o.bold()?.write_str(prev))?;
        stdout.write_spaces(left_width - prev.len())?;
        stdout.write_str(" -> ")?;
        stdout.with_reset(|o| o.bold()?.write_str(new))?;
        stdout.write_str("\n")
    }

    let (old_yaml, old_config) = (yaml, config);

    let mut m = hashmap!();
    m.extend(service.map(|s| ("service", <&str>::from(s))));
    m.extend(contest.as_ref().map(|c| ("contest", c.as_ref())));
    m.extend(language.as_ref().map(|l| ("language", l.as_ref())));

    let (new_yaml, new_config) = yaml::replace_scalars(old_yaml, &m)
        .and_then(|new_yaml| {
            let new_config = serde_yaml::from_str(&new_yaml)?;
            Ok((new_yaml, new_config))
        })
        .or_else::<io::Error, _>(|warning| {
            stderr.with_reset(|o| writeln!(o.fg(11)?, "{}", warning))?;
            stderr.flush()?;
            let mut new_config = serde_yaml::from_str::<Config>(old_yaml)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
            new_config.service = service.unwrap_or(new_config.service);
            new_config.contest = contest.unwrap_or(new_config.contest);
            new_config.language = language.unwrap_or(new_config.language);
            let new_yaml = serde_yaml::to_string(&new_config)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
            Ok((new_yaml, new_config))
        })?;

    let s1 = Some(format!("{:?}", <&str>::from(old_config.service)));
    let s2 = Some(format!("{:?}", <&str>::from(new_config.service)));
    let c1 = Some(format!("{:?}", old_config.contest));
    let c2 = Some(format!("{:?}", new_config.contest));
    let l1 = Some(format!("{:?}", old_config.language));
    let l2 = Some(format!("{:?}", new_config.language));
    let w = [
        s1.as_ref().map(|s| stdout.str_width(s)).unwrap_or(1),
        c1.as_ref().map(|s| stdout.str_width(s)).unwrap_or(1),
        l1.as_ref().map(|s| stdout.str_width(s)).unwrap_or(1),
    ]
    .iter()
    .cloned()
    .max()
    .unwrap();
    print_change(&mut stdout, "service:  ", w, &s1, &s2)?;
    print_change(&mut stdout, "contest:  ", w, &c1, &c2)?;
    print_change(&mut stdout, "language: ", w, &l1, &l2)?;
    Ok((new_yaml, new_config))
}

/// Config.
#[derive(Serialize, Deserialize)]
pub(crate) struct Config {
    #[serde(default)]
    service: ServiceName,
    contest: String,
    language: String,
    #[serde(default)]
    console: Console,
    #[serde(default)]
    shell: HashMap<String, Vec<TemplateBuilder<OsString>>>,
    testfile_path: TemplateBuilder<AbsPathBuf>,
    session: Session,
    judge: Judge,
    #[serde(default)]
    env: BTreeMap<ServiceName, HashMap<String, String>>,
    #[serde(default)]
    hooks: Hooks,
    tester: Option<Language>,
    languages: HashMap<String, Language>,
    #[serde(skip)]
    base_dir: AbsPathBuf,
}

impl Config {
    pub(crate) fn load(
        service: impl Into<Option<ServiceName>>,
        contest: impl Into<Option<String>>,
        language: impl Into<Option<String>>,
        dir: &AbsPath,
    ) -> FileResult<Self> {
        let path = crate::fs::find_path(CONFIG_FILE_NAME, dir)?;
        let mut config = crate::fs::read_yaml::<Self>(&path)?;
        config.base_dir = path.parent().unwrap().to_owned();
        config.service = service.into().unwrap_or(config.service);
        config.contest = contest.into().unwrap_or(config.contest);
        config.language = language.into().unwrap_or(config.language);
        Ok(config)
    }

    /// Gets `service`.
    pub(crate) fn service(&self) -> ServiceName {
        self.service
    }

    /// Gets `contest`.
    pub(crate) fn contest(&self) -> &str {
        &self.contest
    }

    pub(crate) fn console(&self) -> &Console {
        &self.console
    }

    /// Gets `session.timeout`.
    pub(crate) fn session_timeout(&self) -> Option<Duration> {
        self.session.timeout
    }

    pub(crate) fn session_silent(&self) -> bool {
        self.session.silent
    }

    pub(crate) fn session_cookies(&self) -> Template<AbsPathBuf> {
        self.session.cookies.build(AbsPathBufRequirements {
            base_dir: self.base_dir.clone(),
            service: self.service,
            contest: self.contest.clone(),
        })
    }

    pub(crate) fn session_dropbox_auth(&self) -> Option<Template<AbsPathBuf>> {
        match &self.session.dropbox {
            Dropbox::None => None,
            Dropbox::Some { auth } => Some(auth.build(AbsPathBufRequirements {
                base_dir: self.base_dir.clone(),
                service: self.service,
                contest: self.contest.clone(),
            })),
        }
    }

    pub(crate) fn judge_jobs(&self) -> Option<NonZeroUsize> {
        self.judge.jobs
    }

    pub(crate) fn judge_display_limit(&self) -> Option<usize> {
        self.judge.display_limit
    }

    pub(crate) fn switch_hooks(&self, outcome: &SwitchOutcome) -> Template<HookCommands> {
        self.hooks(|hs| &hs.switch, outcome)
    }

    pub(crate) fn download_hooks(&self, outcome: &DownloadOutcome) -> Template<HookCommands> {
        self.hooks(|hs| &hs.download, outcome)
    }

    fn hooks(
        &self,
        f: fn(&Hooks) -> &TemplateBuilder<HookCommands>,
        outcome: &impl Serialize,
    ) -> Template<HookCommands> {
        f(&self.hooks).build(HookCommandsRequirements {
            base_dir: self.base_dir.clone(),
            shell: self.shell.clone(),
            result: Arc::new(serde_json::to_string(outcome)),
        })
    }

    pub(crate) fn download_destinations(
        &self,
        ext: Option<SuiteFileExtension>,
    ) -> DownloadDestinations {
        let scraped = self.testfile_path.build(AbsPathBufRequirements {
            base_dir: self.base_dir.clone(),
            service: self.service,
            contest: self.contest.clone(),
        });
        let text_file_dir = self
            .session
            .download
            .text_file_dir
            .build(AbsPathBufRequirements {
                base_dir: self.base_dir.clone(),
                service: self.service,
                contest: self.contest.clone(),
            });
        let ext = ext.unwrap_or(self.session.download.extension);
        DownloadDestinations::new(scraped, text_file_dir, ext)
    }

    pub(crate) fn testcase_loader(&self) -> TestCaseLoader {
        let path = self.testfile_path.build(AbsPathBufRequirements {
            base_dir: self.base_dir.clone(),
            service: self.service,
            contest: self.contest.clone(),
        });
        TestCaseLoader::new(
            path,
            &self.judge.testfile_extensions,
            self.tester_transpilation(),
            self.tester_compilation(),
            self.tester(),
        )
    }

    pub(crate) fn src_paths(&self) -> HashMap<&str, Template<AbsPathBuf>> {
        let mut templates = hashmap!();
        for lang in self.languages.values() {
            if let Some(lang_id) = lang.language_ids.get(&self.service) {
                let template = lang
                    .src
                    .build(AbsPathBufRequirements {
                        base_dir: self.base_dir.clone(),
                        service: self.service,
                        contest: self.contest.clone(),
                    })
                    .envs(self.env.get(&self.service));
                templates.insert(lang_id.as_str(), template);
            }
        }
        templates
    }

    pub(crate) fn src_to_submit(&self) -> ConfigResult<Template<AbsPathBuf>> {
        let lang = self.find_language()?;
        let builder = match &lang.transpile {
            None => &lang.src,
            Some(transpile) => &transpile.transpiled,
        };
        Ok(builder
            .build(AbsPathBufRequirements {
                base_dir: self.base_dir.clone(),
                service: self.service,
                contest: self.contest.clone(),
            })
            .envs(self.env.get(&self.service)))
    }

    pub(crate) fn lang_id(&self) -> Option<&str> {
        let lang = self.find_language().ok()?;
        lang.language_ids.get(&self.service).map(String::as_str)
    }

    pub(crate) fn solver_compilation(&self) -> ConfigResult<Option<Template<CompilationCommand>>> {
        let lang = self.find_language()?;
        Ok(self.compilation_command(lang))
    }

    pub(crate) fn solver_transpilation(
        &self,
    ) -> ConfigResult<Option<Template<TranspilationCommand>>> {
        let lang = self.find_language()?;
        Ok(self.transpilation_command(lang))
    }

    pub(crate) fn solver(&self) -> ConfigResult<Template<JudgingCommand>> {
        let lang = self.find_language()?;
        Ok(self.judge_command(lang))
    }

    fn tester_transpilation(&self) -> Option<Template<TranspilationCommand>> {
        self.tester
            .as_ref()
            .and_then(|lang| self.transpilation_command(lang))
    }

    fn tester_compilation(&self) -> Option<Template<CompilationCommand>> {
        self.tester
            .as_ref()
            .and_then(|lang| self.compilation_command(lang))
    }

    fn tester(&self) -> Option<Template<JudgingCommand>> {
        self.tester.as_ref().map(|lang| self.judge_command(lang))
    }

    fn transpilation_command(&self, lang: &Language) -> Option<Template<TranspilationCommand>> {
        lang.transpile.as_ref().map(|transpile| {
            transpile
                .command
                .build(TranspilationCommandRequirements {
                    base_dir: self.base_dir.clone(),
                    service: self.service,
                    contest: self.contest.clone(),
                    shell: self.shell.clone(),
                    working_dir: lang.working_directory.clone(),
                    src: lang.src.clone(),
                    transpiled: transpile.transpiled.clone(),
                })
                .envs(self.env.get(&self.service))
        })
    }

    fn compilation_command(&self, lang: &Language) -> Option<Template<CompilationCommand>> {
        lang.compile.as_ref().map(|compile| {
            compile
                .command
                .build(CompilationCommandRequirements {
                    base_dir: self.base_dir.clone(),
                    service: self.service,
                    contest: self.contest.clone(),
                    shell: self.shell.clone(),
                    working_dir: lang.working_directory.clone(),
                    src: lang.src.clone(),
                    transpiled: lang.transpile.as_ref().map(|e| e.transpiled.clone()),
                    bin: compile.bin.clone(),
                })
                .envs(self.env.get(&self.service))
        })
    }

    fn judge_command(&self, lang: &Language) -> Template<JudgingCommand> {
        lang.run
            .command
            .build(JudgingCommandRequirements {
                base_dir: self.base_dir.clone(),
                service: self.service,
                contest: self.contest.clone(),
                shell: self.shell.clone(),
                working_dir: lang.working_directory.clone(),
                src: lang.src.clone(),
                bin: lang.compile.as_ref().map(|e| e.bin.clone()),
                transpiled: lang.transpile.as_ref().map(|e| e.transpiled.clone()),
                crlf_to_lf: lang.run.crlf_to_lf,
            })
            .envs(self.env.get(&self.service))
    }

    fn find_language(&self) -> ConfigResult<&Language> {
        self.languages
            .get(&self.language)
            .ok_or_else(|| ConfigErrorKind::NoSuchLanguage(self.language.clone()).into())
    }
}

#[derive(Default, Serialize, Deserialize)]
pub struct Console {
    #[serde(default)]
    pub(crate) cjk: bool,
    pub(crate) alt_width: Option<usize>,
}

#[derive(Serialize, Deserialize)]
pub(crate) struct Session {
    #[serde(
        serialize_with = "time::ser_secs",
        deserialize_with = "time::de_secs",
        default
    )]
    timeout: Option<Duration>,
    #[serde(default)]
    silent: bool,
    cookies: TemplateBuilder<AbsPathBuf>,
    #[serde(default)]
    dropbox: Dropbox,
    download: Download,
}

enum Dropbox {
    None,
    Some { auth: TemplateBuilder<AbsPathBuf> },
}

impl Default for Dropbox {
    fn default() -> Self {
        Dropbox::None
    }
}

impl Serialize for Dropbox {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        match self {
            Dropbox::None => serializer.serialize_bool(false),
            Dropbox::Some { auth } => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("auth", auth)?;
                map.end()
            }
        }
    }
}

impl<'de> Deserialize<'de> for Dropbox {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum Repr {
            Bool(bool),
            Map { auth: String },
        }

        static SCHEMA_ERR: &str = "expected `false` or `{ auth: <string> }`";
        match Repr::deserialize(deserializer).map_err(|_| serde::de::Error::custom(SCHEMA_ERR))? {
            Repr::Bool(true) => Err(serde::de::Error::custom(SCHEMA_ERR)),
            Repr::Bool(false) => Ok(Dropbox::None),
            Repr::Map { auth } => {
                let auth = auth.parse().map_err(serde::de::Error::custom)?;
                Ok(Dropbox::Some { auth })
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
struct Download {
    extension: SuiteFileExtension,
    text_file_dir: TemplateBuilder<AbsPathBuf>,
}

#[derive(Serialize, Deserialize)]
struct Judge {
    testfile_extensions: BTreeSet<SuiteFileExtension>,
    jobs: Option<NonZeroUsize>,
    #[serde(serialize_with = "ser_size", deserialize_with = "de_size", default)]
    display_limit: Option<usize>,
}

#[allow(clippy::trivially_copy_pass_by_ref)]
fn ser_size<S: Serializer>(
    size: &Option<usize>,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    size.as_ref().map(ToString::to_string).serialize(serializer)
}

fn de_size<'de, D: Deserializer<'de>>(
    deserializer: D,
) -> std::result::Result<Option<usize>, D::Error> {
    match Option::<String>::deserialize(deserializer)? {
        None => Ok(None),
        Some(s) => parse_size(&s).map(Some).map_err(serde::de::Error::custom),
    }
}

fn parse_size(s: &str) -> std::result::Result<usize, &'static str> {
    fn extract_unit(s: &str) -> (&str, f64) {
        if s.ends_with("GiB") {
            (&s[..s.len() - 3], f64::from(0x40_000_000))
        } else if s.ends_with("GB") {
            (&s[..s.len() - 2], f64::from(1_000_000_000))
        } else if s.ends_with("MiB") {
            (&s[..s.len() - 3], f64::from(0x100_000))
        } else if s.ends_with("MB") {
            (&s[..s.len() - 2], f64::from(1_000_000))
        } else if s.ends_with("KiB") {
            (&s[..s.len() - 3], f64::from(0x400))
        } else if s.ends_with("KB") {
            (&s[..s.len() - 2], 1000.0)
        } else if s.ends_with('B') {
            (&s[..s.len() - 1], 1.0)
        } else {
            (s, 1.0)
        }
    }

    let (s, k) = extract_unit(s.trim());
    s.parse::<f64>()
        .ok()
        .and_then(|v| {
            let r = k * v;
            guard!(r.is_finite() && r.is_sign_positive());
            Some(r as usize)
        })
        .ok_or_else(|| "invalid format")
}

#[derive(Serialize, Deserialize)]
struct ServiceConfig {
    language: Option<String>,
    variables: HashMap<String, String>,
}

#[derive(Default, Serialize, Deserialize)]
struct Hooks {
    #[serde(default)]
    switch: TemplateBuilder<HookCommands>,
    #[serde(default)]
    login: TemplateBuilder<HookCommands>,
    #[serde(default)]
    participate: TemplateBuilder<HookCommands>,
    #[serde(default)]
    download: TemplateBuilder<HookCommands>,
    #[serde(default)]
    restore: TemplateBuilder<HookCommands>,
    #[serde(default)]
    judge: TemplateBuilder<HookCommands>,
    #[serde(default)]
    submit: TemplateBuilder<HookCommands>,
}

#[derive(Serialize, Deserialize)]
struct Language {
    src: TemplateBuilder<AbsPathBuf>,
    #[serde(skip_serializing_if = "Option::is_none")]
    transpile: Option<Transpile>,
    #[serde(skip_serializing_if = "Option::is_none")]
    compile: Option<Compile>,
    run: Run,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    language_ids: BTreeMap<ServiceName, String>,
    #[serde(default)]
    working_directory: TemplateBuilder<AbsPathBuf>,
}

#[derive(Serialize, Deserialize)]
struct Transpile {
    transpiled: TemplateBuilder<AbsPathBuf>,
    command: TemplateBuilder<TranspilationCommand>,
}

#[derive(Serialize, Deserialize)]
struct Compile {
    bin: TemplateBuilder<AbsPathBuf>,
    command: TemplateBuilder<CompilationCommand>,
}

#[derive(Serialize, Deserialize)]
struct Run {
    command: TemplateBuilder<JudgingCommand>,
    #[serde(default)]
    crlf_to_lf: bool,
}

#[cfg(test)]
mod tests {
    use crate::config::{generate_yaml, parse_size, replace_values, Config};
    use crate::service::ServiceName;
    use crate::terminal::Ansi;

    use failure::Fallible;

    use std::str;

    #[test]
    fn it_generates_a_valid_yaml() -> serde_yaml::Result<()> {
        serde_yaml::from_str::<Config>(&generate_yaml()).map(|_| ())
    }

    #[test]
    fn test_replace_values() -> Fallible<()> {
        let mut stdout = Ansi::new(Vec::<u8>::new());
        let mut stderr = Ansi::new(Vec::<u8>::new());
        let yaml = generate_yaml();
        let config = serde_yaml::from_str(&yaml)?;
        let (new_yaml, _) = replace_values(
            &yaml,
            &config,
            Some(ServiceName::Yukicoder),
            Some("no".to_owned()),
            Some("rust".to_owned()),
            &mut stdout,
            &mut stderr,
        )?;
        serde_yaml::from_str::<Config>(&new_yaml)?;
        let stdout = str::from_utf8(stdout.get_ref())?;
        let stderr = str::from_utf8(stderr.get_ref())?;
        assert_eq!(
            stdout,
            "service:  \x1b[1m\"atcoder\"\x1b[0m -> \x1b[1m\"yukicoder\"\x1b[0m\n\
             contest:  \x1b[1m\"arc100\"\x1b[0m  -> \x1b[1m\"no\"\x1b[0m\n\
             language: \x1b[1m\"c++\"\x1b[0m     -> \x1b[1m\"rust\"\x1b[0m\n",
        );
        assert_eq!(stderr, "");
        Ok(())
    }

    #[test]
    fn test_parse_size() {
        assert_eq!(parse_size("0"), Ok(0));
        assert_eq!(parse_size("1B"), Ok(1));
        assert_eq!(parse_size("1KB"), Ok(10usize.pow(3)));
        assert_eq!(parse_size("1KiB"), Ok(2usize.pow(10)));
        assert_eq!(parse_size("1MB"), Ok(10usize.pow(6)));
        assert_eq!(parse_size("1MiB"), Ok(2usize.pow(20)));
        assert_eq!(parse_size("1GB"), Ok(10usize.pow(9)));
        assert_eq!(parse_size("1GiB"), Ok(2usize.pow(30)));
        assert_eq!(parse_size("4.2KB"), Ok(4200));
        assert_eq!(parse_size("4.2KiB"), Ok(4300));
        assert_eq!(parse_size("1b"), Err("invalid format"));
        assert_eq!(parse_size("B"), Err("invalid format"));
        assert_eq!(parse_size("-0B"), Err("invalid format"));
        assert_eq!(parse_size("infB"), Err("invalid format"));
        assert_eq!(parse_size("NaNB"), Err("invalid format"));
    }
}
