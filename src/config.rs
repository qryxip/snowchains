use crate::command::{CompilationCommand, HookCommands, JudgingCommand, TranspilationCommand};
use crate::errors::{ConfigErrorKind, ConfigResult, FileResult};
use crate::path::{AbsPath, AbsPathBuf};
use crate::service::{DownloadOutcome, ServiceKind};
use crate::template::{
    AbsPathBufRequirements, CompilationCommandRequirements, HookCommandsRequirements,
    JudgingCommandRequirements, Template, TemplateBuilder, TranspilationCommandRequirements,
};
use crate::terminal::{TermOut, WriteSpaces};
use crate::testsuite::{DownloadDestinations, SuiteFileExtension, TestCaseLoader};
use crate::time;

use heck::{CamelCase, KebabCase, MixedCase, SnakeCase};
use if_chain::if_chain;
use maplit::hashmap;
use serde::ser::SerializeMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_derive::{Deserialize, Serialize};

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::ffi::OsString;
use std::io::Write;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;
use std::{env, fmt, str};

static CONFIG_FILE_NAME: &str = "snowchains.toml";

/// Creates "snowchains.toml" in `directory`.
pub(crate) fn init(mut stdout: impl Write, directory: &AbsPath) -> FileResult<()> {
    let toml = generate_toml();
    let path = directory.join(CONFIG_FILE_NAME);
    crate::fs::write(&path, toml.as_bytes())?;
    writeln!(stdout, "Wrote {}", path.display())?;
    stdout.flush().map_err(Into::into)
}

fn generate_toml() -> String {
    #[cfg(not(windows))]
    static CONSOLE_ALT_WIDTH: &str = "";
    #[cfg(windows)]
    static CONSOLE_ALT_WIDTH: &str = "\n# alt_width = 100";

    #[cfg(not(windows))]
    static EXE: &str = "";
    #[cfg(windows)]
    static EXE: &str = ".exe";

    #[cfg(not(windows))]
    static CRLF_TO_LF_TRUE: &str = "";
    #[cfg(windows)]
    static CRLF_TO_LF_TRUE: &str = "\ncrlf_to_lf = true";

    #[cfg(not(windows))]
    static CRLF_TO_LF_FALSE: &str = "";
    #[cfg(windows)]
    static CRLF_TO_LF_FALSE: &str = "\n# crlf_to_lf = false";

    #[cfg(not(windows))]
    static TESTER_PYTHON3: &str = "./venv/bin/python3";
    #[cfg(windows)]
    static TESTER_PYTHON3: &str = "./venv/Scripts/python.exe";

    #[cfg(not(windows))]
    static VENV_PYTHON3: &str = "\"../../../venvs/python3_${service}/bin/python3\"";
    #[cfg(windows)]
    static VENV_PYTHON3: &str = "\"../../../venvs/python3_${service}/Scripts/python.exe\"";

    #[cfg(not(windows))]
    static VENV_PYPY3: &str = "\"../../../venvs/pypy3_${service}/bin/python3\"";
    #[cfg(windows)]
    static VENV_PYPY3: &str = "\"../../../venvs/pypy3_${service}/Scripts/python.exe\"";

    #[cfg(not(windows))]
    static CSHARP: &str =
        r#"src = "${service}/${contest}/cs/${problem_pascal}/${problem_pascal}.cs"
bin = "${service}/${contest}/cs/${problem_pascal}/bin/Release/${problem_pascal}.exe"
compile = ["mcs", "-o+", "-r:System.Numerics", "-out:${bin}", "${src}"]
run = ["mono", "${bin}"]
working_directory = "${service}/${contest}/cs"
language_ids = { atcoder = "3006", yukicoder = "csharp_mono" }"#;
    #[cfg(windows)]
    static CSHARP: &str =
        r#"src = "${service}/${contest}/cs/${problem_pascal}/${problem_pascal}.cs"
bin = "${service}/${contest}/cs/${problem_pascal}/bin/Release/${problem_pascal}.exe"
compile = ["csc", "/o+", "/r:System.Numerics", "/out:${bin}", "${src}"]
run = ["${bin}"]
crlf_to_lf = true
working_directory = "${service}/${contest}/cs"
language_ids = { atcoder = "3006", yukicoder = "csharp" }"#;

    fn quote_path_normalizing_separator(path: &Path) -> impl fmt::Display {
        let separator = if std::path::is_separator('/') {
            '/'
        } else {
            std::path::MAIN_SEPARATOR
        };
        let path = path
            .to_string_lossy()
            .replace(std::path::MAIN_SEPARATOR, &separator.to_string());
        toml_edit::Value::from(path)
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
            "bash = [{}, \"-c\", {}]",
            quote_path_normalizing_separator(&bash),
            if cfg!(windows) {
                "\"PATH=/usr/bin:$$PATH; ${command}\""
            } else {
                "\"${command}\""
            }
        );
        let powershell = env::split_paths(&env_path)
            .flat_map(|p| vec![p.with_exe("pwsh"), p.with_exe("powershell")])
            .find(|p| cfg!(windows) && p.exists())
            .map(|p| {
                format!(
                    "\nps: [{}, \"-Command\", \"${{command}}\"]",
                    quote_path_normalizing_separator(&p)
                )
            })
            .unwrap_or_default();
        let cmd = env::split_paths(&env_path)
            .map(|p| p.with_exe("cmd"))
            .find(|p| cfg!(windows) && p.exists())
            .map(|p| {
                format!(
                    "\ncmd: [{}, \"/C\", \"${{command}}\"]",
                    quote_path_normalizing_separator(&p)
                )
            })
            .unwrap_or_default();

        let (jq, shell, transpile_java, transpile_scala);
        if cfg!(windows) && !bash_found {
            jq = r#"ps = 'echo "${Env:SNOWCHAINS_RESULT}" | jq'"#;
            shell = "ps";
            transpile_java =
                r#"ps = 'Get-Content "${Env:SNOWCHAINS_SRC}" | ForEach-Object { $_.Replace("class\s+${Env:SNOWCHAINS_PROBLEM_PASCAL}", "class Main") } | sc "${Env:SNOWCHAINS_TRANSPILED}"'"#;
            transpile_scala =
                r#"ps = 'Get-Content "${Env:SNOWCHAINS_SRC}" | ForEach-Object { $_.Replace("object\s+${Env:SNOWCHAINS_PROBLEM_PASCAL}", "object Main") } | sc "${Env:SNOWCHAINS_TRANSPILED}"'"#;
        } else {
            jq = r#"bash = 'echo "$SNOWCHAINS_RESULT" | jq'"#;
            shell = "bash";
            transpile_java =
                r#"bash = 'cat "$SNOWCHAINS_SRC" | sed -r "s/class\s+$SNOWCHAINS_PROBLEM_PASCAL/class Main/g" > "$SNOWCHAINS_TRANSPILED"'"#;
            transpile_scala =
                r#"bash = 'cat "$SNOWCHAINS_SRC" | sed -r "s/object\s+$SNOWCHAINS_PROBLEM_PASCAL/object Main/g" > "$SNOWCHAINS_TRANSPILED"'"#;
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
        let session_cookies = data_local_dir.join("${service}");
        let session_cookies = quote_path_normalizing_separator(&session_cookies);
        let session_dropbox = data_local_dir.join("dropbox.json");
        let session_dropbox = quote_path_normalizing_separator(&session_dropbox);
        (session_cookies, session_dropbox)
    };
    let judge_jobs = num_cpus::get();

    format!(
        r#"service = "atcoder"
contest = "arc100"
language = "c++"

[console]
cjk = false{console_alt_width}

[shell]
{bash}{powershell}{cmd}

[testfiles]
path = "${{service}}/${{contest}}/tests/${{problem_snake}}.${{extension}}"

[session]
timeout = "60s"
silent = false
cookies = {session_cookies}
dropbox = false
# dropbox = {{ auth: {session_dropbox} }}

[session.download]
extension = "yml"
text_file_dir = "${{service}}/${{contest}}/tests/${{problem_snake}}"

[judge]
testfile_extensions = ["json", "toml", "yaml", "yml"]
# jobs = {judge_jobs}
display_limit = "1KiB"

[env.atcoder]
CXXFLAGS = "-std=gnu++1y -I/usr/include/boost -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra"
RUST_VERSION = "1.15.1"

[env.yukicoder]
CXXFLAGS = "-std=gnu++14 -lm -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra"
RUST_VERSION = "1.30.1"

[env.other]
CXXFLAGS = "-std=gnu++17 -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra"
RUST_VERSION = "stable"

# [hooks]
# switch = {{ {jq} }}
# download = {{ {jq} }}

[tester]
src = "testers/py/${{problem_kebab}}.py"
run = {{ {shell} = '{tester_python3} "$SNOWCHAINS_SRC" $SNOWCHAINS_ARGS_JOINED' }}{crlf_to_lf_true}
working_directory = "testers/py"

# [tester]
# src = "testers/hs/app/${{problem_pascal}}.hs"
# bin = "testers/hs/target/${{problem_pascal}}"
# run = {{ {shell} = '"$SNOWCHAINS_BIN" "$SNOWCHAINS_SRC" $SNOWCHAINS_ARGS_JOINED' }}{crlf_to_lf_true}
# working_directory = "testers/hs"

[languages.'c++']
src = "${{service}}/${{contest}}/cpp/${{problem_kebab}}.cpp"
bin = "${{service}}/${{contest}}/cpp/build/${{problem_kebab}}{exe}"
compile = {{ bash = 'g++ $CXXFLAGS -o "$SNOWCHAINS_BIN" "$SNOWCHAINS_SRC"' }}
run = ["${{bin}}"]{crlf_to_lf_true}
working_directory = "${{service}}/${{contest}}/cpp"
language_ids = {{ atcoder = "3003", yukicoder = "cpp14" }}

[languages.rust]
src = "${{service}}/${{contest}}/rs/src/bin/${{problem_kebab}}.rs"
bin = "${{service}}/${{contest}}/rs/target/manually/${{problem_kebab}}{exe}"
compile = ["rustc", "+${{env:RUST_VERSION}}", "-o", "${{bin}}", "${{src}}"]
run = ["${{bin}}"]{crlf_to_lf_false}
working_directory = "${{service}}/${{contest}}/rs"
language_ids = {{ atcoder = "3504", yukicoder = "rust" }}

[languages.go]
src = "${{service}}/${{contest}}/go/${{problem_kebab}}.go"
bin = "${{service}}/${{contest}}/go/${{problem_kebab}}{exe}"
compile = ["go", "build", "-o", "${{bin}}", "${{src}}"]
run = ["${{bin}}"]{crlf_to_lf_false}
working_directory = "${{service}}/${{contest}}/go"
language_ids = {{ atcoder = "3013", yukicoder = "go" }}

[languages.haskell]
src = "${{service}}/${{contest}}/hs/app/${{problem_pascal}}.hs"
bin = "${{service}}/${{contest}}/hs/target/${{problem_pascal}}{exe}"
compile = ["stack", "ghc", "--", "-O2", "-o", "${{bin}}", "${{src}}"]
run = ["${{bin}}"]{crlf_to_lf_false}
working_directory = "${{service}}/${{contest}}/hs"
language_ids = {{ atcoder = "3014", yukicoder = "haskell" }}

[languages.bash]
src = "${{service}}/${{contest}}/bash/${{problem_kebab}}.bash"
run = ["bash", "${{src}}"]{crlf_to_lf_false}
working_directory = "${{service}}/${{contest}}/bash"
language_ids = {{ atcoder = "3001", yukicoder = "sh" }}

[languages.python3]
src = "${{service}}/${{contest}}/py/${{problem_kebab}}.py"
run = [{venv_python3}, "${{src}}"]{crlf_to_lf_true}
working_directory = "${{service}}/${{contest}}/py"
language_ids = {{ atcoder = "3023", yukicoder = "python3" }}

[languages.pypy3]
src = "${{service}}/${{contest}}/py/${{problem_kebab}}.py"
run = [{venv_pypy3}, "${{src}}"]{crlf_to_lf_true}
working_directory = "${{service}}/${{contest}}/py"
language_ids = {{ atcoder = "3510", yukicoder = "pypy3" }}

[languages.java]
src = "${{service}}/${{contest}}/java/src/main/java/${{problem_pascal}}.java"
transpiled = "${{service}}/${{contest}}/java/build/replaced/${{problem_lower}}/src/Main.java"
bin = "${{service}}/${{contest}}/java/build/replaced/${{problem_lower}}/classes/Main.class"
transpile = {{ {transpile_java} }}
compile = ["javac", "-d", "./build/replaced/${{problem_lower}}/classes", "${{transpiled}}"]
run = ["java", "-classpath", "./build/replaced/${{problem_lower}}/classes", "Main"]{crlf_to_lf_true}
working_directory = "${{service}}/${{contest}}/java"
language_ids = {{ atcoder = "3016", yukicoder = "java8" }}

[languages.scala]
src = "${{service}}/${{contest}}/scala/src/main/scala/${{problem_pascal}}.scala"
transpiled = "${{service}}/${{contest}}/scala/target/replaced/${{problem_lower}}/src/Main.scala"
bin = "${{service}}/${{contest}}/scala/target/replaced/${{problem_lower}}/classes/Main.class"
transpile = {{ {transpile_scala} }}
compile = ["scalac", "-optimise", "-d", "./target/replaced/${{problem_lower}}/classes", "${{transpiled}}"]
run = ["scala", "-classpath", "./target/replaced/${{problem_lower}}/classes", "Main"]{crlf_to_lf_true}
working_directory = "${{service}}/${{contest}}/scala"
language_ids = {{ atcoder = "3025", yukicoder = "scala" }}

[languages.'c#']
{csharp}

[languages.text]
src = "${{service}}/${{contest}}/txt/${{problem_snake}}.txt"
run = ["cat", "${{src}}"]
working_directory = "${{service}}/${{contest}}/txt{crlf_to_lf_false}"
language_ids = {{ atcoder = "3027", yukicoder = "text" }}
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
        tester_python3 = TESTER_PYTHON3,
        venv_python3 = VENV_PYTHON3,
        venv_pypy3 = VENV_PYPY3,
        transpile_java = transpile_java,
        transpile_scala = transpile_scala,
        crlf_to_lf_true = CRLF_TO_LF_TRUE,
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
    service: ServiceKind,
    contest: String,
    contest_lower_case: String,
    contest_upper_case: String,
    contest_snake_case: String,
    contest_kebab_case: String,
    contest_mixed_case: String,
    contest_pascal_case: String,
    language: String,
}

/// Changes attributes.
pub(crate) fn switch(
    mut stdout: impl TermOut,
    mut stderr: impl TermOut,
    directory: &AbsPath,
    service: Option<ServiceKind>,
    contest: Option<String>,
    language: Option<String>,
) -> FileResult<(Config, SwitchOutcome)> {
    let path = crate::fs::find_path(CONFIG_FILE_NAME, directory)?;
    let old_toml = crate::fs::read_to_string(&path)?;
    let old_config = crate::fs::read_toml::<Config>(&path)?;
    stdout.apply_conf(&old_config.console);
    stderr.apply_conf(&old_config.console);

    let new_toml = {
        let mut doc = old_toml
            .parse::<toml_edit::Document>()
            .unwrap_or_else(|_| unimplemented!());
        if let Some(service) = service {
            doc["service"] = toml_edit::value(<&str>::from(service));
        }
        if let Some(contest) = &contest {
            doc["contest"] = toml_edit::value(contest.as_str());
        }
        if let Some(language) = &language {
            doc["language"] = toml_edit::value(language.as_str());
        }
        doc.to_string()
    };
    let mut new_config =
        toml::from_str::<Config>(&new_toml).unwrap_or_else(|e| unimplemented!("{:?}", e));

    let old_service = Some(format!("{:?}", <&str>::from(old_config.service)));
    let old_contest = Some(format!("{:?}", old_config.contest));
    let old_language = Some(format!("{:?}", old_config.language));
    let new_service = Some(format!("{:?}", <&str>::from(new_config.service)));
    let new_contest = Some(format!("{:?}", new_config.contest));
    let new_language = Some(format!("{:?}", new_config.language));
    let max_width = [
        old_service
            .as_ref()
            .map(|s| stdout.str_width(s))
            .unwrap_or(1),
        old_contest
            .as_ref()
            .map(|s| stdout.str_width(s))
            .unwrap_or(1),
        old_language
            .as_ref()
            .map(|s| stdout.str_width(s))
            .unwrap_or(1),
    ]
    .iter()
    .cloned()
    .max()
    .unwrap();
    for (title, old, new) in &[
        ("service:  ", &old_service, &new_service),
        ("contest:  ", &old_contest, &new_contest),
        ("language: ", &old_language, &new_language),
    ] {
        let old = old.as_ref().map(String::as_str).unwrap_or("~");
        let new = new.as_ref().map(String::as_str).unwrap_or("~");
        stdout.write_str(title)?;
        stdout.with_reset(|o| o.bold()?.write_str(old))?;
        stdout.write_spaces(max_width - old.len())?;
        stdout.write_str(" -> ")?;
        stdout.with_reset(|o| o.bold()?.write_str(new))?;
        stdout.write_str("\n")?;
    }

    crate::fs::write(&path, new_toml.as_bytes())?;
    writeln!(stdout, "Wrote {}", path.display())?;
    stdout.flush()?;

    let outcome = SwitchOutcome {
        old: SwitchOutcomeAttrs {
            service: old_config.service,
            contest_lower_case: old_config.contest.to_lowercase(),
            contest_upper_case: old_config.contest.to_uppercase(),
            contest_snake_case: old_config.contest.to_snake_case(),
            contest_kebab_case: old_config.contest.to_kebab_case(),
            contest_mixed_case: old_config.contest.to_mixed_case(),
            contest_pascal_case: old_config.contest.to_camel_case(),
            contest: old_config.contest,
            language: old_config.language,
        },
        new: SwitchOutcomeAttrs {
            service: new_config.service,
            contest_lower_case: new_config.contest.to_lowercase(),
            contest_upper_case: new_config.contest.to_uppercase(),
            contest_snake_case: new_config.contest.to_snake_case(),
            contest_kebab_case: new_config.contest.to_kebab_case(),
            contest_mixed_case: new_config.contest.to_mixed_case(),
            contest_pascal_case: new_config.contest.to_camel_case(),
            contest: new_config.contest.clone(),
            language: new_config.language.clone(),
        },
    };
    new_config.base_dir = directory.to_owned();
    Ok((new_config, outcome))
}

/// Config.
#[derive(Serialize, Deserialize)]
pub(crate) struct Config {
    #[serde(default)]
    service: ServiceKind,
    contest: String,
    language: String,
    #[serde(default)]
    console: Console,
    #[serde(default)]
    shell: HashMap<String, Vec<TemplateBuilder<OsString>>>,
    testfiles: Testfiles,
    session: Session,
    judge: Judge,
    #[serde(default)]
    env: BTreeMap<ServiceKind, HashMap<String, String>>,
    #[serde(default)]
    hooks: Hooks,
    tester: Option<Language>,
    languages: HashMap<String, Language>,
    #[serde(skip)]
    base_dir: AbsPathBuf,
}

impl Config {
    pub(crate) fn load(
        service: impl Into<Option<ServiceKind>>,
        contest: impl Into<Option<String>>,
        language: impl Into<Option<String>>,
        dir: &AbsPath,
    ) -> FileResult<Self> {
        let path = crate::fs::find_path(CONFIG_FILE_NAME, dir)?;
        let mut config = crate::fs::read_toml::<Self>(&path)?;
        config.base_dir = path.parent().unwrap().to_owned();
        config.service = service.into().unwrap_or(config.service);
        config.contest = contest.into().unwrap_or(config.contest);
        config.language = language.into().unwrap_or(config.language);
        Ok(config)
    }

    /// Gets `service`.
    pub(crate) fn service(&self) -> ServiceKind {
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
        let scraped = self.testfiles.path.build(AbsPathBufRequirements {
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
        let path = self.testfiles.path.build(AbsPathBufRequirements {
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
        Ok(lang
            .transpiled
            .as_ref()
            .unwrap_or(&lang.src)
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
                .build(TranspilationCommandRequirements {
                    base_dir: self.base_dir.clone(),
                    service: self.service,
                    contest: self.contest.clone(),
                    shell: self.shell.clone(),
                    working_dir: lang.working_directory.clone(),
                    src: lang.src.clone(),
                    transpiled: lang.transpiled.clone(),
                })
                .envs(self.env.get(&self.service))
        })
    }

    fn compilation_command(&self, lang: &Language) -> Option<Template<CompilationCommand>> {
        lang.compile.as_ref().map(|compile| {
            compile
                .build(CompilationCommandRequirements {
                    base_dir: self.base_dir.clone(),
                    service: self.service,
                    contest: self.contest.clone(),
                    shell: self.shell.clone(),
                    working_dir: lang.working_directory.clone(),
                    src: lang.src.clone(),
                    transpiled: lang.transpiled.clone(),
                    bin: lang.bin.clone(),
                })
                .envs(self.env.get(&self.service))
        })
    }

    fn judge_command(&self, lang: &Language) -> Template<JudgingCommand> {
        lang.run
            .build(JudgingCommandRequirements {
                base_dir: self.base_dir.clone(),
                service: self.service,
                contest: self.contest.clone(),
                shell: self.shell.clone(),
                working_dir: lang.working_directory.clone(),
                src: lang.src.clone(),
                transpiled: lang.transpiled.clone(),
                bin: lang.bin.clone(),
                crlf_to_lf: lang.crlf_to_lf,
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
struct Testfiles {
    path: TemplateBuilder<AbsPathBuf>,
}

#[derive(Serialize, Deserialize)]
struct Session {
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
    transpiled: Option<TemplateBuilder<AbsPathBuf>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    bin: Option<TemplateBuilder<AbsPathBuf>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    transpile: Option<TemplateBuilder<TranspilationCommand>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    compile: Option<TemplateBuilder<CompilationCommand>>,
    run: TemplateBuilder<JudgingCommand>,
    #[serde(default)]
    crlf_to_lf: bool,
    #[serde(default)]
    working_directory: TemplateBuilder<AbsPathBuf>,
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    language_ids: BTreeMap<ServiceKind, String>,
}

#[cfg(test)]
mod tests {
    use crate::config::Config;
    use crate::path::AbsPath;
    use crate::service::ServiceKind;
    use crate::terminal::Ansi;

    use failure::Fallible;
    use tempdir::TempDir;

    use std::str;

    #[test]
    fn test_init() -> Fallible<()> {
        let tempdir = TempDir::new("config_test_init")?;
        let mut out = Ansi::new(vec![]);

        super::init(&mut out, AbsPath::try_new(tempdir.path()).unwrap())?;

        let path = tempdir.path().join(super::CONFIG_FILE_NAME);
        let toml = std::fs::read_to_string(&path)?;
        toml::from_str::<Config>(&toml)?;
        assert_eq!(
            str::from_utf8(out.get_ref())?,
            format!("Wrote {}\n", path.display()),
        );
        Ok(())
    }

    #[test]
    fn test_switch() -> Fallible<()> {
        let tempdir = TempDir::new("config_test_init")?;
        let mut stdout = Ansi::new(vec![]);
        let mut stderr = Ansi::new(vec![]);

        let old_toml = super::generate_toml();
        let old_config = toml::from_str::<Config>(&old_toml)?;

        std::fs::write(tempdir.path().join(super::CONFIG_FILE_NAME), &old_toml)?;
        super::switch(
            &mut stdout,
            &mut stderr,
            AbsPath::try_new(tempdir.path()).unwrap(),
            Some(ServiceKind::Yukicoder),
            Some("no".to_owned()),
            Some("rust".to_owned()),
        )?;

        let new_toml = std::fs::read_to_string(tempdir.path().join(super::CONFIG_FILE_NAME))?;
        let new_config = toml::from_str::<Config>(&new_toml)?;

        assert_eq!(old_config.service, ServiceKind::Atcoder);
        assert_eq!(old_config.contest, "arc100");
        assert_eq!(old_config.language, "c++");
        assert_eq!(new_config.service, ServiceKind::Yukicoder);
        assert_eq!(new_config.contest, "no");
        assert_eq!(new_config.language, "rust");

        assert_eq!(
            str::from_utf8(stdout.get_ref())?,
            format!(
                "service:  \x1b[1m\"atcoder\"\x1b[0m -> \x1b[1m\"yukicoder\"\x1b[0m\n\
                 contest:  \x1b[1m\"arc100\"\x1b[0m  -> \x1b[1m\"no\"\x1b[0m\n\
                 language: \x1b[1m\"c++\"\x1b[0m     -> \x1b[1m\"rust\"\x1b[0m\n\
                 Wrote {}\n",
                tempdir.path().join(super::CONFIG_FILE_NAME).display(),
            ),
        );
        assert!(stderr.get_ref().is_empty());
        Ok(())
    }

    #[test]
    fn test_parse_size() {
        assert_eq!(super::parse_size("0"), Ok(0));
        assert_eq!(super::parse_size("1B"), Ok(1));
        assert_eq!(super::parse_size("1KB"), Ok(10usize.pow(3)));
        assert_eq!(super::parse_size("1KiB"), Ok(2usize.pow(10)));
        assert_eq!(super::parse_size("1MB"), Ok(10usize.pow(6)));
        assert_eq!(super::parse_size("1MiB"), Ok(2usize.pow(20)));
        assert_eq!(super::parse_size("1GB"), Ok(10usize.pow(9)));
        assert_eq!(super::parse_size("1GiB"), Ok(2usize.pow(30)));
        assert_eq!(super::parse_size("4.2KB"), Ok(4200));
        assert_eq!(super::parse_size("4.2KiB"), Ok(4300));
        assert_eq!(super::parse_size("1b"), Err("invalid format"));
        assert_eq!(super::parse_size("B"), Err("invalid format"));
        assert_eq!(super::parse_size("-0B"), Err("invalid format"));
        assert_eq!(super::parse_size("infB"), Err("invalid format"));
        assert_eq!(super::parse_size("NaNB"), Err("invalid format"));
    }
}
