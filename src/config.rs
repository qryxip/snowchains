use crate::command::{CompilationCommand, HookCommands, JudgingCommand, TranspilationCommand};
use crate::errors::{ConfigErrorKind, ConfigResult, FileResult};
use crate::path::{AbsPath, AbsPathBuf};
use crate::service::ServiceKind;
use crate::template::{
    AbsPathBufRequirements, CompilationCommandRequirements, HookCommandsRequirements,
    JudgingCommandRequirements, Template, TemplateBuilder, TranspilationCommandRequirements,
};
use crate::terminal::{TermOut, WriteSpaces as _};
use crate::testsuite::{DownloadDestinations, SuiteFileExtension, TestCaseLoader};
use crate::time;
use crate::util::combine::OnelinePosition;

use heck::{CamelCase as _, KebabCase as _, MixedCase as _, SnakeCase as _};
use if_chain::if_chain;
use indexmap::IndexMap;
use maplit::hashmap;
use serde::ser::SerializeMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_derive::{Deserialize, Serialize};
use strum_macros::EnumString;

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::ffi::OsString;
use std::fmt::{self, Write as _};
use std::io::Write;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;
use std::{env, iter, str};

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
    static CRLF_TO_LF_TRUE_COMMENTED_OUT: &str = "";
    #[cfg(windows)]
    static CRLF_TO_LF_TRUE_COMMENTED_OUT: &str = "\n# crlf_to_lf = true";

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
        r#"[languages.'c#']
src = "${service}/${snake_case(contest)}/cs/${pascal_case(problem)}/${pascal_case(problem)}.cs"
bin = "${service}/${snake_case(contest)}/cs/${pascal_case(problem)}/bin/Release/${pascal_case(problem)}.exe"
compile = ["mcs", "-o+", "-r:System.Numerics", "-out:${bin}", "${src}"]
run = ["mono", "${bin}"]
working_directory = "${service}/${snake_case(contest)}/cs"

[languages.'c#'.names]
atcoder = "C# (Mono 4.6.2.0)"
codeforces = "C# Mono 5.18"
yukicoder = "C#(mono) (mono 5.16.0.187)""#;
    #[cfg(windows)]
    static CSHARP: &str =
        r#"[languages.'c#']
src = "${service}/${snake_case(contest)}/cs/${pascal_case(problem)}/${pascal_case(problem)}.cs"
bin = "${service}/${snake_case(contest)}/cs/${pascal_case(problem)}/bin/Release/${pascal_case(problem)}.exe"
compile = ["csc", "/o+", "/r:System.Numerics", "/out:${bin}", "${src}"]
run = ["${bin}"]
crlf_to_lf = true
working_directory = "${service}/${snake_case(contest)}/cs"

[languages.'c#'.names]
atcoder = "C# (Mono 4.6.2.0)"
codeforces = "C# Mono 5.18"
yukicoder = "C# (csc 2.8.2.62916)""#;

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
                    "\nps = [{}, \"-Command\", \"${{command}}\"]",
                    quote_path_normalizing_separator(&p)
                )
            })
            .unwrap_or_default();
        let cmd = env::split_paths(&env_path)
            .map(|p| p.with_exe("cmd"))
            .find(|p| cfg!(windows) && p.exists())
            .map(|p| {
                format!(
                    "\ncmd = [{}, \"/C\", \"${{command}}\"]",
                    quote_path_normalizing_separator(&p)
                )
            })
            .unwrap_or_default();

        let (jq, shell, transpile_java, transpile_scala);
        if cfg!(windows) && !bash_found {
            jq = r#"ps = 'echo "${Env:SNOWCHAINS_RESULT}" | jq'"#;
            shell = "ps";
            transpile_java =
                r#"ps = 'Get-Content "${Env:SNOWCHAINS_SRC}" | ForEach-Object { $_.Replace("class\s+${Env:SNOWCHAINS_PROBLEM_PASCAL_CASE}", "class Main") } | sc "${Env:SNOWCHAINS_TRANSPILED}"'"#;
            transpile_scala =
                r#"ps = 'Get-Content "${Env:SNOWCHAINS_SRC}" | ForEach-Object { $_.Replace("object\s+${Env:SNOWCHAINS_PROBLEM_PASCAL_CASE}", "object Main") } | sc "${Env:SNOWCHAINS_TRANSPILED}"'"#;
        } else {
            jq = r#"bash = 'echo "$SNOWCHAINS_RESULT" | jq'"#;
            shell = "bash";
            transpile_java =
                r#"bash = 'cat "$SNOWCHAINS_SRC" | sed -r "s/class\s+$SNOWCHAINS_PROBLEM_PASCAL_CASE/class Main/g" > "$SNOWCHAINS_TRANSPILED"'"#;
            transpile_scala =
                r#"bash = 'cat "$SNOWCHAINS_SRC" | sed -r "s/object\s+$SNOWCHAINS_PROBLEM_PASCAL_CASE/object Main/g" > "$SNOWCHAINS_TRANSPILED"'"#;
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
    let (session_cookies, session_api_tokens, session_dropbox) = {
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
        let session_api_tokens = data_local_dir.join("api_tokens").join("${service}.json");
        let session_api_tokens = quote_path_normalizing_separator(&session_api_tokens);
        let session_dropbox = data_local_dir.join("dropbox.json");
        let session_dropbox = quote_path_normalizing_separator(&session_dropbox);
        (session_cookies, session_api_tokens, session_dropbox)
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
path = "${{service}}/${{snake_case(contest)}}/tests/${{snake_case(problem)}}.${{extension}}"

[session]
timeout = "60s"
silent = false
robots = true
cookies = {session_cookies}
api_tokens = {session_api_tokens}
dropbox = false
# dropbox = {{ auth: {session_dropbox} }}

[session.download]
extension = "yml"
text_file_dir = "${{service}}/${{snake_case(contest)}}/tests/${{snake_case(problem)}}"

[judge]
testfile_extensions = ["json", "toml", "yaml", "yml"]
# jobs = {judge_jobs}
display_limit = "1KiB"

[env.true]
CXXFLAGS = "-std=gnu++17 -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra"
RUST_VERSION = "stable"
RUST_OPT_LEVEL = "0"

[env.'mode = "release"']
CXXFLAGS = "-std=gnu++17 -O2 -Wall -Wextra"
RUST_OPT_LEVEL = "2"

[env.'and(service = "atcoder", mode = "debug")']
CXXFLAGS = "-std=gnu++1y -I/usr/include/boost -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra"

[env.'and(service = "atcoder", mode = "release")']
CXXFLAGS = "-std=gnu++1y -I/usr/include/boost -O2 -Wall -Wextra"
RUST_VERSION = "1.15.1"

[env.'and(service = "codeforces", mode = "debug")']
CXXFLAGS = "-std=gnu++17 -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra"

[env.'and(service = "codeforces", mode = "release")']
CXXFLAGS = "-std=gnu++17 -O2 -Wall -Wextra"
RUST_VERSION = "1.31.1"

[env.'and(service = "yukicoder", mode = "debug")']
CXXFLAGS = "-std=gnu++14 -lm -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra"

[env.'and(service = "yukicoder", mode = "release")']
CXXFLAGS = "-std=gnu++1z -lm -O2 -Wall -Wextra"
RUST_VERSION = "1.30.1"

# [hooks]
# switch = {{ {jq} }}
# download = {{ {jq} }}

[tester]
src = "testers/py/${{kebab_case(problem)}}.py"
run = {{ {shell} = '{tester_python3} "$SNOWCHAINS_SRC" $SNOWCHAINS_ARGS_JOINED' }}{crlf_to_lf_true}
working_directory = "testers/py"

# [tester]
# src = "testers/hs/app/${{pascal_case(problem)}}.hs"
# bin = "testers/hs/target/${{pascal_case(problem)}}"
# run = {{ {shell} = '"$SNOWCHAINS_BIN" "$SNOWCHAINS_SRC" $SNOWCHAINS_ARGS_JOINED' }}{crlf_to_lf_true_commented_out}
# working_directory = "testers/hs"

[languages.'c++']
src = "${{service}}/${{snake_case(contest)}}/cpp/${{kebab_case(problem)}}.cpp"
bin = "${{service}}/${{snake_case(contest)}}/cpp/build/${{kebab_case(problem)}}{exe}"
compile = {{ bash = 'g++ $CXXFLAGS -o "$SNOWCHAINS_BIN" "$SNOWCHAINS_SRC"' }}
run = ["${{bin}}"]{crlf_to_lf_true}
working_directory = "${{service}}/${{snake_case(contest)}}/cpp"

[languages.'c++'.names]
atcoder = "C++14 (GCC 5.4.1)"
codeforces = "GNU G++17 7.3.0"
yukicoder = "C++17(1z） (gcc 8.2.0)"

[languages.rust]
src = "${{service}}/${{snake_case(contest)}}/rs/src/bin/${{kebab_case(problem)}}.rs"
bin = "${{service}}/${{snake_case(contest)}}/rs/target/manually/${{mode}}/${{kebab_case(problem)}}"
compile = ["rustc", "+${{env:RUST_VERSION}}", "-C", "opt-level=${{env:RUST_OPT_LEVEL}}", "-o", "${{bin}}", "${{src}}"]
run = ["${{bin}}"]
working_directory = "${{service}}/${{snake_case(contest)}}/rs"

[languages.rust.names]
atcoder = "Rust (1.15.1)"
codeforces = "Rust 1.31.1"
yukicoder = "Rust (1.30.1)"

[languages.go]
src = "${{service}}/${{snake_case(contest)}}/go/${{kebab_case(problem)}}.go"
bin = "${{service}}/${{snake_case(contest)}}/go/${{kebab_case(problem)}}{exe}"
compile = ["go", "build", "-o", "${{bin}}", "${{src}}"]
run = ["${{bin}}"]{crlf_to_lf_false}
working_directory = "${{service}}/${{snake_case(contest)}}/go"

[languages.go.names]
atcoder = "Go (1.6)"
codeforces = "Go 1.11.4"
yukicoder = "Go (1.11.2)"

[languages.haskell]
src = "${{service}}/${{snake_case(contest)}}/hs/app/${{pascal_case(problem)}}.hs"
bin = "${{service}}/${{snake_case(contest)}}/hs/target/${{pascal_case(problem)}}{exe}"
compile = ["stack", "ghc", "--", "-O2", "-o", "${{bin}}", "${{src}}"]
run = ["${{bin}}"]{crlf_to_lf_false}
working_directory = "${{service}}/${{snake_case(contest)}}/hs"

[languages.haskell.names]
atcoder = "Haskell (GHC 7.10.3)"
codeforces = "Haskell GHC 8.6.3"
yukicoder = "Haskell (8.6.2)"

[languages.bash]
src = "${{service}}/${{snake_case(contest)}}/bash/${{kebab_case(problem)}}.bash"
run = ["bash", "${{src}}"]{crlf_to_lf_false}
working_directory = "${{service}}/${{snake_case(contest)}}/bash"

[languages.bash.names]
atcoder = "Bash (GNU bash v4.3.11)"
yukicoder = "Bash (Bash 4.2.46)"

[languages.python3]
src = "${{service}}/${{snake_case(contest)}}/py/${{kebab_case(problem)}}.py"
run = [{venv_python3}, "${{src}}"]{crlf_to_lf_true}
working_directory = "${{service}}/${{snake_case(contest)}}/py"

[languages.python3.names]
atcoder = "Python3 (3.4.3)"
codeforces = "Python 3.7.2"
yukicoder = "Python3 (3.7.1 + numpy 1.14.5 + scipy 1.1.0)"

[languages.pypy3]
src = "${{service}}/${{snake_case(contest)}}/py/${{kebab_case(problem)}}.py"
run = [{venv_pypy3}, "${{src}}"]{crlf_to_lf_true}
working_directory = "${{service}}/${{snake_case(contest)}}/py"

[languages.pypy3.names]
atcoder = "PyPy3 (2.4.0)"
codeforces = "PyPy 3.5 (6.0.0)"
yukicoder = "PyPy3 (6.0.0)"

[languages.java]
src = "${{service}}/${{snake_case(contest)}}/java/src/main/java/${{pascal_case(problem)}}.java"
transpiled = "${{service}}/${{snake_case(contest)}}/java/build/replaced/${{lower_case(pascal_case(problem))}}/src/Main.java"
bin = "${{service}}/${{snake_case(contest)}}/java/build/replaced/${{lower_case(pascal_case(problem))}}/classes/Main.class"
transpile = {{ {transpile_java} }}
compile = ["javac", "-d", "./build/replaced/${{lower_case(pascal_case(problem))}}/classes", "${{transpiled}}"]
run = ["java", "-classpath", "./build/replaced/${{lower_case(pascal_case(problem))}}/classes", "Main"]{crlf_to_lf_true}
working_directory = "${{service}}/${{snake_case(contest)}}/java"

[languages.java.names]
atcoder = "Java8 (OpenJDK 1.8.0)"
codeforces = "Java 1.8.0_162"
yukicoder = "Java8 (openjdk 1.8.0.191)"

[languages.scala]
src = "${{service}}/${{snake_case(contest)}}/scala/src/main/scala/${{pascal_case(problem)}}.scala"
transpiled = "${{service}}/${{snake_case(contest)}}/scala/target/replaced/${{lower_case(pascal_case(problem))}}/src/Main.scala"
bin = "${{service}}/${{snake_case(contest)}}/scala/target/replaced/${{lower_case(pascal_case(problem))}}/classes/Main.class"
transpile = {{ {transpile_scala} }}
compile = ["scalac", "-optimise", "-d", "./target/replaced/${{lower_case(pascal_case(problem))}}/classes", "${{transpiled}}"]
run = ["scala", "-classpath", "./target/replaced/${{lower_case(pascal_case(problem))}}/classes", "Main"]{crlf_to_lf_true}
working_directory = "${{service}}/${{snake_case(contest)}}/scala"

[languages.scala.names]
atcoder = "Scala (2.11.7)"
codeforces = "Scala 2.12.8"
yukicoder = "Scala(Beta) (2.12.7)"

{csharp}

[languages.text]
src = "${{service}}/${{snake_case(contest)}}/txt/${{snake_case(problem)}}.txt"
run = ["cat", "${{src}}"]
working_directory = "${{service}}/${{snake_case(contest)}}/txt"{crlf_to_lf_false}

[languages.text.names]
atcoder = "Text (cat)"
yukicoder = "Text (cat 8.22)"
"#,
        console_alt_width = CONSOLE_ALT_WIDTH,
        session_cookies = session_cookies,
        session_api_tokens = session_api_tokens,
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
        crlf_to_lf_true_commented_out = CRLF_TO_LF_TRUE_COMMENTED_OUT,
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
    contest: Option<&str>,
    language: Option<&str>,
) -> FileResult<(Config, SwitchOutcome)> {
    let path = crate::fs::find_path(CONFIG_FILE_NAME, directory)?;
    let old_toml = crate::fs::read_to_string(&path)?;
    let old_config = crate::fs::read_toml::<Config>(&path)?;
    stdout.apply_conf(&old_config.console);
    stderr.apply_conf(&old_config.console);

    let new_toml = {
        let mut doc = old_toml
            .parse::<toml_edit::Document>()
            .unwrap_or_else(|e| unimplemented!("{}", e));
        if let Some(service) = service {
            doc["service"] = toml_edit::value(<&str>::from(service));
        }
        if let Some(contest) = contest {
            doc["contest"] = toml_edit::value(contest);
        }
        if let Some(language) = language {
            doc["language"] = toml_edit::value(language);
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

#[derive(Clone, Copy, strum_macros::Display, Debug, EnumString, Serialize)]
#[strum(serialize_all = "snake_case")]
#[serde(rename_all = "snake_case")]
pub enum Mode {
    Debug,
    Release,
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
    env: Env,
    #[serde(default)]
    hooks: Hooks,
    tester: Option<Language>,
    languages: HashMap<String, Language>,
    #[serde(skip)]
    base_dir: AbsPathBuf,
}

impl Config {
    pub(crate) fn load(
        service: Option<ServiceKind>,
        contest: Option<&str>,
        language: Option<&str>,
        dir: &AbsPath,
    ) -> FileResult<Self> {
        let path = crate::fs::find_path(CONFIG_FILE_NAME, dir)?;
        let mut config = crate::fs::read_toml::<Self>(&path)?;
        config.base_dir = path.parent().unwrap().to_owned();
        config.service = service.unwrap_or(config.service);
        config.contest = contest.map(ToOwned::to_owned).unwrap_or(config.contest);
        config.language = language.map(ToOwned::to_owned).unwrap_or(config.language);
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

    /// Gets `console`.
    pub(crate) fn console(&self) -> &Console {
        &self.console
    }

    /// Gets `session.timeout`.
    pub(crate) fn session_timeout(&self) -> Option<Duration> {
        self.session.timeout
    }

    /// Gets `session.silent`.
    pub(crate) fn session_silent(&self) -> bool {
        self.session.silent
    }

    /// Gets `session.robots`.
    pub(crate) fn session_robots(&self) -> bool {
        self.session.robots
    }

    /// Gets `session.cookies`.
    pub(crate) fn session_cookies(&self) -> Template<AbsPathBuf> {
        self.build_path_template(&self.session.cookies, None)
    }

    /// Gets `session.api_tokens`.
    pub(crate) fn session_api_tokens(&self) -> Template<AbsPathBuf> {
        self.build_path_template(&self.session.api_tokens, None)
    }

    /// Gets `session.dropbox.auth` as `Option`.
    pub(crate) fn session_dropbox_auth(&self) -> Option<Template<AbsPathBuf>> {
        match &self.session.dropbox {
            Dropbox::None => None,
            Dropbox::Some { auth } => Some(self.build_path_template(&auth, None)),
        }
    }

    /// Gets `judge.jobs`.
    pub(crate) fn judge_jobs(&self) -> Option<NonZeroUsize> {
        self.judge.jobs
    }

    /// Gets `judge.display_limit`.
    pub(crate) fn judge_display_limit(&self) -> Option<usize> {
        self.judge.display_limit
    }

    /// Gets `hooks.switch`.
    pub(crate) fn switch_hooks(&self, outcome: &impl Serialize) -> Template<HookCommands> {
        self.hooks(|hs| &hs.switch, outcome)
    }

    /// Gets `hooks.download`.
    pub(crate) fn download_hooks(&self, outcome: &impl Serialize) -> Template<HookCommands> {
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

    /// Constructs a `DownloadDestinations`.
    pub(crate) fn download_destinations(
        &self,
        ext: Option<SuiteFileExtension>,
    ) -> DownloadDestinations {
        let scraped = self.build_path_template(&self.testfiles.path, None);
        let text_file_dir = self.build_path_template(&self.session.download.text_file_dir, None);
        let ext = ext.unwrap_or(self.session.download.extension);
        DownloadDestinations::new(scraped, text_file_dir, ext)
    }

    /// Constructs a `TestCaseLoader`.
    pub(crate) fn testcase_loader(&self, mode: Mode) -> ConfigResult<TestCaseLoader> {
        let tester_transpilation = self.tester_transpilation(mode)?;
        let tester_compilation = self.tester_compilation(mode)?;
        let tester = self.tester(mode)?;
        Ok(TestCaseLoader::new(
            self.build_path_template(&self.testfiles.path, Some(mode)),
            &self.judge.testfile_extensions,
            tester_transpilation,
            tester_compilation,
            tester,
        ))
    }

    /// Gets paths to the source files.
    pub(crate) fn src_paths(
        &self,
        mode: Mode,
    ) -> ConfigResult<HashMap<&str, Template<AbsPathBuf>>> {
        self.languages
            .values()
            .flat_map(|l| l.names.get(&self.service).map(|n| (l, n)))
            .map(|(lang, name)| {
                let template = self
                    .build_path_template(&lang.src, Some(mode))
                    .envs(self.env_vars(mode)?);
                Ok((name.as_ref(), template))
            })
            .collect()
    }

    /// Gets path to the source file to submit.
    pub(crate) fn src_to_submit(&self, mode: Mode) -> ConfigResult<Template<AbsPathBuf>> {
        let lang = self.find_language()?;
        let env_vars = self.env_vars(mode)?;
        Ok(self
            .build_path_template(lang.transpiled.as_ref().unwrap_or(&lang.src), Some(mode))
            .envs(env_vars))
    }

    fn build_path_template(
        &self,
        template: &TemplateBuilder<AbsPathBuf>,
        mode: Option<Mode>,
    ) -> Template<AbsPathBuf> {
        template.build(AbsPathBufRequirements {
            base_dir: self.base_dir.clone(),
            service: self.service,
            contest: self.contest.clone(),
            mode,
        })
    }

    pub(crate) fn lang_name(&self) -> ConfigResult<&str> {
        let lang = self.find_language()?;
        lang.names
            .get(&self.service)
            .map(AsRef::as_ref)
            .ok_or_else(|| {
                ConfigErrorKind::LangNameRequired(self.language.clone(), self.service).into()
            })
    }

    pub(crate) fn solver_compilation(
        &self,
        mode: Mode,
    ) -> ConfigResult<Option<Template<CompilationCommand>>> {
        let lang = self.find_language()?;
        self.compilation_command(lang, mode)
    }

    pub(crate) fn solver_transpilation(
        &self,
        mode: Mode,
    ) -> ConfigResult<Option<Template<TranspilationCommand>>> {
        let lang = self.find_language()?;
        self.transpilation_command(lang, mode)
    }

    pub(crate) fn solver(&self, mode: Mode) -> ConfigResult<Template<JudgingCommand>> {
        let lang = self.find_language()?;
        self.judge_command(lang, mode)
    }

    fn tester_transpilation(
        &self,
        mode: Mode,
    ) -> ConfigResult<Option<Template<TranspilationCommand>>> {
        match &self.tester {
            None => Ok(None),
            Some(tester) => self.transpilation_command(tester, mode),
        }
    }

    fn tester_compilation(&self, mode: Mode) -> ConfigResult<Option<Template<CompilationCommand>>> {
        match &self.tester {
            None => Ok(None),
            Some(tester) => self.compilation_command(tester, mode),
        }
    }

    fn tester(&self, mode: Mode) -> ConfigResult<Option<Template<JudgingCommand>>> {
        self.tester
            .as_ref()
            .map(|lang| self.judge_command(lang, mode))
            .transpose()
    }

    fn transpilation_command(
        &self,
        lang: &Language,
        mode: Mode,
    ) -> ConfigResult<Option<Template<TranspilationCommand>>> {
        lang.transpile
            .as_ref()
            .map(|transpile| {
                let env_vars = self.env_vars(mode)?;
                Ok(transpile
                    .build(TranspilationCommandRequirements {
                        base_dir: self.base_dir.clone(),
                        service: self.service,
                        contest: self.contest.clone(),
                        mode,
                        shell: self.shell.clone(),
                        working_dir: lang.working_directory.clone(),
                        src: lang.src.clone(),
                        transpiled: lang.transpiled.clone(),
                    })
                    .envs(env_vars))
            })
            .transpose()
    }

    fn compilation_command(
        &self,
        lang: &Language,
        mode: Mode,
    ) -> ConfigResult<Option<Template<CompilationCommand>>> {
        lang.compile
            .as_ref()
            .map(|compile| {
                let env_vars = self.env_vars(mode)?;
                Ok(compile
                    .build(CompilationCommandRequirements {
                        base_dir: self.base_dir.clone(),
                        service: self.service,
                        contest: self.contest.clone(),
                        mode,
                        shell: self.shell.clone(),
                        working_dir: lang.working_directory.clone(),
                        src: lang.src.clone(),
                        transpiled: lang.transpiled.clone(),
                        bin: lang.bin.clone(),
                    })
                    .envs(env_vars))
            })
            .transpose()
    }

    fn judge_command(&self, lang: &Language, mode: Mode) -> ConfigResult<Template<JudgingCommand>> {
        let env_vars = self.env_vars(mode)?;
        Ok(lang
            .run
            .build(JudgingCommandRequirements {
                base_dir: self.base_dir.clone(),
                service: self.service,
                contest: self.contest.clone(),
                mode,
                shell: self.shell.clone(),
                working_dir: lang.working_directory.clone(),
                src: lang.src.clone(),
                transpiled: lang.transpiled.clone(),
                bin: lang.bin.clone(),
                crlf_to_lf: lang.crlf_to_lf,
            })
            .envs(env_vars))
    }

    fn env_vars(&self, mode: Mode) -> ConfigResult<HashMap<String, String>> {
        let Self {
            service,
            contest,
            language,
            env,
            ..
        } = self;
        let prop_values = hashmap!(
            "service" => service.to_string(),
            "contest" => contest.clone(),
            "language" => language.clone(),
            "mode" => mode.to_string(),
        );
        let mut ret = hashmap!();
        for (pred, env_values) in &env.values {
            if pred.eval(&prop_values)? {
                ret.extend(env_values.iter().map(|(k, v)| (k.clone(), v.clone())));
            }
        }
        Ok(ret)
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
    #[serde(default = "const_true")]
    robots: bool,
    api_tokens: TemplateBuilder<AbsPathBuf>,
    cookies: TemplateBuilder<AbsPathBuf>,
    #[serde(default)]
    dropbox: Dropbox,
    download: Download,
}

const fn const_true() -> bool {
    true
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

#[derive(Default, Serialize, Deserialize)]
#[serde(transparent)]
struct Env {
    values: IndexMap<Predicate, BTreeMap<String, String>>,
}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
enum Predicate {
    True,
    False,
    Equal(Atom, Atom),
    Apply(String, Vec<Self>),
}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
enum Atom {
    Symbol(String),
    Literal(String),
}

impl Predicate {
    fn eval(&self, values: &HashMap<&'static str, String>) -> ConfigResult<bool> {
        fn eval_atom(atom: &Atom, values: &HashMap<&'static str, String>) -> ConfigResult<String> {
            match atom {
                Atom::Symbol(s) => values
                    .get(s.as_str())
                    .cloned()
                    .ok_or_else(|| ConfigErrorKind::UndefinedSymbol(s.to_owned()).into()),
                Atom::Literal(s) => Ok(s.clone()),
            }
        }

        match self {
            Predicate::True => Ok(true),
            Predicate::False => Ok(false),
            Predicate::Equal(l, r) => {
                let l = eval_atom(l, values)?;
                let r = eval_atom(r, values)?;
                Ok(l == r)
            }
            Predicate::Apply(f, xs) => match f.as_ref() {
                "not" => match xs.as_slice() {
                    [x] => x.eval(values).map(|p| !p),
                    xs => Err(ConfigErrorKind::WrongNumParamsForNot(xs.len()).into()),
                },
                "and" => {
                    for x in xs {
                        if !x.eval(values)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                }
                "or" => {
                    for x in xs {
                        if x.eval(values)? {
                            return Ok(true);
                        }
                    }
                    Ok(false)
                }
                f => Err(ConfigErrorKind::UndefinedFunction(f.to_owned()).into()),
            },
        }
    }
}

impl Serialize for Predicate {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        fn fmt_predicate(pred: &Predicate, fmt: &mut String) {
            match pred {
                Predicate::True => fmt.write_str("true").unwrap(),
                Predicate::False => fmt.write_str("false").unwrap(),
                Predicate::Equal(l, r) => {
                    fmt_atom(l, fmt);
                    fmt.write_str(" = ").unwrap();
                    fmt_atom(r, fmt);
                }
                Predicate::Apply(f, xs) => {
                    write!(fmt, "{}(", f).unwrap();
                    for (i, x) in xs.iter().enumerate() {
                        if i > 0 {
                            fmt.write_str(", ").unwrap();
                        }
                        fmt_predicate(x, fmt);
                    }
                    fmt.write_str(")").unwrap();
                }
            }
        }

        fn fmt_atom(atom: &Atom, fmt: &mut String) {
            match atom {
                Atom::Symbol(s) => fmt.write_str(s).unwrap(),
                Atom::Literal(s) => write!(fmt, "\"{}\"", s).unwrap(),
            }
        }

        let mut s = "".to_owned();
        fmt_predicate(self, &mut s);
        serializer.serialize_str(&s)
    }
}

impl<'de> Deserialize<'de> for Predicate {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        use combine::char::{char, spaces};
        use combine::parser::choice::or;
        use combine::stream::state::State;
        use combine::{choice, easy, eof, many, many1, none_of, optional, parser, satisfy};
        use combine::{ParseResult, Parser};

        fn parse_predicate<'a>(
            input: &mut easy::Stream<State<&'a str, OnelinePosition>>,
        ) -> ParseResult<Predicate, easy::Stream<State<&'a str, OnelinePosition>>> {
            enum EqRhsOrFnArg {
                EqRhs(Atom),
                FnArg(Vec<Predicate>),
            }

            spaces()
                .with(or(
                    literal()
                        .skip(spaces())
                        .skip(char('='))
                        .skip(spaces())
                        .and(atom())
                        .map(|(l, r)| Predicate::Equal(l, r)),
                    identifier()
                        .skip(spaces())
                        .and(optional(choice((
                            char('=')
                                .skip(spaces())
                                .with(atom())
                                .map(EqRhsOrFnArg::EqRhs),
                            char('(')
                                .skip(spaces())
                                .with(optional(
                                    parser(parse_predicate).and(many::<Vec<_>, _>(
                                        spaces()
                                            .skip(char(','))
                                            .skip(spaces())
                                            .with(parser(parse_predicate)),
                                    )),
                                ))
                                .skip(spaces())
                                .skip(char(')'))
                                .map(|args| {
                                    let args = args
                                        .map(|(arg0, rest)| {
                                            let mut args = vec![arg0];
                                            args.extend(rest);
                                            args
                                        })
                                        .unwrap_or_default();
                                    EqRhsOrFnArg::FnArg(args)
                                }),
                        ))))
                        .and_then(|(l, r)| match r {
                            None => match l.as_ref() {
                                "true" => Ok(Predicate::True),
                                "false" => Ok(Predicate::False),
                                _ => Err(easy::Error::Message(easy::Info::Borrowed(
                                    "Only `true` and `false` are allowed",
                                ))),
                            },
                            Some(EqRhsOrFnArg::EqRhs(r)) => {
                                Ok(Predicate::Equal(Atom::Symbol(l), r))
                            }
                            Some(EqRhsOrFnArg::FnArg(r)) => Ok(Predicate::Apply(l, r)),
                        }),
                ))
                .skip(spaces())
                .parse_stream(input)
        }

        fn atom<'a>(
        ) -> impl Parser<Input = easy::Stream<State<&'a str, OnelinePosition>>, Output = Atom>
        {
            or(identifier().map(Atom::Symbol), literal())
        }

        fn literal<'a>(
        ) -> impl Parser<Input = easy::Stream<State<&'a str, OnelinePosition>>, Output = Atom>
        {
            or(literal_with('\''), literal_with('"'))
        }

        fn literal_with<'a>(
            quote: char,
        ) -> impl Parser<Input = easy::Stream<State<&'a str, OnelinePosition>>, Output = Atom>
        {
            char(quote)
                .with(many1(none_of(iter::once(quote))))
                .skip(char(quote))
                .map(Atom::Literal)
        }

        fn identifier<'a>(
        ) -> impl Parser<Input = easy::Stream<State<&'a str, OnelinePosition>>, Output = String>
        {
            many1(satisfy(|c| match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
                _ => false,
            }))
        }

        let input = String::deserialize(deserializer)?;
        parser(parse_predicate)
            .skip(eof())
            .easy_parse(State::with_positioner(&input, OnelinePosition::new()))
            .map(|(p, _)| p)
            .map_err(serde::de::Error::custom)
    }
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
    names: BTreeMap<ServiceKind, String>,
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
            Some("no"),
            Some("rust"),
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
