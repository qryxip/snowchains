use crate::command::{CompilationCommand, HookCommands, JudgingCommand, TranspilationCommand};
use crate::errors::{ConfigError, ConfigErrorKind, ConfigResult, FileResult};
use crate::outcome::Outcome;
use crate::path::{AbsPath, AbsPathBuf};
use crate::service::ServiceKind;
use crate::template::{
    AbsPathBufRequirements, CompilationCommandRequirements, HookCommandsRequirements,
    JudgingCommandRequirements, Template, TemplateBuilder, TranspilationCommandRequirements,
};
use crate::terminal::{HasTermProps, ModifyTermProps, WriteExt as _};
use crate::testsuite::{Destinations, SuiteFileExtension, TestCaseLoader};
use crate::time;
use crate::util::combine::ParseFieldError;

use snowchains_proc_macros::{ArgEnum, DeserializeAsString, SerializeAsString};

use heck::{CamelCase as _, KebabCase as _, MixedCase as _, SnakeCase as _};
use if_chain::if_chain;
use indexmap::{indexmap, IndexMap};
use maplit::hashmap;
use matches::matches;
use once_cell::sync::Lazy;
use serde::ser::SerializeMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use termcolor::WriteColor;
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::ffi::OsString;
use std::io::{self, Write};
use std::num::NonZeroUsize;
use std::ops::Not;
use std::path::{Path, PathBuf};
use std::str::{self, FromStr};
use std::time::Duration;
use std::{env, fmt};

static CONFIG_FILE_NAME: &str = "snowchains.toml";

/// Creates "snowchains.toml" in `directory`.
pub(crate) fn init(mut stderr: impl Write, directory: &AbsPath) -> FileResult<()> {
    static TARGET_JSON: &str = r#"{
  "service": "atcoder",
  "contest": "arc100",
  "language": "c++"
}
"#;
    let toml_path = directory.join(CONFIG_FILE_NAME);
    let toml = generate_toml();
    let target_path = directory.join(".snowchains").join("target.json");
    for (path, content) in &[(toml_path, toml.as_str()), (target_path, TARGET_JSON)] {
        crate::fs::write(path, content.as_bytes())?;
        writeln!(stderr, "Wrote {}", path.display())?;
    }
    stderr.flush().map_err(Into::into)
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
    static CSHARP: &str = r#"[languages.'c#']
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
    static CSHARP: &str = r#"[languages.'c#']
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

    let (bash, powershell, cmd, ruby, shell, transpile_java, transpile_scala) = {
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

        let ruby = env::split_paths(&env_path)
            .map(|p| p.with_exe("ruby"))
            .find(|p| p.exists() && p.to_str().is_some())
            .unwrap_or_else(|| {
                PathBuf::from(if cfg!(windows) {
                    r"C:\Ruby26-x64"
                } else {
                    "/usr/bin/ruby"
                })
            });
        let ruby = format!(
            "\nruby = {{ runner = {}, extension = \"rb\" }}",
            quote_path_normalizing_separator(&ruby),
        );

        let (shell, transpile_java, transpile_scala);
        if cfg!(windows) && !bash_found {
            shell = "ps";
            transpile_java = r#"ps = 'Get-Content "${Env:SNOWCHAINS_SRC}" | ForEach-Object { $_.Replace("class\s+${Env:SNOWCHAINS_PROBLEM_PASCAL_CASE}", "class Main") } | sc "${Env:SNOWCHAINS_TRANSPILED}"'"#;
            transpile_scala = r#"ps = 'Get-Content "${Env:SNOWCHAINS_SRC}" | ForEach-Object { $_.Replace("object\s+${Env:SNOWCHAINS_PROBLEM_PASCAL_CASE}", "object Main") } | sc "${Env:SNOWCHAINS_TRANSPILED}"'"#;
        } else {
            shell = "bash";
            transpile_java = r#"bash = 'cat "$SNOWCHAINS_SRC" | sed -r "s/class\s+$SNOWCHAINS_PROBLEM_PASCAL_CASE/class Main/g" > "$SNOWCHAINS_TRANSPILED"'"#;
            transpile_scala = r#"bash = 'cat "$SNOWCHAINS_SRC" | sed -r "s/object\s+$SNOWCHAINS_PROBLEM_PASCAL_CASE/object Main/g" > "$SNOWCHAINS_TRANSPILED"'"#;
        };

        (
            bash,
            powershell,
            cmd,
            ruby,
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
        let session_cookies = data_local_dir.join("cookies.json");
        let session_cookies = quote_path_normalizing_separator(&session_cookies);
        let session_api_tokens = data_local_dir.join("api_tokens").join("${service}.json");
        let session_api_tokens = quote_path_normalizing_separator(&session_api_tokens);
        let session_dropbox = data_local_dir.join("dropbox.json");
        let session_dropbox = quote_path_normalizing_separator(&session_dropbox);
        (session_cookies, session_api_tokens, session_dropbox)
    };
    let judge_jobs = num_cpus::get();

    format!(
        r#"target = ".snowchains/target.json"

[console]
cjk = false{console_alt_width}

[shell]
{bash}{powershell}{cmd}{ruby}

[testfiles]
path = ".snowchains/tests/${{service}}/${{snake_case(contest)}}/${{snake_case(problem)}}.${{extension}}"

[session]
timeout = "60s"
silent = false
robots = true
cookies = {session_cookies}
api_tokens = {session_api_tokens}
dropbox = false
# dropbox = {{ auth: {session_dropbox} }}

[session.retry]
retries = 2
method = ["get"]

[session.retrieve]
extension = "yml"
text_file_dir = ".snowchains/tests/${{service}}/${{snake_case(contest)}}/${{snake_case(problem)}}"

[judge]
testfile_extensions = ["json", "toml", "yaml", "yml"]
# jobs = {judge_jobs}
display_limit = "1KiB"

[env."t"]
CXXFLAGS = "-std=gnu++17 -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra"
RUST_VERSION = "stable"
RUST_OPT_LEVEL = "0"

[env."(equal mode 'release)"]
CXXFLAGS = "-std=gnu++17 -O2 -Wall -Wextra"
RUST_OPT_LEVEL = "2"

[env."(equal '(service mode) '('atcoder 'debug))"]
CXXFLAGS = "-std=gnu++1y -I/usr/include/boost -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra"

[env."(equal '(service mode) '('atcoder 'release))"]
CXXFLAGS = "-std=gnu++1y -I/usr/include/boost -O2 -Wall -Wextra"
RUST_VERSION = "1.15.1"

[env."(equal '(service mode) '('codeforces 'debug))"]
CXXFLAGS = "-std=gnu++17 -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra"

[env."(equal '(service mode) '('codeforces 'release))"]
CXXFLAGS = "-std=gnu++17 -O2 -Wall -Wextra"
RUST_VERSION = "1.31.1"

[env."(equal '(service mode) '('yukicoder 'debug))"]
CXXFLAGS = "-std=gnu++14 -lm -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra"

[env."(equal '(service mode) '('yukicoder 'release))"]
CXXFLAGS = "-std=gnu++1z -lm -O2 -Wall -Wextra"
RUST_VERSION = "1.30.1"

# [hooks]
# switch = ['jq']
# login = ['jq']
# participate = ['jq']
# judge = ['jq']
# submit = ['jq']

# [hooks.retrieve]
# testcases = ['jq']
# submissions = ['jq']
# languages = ['jq']

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
yukicoder = "C++17(1z) (gcc 8.2.0)"

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
        ruby = ruby,
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

#[derive(Debug, Serialize)]
pub(crate) struct SwitchOutcome {
    old: SwitchOutcomeAttrs,
    new: SwitchOutcomeAttrs,
}

impl Outcome for SwitchOutcome {
    fn is_success(&self) -> bool {
        true
    }

    fn print_pretty(&self, _: bool, mut stdout: impl WriteColor + HasTermProps) -> io::Result<()> {
        let old_service = Some(format!("{:?}", <&str>::from(self.old.service)));
        let old_contest = Some(format!("{:?}", self.old.contest));
        let old_language = Some(format!("{:?}", self.old.language));
        let new_service = Some(format!("{:?}", <&str>::from(self.new.service)));
        let new_contest = Some(format!("{:?}", self.new.contest));
        let new_language = Some(format!("{:?}", self.new.language));

        let str_width = stdout.str_width_fn();
        let max_width = [
            old_service.as_deref().map(str_width).unwrap_or(1),
            old_contest.as_deref().map(str_width).unwrap_or(1),
            old_language.as_deref().map(str_width).unwrap_or(1),
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
            let old = old.as_deref().unwrap_or("~");
            let new = new.as_deref().unwrap_or("~");
            stdout.write_str(title)?;
            stdout.set_color(color!(bold))?;
            stdout.write_str(old)?;
            stdout.reset()?;
            stdout.write_spaces(max_width - str_width(old))?;
            stdout.write_str(" -> ")?;
            stdout.set_color(color!(bold))?;
            stdout.write_str(new)?;
            stdout.reset()?;
            writeln!(stdout)?;
        }
        stdout.flush()
    }
}

#[derive(Debug, Serialize)]
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
    mut stdout: impl ModifyTermProps,
    mut stderr: impl Write + ModifyTermProps,
    directory: &AbsPath,
    service: Option<ServiceKind>,
    contest: Option<&str>,
    language: Option<&str>,
) -> FileResult<(Config, SwitchOutcome)> {
    let path = crate::fs::find_path(CONFIG_FILE_NAME, directory)?;
    let base_dir = path.parent().unwrap().to_owned();
    let inner = crate::fs::read_toml::<Inner>(&path)?;
    let path = directory.join_expanding_user(&inner.target)?;
    let old_target = crate::fs::read_json::<Target>(&path)?;
    let new_target = Target {
        service: service.unwrap_or(old_target.service),
        contest: contest.unwrap_or(&old_target.contest).to_owned(),
        language: language.unwrap_or(&old_target.language).to_owned(),
    };

    crate::fs::write_json_pretty(&path, &new_target)?;
    writeln!(stderr, "Wrote {}", path.display())?;
    stderr.flush()?;

    inner.console.modify_term_props(&mut stdout, &mut stderr);

    let outcome = SwitchOutcome {
        old: SwitchOutcomeAttrs {
            service: old_target.service,
            contest_lower_case: old_target.contest.to_lowercase(),
            contest_upper_case: old_target.contest.to_uppercase(),
            contest_snake_case: old_target.contest.to_snake_case(),
            contest_kebab_case: old_target.contest.to_kebab_case(),
            contest_mixed_case: old_target.contest.to_mixed_case(),
            contest_pascal_case: old_target.contest.to_camel_case(),
            contest: old_target.contest,
            language: old_target.language,
        },
        new: SwitchOutcomeAttrs {
            service: new_target.service,
            contest_lower_case: new_target.contest.to_lowercase(),
            contest_upper_case: new_target.contest.to_uppercase(),
            contest_snake_case: new_target.contest.to_snake_case(),
            contest_kebab_case: new_target.contest.to_kebab_case(),
            contest_mixed_case: new_target.contest.to_mixed_case(),
            contest_pascal_case: new_target.contest.to_camel_case(),
            contest: new_target.contest.clone(),
            language: new_target.language.clone(),
        },
    };
    let new_config = Config {
        target: new_target,
        inner,
        base_dir,
    };
    Ok((new_config, outcome))
}

#[derive(Clone, Copy, Debug, ArgEnum, Serialize)]
#[arg_enum(rename_all = "kebab-case")]
#[serde(rename_all = "kebab-case")]
pub enum Mode {
    Debug,
    Release,
}

/// Config.
#[derive(Debug)]
pub(crate) struct Config {
    inner: Inner,
    target: Target,
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
        let inner = crate::fs::read_toml::<Inner>(&path)?;
        let base_dir = path.parent().unwrap().to_owned();
        let path = base_dir.join_expanding_user(&inner.target)?;
        let mut target = crate::fs::read_json::<Target>(&path)?;
        target.service = service.unwrap_or(target.service);
        target.contest = contest.map(ToOwned::to_owned).unwrap_or(target.contest);
        target.language = language.map(ToOwned::to_owned).unwrap_or(target.language);
        Ok(Self {
            target,
            inner,
            base_dir,
        })
    }

    pub(crate) fn inner(&self) -> &Inner {
        &self.inner
    }

    pub(crate) fn target_with_case_converted_names(&self) -> IndexMap<&'static str, String> {
        indexmap! {
            "service" => self.target.service.to_string(),
            "contest" => self.target.contest.clone(),
            "contest_lower_case" => self.target.contest.to_lowercase(),
            "contest_upper_case" => self.target.contest.to_uppercase(),
            "contest_snake_case" => self.target.contest.to_snake_case(),
            "contest_kebab_case" => self.target.contest.to_kebab_case(),
            "contest_mixed_case" => self.target.contest.to_mixed_case(),
            "contest_pascal_case" => self.target.contest.to_camel_case(),
            "language" => self.target.language.clone(),
        }
    }

    pub(crate) fn base_dir(&self) -> &AbsPath {
        &self.base_dir
    }

    /// Gets `service`.
    pub(crate) fn service(&self) -> ServiceKind {
        self.target.service
    }

    /// Gets `contest`.
    pub(crate) fn contest(&self) -> &str {
        &self.target.contest
    }

    /// Gets `console`.
    pub(crate) fn console(&self) -> &Console {
        &self.inner.console
    }

    /// Gets `session.timeout`.
    pub(crate) fn session_timeout(&self) -> Option<Duration> {
        self.inner.session.timeout
    }

    /// Gets `session.silent`.
    pub(crate) fn session_silent(&self) -> bool {
        self.inner.session.silent
    }

    /// Gets `session.robots`.
    pub(crate) fn session_robots(&self) -> bool {
        self.inner.session.robots
    }

    /// Gets `session.cookies`.
    pub(crate) fn session_cookies(&self) -> Template<AbsPathBuf> {
        self.build_path_template(&self.inner.session.cookies, None)
    }

    /// Gets `session.api_tokens`.
    pub(crate) fn session_api_tokens(&self) -> Template<AbsPathBuf> {
        self.build_path_template(&self.inner.session.api_tokens, None)
    }

    /// Gets `session.dropbox.auth` as `Option`.
    pub(crate) fn session_dropbox_auth(&self) -> Option<Template<AbsPathBuf>> {
        match &self.inner.session.dropbox {
            Dropbox::None => None,
            Dropbox::Some { auth } => Some(self.build_path_template(&auth, None)),
        }
    }

    pub(crate) fn session_retries_on_get(&self) -> u32 {
        if self.inner.session.retry.method.contains(&RetryMethod::Get) {
            self.inner.session.retry.retries
        } else {
            0
        }
    }

    /// Gets `judge.jobs`.
    pub(crate) fn judge_jobs(&self) -> Option<NonZeroUsize> {
        self.inner.judge.jobs
    }

    /// Gets `judge.display_limit`.
    pub(crate) fn judge_display_limit(&self) -> Option<usize> {
        self.inner.judge.display_limit
    }

    pub(crate) fn hooks(&self, kind: SubCommandKind) -> Template<HookCommands> {
        static DEFAULT: Lazy<TemplateBuilder<HookCommands>> = Lazy::new(TemplateBuilder::default);
        self.inner
            .hooks
            .get(kind)
            .unwrap_or_else(|| &DEFAULT)
            .build(HookCommandsRequirements {
                base_dir: self.base_dir.clone(),
                shell: self.inner.shell.clone(),
            })
    }

    /// Constructs a `Destinations`.
    pub(crate) fn destinations(&self, ext: Option<SuiteFileExtension>) -> Destinations {
        let scraped = self.build_path_template(&self.inner.testfiles.path, None);
        let text_file_dir =
            self.build_path_template(&self.inner.session.retrieve.text_file_dir, None);
        let ext = ext.unwrap_or(self.inner.session.retrieve.extension);
        Destinations::new(scraped, text_file_dir, ext)
    }

    /// Constructs a `TestCaseLoader`.
    pub(crate) fn testcase_loader(&self, mode: Mode) -> ConfigResult<TestCaseLoader> {
        let tester_transpilation = self.tester_transpilation(mode)?;
        let tester_compilation = self.tester_compilation(mode)?;
        let tester = self.tester(mode)?;
        Ok(TestCaseLoader::new(
            self.build_path_template(&self.inner.testfiles.path, Some(mode)),
            &self.inner.judge.testfile_extensions,
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
        self.inner
            .languages
            .values()
            .flat_map(|l| l.names.get(&self.target.service).map(|n| (l, n)))
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
            service: self.target.service,
            contest: self.target.contest.clone(),
            mode,
        })
    }

    pub(crate) fn lang_name(&self) -> ConfigResult<&str> {
        let lang = self.find_language()?;
        lang.names
            .get(&self.target.service)
            .map(AsRef::as_ref)
            .ok_or_else(|| {
                ConfigErrorKind::LangNameRequired(self.target.language.clone(), self.target.service)
                    .into()
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
        match &self.inner.tester {
            None => Ok(None),
            Some(tester) => self.transpilation_command(tester, mode),
        }
    }

    fn tester_compilation(&self, mode: Mode) -> ConfigResult<Option<Template<CompilationCommand>>> {
        match &self.inner.tester {
            None => Ok(None),
            Some(tester) => self.compilation_command(tester, mode),
        }
    }

    fn tester(&self, mode: Mode) -> ConfigResult<Option<Template<JudgingCommand>>> {
        self.inner
            .tester
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
                        service: self.target.service,
                        contest: self.target.contest.clone(),
                        mode,
                        shell: self.inner.shell.clone(),
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
                        service: self.target.service,
                        contest: self.target.contest.clone(),
                        mode,
                        shell: self.inner.shell.clone(),
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
                service: self.target.service,
                contest: self.target.contest.clone(),
                mode,
                shell: self.inner.shell.clone(),
                working_dir: lang.working_directory.clone(),
                src: lang.src.clone(),
                transpiled: lang.transpiled.clone(),
                bin: lang.bin.clone(),
                crlf_to_lf: lang.crlf_to_lf,
            })
            .envs(env_vars))
    }

    fn env_vars(&self, mode: Mode) -> ConfigResult<HashMap<String, String>> {
        let prop_values = indexmap!(
            "service" => self.target.service.to_string(),
            "contest" => self.target.contest.clone(),
            "language" => self.target.language.clone(),
            "mode" => mode.to_string(),
        );
        let mut ret = hashmap!();
        for (pred, env_values) in &self.inner.env.values {
            if pred.eval(&prop_values)? {
                ret.extend(env_values.iter().map(|(k, v)| (k.clone(), v.clone())));
            }
        }
        Ok(ret)
    }

    fn find_language(&self) -> ConfigResult<&Language> {
        self.inner
            .languages
            .get(&self.target.language)
            .ok_or_else(|| ConfigErrorKind::NoSuchLanguage(self.target.language.clone()).into())
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct Inner {
    target: PathBuf,
    #[serde(default)]
    console: Console,
    #[serde(default)]
    shell: IndexMap<String, Shell>,
    testfiles: Testfiles,
    session: Session,
    judge: Judge,
    #[serde(default)]
    env: Env,
    #[serde(default)]
    hooks: Hooks,
    tester: Option<Language>,
    languages: HashMap<String, Language>,
}

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct Console {
    #[serde(default)]
    cjk: bool,
    alt_width: Option<usize>,
}

impl Console {
    pub(crate) fn modify_term_props(
        &self,
        mut stdout: impl ModifyTermProps,
        mut stderr: impl ModifyTermProps,
    ) {
        let char_width: fn(char) -> Option<usize> = if self.cjk {
            UnicodeWidthChar::width_cjk
        } else {
            UnicodeWidthChar::width
        };
        let str_width: fn(&str) -> usize = if self.cjk {
            UnicodeWidthStr::width_cjk
        } else {
            UnicodeWidthStr::width
        };

        stdout.modify_term_props(|props| {
            props.char_width = char_width;
            props.str_width = str_width
        });
        stderr.modify_term_props(|props| {
            props.char_width = char_width;
            props.str_width = str_width
        });
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum Shell {
    Args(Vec<TemplateBuilder<OsString>>),
    File {
        runner: TemplateBuilder<OsString>,
        extension: TemplateBuilder<OsString>,
    },
}

#[derive(Debug, Serialize, Deserialize)]
struct Testfiles {
    path: TemplateBuilder<AbsPathBuf>,
}

#[derive(Debug, Serialize, Deserialize)]
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
    #[serde(default)]
    retry: Retry,
    retrieve: Retrieve,
}

const fn const_true() -> bool {
    true
}

#[derive(Debug)]
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

#[derive(Debug, Default, Serialize, Deserialize)]
struct Retry {
    retries: u32,
    method: BTreeSet<RetryMethod>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
enum RetryMethod {
    Get,
}

#[derive(Debug, Serialize, Deserialize)]
struct Retrieve {
    extension: SuiteFileExtension,
    text_file_dir: TemplateBuilder<AbsPathBuf>,
}

#[derive(Debug, Serialize, Deserialize)]
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

fn parse_size(s: &str) -> std::result::Result<usize, ParseFieldError<&str>> {
    use combine::char::{char, digit, string};
    use combine::parser::choice::or;
    use combine::parser::range::recognize;
    use combine::stream::state::{IndexPositioner, State};
    use combine::{choice, eof, optional, skip_many, skip_many1, Parser as _};

    static GRAMMER: &str = r#"Size  ::= Float Unit
Unit  ::= ( 'B' | ( [KMG] ( 'B' | 'iB' ) ) )?
Float ::= ( Digit+ ( '.' Digit* )? | '.' Digit+ ) Exp?
Exp   ::= [eE] [+-]? Digit+
Digit ::= [0-9]
"#;

    let exp = or(char('e'), char('E'))
        .and(optional(or(char('+'), char('-'))))
        .and(skip_many1(digit()));

    let float = recognize(
        or(
            skip_many1(digit())
                .and(optional(char('.').and(skip_many(digit()))))
                .map(|_| ()),
            char('.').and(skip_many1(digit())).map(|_| ()),
        )
        .and(optional(exp)),
    )
    .and_then(f64::from_str);

    let unit = optional(or(
        char('B').map(|_| 1.0),
        choice((char('K'), char('M'), char('G')))
            .and(or(string("B"), string("iB")))
            .map(|(c, s)| match (c, s) {
                ('K', "B") => 1000.0,
                ('K', "iB") => f64::from(0x400),
                ('M', "B") => f64::from(1_000_000),
                ('M', "iB") => f64::from(0x100_000),
                ('G', "B") => f64::from(1_000_000_000),
                ('G', "iB") => f64::from(0x40_000_000),
                _ => unreachable!(),
            }),
    ))
    .map(|o| o.unwrap_or(1.0));

    let ((float, unit), _) = float
        .and(unit)
        .skip(eof())
        .easy_parse(State::with_positioner(s, IndexPositioner::new()))
        .map_err(|e| ParseFieldError::new(s, e, GRAMMER))?;
    let size = float * unit;
    debug_assert!(size.is_sign_positive() && size.is_finite());
    Ok(size as usize)
}

#[derive(Default, Debug, Serialize, Deserialize)]
#[serde(transparent)]
struct Env {
    values: IndexMap<Predicate, BTreeMap<String, String>>,
}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Hash, SerializeAsString, DeserializeAsString)]
enum Predicate {
    T,
    LitStr(String),
    Symbol(String),
    Var(String),
    List(Vec<Self>),
    Apply(String, Vec<Self>),
}

impl Predicate {
    fn eval(&self, symbols: &IndexMap<&'static str, String>) -> ConfigResult<bool> {
        #[derive(PartialEq)]
        enum Atom<'a> {
            T,
            Str(&'a str),
            Symbol(&'a str),
            List(Vec<Self>),
        }

        impl<'a> From<bool> for Atom<'a> {
            fn from(p: bool) -> Self {
                if p {
                    Atom::T
                } else {
                    Atom::List(vec![])
                }
            }
        }

        impl<'a> From<Atom<'a>> for bool {
            fn from(atom: Atom<'a>) -> bool {
                match atom {
                    Atom::T | Atom::Str(_) | Atom::Symbol(_) => true,
                    Atom::List(l) => !l.is_empty(),
                }
            }
        }

        impl Not for Atom<'_> {
            type Output = Self;

            fn not(self) -> Self {
                Self::from(!bool::from(self))
            }
        }

        fn eval_as_atom<'a>(
            predicate: &'a Predicate,
            symbols: &'a IndexMap<&'static str, String>,
        ) -> ConfigResult<Atom<'a>> {
            match predicate {
                Predicate::T => Ok(Atom::T),
                Predicate::LitStr(s) => Ok(Atom::Str(s)),
                Predicate::Symbol(s) => Ok(Atom::Symbol(s)),
                Predicate::Var(x) => symbols
                    .get(x.as_str())
                    .map(|s| Atom::Symbol(s))
                    .ok_or_else(|| undefined_variable_error(predicate, x, symbols.keys())),
                Predicate::List(l) => l
                    .iter()
                    .map(|p| eval_as_atom(p, symbols))
                    .collect::<ConfigResult<Vec<_>>>()
                    .map(Atom::List),
                Predicate::Apply(f, xs) => match f.as_ref() {
                    "and" => xs
                        .iter()
                        .map(|x| eval_as_atom(x, symbols))
                        .collect::<ConfigResult<Vec<_>>>()
                        .map(|atoms| Atom::from(atoms.into_iter().all(Into::into))),
                    "or" => xs
                        .iter()
                        .map(|x| eval_as_atom(x, symbols))
                        .collect::<ConfigResult<Vec<_>>>()
                        .map(|atoms| Atom::from(atoms.into_iter().any(Into::into))),
                    "not" => match &xs[..] {
                        [x] => eval_as_atom(x, symbols).map(Not::not),
                        xs => Err(wrong_num_params_error(predicate, "not", 1, xs.len())),
                    },
                    "equal" => match &xs[..] {
                        [x1, x2] => {
                            let (x1, x2) = (eval_as_atom(x1, symbols)?, eval_as_atom(x2, symbols)?);
                            Ok(Atom::from(x1 == x2))
                        }
                        xs => Err(wrong_num_params_error(predicate, "equal", 2, xs.len())),
                    },
                    f => Err(undefined_function_error(predicate, f)),
                },
            }
        }

        fn undefined_variable_error<'a>(
            predicate: &Predicate,
            x: &str,
            expected: impl Iterator<Item = &'a &'static str>,
        ) -> ConfigError {
            let msg = format!(
                "Undefined variable: {:?} (expected: {:?})",
                x,
                expected.collect::<Vec<_>>(),
            );
            error(predicate, msg)
        }

        fn undefined_function_error(predicate: &Predicate, f: &str) -> ConfigError {
            let msg = format!(
                r#"Undefined function: {:?} (expected "and", "or", "not", or "equal")"#,
                f,
            );
            error(predicate, msg)
        }

        fn wrong_num_params_error(
            predicate: &Predicate,
            name: &'static str,
            expected: usize,
            actual: usize,
        ) -> ConfigError {
            let msg = format!(
                "{:?} takes {} but {} supplied",
                name,
                plural!(expected, "parameter", "parameters"),
                plural!(actual, "parameter was", "parameters were"),
            );
            error(predicate, msg)
        }

        fn error(predicate: &Predicate, msg: String) -> ConfigError {
            failure::err_msg(msg)
                .context(ConfigErrorKind::Eval(predicate.to_string()))
                .into()
        }

        eval_as_atom(self, symbols).map(Into::into)
    }
}

impl fmt::Display for Predicate {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Predicate::T => fmt::Display::fmt("t", fmt),
            Predicate::Var(s) => fmt::Display::fmt(s, fmt),
            Predicate::Symbol(s) => write!(fmt, "'{}", s),
            Predicate::LitStr(s) => {
                fmt::Display::fmt("\"", fmt)?;
                s.chars().try_for_each(|c| match c {
                    '"' => fmt::Display::fmt("\"", fmt),
                    c => write!(fmt, "{}", c),
                })?;
                fmt::Display::fmt("\"", fmt)
            }
            Predicate::List(l) if l.is_empty() => fmt::Display::fmt("nil", fmt),
            Predicate::List(l) if l.len() == 1 => write!(fmt, "'({})", l[0]),
            Predicate::List(l) => {
                write!(fmt, "'({}", l[0])?;
                l[1..].iter().try_for_each(|x| write!(fmt, " {}", x))?;
                fmt::Display::fmt(")", fmt)
            }
            Predicate::Apply(f, xs) => {
                write!(fmt, "({}", f)?;
                xs.iter().try_for_each(|x| write!(fmt, " {}", x))?;
                fmt::Display::fmt(")", fmt)
            }
        }
    }
}

impl FromStr for Predicate {
    type Err = ParseFieldError<String>;

    fn from_str(input: &str) -> std::result::Result<Self, ParseFieldError<String>> {
        use combine::char::{char, space, spaces, string};
        use combine::parser::choice::or;
        use combine::parser::range::recognize;
        use combine::stream::state::{IndexPositioner, State};
        use combine::{choice, easy, eof, many, parser, satisfy, skip_many, skip_many1};
        use combine::{ParseResult, Parser};

        static GRAMMER: &str = r#"Predicate    ::= Space* Expr Space*
Expr         ::= ConstOrVar | LitStr | SymbolOrList | Apply
ConstOrVar   ::= "t" | "nil" | Ident
LitStr       ::= '"' ( [^"\] | '\"' )* '"'
SymbolOrList ::= Symbol | List
Symbol       ::= "'" Space* Ident
List         ::= "'" Space* "(" Space* Expr ( Space+ Expr )* Space* ")"
Apply        ::= "(" Space* Ident ( Space+ Expr )* Space* ")"
Ident        ::= ( [a-zA-Z] | [-=_] ) ( [a-zA-Z0-9] | [-=_] )*
Space        ::= ? White_Space character ?
"#;

        fn parse_expr<'a>(
            input: &mut easy::Stream<State<&'a str, IndexPositioner>>,
        ) -> ParseResult<Predicate, easy::Stream<State<&'a str, IndexPositioner>>> {
            let const_or_var = ident().map(|s| match s.as_ref() {
                "t" => Predicate::T,
                "nil" => Predicate::List(vec![]),
                _ => Predicate::Var(s),
            });

            let litstr = char('"')
                .with(many(or(
                    satisfy(|c| !['"', '\\'].contains(&c)).expected(r#"[^"\]"#),
                    string("\\\"").map(|_| '"'),
                )))
                .skip(char('"'))
                .map(Predicate::LitStr);

            let symbol_or_list = char('\'').skip(spaces()).with(or(
                ident().map(Predicate::Symbol),
                char('(')
                    .skip(spaces())
                    .with(parser(parse_expr))
                    .and(many(skip_many1(space()).with(parser(parse_expr))))
                    .skip(spaces())
                    .skip(char(')'))
                    .map(|(x, xs): (_, Vec<_>)| {
                        let mut l = vec![x];
                        l.extend(xs);
                        Predicate::List(l)
                    }),
            ));

            let apply = char('(')
                .skip(spaces())
                .with(ident())
                .and(many(skip_many1(space()).with(parser(parse_expr))))
                .skip(spaces())
                .skip(char(')'))
                .map(|(f, xs)| Predicate::Apply(f, xs));

            choice((const_or_var, litstr, symbol_or_list, apply)).parse_stream(input)
        }

        fn ident<'a>(
        ) -> impl Parser<Input = easy::Stream<State<&'a str, IndexPositioner>>, Output = String>
        {
            recognize(
                satisfy(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '-' | '=' | '_'))
                    .expected("[a-zA-Z] | [-=_]")
                    .and(skip_many(
                        satisfy(
                            |c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '=' | '_'),
                        )
                        .expected("[a-zA-Z0-9] | [-=_]"),
                    )),
            )
            .map(ToOwned::to_owned)
        }

        spaces()
            .with(parser(parse_expr))
            .skip(spaces())
            .skip(eof())
            .easy_parse(State::with_positioner(input, IndexPositioner::new()))
            .map(|(p, _)| p)
            .map_err(|e| ParseFieldError::new(input, e, GRAMMER).into_owned())
    }
}

#[derive(Default, Debug, Serialize, Deserialize)]
struct Hooks {
    switch: Option<TemplateBuilder<HookCommands>>,
    login: Option<TemplateBuilder<HookCommands>>,
    participate: Option<TemplateBuilder<HookCommands>>,
    #[serde(default)]
    retrieve: HooksRetrieve,
    judge: Option<TemplateBuilder<HookCommands>>,
    submit: Option<TemplateBuilder<HookCommands>>,
}

#[derive(Default, Debug, Serialize, Deserialize)]
struct HooksRetrieve {
    testcases: Option<TemplateBuilder<HookCommands>>,
    languages: Option<TemplateBuilder<HookCommands>>,
    submissions: Option<TemplateBuilder<HookCommands>>,
}

impl Hooks {
    fn get(&self, kind: SubCommandKind) -> Option<&TemplateBuilder<HookCommands>> {
        match kind {
            SubCommandKind::Switch => self.switch.as_ref(),
            SubCommandKind::Login => self.login.as_ref(),
            SubCommandKind::Participate => self.participate.as_ref(),
            SubCommandKind::RetrieveTestcases => self.retrieve.testcases.as_ref(),
            SubCommandKind::RetrieveLanguages => self.retrieve.languages.as_ref(),
            SubCommandKind::RetrieveSubmissions => self.retrieve.submissions.as_ref(),
            SubCommandKind::Judge => self.judge.as_ref(),
            SubCommandKind::Submit => self.submit.as_ref(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum SubCommandKind {
    Switch,
    Login,
    Participate,
    RetrieveTestcases,
    RetrieveLanguages,
    RetrieveSubmissions,
    Judge,
    Submit,
}

#[derive(Debug, Serialize, Deserialize)]
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

#[derive(Debug, Serialize, Deserialize)]
struct Target {
    service: ServiceKind,
    contest: String,
    language: String,
}

#[cfg(test)]
mod tests {
    use crate::config::{Inner, SwitchOutcome, SwitchOutcomeAttrs, Target};
    use crate::outcome::Outcome as _;
    use crate::path::AbsPath;
    use crate::service::ServiceKind;
    use crate::terminal::{AnsiWithProps, HasTermProps as _, ModifyTermProps as _};
    use crate::util::combine::ParseFieldError;

    use difference::assert_diff;
    use failure::Fallible;
    use once_cell::sync::Lazy;
    use pretty_assertions::assert_eq;
    use tempdir::TempDir;
    use termcolor::{Ansi, ColorSpec, WriteColor as _};
    use unicode_width::UnicodeWidthStr;

    use std::convert::TryFrom as _;
    use std::fs::File;
    use std::io::Write as _;
    use std::{env, str};

    #[test]
    fn test_init() -> Fallible<()> {
        let tempdir = dunce::canonicalize(&env::temp_dir())?;
        let tempdir = TempDir::new_in(&tempdir, "config_test_init")?;
        let mut stderr = vec![];

        super::init(&mut stderr, AbsPath::try_new(tempdir.path()).unwrap())?;

        let toml_path = tempdir.path().join(super::CONFIG_FILE_NAME);
        let toml = std::fs::read_to_string(&toml_path)?;
        toml::from_str::<Inner>(&toml)?;

        let json_path = tempdir.path().join(".snowchains").join("target.json");
        serde_json::from_reader::<_, Target>(File::open(&json_path)?)?;

        assert_diff!(
            str::from_utf8(&stderr)?,
            &format!(
                "Wrote {}\nWrote {}\n",
                toml_path.display(),
                json_path.display(),
            ),
            "\n",
            0
        );
        Ok(())
    }

    #[test]
    fn test_switch() -> Fallible<()> {
        static OLD_JSON: &str = r#"{
  "service": "atcoder",
  "contest": "arc100",
  "language": "c++"
}
"#;

        let tempdir = dunce::canonicalize(&env::temp_dir())?;
        let tempdir = TempDir::new_in(&tempdir, "config_test_switch")?;
        let mut stdout = AnsiWithProps::new();
        let mut stderr = AnsiWithProps::new();
        stdout.modify_term_props(|p| p.str_width = UnicodeWidthStr::width_cjk);
        stderr.modify_term_props(|p| p.str_width = UnicodeWidthStr::width_cjk);

        let toml = super::generate_toml();
        let old_target = serde_json::from_str::<Target>(OLD_JSON)?;
        let toml_path = tempdir.path().join(super::CONFIG_FILE_NAME);
        let json_path = tempdir.path().join(".snowchains").join("target.json");

        std::fs::create_dir(tempdir.path().join(".snowchains"))?;
        std::fs::write(&toml_path, &toml)?;
        std::fs::write(&json_path, OLD_JSON)?;

        super::switch(
            &mut stdout,
            &mut stderr,
            AbsPath::try_new(tempdir.path()).unwrap(),
            Some(ServiceKind::Yukicoder),
            Some("no"),
            Some("rust"),
        )?;

        let new_target = serde_json::from_reader::<_, Target>(File::open(&json_path)?)?;

        assert_eq!(old_target.service, ServiceKind::Atcoder);
        assert_eq!(old_target.contest, "arc100");
        assert_eq!(old_target.language, "c++");
        assert_eq!(new_target.service, ServiceKind::Yukicoder);
        assert_eq!(new_target.contest, "no");
        assert_eq!(new_target.language, "rust");

        const AMBIGUOUS_WIDTH_CHAR: char = '★';
        assert_eq!(stdout.char_width(AMBIGUOUS_WIDTH_CHAR), Some(1));
        assert_eq!(stderr.char_width(AMBIGUOUS_WIDTH_CHAR), Some(1));
        assert_eq!(stdout.str_width(&AMBIGUOUS_WIDTH_CHAR.to_string()), 1);
        assert_eq!(stderr.str_width(&AMBIGUOUS_WIDTH_CHAR.to_string()), 1);

        let expected_stderr = format!("Wrote {}\n", json_path.display());
        assert_eq!(String::try_from(stdout)?, "");
        assert_eq!(String::try_from(stderr)?, expected_stderr);
        Ok(())
    }

    #[test]
    fn test_switch_outcome_print_pretty() -> Fallible<()> {
        let outcome = SwitchOutcome {
            old: SwitchOutcomeAttrs {
                service: ServiceKind::Atcoder,
                contest: "arc100".to_owned(),
                contest_lower_case: "arc100".to_owned(),
                contest_upper_case: "ARC100".to_owned(),
                contest_snake_case: "arc100".to_owned(),
                contest_kebab_case: "arc100".to_owned(),
                contest_mixed_case: "arc100".to_owned(),
                contest_pascal_case: "Arc100".to_owned(),
                language: "c++".to_owned(),
            },
            new: SwitchOutcomeAttrs {
                service: ServiceKind::Yukicoder,
                contest_lower_case: "no".to_owned(),
                contest_upper_case: "NO".to_owned(),
                contest_snake_case: "no".to_owned(),
                contest_kebab_case: "no".to_owned(),
                contest_mixed_case: "no".to_owned(),
                contest_pascal_case: "No".to_owned(),
                contest: "no".to_owned(),
                language: "rust".to_owned(),
            },
        };

        static EXPECTED: Lazy<String> = Lazy::new(|| {
            let mut expected = Ansi::new(vec![]);

            let mut print_line = |name: &str, from: &str, arrow: &str, to: &str| {
                expected.write_all(name.as_ref()).unwrap();
                expected.set_color(ColorSpec::new().set_bold(true)).unwrap();
                expected.write_all(from.as_ref()).unwrap();
                expected.reset().unwrap();
                expected.write_all(arrow.as_ref()).unwrap();
                expected.set_color(ColorSpec::new().set_bold(true)).unwrap();
                expected.write_all(to.as_ref()).unwrap();
                expected.reset().unwrap();
                expected.write_all(b"\n").unwrap();
            };

            print_line("service:  ", "\"atcoder\"", " -> ", "\"yukicoder\"");
            print_line("contest:  ", "\"arc100\"", "  -> ", "\"no\"");
            print_line("language: ", "\"c++\"", "     -> ", "\"rust\"");

            String::from_utf8(expected.into_inner()).unwrap()
        });

        let mut stdout = AnsiWithProps::new();
        outcome.print_pretty(false, &mut stdout)?;
        assert_eq!(String::try_from(stdout)?, *EXPECTED);
        Ok(())
    }

    #[test]
    fn test_parse_size() -> std::result::Result<(), ParseFieldError<&'static str>> {
        assert_eq!(super::parse_size("0")?, 0);
        assert_eq!(super::parse_size("1B")?, 1);
        assert_eq!(super::parse_size("1KB")?, 10usize.pow(3));
        assert_eq!(super::parse_size("1KiB")?, 2usize.pow(10));
        assert_eq!(super::parse_size("1MB")?, 10usize.pow(6));
        assert_eq!(super::parse_size("1MiB")?, 2usize.pow(20));
        assert_eq!(super::parse_size("1GB")?, 10usize.pow(9));
        assert_eq!(super::parse_size("1GiB")?, 2usize.pow(30));
        assert_eq!(super::parse_size("4.2KB")?, 4200);
        assert_eq!(super::parse_size("4.2KiB")?, 4300);
        super::parse_size("1b").unwrap_err();
        super::parse_size("B").unwrap_err();
        super::parse_size("-0B").unwrap_err();
        super::parse_size("infB").unwrap_err();
        super::parse_size("NaNB").unwrap_err();
        Ok(())
    }
}
