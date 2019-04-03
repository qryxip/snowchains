#[allow(dead_code)]
mod service;

use snowchains::app::{App, ListLangs, Opt, Submit};
use snowchains::config;
use snowchains::errors::{ServiceError, ServiceErrorKind};
use snowchains::service::ServiceKind;
use snowchains::terminal::{AnsiColorChoice, Term as _, TermImpl};

use difference::assert_diff;
use failure::Fallible;
use if_chain::if_chain;
use indexmap::IndexMap;
use once_cell::sync::Lazy;
use once_cell::sync_lazy;
use pretty_assertions::assert_eq;
use regex::Regex;
use serde_derive::Deserialize;

use std::mem;

#[test]
fn it_logins() -> Fallible<()> {
    fn login(app: App<TermImpl<&[u8], Vec<u8>, Vec<u8>>>) -> snowchains::Result<()> {
        service::login(app, ServiceKind::Yukicoder)
    }

    let _ = env_logger::try_init();
    let stdin = format!("{}\n", service::env_var("YUKICODER_REVEL_SESSION")?);
    service::test_in_tempdir("it_logins", &stdin, login)
}

#[test]
fn it_downloads_testcases() -> Fallible<()> {
    let _ = env_logger::try_init();
    service::test_in_tempdir(
        "it_downloads_test_cases_from_master",
        &format!("Y\n{}\n", service::env_var("YUKICODER_REVEL_SESSION")?),
        |app| -> Fallible<()> {
            static CONTEST: &str = "no";
            let wd = app.working_dir.clone();
            service::download(app, ServiceKind::Yukicoder, CONTEST, &["3", "725", "726"])?;
            service::confirm_num_cases(
                &wd,
                ServiceKind::Yukicoder,
                CONTEST,
                &[("3", 31), ("725", 9), ("726", 25)],
            )
        },
    )
}

#[test]
fn it_fails_to_submit_if_the_lang_name_is_invalid() -> Fallible<()> {
    let _ = env_logger::try_init();
    service::test_in_tempdir(
        "it_fails_to_submit_if_the_lang_name_is_invalid",
        &format!("{}\n", service::env_var("YUKICODER_REVEL_SESSION")?),
        |mut app| -> Fallible<()> {
            static CODE: &[u8] = b"#";
            let dir = app.working_dir.join("yukicoder").join("no").join("py");
            std::fs::create_dir_all(&dir)?;
            std::fs::write(&dir.join("9000.py"), CODE)?;
            let err = app
                .run(Opt::Submit(Submit {
                    json: false,
                    open: false,
                    force_compile: false,
                    only_transpile: false,
                    no_judge: true,
                    debug: false,
                    no_check_duplication: false,
                    service: Some(ServiceKind::Yukicoder),
                    contest: Some("no".to_owned()),
                    language: Some("python3-with-invalid-lang-names".to_owned()),
                    mode: config::Mode::Release,
                    jobs: None,
                    color_choice: AnsiColorChoice::Never,
                    problem: "9000".to_owned(),
                }))
                .unwrap_err();
            if_chain! {
                if let snowchains::Error::Service(ServiceError::Context(ctx)) = &err;
                if let ServiceErrorKind::NoSuchLang(lang_name) = ctx.get_context();
                then {
                    assert_eq!(lang_name, "invalid");
                    Ok(())
                } else {
                    Err(err.into())
                }
            }
        },
    )
}

#[test]
#[ignore]
fn it_submits_to_no_9000() -> Fallible<()> {
    let _ = env_logger::try_init();
    service::test_in_tempdir(
        "it_submits_to_no_9000",
        &format!("{}\n", service::env_var("YUKICODER_REVEL_SESSION")?),
        |mut app| -> Fallible<()> {
            static CODE: &[u8] = b"Hello World!\n";
            let dir = app.working_dir.join("yukicoder").join("no").join("txt");
            std::fs::create_dir_all(&dir)?;
            std::fs::write(&dir.join("9000.txt"), CODE)?;
            app.run(Opt::Submit(Submit {
                json: true,
                open: false,
                force_compile: false,
                only_transpile: false,
                no_judge: true,
                debug: false,
                no_check_duplication: false,
                service: Some(ServiceKind::Yukicoder),
                contest: Some("no".to_owned()),
                language: Some("text".to_owned()),
                mode: config::Mode::Release,
                jobs: None,
                color_choice: AnsiColorChoice::Never,
                problem: "9000".to_owned(),
            }))?;
            let stdout = String::from_utf8(mem::replace(app.term.stdout().get_mut(), vec![]))?;
            serde_json::from_str::<serde_json::Value>(&stdout)?;
            Ok(())
        },
    )
}

#[test]
fn it_list_languages() -> Fallible<()> {
    #[derive(Deserialize)]
    struct Stdout {
        available_languages: IndexMap<String, String>,
    }

    let _ = env_logger::try_init();
    service::test_in_tempdir(
        "it_list_languages",
        &format!("{}\n", service::env_var("YUKICODER_REVEL_SESSION")?),
        |mut app| -> Fallible<()> {
            static MASK_USERNAME: Lazy<Regex> = sync_lazy!(Regex::new("Username: .*").unwrap());

            app.run(Opt::ListLangs(ListLangs {
                json: true,
                service: Some(ServiceKind::Yukicoder),
                contest: Some("no".to_owned()),
                color_choice: AnsiColorChoice::Never,
                problem: Some("9000".to_owned()),
            }))?;
            let stdout = String::from_utf8(mem::replace(app.term.stdout().get_mut(), vec![]))?;
            let stderr = String::from_utf8(mem::replace(app.term.stderr().get_mut(), vec![]))?;
            let stdout = serde_json::from_str::<Stdout>(&stdout)?;
            assert_eq!(stdout.available_languages.len(), 43);
            assert_diff!(
                &MASK_USERNAME.replace(&stderr, "Username: ██████████"),
                r#"Target: no/9000
GET https://yukicoder.me/ ... 200 OK

Input "REVEL_SESSION".

Firefox: sqlite3 ~/path/to/cookies.sqlite 'SELECT value FROM moz_cookies WHERE baseDomain="yukicoder.me" AND name="REVEL_SESSION"'
Chrome: chrome://settings/cookies/detail?site=yukicoder.me&search=cookie

REVEL_SESSION: GET https://yukicoder.me/ ... 200 OK
Username: ██████████
GET https://yukicoder.me/problems/no/9000/submit ... 200 OK
+----------------------------------------------+-------------+
| Name                                         | ID          |
+----------------------------------------------+-------------+
| C++11 (gcc 4.8.5)                            | cpp         |
+----------------------------------------------+-------------+
| C++14 (gcc 8.2.0)                            | cpp14       |
+----------------------------------------------+-------------+
| C++17(1z） (gcc 8.2.0)                       | cpp17       |
+----------------------------------------------+-------------+
| C++17(clang Beta) (clang 3.8.0-2ubuntu4)     | cpp-clang   |
+----------------------------------------------+-------------+
| C (gcc 8.2.0)                                | c11         |
+----------------------------------------------+-------------+
| C90 (gcc 4.8.5)                              | c           |
+----------------------------------------------+-------------+
| Java8 (openjdk 1.8.0.191)                    | java8       |
+----------------------------------------------+-------------+
| C# (csc 2.8.2.62916)                         | csharp      |
+----------------------------------------------+-------------+
| C#(mono) (mono 5.16.0.187)                   | csharp_mono |
+----------------------------------------------+-------------+
| Perl (5.16.3)                                | perl        |
+----------------------------------------------+-------------+
| Perl6 (rakudo 2018.10-174-g10693d1)          | perl6       |
+----------------------------------------------+-------------+
| PHP (5.4.16)                                 | php         |
+----------------------------------------------+-------------+
| PHP7 (7.2.12)                                | php7        |
+----------------------------------------------+-------------+
| Python2 (2.7.15)                             | python      |
+----------------------------------------------+-------------+
| Python3 (3.7.1 + numpy 1.14.5 + scipy 1.1.0) | python3     |
+----------------------------------------------+-------------+
| PyPy2 (6.0.0)                                | pypy2       |
+----------------------------------------------+-------------+
| PyPy3 (6.0.0)                                | pypy3       |
+----------------------------------------------+-------------+
| Ruby (ruby 2.5.3p57)                         | ruby        |
+----------------------------------------------+-------------+
| D (dmd 2.083.0)                              | d           |
+----------------------------------------------+-------------+
| Go (1.11.2)                                  | go          |
+----------------------------------------------+-------------+
| Haskell (8.6.2)                              | haskell     |
+----------------------------------------------+-------------+
| Scala(Beta) (2.12.7)                         | scala       |
+----------------------------------------------+-------------+
| Nim (0.19.9)                                 | nim         |
+----------------------------------------------+-------------+
| Rust (1.30.1)                                | rust        |
+----------------------------------------------+-------------+
| Kotlin (1.3.1)                               | kotlin      |
+----------------------------------------------+-------------+
| Scheme (Gauche-0.9.5)                        | scheme      |
+----------------------------------------------+-------------+
| Crystal (0.27.0)                             | crystal     |
+----------------------------------------------+-------------+
| Swift(Beta) (4.2.1)                          | swift       |
+----------------------------------------------+-------------+
| OCaml (4.05.0)                               | ocaml       |
+----------------------------------------------+-------------+
| Clojure(Beta) (1.9.0.394)                    | clojure     |
+----------------------------------------------+-------------+
| F# (4.1)                                     | fsharp      |
+----------------------------------------------+-------------+
| Elixir (1.8.0)                               | elixir      |
+----------------------------------------------+-------------+
| Lua (LuaJit 2.0.5)                           | lua         |
+----------------------------------------------+-------------+
| Fortran (gFortran 4.8.5)                     | fortran     |
+----------------------------------------------+-------------+
| JavaScript (node v11.2.0)                    | node        |
+----------------------------------------------+-------------+
| Kuin(beta) (2019_03_17 on Wine)              | kuin        |
+----------------------------------------------+-------------+
| Vim script (v8.1.127)                        | vim         |
+----------------------------------------------+-------------+
| Bash (Bash 4.2.46)                           | sh          |
+----------------------------------------------+-------------+
| Text (cat 8.22)                              | text        |
+----------------------------------------------+-------------+
| Assembler (nasm 2.10.07)                     | nasm        |
+----------------------------------------------+-------------+
| cLay (20180730-1)                            | clay        |
+----------------------------------------------+-------------+
| Brainfuck (BFI 1.1)                          | bf          |
+----------------------------------------------+-------------+
| Whitespace (0.3)                             | Whitespace  |
+----------------------------------------------+-------------+
"#,
                "\n",
                0
            );
            Ok(())
        },
    )
}
