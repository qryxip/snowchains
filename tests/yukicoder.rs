mod common;

use crate::common::service;

use snowchains::config;
use snowchains::errors::{ServiceError, ServiceErrorKind};
use snowchains::service::ServiceKind;
use snowchains::terminal::{AnsiColorChoice, Dumb, TtyOrPiped};
use snowchains::{Opt, OutputKind, Retrieve, RetrieveLanguages, RetrieveSubmissions, Submit};

use difference::assert_diff;
use failure::Fallible;
use if_chain::if_chain;
use once_cell::sync::Lazy;
use pretty_assertions::assert_eq;
use regex::Regex;

use std::convert::TryFrom as _;

#[test]
fn it_logins() -> Fallible<()> {
    fn login(ctx: snowchains::Context<TtyOrPiped<&[u8]>, Dumb, Dumb>) -> Fallible<()> {
        service::login(ctx, ServiceKind::Yukicoder).map_err(Into::into)
    }

    let stdin = format!("{}\n", service::env_var("YUKICODER_REVEL_SESSION")?);
    service::test_in_tempdir("it_logins", &stdin, login)
}

#[test]
fn it_scrapes_and_downloads_testcases() -> Fallible<()> {
    service::test_in_tempdir(
        "it_downloads_test_cases_from_master",
        &format!("Y\n{}\n", service::env_var("YUKICODER_REVEL_SESSION")?),
        |ctx| -> _ {
            static CONTEST: &str = "no";
            let wd = ctx.cwd.clone();
            service::retrieve_testcases(
                ctx,
                ServiceKind::Yukicoder,
                CONTEST,
                &["3", "725", "726"],
            )?;
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
fn retrieve_submissions_command_works_without_error() -> Fallible<()> {
    service::test_in_tempdir(
        "retrieve_submissions_command_works_without_error",
        &format!("{}\n", service::env_var("YUKICODER_REVEL_SESSION")?),
        |mut ctx| -> _ {
            let opt = Opt::Retrieve(Retrieve::Submissions(RetrieveSubmissions {
                fetch_all: false,
                no_save: true,
                verbose: false,
                json: false,
                colorize: false,
                service: Some(ServiceKind::Yukicoder),
                contest: Some("no".to_owned()),
                mode: config::Mode::Debug,
                problems: vec!["1".to_owned()],
                output: OutputKind::Json,
                color_choice: AnsiColorChoice::Never,
            }));
            let code = snowchains::run(opt, &mut ctx)?;
            assert_eq!(code, 0);
            let stdout = String::try_from(ctx.stdout)?;
            let stderr = String::try_from(ctx.stderr)?;
            serde_json::from_str::<serde_json::Value>(&stdout)?;
            assert!(stderr.starts_with(
                &r#"
GET https://yukicoder.me/api/v1/problems ... 200 OK
GET https://yukicoder.me/api/v1/languages ... 200 OK
GET https://yukicoder.me/problems/no/1/submissions?my_submission=enabled ... 200 OK
"#[1..],
            ));
            Ok(())
        },
    )
}

#[test]
fn it_fails_to_submit_if_the_lang_name_is_invalid() -> Fallible<()> {
    service::test_in_tempdir(
        "it_fails_to_submit_if_the_lang_name_is_invalid",
        &format!("{}\n", service::env_var("YUKICODER_REVEL_SESSION")?),
        |mut ctx| -> _ {
            static CODE: &[u8] = b"#";
            let dir = ctx.cwd.join("yukicoder").join("no").join("py");
            std::fs::create_dir_all(&dir)?;
            std::fs::write(&dir.join("9000.py"), CODE)?;
            let opt = Opt::Submit(Submit {
                open: false,
                force_compile: false,
                only_transpile: false,
                no_judge: true,
                debug: false,
                no_check_duplication: false,
                verbose: false,
                json: false,
                colorize: false,
                service: Some(ServiceKind::Yukicoder),
                contest: Some("no".to_owned()),
                language: Some("python3-with-invalid-lang-names".to_owned()),
                mode: config::Mode::Release,
                jobs: None,
                output: OutputKind::Pretty,
                color_choice: AnsiColorChoice::Never,
                problem: "9000".to_owned(),
            });
            let err = snowchains::run(opt, &mut ctx).unwrap_err();
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
    service::test_in_tempdir(
        "it_submits_to_no_9000",
        &format!("{}\n", service::env_var("YUKICODER_REVEL_SESSION")?),
        |mut ctx| -> _ {
            static CODE: &[u8] = b"Hello World!\n";
            let dir = ctx.cwd.join("yukicoder").join("no").join("txt");
            std::fs::create_dir_all(&dir)?;
            std::fs::write(&dir.join("9000.txt"), CODE)?;
            let opt = Opt::Submit(Submit {
                open: false,
                force_compile: false,
                only_transpile: false,
                no_judge: true,
                debug: false,
                no_check_duplication: false,
                verbose: false,
                json: false,
                colorize: false,
                service: Some(ServiceKind::Yukicoder),
                contest: Some("no".to_owned()),
                language: Some("text".to_owned()),
                mode: config::Mode::Release,
                jobs: None,
                output: OutputKind::Json,
                color_choice: AnsiColorChoice::Never,
                problem: "9000".to_owned(),
            });
            let code = snowchains::run(opt, &mut ctx)?;
            assert_eq!(code, 0);
            let stdout = String::try_from(ctx.stdout)?;
            String::try_from(ctx.stderr)?;
            serde_json::from_str::<serde_json::Value>(&stdout)?;
            Ok(())
        },
    )
}

#[test]
fn it_retrieves_languages() -> Fallible<()> {
    service::test_in_tempdir(
        "it_retrieves_languages",
        &format!("{}\n", service::env_var("YUKICODER_REVEL_SESSION")?),
        |mut ctx| -> _ {
            static MASK_USERNAME: Lazy<Regex> = Lazy::new(|| Regex::new("Username: .*").unwrap());

            let opt = Opt::Retrieve(Retrieve::Languages(RetrieveLanguages {
                json: false,
                colorize: false,
                service: Some(ServiceKind::Yukicoder),
                contest: Some("no".to_owned()),
                output: OutputKind::Pretty,
                color_choice: AnsiColorChoice::Never,
                problem: Some("9000".to_owned()),
            }));
            let code = snowchains::run(opt, &mut ctx)?;
            assert_eq!(code, 0);
            let stdout = String::try_from(ctx.stdout)?;
            let stderr = String::try_from(ctx.stderr)?;
            assert_diff!(
                &r#"
┌──────────────────────────────────────────────┬─────────────┐
│ Name                                         │ ID          │
├──────────────────────────────────────────────┼─────────────┤
│ C++14 (gcc 9.3.0)                            │ cpp14       │
├──────────────────────────────────────────────┼─────────────┤
│ C++17(1z) (gcc 9.3.0)                        │ cpp17       │
├──────────────────────────────────────────────┼─────────────┤
│ C++17(clang Beta) (clang 7.0.0)              │ cpp-clang   │
├──────────────────────────────────────────────┼─────────────┤
│ C++11 (gcc 4.8.5)                            │ cpp         │
├──────────────────────────────────────────────┼─────────────┤
│ C (gcc 9.3.0)                                │ c11         │
├──────────────────────────────────────────────┼─────────────┤
│ C90 (gcc 4.8.5)                              │ c           │
├──────────────────────────────────────────────┼─────────────┤
│ Java11 (openjdk 11.0.6)                      │ java8       │
├──────────────────────────────────────────────┼─────────────┤
│ C# (csc 3.5.0-beta1-19606-04)                │ csharp      │
├──────────────────────────────────────────────┼─────────────┤
│ C#(mono) (mono 6.8.0.105)                    │ csharp_mono │
├──────────────────────────────────────────────┼─────────────┤
│ Perl (5.16.3)                                │ perl        │
├──────────────────────────────────────────────┼─────────────┤
│ Perl6 (rakudo 2019.11-303-g5c65a12)          │ perl6       │
├──────────────────────────────────────────────┼─────────────┤
│ PHP (5.4.16)                                 │ php         │
├──────────────────────────────────────────────┼─────────────┤
│ PHP7 (7.4.1)                                 │ php7        │
├──────────────────────────────────────────────┼─────────────┤
│ Python2 (2.7.17)                             │ python      │
├──────────────────────────────────────────────┼─────────────┤
│ Python3 (3.8.2 + numpy 1.14.5 + scipy 1.1.0) │ python3     │
├──────────────────────────────────────────────┼─────────────┤
│ PyPy2 (7.1.1)                                │ pypy2       │
├──────────────────────────────────────────────┼─────────────┤
│ PyPy3 (7.0.0)                                │ pypy3       │
├──────────────────────────────────────────────┼─────────────┤
│ Ruby (2.7.0p0)                               │ ruby        │
├──────────────────────────────────────────────┼─────────────┤
│ D (dmd 2.091.0)                              │ d           │
├──────────────────────────────────────────────┼─────────────┤
│ Go (1.14)                                    │ go          │
├──────────────────────────────────────────────┼─────────────┤
│ Haskell (8.8.1)                              │ haskell     │
├──────────────────────────────────────────────┼─────────────┤
│ Scala(Beta) (2.13.1)                         │ scala       │
├──────────────────────────────────────────────┼─────────────┤
│ Nim (1.0.6)                                  │ nim         │
├──────────────────────────────────────────────┼─────────────┤
│ Rust (1.42.0)                                │ rust        │
├──────────────────────────────────────────────┼─────────────┤
│ Kotlin (1.3.70)                              │ kotlin      │
├──────────────────────────────────────────────┼─────────────┤
│ Scheme (Gauche-0.9.9)                        │ scheme      │
├──────────────────────────────────────────────┼─────────────┤
│ Crystal (0.33.0)                             │ crystal     │
├──────────────────────────────────────────────┼─────────────┤
│ Swift(Beta) (5.1.3)                          │ swift       │
├──────────────────────────────────────────────┼─────────────┤
│ OCaml (4.05.0)                               │ ocaml       │
├──────────────────────────────────────────────┼─────────────┤
│ Clojure(Beta) (1.10.1.469)                   │ clojure     │
├──────────────────────────────────────────────┼─────────────┤
│ F# (4.5)                                     │ fsharp      │
├──────────────────────────────────────────────┼─────────────┤
│ Elixir (1.10.0-dev)                          │ elixir      │
├──────────────────────────────────────────────┼─────────────┤
│ Lua (LuaJit 2.0.5)                           │ lua         │
├──────────────────────────────────────────────┼─────────────┤
│ Fortran (gFortran 4.8.5)                     │ fortran     │
├──────────────────────────────────────────────┼─────────────┤
│ JavaScript (node v13.5.0)                    │ node        │
├──────────────────────────────────────────────┼─────────────┤
│ Common Lisp (sbcl 1.5.5)                     │ lisp        │
├──────────────────────────────────────────────┼─────────────┤
│ Kuin(beta) (v.2019.9.17 on Wine)             │ kuin        │
├──────────────────────────────────────────────┼─────────────┤
│ KuinKuin(beta) (KuinInKuin)                  │ kuininkuin  │
├──────────────────────────────────────────────┼─────────────┤
│ Vim script (v8.1.127)                        │ vim         │
├──────────────────────────────────────────────┼─────────────┤
│ Bash (Bash 4.2.46)                           │ sh          │
├──────────────────────────────────────────────┼─────────────┤
│ Text (cat 8.22)                              │ text        │
├──────────────────────────────────────────────┼─────────────┤
│ Assembler (nasm 2.10.07)                     │ nasm        │
├──────────────────────────────────────────────┼─────────────┤
│ cLay (20200317-1)                            │ clay        │
├──────────────────────────────────────────────┼─────────────┤
│ Brainfuck (BFI 1.1)                          │ bf          │
├──────────────────────────────────────────────┼─────────────┤
│ Whitespace (0.3)                             │ Whitespace  │
└──────────────────────────────────────────────┴─────────────┘
"#[1..],
                &stdout,
                "\n",
                0
            );
            assert_diff!(
                &MASK_USERNAME.replace(&stderr, "Username: ██████████"),
                &r#"
GET https://yukicoder.me/ ... 200 OK

Input "REVEL_SESSION".

Chrome: chrome://settings/cookies/detail?site=yukicoder.me&search=cookie
Firefox: sqlite3 "$YOUR_FIREFOX_PROFILE/cookies.sqlite" 'SELECT value FROM moz_cookies WHERE baseDomain="yukicoder.me" AND name="REVEL_SESSION"'

REVEL_SESSION: GET https://yukicoder.me/ ... 200 OK
Confirmed.
Username: ██████████
GET https://yukicoder.me/problems/no/9000/submit ... 200 OK

"#[1..],
                "\n",
                0
            );
            Ok(())
        },
    )
}
