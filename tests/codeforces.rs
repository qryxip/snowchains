mod common;

use crate::common::service;

use snowchains::app::{App, Login, Opt, OutputKind, Retrieve, RetrieveLanguages, Submit};
use snowchains::config;
use snowchains::errors::{ServiceError, ServiceErrorKind};
use snowchains::service::ServiceKind;
use snowchains::terminal::{AnsiColorChoice, Dumb, TtyOrPiped};

use difference::assert_diff;
use failure::Fallible;
use if_chain::if_chain;
use once_cell::sync::Lazy;
use pretty_assertions::assert_eq;
use regex::Regex;

use std::convert::TryFrom as _;

#[test]
fn it_logins() -> Fallible<()> {
    fn login(mut app: App<TtyOrPiped<&[u8]>, Dumb, Dumb>) -> Fallible<()> {
        static MASK_API_KEY: Lazy<Regex> = Lazy::new(|| Regex::new("apiKey=[0-9a-f]+").unwrap());
        static MASK_HANDLES: Lazy<Regex> =
            Lazy::new(|| Regex::new(r"handles=[0-9a-zA-Z_\-]+").unwrap());
        static MASK_TIME: Lazy<Regex> = Lazy::new(|| Regex::new("time=[0-9]+").unwrap());
        static MASK_API_SIG: Lazy<Regex> = Lazy::new(|| Regex::new("apiSig=[0-9a-f]+").unwrap());

        let code = app.run(Opt::Login(Login {
            json: false,
            colorize: false,
            output: OutputKind::Json,
            color_choice: AnsiColorChoice::Never,
            service: ServiceKind::Codeforces,
        }))?;
        assert_eq!(code, 0);

        let stdout = String::try_from(app.stdout)?;
        let stderr = String::try_from(app.stderr)?;

        serde_json::from_str::<serde_json::Value>(&stdout)?;

        let stderr = MASK_API_KEY.replace(&stderr, "apiKey=██████████");
        let stderr = MASK_HANDLES.replace(&stderr, "handles=██████████");
        let stderr = MASK_TIME.replace(&stderr, "time=██████████");
        let stderr = MASK_API_SIG.replace(&stderr, "apiSig=██████████");
        assert_diff!(
            &stderr,
            &r#"
GET https://codeforces.com/enter ... 200 OK
Handle/Email: Password: POST https://codeforces.com/enter ... 302 Found
GET https://codeforces.com/enter ... 302 Found
API Key: API Secret: GET https://codeforces.com/api/user.info?apiKey=██████████&handles=██████████&time=██████████&apiSig=██████████ ... 200 OK
"#[1..],
            "\n",
            0
        );
        Ok(())
    }

    let stdin = credentials_as_input()?;
    service::test_in_tempdir("it_logins", &stdin, login)
}

#[test]
fn it_fails_to_submit_if_the_lang_name_is_invalid() -> Fallible<()> {
    service::test_in_tempdir(
        "it_fails_to_submit_if_the_lang_name_is_invalid",
        &credentials_as_input()?,
        |mut app| -> Fallible<()> {
            static CODE: &[u8] = b"#";
            let dir = app.working_dir.join("codeforces").join("1000").join("py");
            std::fs::create_dir_all(&dir)?;
            std::fs::write(&dir.join("a.py"), CODE)?;
            let err = app
                .run(Opt::Submit(Submit {
                    open: false,
                    force_compile: false,
                    only_transpile: false,
                    no_judge: true,
                    debug: false,
                    no_check_duplication: false,
                    verbose: false,
                    json: false,
                    colorize: false,
                    service: Some(ServiceKind::Codeforces),
                    contest: Some("1000".to_owned()),
                    language: Some("python3-with-invalid-lang-names".to_owned()),
                    mode: config::Mode::Release,
                    jobs: None,
                    output: OutputKind::Pretty,
                    color_choice: AnsiColorChoice::Never,
                    problem: "a".to_owned(),
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
fn it_retrieves_languages() -> Fallible<()> {
    service::test_in_tempdir(
        "it_retrieves_languages",
        &credentials_as_input()?,
        |mut app| -> Fallible<()> {
            let code = app.run(Opt::Retrieve(Retrieve::Languages(RetrieveLanguages {
                json: false,
                colorize: false,
                service: Some(ServiceKind::Codeforces),
                contest: Some("1000".to_owned()),
                output: OutputKind::Pretty,
                color_choice: AnsiColorChoice::Never,
                problem: Some("a".to_owned()),
            })))?;
            assert_eq!(code, 0);
            assert_diff!(
                &String::try_from(app.stdout)?,
                &r#"
┌───────────────────────────┬────┐
│ Name                      │ ID │
├───────────────────────────┼────┤
│ GNU GCC C11 5.1.0         │ 43 │
├───────────────────────────┼────┤
│ Clang++17 Diagnostics     │ 52 │
├───────────────────────────┼────┤
│ GNU G++11 5.1.0           │ 42 │
├───────────────────────────┼────┤
│ GNU G++14 6.4.0           │ 50 │
├───────────────────────────┼────┤
│ GNU G++17 7.3.0           │ 54 │
├───────────────────────────┼────┤
│ Microsoft Visual C++ 2010 │ 2  │
├───────────────────────────┼────┤
│ Microsoft Visual C++ 2017 │ 59 │
├───────────────────────────┼────┤
│ C# Mono 5.18              │ 9  │
├───────────────────────────┼────┤
│ D DMD32 v2.083.1          │ 28 │
├───────────────────────────┼────┤
│ Go 1.11.4                 │ 32 │
├───────────────────────────┼────┤
│ Haskell GHC 8.6.3         │ 12 │
├───────────────────────────┼────┤
│ Java 1.8.0_162            │ 36 │
├───────────────────────────┼────┤
│ Kotlin 1.3.10             │ 48 │
├───────────────────────────┼────┤
│ OCaml 4.02.1              │ 19 │
├───────────────────────────┼────┤
│ Delphi 7                  │ 3  │
├───────────────────────────┼────┤
│ Free Pascal 3.0.2         │ 4  │
├───────────────────────────┼────┤
│ PascalABC.NET 3.4.2       │ 51 │
├───────────────────────────┼────┤
│ Perl 5.20.1               │ 13 │
├───────────────────────────┼────┤
│ PHP 7.2.13                │ 6  │
├───────────────────────────┼────┤
│ Python 2.7.15             │ 7  │
├───────────────────────────┼────┤
│ Python 3.7.2              │ 31 │
├───────────────────────────┼────┤
│ PyPy 2.7 (6.0.0)          │ 40 │
├───────────────────────────┼────┤
│ PyPy 3.5 (6.0.0)          │ 41 │
├───────────────────────────┼────┤
│ Ruby 2.0.0p645            │ 8  │
├───────────────────────────┼────┤
│ Rust 1.31.1               │ 49 │
├───────────────────────────┼────┤
│ Scala 2.12.8              │ 20 │
├───────────────────────────┼────┤
│ JavaScript V8 4.8.0       │ 34 │
├───────────────────────────┼────┤
│ Node.js 9.4.0             │ 55 │
└───────────────────────────┴────┘
"#[1..],
                "\n",
                0
            );
            assert_diff!(
                &String::try_from(app.stderr)?,
                &r#"
GET https://codeforces.com/enter ... 200 OK
Handle/Email: Password: POST https://codeforces.com/enter ... 302 Found
GET https://codeforces.com/enter ... 302 Found
GET https://codeforces.com/contest/1000/submit ... 200 OK

"#[1..],
                "\n",
                0
            );
            Ok(())
        },
    )
}

fn credentials_as_input() -> Fallible<String> {
    let username = service::env_var("CODEFORCES_USERNAME")?;
    let password = service::env_var("CODEFORCES_PASSWORD")?;
    let api_key = service::env_var("CODEFORCES_API_KEY")?;
    let api_secret = service::env_var("CODEFORCES_API_SECRET")?;
    Ok(format!(
        "{}\n{}\n{}\n{}\n",
        username, password, api_key, api_secret,
    ))
}
