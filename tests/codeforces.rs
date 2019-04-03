mod service;

use snowchains::app::{App, ListLangs, Login, Opt, Submit};
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

use std::str;

#[test]
fn it_logins() -> Fallible<()> {
    fn login(mut app: App<TermImpl<&[u8], Vec<u8>, Vec<u8>>>) -> Fallible<()> {
        static MASK_API_KEY: Lazy<Regex> = sync_lazy!(Regex::new("apiKey=[0-9a-f]+").unwrap());
        static MASK_HANDLES: Lazy<Regex> =
            sync_lazy!(Regex::new(r"handles=[0-9a-zA-Z_\-]+").unwrap());
        static MASK_TIME: Lazy<Regex> = sync_lazy!(Regex::new("time=[0-9]+").unwrap());
        static MASK_API_SIG: Lazy<Regex> = sync_lazy!(Regex::new("apiSig=[0-9a-f]+").unwrap());

        app.run(Opt::Login(Login {
            json: true,
            color_choice: AnsiColorChoice::Never,
            service: ServiceKind::Codeforces,
        }))?;
        let (_, stdout, stderr) = app.term.split_mut();
        let stdout = str::from_utf8(stdout.get_ref())?;
        let stderr = str::from_utf8(stderr.get_ref())?;
        let stderr = MASK_API_KEY.replace(stderr, "apiKey=██████████");
        let stderr = MASK_HANDLES.replace(&stderr, "handles=██████████");
        let stderr = MASK_TIME.replace(&stderr, "time=██████████");
        let stderr = MASK_API_SIG.replace(&stderr, "apiSig=██████████");
        serde_json::from_str::<serde_json::Map<String, serde_json::Value>>(stdout)?;
        assert_diff!(
            &stderr,
            r#"GET https://codeforces.com/enter ... 200 OK
Handle/Email: Password: POST https://codeforces.com/enter ... 302 Found
GET https://codeforces.com/enter ... 302 Found
API Key: API Secret: GET https://codeforces.com/api/user.info?apiKey=██████████&handles=██████████&time=██████████&apiSig=██████████ ... 200 OK
"#,
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
    let _ = env_logger::try_init();
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
                    json: false,
                    open: false,
                    force_compile: false,
                    only_transpile: false,
                    no_judge: true,
                    debug: false,
                    no_check_duplication: false,
                    service: Some(ServiceKind::Codeforces),
                    contest: Some("1000".to_owned()),
                    language: Some("python3-with-invalid-lang-names".to_owned()),
                    mode: config::Mode::Release,
                    jobs: None,
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
fn it_list_languages() -> Fallible<()> {
    #[derive(Deserialize)]
    struct Stdout {
        available_languages: IndexMap<String, String>,
    }

    let _ = env_logger::try_init();
    service::test_in_tempdir(
        "it_list_languages",
        &credentials_as_input()?,
        |mut app| -> Fallible<()> {
            app.run(Opt::ListLangs(ListLangs {
                json: true,
                service: Some(ServiceKind::Codeforces),
                contest: Some("1000".to_owned()),
                color_choice: AnsiColorChoice::Never,
                problem: Some("a".to_owned()),
            }))?;
            let (_, stdout, stderr) = app.term.split_mut();
            let stdout = str::from_utf8(stdout.get_ref())?;
            let stderr = str::from_utf8(stderr.get_ref())?;
            let stdout = serde_json::from_str::<Stdout>(stdout)?;
            assert_eq!(stdout.available_languages.len(), 28);
            assert_diff!(
                &stderr,
                r#"Target: 1000/A
GET https://codeforces.com/enter ... 200 OK
Handle/Email: Password: POST https://codeforces.com/enter ... 302 Found
GET https://codeforces.com/enter ... 302 Found
GET https://codeforces.com/contest/1000/submit ... 200 OK
+---------------------------+----+
| Name                      | ID |
+---------------------------+----+
| GNU GCC C11 5.1.0         | 43 |
+---------------------------+----+
| Clang++17 Diagnostics     | 52 |
+---------------------------+----+
| GNU G++11 5.1.0           | 42 |
+---------------------------+----+
| GNU G++14 6.4.0           | 50 |
+---------------------------+----+
| GNU G++17 7.3.0           | 54 |
+---------------------------+----+
| Microsoft Visual C++ 2010 | 2  |
+---------------------------+----+
| Microsoft Visual C++ 2017 | 59 |
+---------------------------+----+
| C# Mono 5.18              | 9  |
+---------------------------+----+
| D DMD32 v2.083.1          | 28 |
+---------------------------+----+
| Go 1.11.4                 | 32 |
+---------------------------+----+
| Haskell GHC 8.6.3         | 12 |
+---------------------------+----+
| Java 1.8.0_162            | 36 |
+---------------------------+----+
| Kotlin 1.3.10             | 48 |
+---------------------------+----+
| OCaml 4.02.1              | 19 |
+---------------------------+----+
| Delphi 7                  | 3  |
+---------------------------+----+
| Free Pascal 3.0.2         | 4  |
+---------------------------+----+
| PascalABC.NET 3.4.2       | 51 |
+---------------------------+----+
| Perl 5.20.1               | 13 |
+---------------------------+----+
| PHP 7.2.13                | 6  |
+---------------------------+----+
| Python 2.7.15             | 7  |
+---------------------------+----+
| Python 3.7.2              | 31 |
+---------------------------+----+
| PyPy 2.7 (6.0.0)          | 40 |
+---------------------------+----+
| PyPy 3.5 (6.0.0)          | 41 |
+---------------------------+----+
| Ruby 2.0.0p645            | 8  |
+---------------------------+----+
| Rust 1.31.1               | 49 |
+---------------------------+----+
| Scala 2.12.8              | 20 |
+---------------------------+----+
| JavaScript V8 4.8.0       | 34 |
+---------------------------+----+
| Node.js 9.4.0             | 55 |
+---------------------------+----+
"#,
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
