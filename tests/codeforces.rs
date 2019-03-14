mod service;

use snowchains::app::{App, Opt};
use snowchains::config;
use snowchains::errors::{ServiceError, ServiceErrorKind};
use snowchains::service::ServiceKind;
use snowchains::terminal::{AnsiColorChoice, Term as _, TermImpl};

use failure::Fallible;
use if_chain::if_chain;

#[test]
fn it_logins() -> Fallible<()> {
    fn login(mut app: App<TermImpl<&[u8], Vec<u8>, Vec<u8>>>) -> snowchains::Result<()> {
        app.run(Opt::Login {
            color_choice: AnsiColorChoice::Never,
            service: ServiceKind::Codeforces,
        })
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
                .run(Opt::Submit {
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
                })
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
    let _ = env_logger::try_init();
    service::test_in_tempdir(
        "it_list_languages",
        &credentials_as_input()?,
        |mut app| -> Fallible<()> {
            app.run(Opt::ListLangs {
                service: Some(ServiceKind::Codeforces),
                contest: Some("1000".to_owned()),
                color_choice: AnsiColorChoice::Never,
                problem: Some("a".to_owned()),
            })?;
            let stdout = String::from_utf8(app.term.stdout().get_ref().to_owned())?;
            let stderr = String::from_utf8(app.term.stderr().get_ref().to_owned())?;
            assert_eq!(
                stdout,
                r#"+--------------------------------+----+
| Name                           | ID |
+--------------------------------+----+
| GNU GCC C11 5.1.0              | 43 |
+--------------------------------+----+
| Clang++17 Diagnostics          | 52 |
+--------------------------------+----+
| GNU G++11 5.1.0                | 42 |
+--------------------------------+----+
| GNU G++14 6.4.0                | 50 |
+--------------------------------+----+
| GNU G++17 7.3.0                | 54 |
+--------------------------------+----+
| Microsoft Visual C++ 2010      | 2  |
+--------------------------------+----+
| Microsoft Visual C++ 2017      | 59 |
+--------------------------------+----+
| C# Mono 5.18                   | 9  |
+--------------------------------+----+
| D DMD32 v2.083.1               | 28 |
+--------------------------------+----+
| Go 1.11.4                      | 32 |
+--------------------------------+----+
| Haskell GHC 7.8.3 (2014.2.0.0) | 12 |
+--------------------------------+----+
| Java 1.8.0_162                 | 36 |
+--------------------------------+----+
| Kotlin 1.3.10                  | 48 |
+--------------------------------+----+
| OCaml 4.02.1                   | 19 |
+--------------------------------+----+
| Delphi 7                       | 3  |
+--------------------------------+----+
| Free Pascal 3.0.2              | 4  |
+--------------------------------+----+
| PascalABC.NET 3.4.2            | 51 |
+--------------------------------+----+
| Perl 5.20.1                    | 13 |
+--------------------------------+----+
| PHP 7.2.13                     | 6  |
+--------------------------------+----+
| Python 2.7.15                  | 7  |
+--------------------------------+----+
| Python 3.7.2                   | 31 |
+--------------------------------+----+
| PyPy 2.7 (6.0.0)               | 40 |
+--------------------------------+----+
| PyPy 3.5 (6.0.0)               | 41 |
+--------------------------------+----+
| Ruby 2.0.0p645                 | 8  |
+--------------------------------+----+
| Rust 1.31.1                    | 49 |
+--------------------------------+----+
| Scala 2.12.8                   | 20 |
+--------------------------------+----+
| JavaScript V8 4.8.0            | 34 |
+--------------------------------+----+
| Node.js 9.4.0                  | 55 |
+--------------------------------+----+
"#,
            );
            assert_eq!(
                stderr,
                r#"Target: 1000/A
GET https://codeforces.com/enter ... 200 OK
Handle/Email: Password: POST https://codeforces.com/enter ... 302 Found
GET https://codeforces.com/enter ... 302 Found
GET https://codeforces.com/contest/1000/submit ... 200 OK
"#
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
