mod service;

use snowchains::app::{App, Opt};
use snowchains::errors::{ServiceError, ServiceErrorKind};
use snowchains::service::ServiceKind;
use snowchains::terminal::{AnsiColorChoice, TermImpl};

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
fn it_fails_to_submit_if_the_lang_id_is_invalid() -> Fallible<()> {
    let _ = env_logger::try_init();
    service::test_in_tempdir(
        "it_fails_to_submit_if_the_lang_id_is_invalid",
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
                    no_check_duplication: false,
                    service: Some(ServiceKind::Codeforces),
                    contest: Some("1000".to_owned()),
                    language: Some("python3-with-invalid-lang-ids".to_owned()),
                    jobs: None,
                    color_choice: AnsiColorChoice::Never,
                    problem: "a".to_owned(),
                })
                .unwrap_err();
            if_chain! {
                if let snowchains::Error::Service(ServiceError::Context(ctx)) = &err;
                if let ServiceErrorKind::NoSuchLangId(lang_id) = ctx.get_context();
                then {
                    assert_eq!(lang_id, "invalid");
                    Ok(())
                } else {
                    Err(err.into())
                }
            }
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
