#[allow(dead_code)]
mod service;

use snowchains::app::{App, Opt};
use snowchains::errors::{ServiceError, ServiceErrorKind};
use snowchains::service::ServiceKind;
use snowchains::terminal::{AnsiColorChoice, TermImpl};

use failure::Fallible;
use if_chain::if_chain;

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
fn it_fails_to_submit_if_the_lang_id_is_invalid() -> Fallible<()> {
    let _ = env_logger::try_init();
    service::test_in_tempdir(
        "it_fails_to_submit_if_the_lang_id_is_invalid",
        &format!("{}\n", service::env_var("YUKICODER_REVEL_SESSION")?),
        |mut app| -> Fallible<()> {
            static CODE: &[u8] = b"#";
            let dir = app.working_dir.join("yukicoder").join("no").join("py");
            std::fs::create_dir_all(&dir)?;
            std::fs::write(&dir.join("9000.py"), CODE)?;
            let err = app
                .run(Opt::Submit {
                    open: false,
                    force_compile: false,
                    only_transpile: false,
                    no_judge: true,
                    no_check_duplication: false,
                    service: Some(ServiceKind::Yukicoder),
                    contest: Some("no".to_owned()),
                    language: Some("python3-with-invalid-lang-ids".to_owned()),
                    jobs: None,
                    color_choice: AnsiColorChoice::Never,
                    problem: "9000".to_owned(),
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
            app.run(Opt::Submit {
                open: false,
                force_compile: false,
                only_transpile: false,
                no_judge: true,
                no_check_duplication: false,
                service: Some(ServiceKind::Yukicoder),
                contest: Some("no".to_owned()),
                language: Some("text".to_owned()),
                jobs: None,
                color_choice: AnsiColorChoice::Never,
                problem: "9000".to_owned(),
            })
            .map_err(Into::into)
        },
    )
}
