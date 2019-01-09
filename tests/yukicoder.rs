#[allow(dead_code)]
mod common;

use snowchains::app::{App, Opt};
use snowchains::errors::{ServiceError, ServiceErrorKind};
use snowchains::path::AbsPath;
use snowchains::service::ServiceName;
use snowchains::terminal::{AnsiColorChoice, TermImpl};

use failure::Fallible;
use if_chain::if_chain;

use std::io;

#[test]
fn it_logins() {
    let _ = env_logger::try_init();
    let credentials = common::credentials_from_env_vars().unwrap();
    common::test_in_tempdir("it_logins", credentials, login).unwrap();
}

#[test]
fn it_raises_an_error_when_login_fails() {
    let _ = env_logger::try_init();
    let err = common::test_in_tempdir(
        "it_raises_an_error_when_login_fails",
        common::dummy_credentials(),
        login,
    )
    .unwrap_err();
    if_chain! {
        if let Some(snowchains::Error::Service(ServiceError::Context(ctx))) = err.downcast_ref();
        if let ServiceErrorKind::LoginOnTest = ctx.get_context();
        then {
        } else {
            panic!("{:?}", err);
        }
    }
}

#[test]
fn it_downloads_testcases() {
    let _ = env_logger::try_init();
    let credentials = common::credentials_from_env_vars().unwrap();
    common::test_in_tempdir(
        "it_downloads_test_cases_from_master",
        credentials,
        |app| -> Fallible<()> {
            static CONTEST: &str = "no";
            let wd = app.working_dir.clone();
            download(app, CONTEST, &["3", "725", "726"])?;
            confirm_num_cases(&wd, CONTEST, &[("3", 31), ("725", 9), ("726", 25)]);
            Ok(())
        },
    )
    .unwrap();
}

fn login(app: App<TermImpl<io::Empty, io::Sink, io::Sink>>) -> snowchains::Result<()> {
    common::login(app, ServiceName::Yukicoder)
}

fn download(
    app: App<TermImpl<io::Empty, io::Sink, io::Sink>>,
    contest: &str,
    problems: &[&str],
) -> snowchains::Result<()> {
    common::download(app, ServiceName::Yukicoder, contest, problems)
}

fn confirm_num_cases(wd: &AbsPath, contest: &str, pairs: &[(&str, usize)]) {
    common::confirm_num_cases(wd, ServiceName::Yukicoder, contest, pairs)
}

#[test]
#[ignore]
fn it_submits_to_no_9000() {
    let _ = env_logger::try_init();
    let credentials = common::credentials_from_env_vars().unwrap();
    common::test_in_tempdir(
        "it_submits_to_no_9000",
        credentials,
        |mut app| -> Fallible<()> {
            static CODE: &[u8] = b"Hello, World!";
            let wd = app.working_dir.join("yukicoder").join("no").join("txt");
            std::fs::create_dir_all(&wd)?;
            std::fs::write(&wd.join("9000.txt"), CODE)?;
            app.run(Opt::Submit {
                open: false,
                force_compile: false,
                only_transpile: false,
                no_judge: true,
                no_check_duplication: false,
                service: Some(ServiceName::Yukicoder),
                contest: Some("no".to_owned()),
                language: Some("text".to_owned()),
                jobs: None,
                color_choice: AnsiColorChoice::Never,
                problem: "9000".to_owned(),
            })
            .map_err(Into::into)
        },
    )
    .unwrap();
}
