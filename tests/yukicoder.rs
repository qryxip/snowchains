extern crate snowchains;

extern crate env_logger;
extern crate failure;
extern crate if_chain;
extern crate serde;
extern crate serde_derive;
extern crate serde_yaml;
extern crate strum;
extern crate tempdir;

#[allow(dead_code)]
mod common;

use snowchains::app::App;
use snowchains::errors::{ServiceError, ServiceErrorKind};
use snowchains::path::AbsPath;
use snowchains::service::ServiceName;
use snowchains::terminal::TermImpl;

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
