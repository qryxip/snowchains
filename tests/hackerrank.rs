extern crate snowchains;

extern crate env_logger;
extern crate failure;
extern crate heck;
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
use snowchains::service::ServiceName;
use snowchains::terminal::TermImpl;

use failure::Fallible;
use heck::SnakeCase as _SnakeCase;
use if_chain::if_chain;

use std::io;

#[test]
fn it_logins() {
    let _ = env_logger::try_init();
    let credentials = common::credentials_from_env_vars().unwrap();
    common::test_in_tempdir("it_logins", credentials, login).unwrap();
}

#[test]
fn it_raises_an_error_if_when_login_fails() {
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
fn it_downloads_testcases_from_master() {
    let _ = env_logger::try_init();
    let credentials = common::credentials_from_env_vars().unwrap();
    common::test_in_tempdir(
        "it_downloads_test_cases_from_master",
        credentials,
        |app| -> Fallible<()> {
            static CONTEST: &str = "master";
            static PROBLEMS: &[&str] = &["solve-me-first", "simple-array-sum"];
            let wd = app.working_dir.clone();
            download(app, CONTEST, PROBLEMS)?;
            for problem in PROBLEMS {
                common::confirm_zip_exists(&wd, CONTEST, &problem.to_snake_case())?;
            }
            Ok(())
        },
    )
    .unwrap();
}

#[test]
fn it_downloads_testcases_from_hourrank_20() {
    let _ = env_logger::try_init();
    let credentials = common::credentials_from_env_vars().unwrap();
    common::test_in_tempdir(
        "it_downloads_test_cases_from_hourrank_20",
        credentials,
        |app| -> Fallible<()> {
            static CONTEST: &str = "hourrank-20";
            static PROBLEM: &str = "hot-and-cold";
            let wd = app.working_dir.clone();
            download(app, CONTEST, &[PROBLEM])?;
            common::confirm_zip_exists(&wd, CONTEST, &PROBLEM.to_snake_case()).map_err(Into::into)
        },
    )
    .unwrap();
}

fn login(app: App<TermImpl<io::Empty, io::Sink, io::Sink>>) -> snowchains::Result<()> {
    common::login(app, ServiceName::Hackerrank)
}

fn download(
    app: App<TermImpl<io::Empty, io::Sink, io::Sink>>,
    contest: &str,
    problems: &[&str],
) -> snowchains::Result<()> {
    common::download(app, ServiceName::Hackerrank, contest, problems)
}
