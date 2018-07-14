extern crate snowchains;

extern crate env_logger;
extern crate failure;
extern crate serde;
extern crate serde_yaml;
extern crate tempdir;

#[allow(dead_code)]
mod common;

use snowchains::{Credentials, Prop, ServiceName};

use std::env;

#[test]
#[ignore]
fn it_logins() {
    let _ = env_logger::try_init();
    let credentials = credentials_from_env_vars().unwrap();
    common::test("it_logins", credentials, |prop| login(prop)).unwrap();
}

#[test]
#[ignore]
#[should_panic(
    expected = "called `Result::unwrap()` on an `Err` value: Service(WrongCredentialsOnTest)"
)]
fn it_raises_an_error_if_the_credentials_are_wrong() {
    let _ = env_logger::try_init();
    common::test(
        "it_raises_an_error_if_the_credentials_is_wrong",
        common::dummy_credentials(),
        login,
    ).unwrap();
}

#[test]
#[ignore]
fn it_downloads_testcases_from_master() {
    let _ = env_logger::try_init();
    let credentials = credentials_from_env_vars().unwrap();
    common::test(
        "it_downloads_test_cases_from_master",
        credentials,
        |prop| -> Result<(), failure::Error> {
            static CONTEST: &str = "master";
            static PROBLEMS: &[&str] = &["solve-me-first", "simple-array-sum"];
            download(prop, CONTEST, PROBLEMS)?;
            for problem in PROBLEMS {
                common::confirm_zip_exists(prop, CONTEST, problem)?;
            }
            Ok(())
        },
    ).unwrap();
}

#[test]
#[ignore]
fn it_downloads_testcases_from_hourrank_20() {
    let _ = env_logger::try_init();
    let credentials = credentials_from_env_vars().unwrap();
    common::test(
        "it_downloads_test_cases_from_hourrank_20",
        credentials,
        |prop| -> Result<(), failure::Error> {
            static CONTEST: &str = "hourrank-20";
            static PROBLEM: &str = "hot-and-cold";
            download(prop, CONTEST, &[PROBLEM])?;
            common::confirm_zip_exists(prop, CONTEST, PROBLEM).map_err(Into::into)
        },
    ).unwrap();
}

fn login(prop: &Prop) -> snowchains::Result<()> {
    common::login(prop, ServiceName::HackerRank)
}

fn download(prop: &Prop, contest: &str, problems: &[&str]) -> snowchains::Result<()> {
    common::download(prop, ServiceName::HackerRank, contest, problems)
}

fn credentials_from_env_vars() -> Result<Credentials, env::VarError> {
    Credentials::from_env_vars("HACKERRANK_USERNAME", "HACKERRANK_PASSWORD")
}
