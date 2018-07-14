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
use std::rc::Rc;

#[test]
#[ignore]
fn it_logins() {
    let _ = env_logger::try_init();
    let credentials = credentials_from_env_var().unwrap();
    common::test("it_logins", credentials, login).unwrap();
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
fn it_downloads_testcases() {
    let _ = env_logger::try_init();
    let credentials = credentials_from_env_var().unwrap();
    common::test(
        "it_downloads_test_cases_from_master",
        credentials,
        |prop| -> Result<(), failure::Error> {
            static CONTEST: &str = "no";
            download(prop, CONTEST, &["1", "2", "3"])?;
            confirm_num_cases(prop, CONTEST, &[("1", 3), ("2", 4), ("3", 3)]);
            Ok(())
        },
    ).unwrap();
}

fn login(prop: &Prop) -> snowchains::Result<()> {
    common::login(prop, ServiceName::Yukicoder)
}

fn download(prop: &Prop, contest: &str, problems: &[&str]) -> snowchains::Result<()> {
    common::download(prop, ServiceName::Yukicoder, contest, problems)
}

fn confirm_num_cases(prop: &Prop, contest: &str, pairs: &[(&str, usize)]) {
    common::confirm_num_cases(prop, ServiceName::Yukicoder, contest, pairs)
}

fn credentials_from_env_var() -> Result<Credentials, env::VarError> {
    env::var("YUKICODER_REVEL_SESSION")
        .map(Rc::new)
        .map(Credentials::RevelSession)
}
