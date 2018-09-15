extern crate snowchains;

extern crate env_logger;
extern crate failure;
extern crate serde;
extern crate serde_yaml;
extern crate tempdir;

#[allow(dead_code)]
mod common;

use snowchains::app::App;
use snowchains::console::NullConsole;
use snowchains::service::ServiceName;

#[test]
#[ignore]
fn it_logins() {
    let _ = env_logger::try_init();
    let credentials = common::credentials_from_env_vars().unwrap();
    common::test_in_tempdir("it_logins", credentials, login);
}

#[test]
#[ignore]
#[should_panic(
    expected = "called `Result::unwrap()` on an `Err` value: Service(WrongCredentialsOnTest)"
)]
fn it_raises_an_error_if_when_login_fails() {
    let _ = env_logger::try_init();
    common::test_in_tempdir(
        "it_raises_an_error_if_the_credentials_is_wrong",
        common::dummy_credentials(),
        login,
    );
}

#[test]
#[ignore]
fn it_downloads_testcases_from_master() {
    let _ = env_logger::try_init();
    let credentials = common::credentials_from_env_vars().unwrap();
    common::test_in_tempdir(
        "it_downloads_test_cases_from_master",
        credentials,
        |app| -> Result<(), failure::Error> {
            static CONTEST: &str = "master";
            static PROBLEMS: &[&str] = &["solve-me-first", "simple-array-sum"];
            let wd = app.working_dir.clone();
            download(app, CONTEST, PROBLEMS)?;
            for problem in PROBLEMS {
                common::confirm_zip_exists(&wd, CONTEST, problem)?;
            }
            Ok(())
        },
    );
}

#[test]
#[ignore]
fn it_downloads_testcases_from_hourrank_20() {
    let _ = env_logger::try_init();
    let credentials = common::credentials_from_env_vars().unwrap();
    common::test_in_tempdir(
        "it_downloads_test_cases_from_hourrank_20",
        credentials,
        |app| -> Result<(), failure::Error> {
            static CONTEST: &str = "hourrank-20";
            static PROBLEM: &str = "hot-and-cold";
            let wd = app.working_dir.clone();
            download(app, CONTEST, &[PROBLEM])?;
            common::confirm_zip_exists(&wd, CONTEST, PROBLEM).map_err(Into::into)
        },
    );
}

fn login(app: App<NullConsole>) -> snowchains::Result<()> {
    common::login(app, ServiceName::Hackerrank)
}

fn download(app: App<NullConsole>, contest: &str, problems: &[&str]) -> snowchains::Result<()> {
    common::download(app, ServiceName::Hackerrank, contest, problems)
}
