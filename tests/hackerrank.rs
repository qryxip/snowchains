extern crate snowchains;

extern crate env_logger;
extern crate failure;
extern crate tempdir;

use snowchains::palette::ColorChoice;
use snowchains::path::AbsPathBuf;
use snowchains::{Credentials, Opt, Prop, ServiceName};

use failure::Fail;
use tempdir::TempDir;

use std::borrow::Cow;
use std::path::PathBuf;
use std::rc::Rc;

#[test]
#[ignore]
fn it_logins() {
    let _ = env_logger::try_init();
    test("it_logins", false, login).unwrap();
}

#[test]
#[ignore]
#[should_panic(
    expected = "called `Result::unwrap()` on an `Err` value: Service(WrongCredentialsOnTest)"
)]
fn it_raises_an_error_if_the_credentials_are_wrong() {
    let _ = env_logger::try_init();
    test(
        "it_raises_an_error_if_the_credentials_is_wrong",
        true,
        login,
    ).unwrap();
}

fn login(prop: &Prop) -> snowchains::Result<()> {
    Opt::Login {
        color_choice: ColorChoice::Never,
        service: ServiceName::HackerRank,
    }.run(prop)
}

fn test<E: Fail>(
    tempdir_prefix: &str,
    use_dummy_credentials: bool,
    f: impl FnOnce(&Prop) -> Result<(), E>,
) -> Result<(), failure::Error> {
    let tempdir = TempDir::new(tempdir_prefix)?;
    let result = (|| -> Result<(), failure::Error> {
        let credentials = if use_dummy_credentials {
            Credentials::UserNameAndPassword(Rc::new("".to_owned()), Rc::new("".to_owned()))
        } else {
            Credentials::from_env_vars("HACKERRANK_USERNAME", "HACKERRANK_PASSWORD")?
        };
        let prop = Prop {
            working_dir: AbsPathBuf::new_or_panic(tempdir.path()),
            cookies_on_init: Cow::from("$service"),
            credentials,
        };
        Opt::Init {
            color_choice: ColorChoice::Never,
            directory: PathBuf::from("."),
        }.run(&prop)?;
        f(&prop).map_err(Into::into)
    })();
    tempdir.close()?;
    result
}
