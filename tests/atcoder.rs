extern crate snowchains;

#[macro_use]
extern crate serde_derive;

extern crate env_logger;
extern crate failure;
extern crate serde;
extern crate serde_yaml;
extern crate tempdir;

mod common;

use snowchains::app::{App, Opt};
use snowchains::console::{ColorChoice, Console};
use snowchains::ServiceName;

use std::fs::File;
use std::io;
use std::path::Path;

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
fn it_raises_an_when_login_fails() {
    let _ = env_logger::try_init();
    let credentials = common::dummy_credentials();
    common::test_in_tempdir("it_raises_an_when_login_fails", credentials, login);
}

fn login(mut app: App<Console<io::Empty, io::Sink, io::Sink>>) -> snowchains::Result<()> {
    app.run(Opt::Login {
        color_choice: ColorChoice::Never,
        service: ServiceName::Atcoder,
    })
}

#[test]
#[ignore]
fn it_scrapes_samples_from_practice() {
    let _ = env_logger::try_init();
    let credentials = common::credentials_from_env_vars().unwrap();
    common::test_in_tempdir(
        "it_scrapes_samples_from_practice",
        credentials,
        |mut app| -> snowchains::Result<()> {
            app.run(Opt::Download {
                open_browser: false,
                service: Some(ServiceName::Atcoder),
                contest: Some("practice".to_owned()),
                problems: vec![],
                color_choice: ColorChoice::Never,
            })?;
            let download_dir = app
                .working_dir
                .join("snowchains")
                .join("atcoder")
                .join("practice");
            just_confirm_num_samples_and_timelimit(&download_dir, "a", 2, 2000);
            just_confirm_num_samples_and_timelimit(&download_dir, "b", 0, 2000);
            Ok(())
        },
    );
}

#[test]
#[ignore]
fn it_scrapes_samples_from_arc058() {
    let _ = env_logger::try_init();
    let credentials = common::credentials_from_env_vars().unwrap();
    common::test_in_tempdir(
        "it_scrapes_samples_from_arc058",
        credentials,
        |mut app| -> snowchains::Result<()> {
            app.run(Opt::Download {
                open_browser: false,
                service: Some(ServiceName::Atcoder),
                contest: Some("arc058".to_owned()),
                problems: vec![],
                color_choice: ColorChoice::Never,
            })?;
            let download_dir = app
                .working_dir
                .join("snowchains")
                .join("atcoder")
                .join("arc058");
            just_confirm_num_samples_and_timelimit(&download_dir, "c", 2, 2000);
            just_confirm_num_samples_and_timelimit(&download_dir, "d", 4, 2000);
            just_confirm_num_samples_and_timelimit(&download_dir, "e", 4, 4000);
            just_confirm_num_samples_and_timelimit(&download_dir, "f", 3, 5000);
            Ok(())
        },
    );
}

fn just_confirm_num_samples_and_timelimit(dir: &Path, name: &str, n: usize, t: u64) {
    #[derive(Deserialize)]
    #[serde(tag = "type", rename_all = "lowercase")]
    enum TestSuite {
        Simple {
            timelimit: u64,
            cases: Vec<serde_yaml::Mapping>,
        },
        Interactive {
            timelimit: u64,
            each_args: Vec<serde_yaml::Sequence>,
        },
    }
    let path = dir.join(name).with_extension("yaml");
    let file = File::open(&path).unwrap();
    match serde_yaml::from_reader::<_, TestSuite>(file).unwrap() {
        TestSuite::Simple { timelimit, cases } => {
            assert_eq!(t, timelimit);
            assert_eq!(n, cases.len())
        }
        TestSuite::Interactive {
            timelimit,
            each_args,
        } => {
            assert_eq!(t, timelimit);
            assert_eq!(n, each_args.len())
        }
    }
}

#[test]
#[ignore]
fn it_submits_to_practice_a() {
    let _ = env_logger::try_init();
    let credentials = common::credentials_from_env_vars().unwrap();
    common::test_in_tempdir(
        "it_submits_to_practice_a",
        credentials,
        |mut app| -> Result<(), failure::Error> {
            static CODE: &[u8] = br#"#!/usr/bin/env python3


def main():
    (a, (b, c), s) = (int(input()), map(int, input().split()), input())
    print('{{}} {{}}'.format(a + b + c, s))


if __name__ == '__main__':
    main()
"#;
            std::fs::create_dir(&app.working_dir.join("py"))?;
            std::fs::write(&app.working_dir.join("py").join("a.py"), CODE)?;
            app.run(Opt::Submit {
                open_browser: false,
                skip_judging: true,
                skip_checking_duplication: false,
                language: Some("python3".to_owned()),
                service: Some(ServiceName::Atcoder),
                contest: Some("practice".to_owned()),
                color_choice: ColorChoice::Never,
                problem: "a".to_owned(),
            }).map_err(Into::into)
        },
    );
}
