extern crate snowchains;

#[macro_use]
extern crate serde_derive;

extern crate env_logger;
extern crate serde_yaml;
extern crate tempdir;

use snowchains::terminal::TerminalMode;
use snowchains::{util, Credentials, Opt, Prop, ServiceName};

use tempdir::TempDir;

use std::fs::File;
use std::path::{Path, PathBuf};
use std::rc::Rc;

#[test]
#[ignore]
fn it_logins() {
    fn login(prop: &Prop) -> snowchains::Result<()> {
        Opt::Login {
            service: ServiceName::AtCoder,
        }.run(prop)
    }
    let _ = env_logger::try_init();
    let (tempdir1, prop1) = setup("it_logins_1", empty_credentials());
    let (tempdir2, prop2) = setup("it_logins_2", credentials_from_env_vars());
    login(&prop1).unwrap_err();
    login(&prop2).unwrap();
    tempdir1.close().unwrap();
    tempdir2.close().unwrap();
}

#[test]
#[ignore]
fn it_scrapes_samples_from_practice() {
    let _ = env_logger::try_init();
    let (tempdir, prop) = setup(
        "it_scrapes_samples_from_practice",
        credentials_from_env_vars(),
    );
    Opt::Download {
        service: Some(ServiceName::AtCoder),
        contest: Some("practice".to_owned()),
        open_browser: false,
    }.run(&prop)
        .unwrap();
    let download_dir = tempdir
        .path()
        .join("snowchains")
        .join("atcoder")
        .join("practice");
    just_confirm_num_samples_and_timelimit(&download_dir, "a", 2, 2000);
    just_confirm_num_samples_and_timelimit(&download_dir, "b", 0, 2000);
    tempdir.close().unwrap();
}

#[test]
#[ignore]
fn it_scrapes_samples_from_arc058() {
    let _ = env_logger::try_init();
    let (tempdir, prop) = setup("it_scrapes_samples_from_arc058", Credentials::None);
    Opt::Download {
        service: Some(ServiceName::AtCoder),
        contest: Some("arc058".to_owned()),
        open_browser: false,
    }.run(&prop)
        .unwrap();
    let download_dir = tempdir
        .path()
        .join("snowchains")
        .join("atcoder")
        .join("arc058");
    just_confirm_num_samples_and_timelimit(&download_dir, "c", 2, 2000);
    just_confirm_num_samples_and_timelimit(&download_dir, "d", 4, 2000);
    just_confirm_num_samples_and_timelimit(&download_dir, "e", 4, 4000);
    just_confirm_num_samples_and_timelimit(&download_dir, "f", 3, 5000);
    tempdir.close().unwrap();
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
    println!("Opening {}", path.display());
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
    static CODE: &[u8] = br#"#!/usr/bin/env python3


def main():
    (a, (b, c), s) = (int(input()), map(int, input().split()), input())
    print('{{}} {{}}'.format(a + b + c, s))


if __name__ == '__main__':
    main()
"#;
    let _ = env_logger::try_init();
    let (tempdir, prop) = setup("it_submits_to_practice_a", credentials_from_env_vars());
    util::fs::write(&tempdir.path().join("py").join("a.py"), CODE).unwrap();
    Opt::Submit {
        target: "a".to_owned(),
        language: Some("python3".to_owned()),
        service: Some(ServiceName::AtCoder),
        contest: Some("practice".to_owned()),
        open_browser: false,
        skip_judging: true,
        skip_checking_duplication: false,
    }.run(&prop)
        .unwrap();
    tempdir.close().unwrap();
}

fn setup(tempdir_prefix: &str, credentials: Credentials) -> (TempDir, Prop) {
    let tempdir = TempDir::new(tempdir_prefix).unwrap();
    let prop = Prop {
        working_dir: tempdir.path().to_owned(),
        default_lang_on_init: Some("python3"),
        cookies_dir: tempdir.path().to_owned(),
        terminal_mode: TerminalMode::Plain,
        credentials,
    };
    Opt::Init {
        directory: PathBuf::from("."),
    }.run(&prop)
        .unwrap();
    (tempdir, prop)
}

fn credentials_from_env_vars() -> Credentials {
    Credentials::from_env_vars("ATCODER_USERNAME", "ATCODER_PASSWORD").unwrap()
}

fn empty_credentials() -> Credentials {
    Credentials::Some {
        username: Rc::new("".to_owned()),
        password: Rc::new("".to_owned()),
    }
}
