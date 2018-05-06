extern crate snowchains;

#[macro_use]
extern crate serde_derive;

extern crate env_logger;
extern crate httpsession;
extern crate serde_yaml;
extern crate tempdir;

use snowchains::{terminal, util, ServiceName};
use snowchains::entrypoint::{Opt, Prop};

use httpsession::ColorMode;
use tempdir::TempDir;

use std::env;
use std::ffi::OsStr;
use std::fs::File;
use std::path::{Path, PathBuf};

#[test]
#[ignore]
fn it_logins() {
    fn login(prop: &Prop) -> snowchains::Result<()> {
        Opt::Login {
            service: ServiceName::AtCoder,
        }.run(prop)
    }
    let _ = env_logger::try_init();
    terminal::disable_color();
    let (tempdir1, prop1) = setup("it_logins_1", Credentials::Empty);
    let (tempdir2, prop2) = setup("it_logins_2", Credentials::EnvVars);
    login(&prop1).unwrap_err();
    login(&prop2).unwrap();
    tempdir1.close().unwrap();
    tempdir2.close().unwrap();
}

#[test]
#[ignore]
fn it_scrapes_samples_from_practice() {
    let _ = env_logger::try_init();
    let (tempdir, prop) = setup("it_scrapes_samples_from_practice", Credentials::EnvVars);
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
    fn code() -> String {
        let homedir = env::home_dir();
        let username = homedir
            .as_ref()
            .and_then(|h| h.file_name().map(OsStr::to_string_lossy))
            .unwrap_or_default();
        format!(
            r#""""Submitted by {}"""


def main():
    (a, (b, c), s) = (int(input()), map(int, input().split()), input())
    print('{{}} {{}}'.format(a + b + c, s))


if __name__ == '__main__':
    main()
"#,
            username,
        )
    }

    let _ = env_logger::try_init();
    terminal::disable_color();
    let (tempdir, prop) = setup("it_submits_to_practice_a", Credentials::EnvVars);

    util::fs::write(&tempdir.path().join("py").join("a.py"), code().as_bytes()).unwrap();

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
    terminal::disable_color();
    let tempdir = TempDir::new(tempdir_prefix).unwrap();
    let credentials = match credentials {
        Credentials::None => None,
        Credentials::Empty => Some(("".to_owned(), "".to_owned())),
        Credentials::EnvVars => {
            let username = env::var("ATCODER_USERNAME").unwrap();
            let password = env::var("ATCODER_PASSWORD").unwrap();
            Some((username, password))
        }
    };
    let prop = Prop {
        working_dir: tempdir.path().to_owned(),
        default_lang_on_init: Some("python3"),
        cookie_dir: tempdir.path().to_owned(),
        color_mode: ColorMode::NoColor,
        credentials,
    };
    Opt::Init {
        directory: PathBuf::from("."),
    }.run(&prop)
        .unwrap();
    (tempdir, prop)
}

enum Credentials {
    None,
    Empty,
    EnvVars,
}
