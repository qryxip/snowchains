extern crate snowchains;

extern crate env_logger;
extern crate httpsession;
extern crate tempdir;

use snowchains::config::ServiceName;
use snowchains::service::{DownloadProp, InitProp, SubmitProp};
use snowchains::service::atcoder_beta;
use snowchains::terminal;
use snowchains::testsuite::{ExpectedStdout, SuiteFileExtension, SuiteFilePaths, TestCases};

use httpsession::ColorMode;
use tempdir::TempDir;

use std::env;
use std::ffi::OsStr;
use std::fs::File;
use std::io::Write as _Write;
use std::path::Path;
use std::time::Duration;

#[test]
#[ignore]
fn it_logins() {
    let _ = env_logger::try_init();
    terminal::disable_color();
    let tempdir = TempDir::new("atcoder_it_logins").unwrap();
    atcoder_beta::login(&init_prop(&tempdir, false)).unwrap_err();
    atcoder_beta::login(&init_prop(&tempdir, true)).unwrap();
    atcoder_beta::login(&init_prop(&tempdir, false)).unwrap();
    tempdir.close().unwrap();
}

#[test]
#[ignore]
fn it_scrapes_samples_from_practice() {
    let _ = env_logger::try_init();
    terminal::disable_color();
    let tempdir = TempDir::new("atcoder_it_scrapes_samples_from_practice").unwrap();
    let init_prop = init_prop(&tempdir, true);
    let download_dir = tempdir.path().join("practice");
    let download_prop = DownloadProp::new(
        "practice",
        download_dir.clone(),
        SuiteFileExtension::Yaml,
        false,
    );
    atcoder_beta::download(&init_prop, download_prop).unwrap();
    static SAMPLES_A: &[(u64, &str, &str)] = &[
        (2, "1\n2 3\ntest\n", "6 test\n"),
        (2, "72\n128 256\nmyonmyon\n", "456 myonmyon\n"),
    ];
    check_samples("a", &download_dir, SAMPLES_A);
    let cases = SuiteFilePaths::new(&download_dir, "b", &[SuiteFileExtension::Yaml])
        .load_merging(false)
        .unwrap();
    match cases {
        TestCases::Simple(_) => panic!(),
        TestCases::Interactive(ref cases) => assert!(cases.is_empty()),
    }
    tempdir.close().unwrap();
}

#[test]
#[ignore]
fn it_scrapes_samples_from_arc058() {
    let _ = env_logger::try_init();
    terminal::disable_color();
    let tempdir = TempDir::new("atcoder_it_scrapes_samples_from_arc058").unwrap();
    let init_prop = init_prop(&tempdir, false);
    let download_dir = tempdir.path().join("arc058");
    let download_prop = DownloadProp::new(
        "arc058",
        download_dir.clone(),
        SuiteFileExtension::Yaml,
        false,
    );
    atcoder_beta::download(&init_prop, download_prop).unwrap();
    static SAMPLES_C: &[(u64, &str, &str)] = &[
        (2, "1000 8\n1 3 4 5 6 7 8 9\n", "2000\n"),
        (2, "9999 1\n0\n", "9999\n"),
    ];
    static SAMPLES_D: &[(u64, &str, &str)] = &[
        (2, "2 3 1 1\n", "2\n"),
        (2, "10 7 3 4\n", "3570\n"),
        (2, "100000 100000 99999 99999\n", "1\n"),
        (2, "100000 100000 44444 55555\n", "738162020\n"),
    ];
    static SAMPLES_E: &[(u64, &str, &str)] = &[
        (4, "3 5 7 5\n", "1\n"),
        (4, "4 5 7 5\n", "34\n"),
        (4, "37 4 2 3\n", "863912418\n"),
        (4, "40 5 7 5\n", "562805100\n"),
    ];
    static SAMPLES_F: &[(u64, &str, &str)] = &[
        (5, "3 7\nat\ncoder\ncodar\n", "atcodar\n"),
        (5, "3 7\ncoder\ncodar\nat\n", "codarat\n"),
        (
            5,
            "4 13\nkyuri\nnamida\nzzzzzzz\naaaaaa\n",
            "namidazzzzzzz\n",
        ),
    ];
    for &(name, expected) in &[
        ("c", SAMPLES_C),
        ("d", SAMPLES_D),
        ("e", SAMPLES_E),
        ("f", SAMPLES_F),
    ] {
        check_samples(name, &download_dir, expected);
    }
    tempdir.close().unwrap();
}

fn check_samples(name: &str, download_dir: &Path, expected: &[(u64, &str, &str)]) {
    let cases = SuiteFilePaths::new(download_dir, name, &[SuiteFileExtension::Yaml])
        .load_merging(false)
        .unwrap();
    match cases {
        TestCases::Interactive(_) => panic!(),
        TestCases::Simple(ref cases) => {
            assert_eq!(expected.len(), cases.len());
            for (&(t1, i1, o1), c) in expected.iter().zip(cases.iter()) {
                let (i2, o2, t2) = c.values();
                assert_eq!(Some(Duration::from_secs(t1)), t2);
                assert_eq!(i1, i2.as_str());
                if cfg!(windows) {
                    match *o2 {
                        ExpectedStdout::Lines(_) => {}
                        _ => panic!(),
                    }
                    assert!(o2.accepts(&o1));
                } else {
                    match *o2 {
                        ExpectedStdout::Exact(ref o2) => assert_eq!(o1, o2),
                        _ => panic!(),
                    }
                }
            }
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

    let tempdir = TempDir::new("it_submits_to_practice_a").unwrap();
    let src = tempdir.path().join("a.py");
    File::create(&src)
        .unwrap()
        .write_all(code().as_bytes())
        .unwrap();
    let init_prop = init_prop(&tempdir, true);
    let submit_prop = SubmitProp::new("practice", "a".to_owned(), 3023, src, None, false, true);
    atcoder_beta::submit(&init_prop, submit_prop).unwrap();
}

fn init_prop(tempdir: &TempDir, credentials: bool) -> InitProp {
    let credentials = if credentials {
        let username = env::var("ATCODER_USERNAME").unwrap();
        let password = env::var("ATCODER_PASSWORD").unwrap();
        Some((username, password))
    } else {
        Some(("invalid-username".to_owned(), "invalid-password".to_owned()))
    };
    let cookie_path = tempdir.path().join(ServiceName::AtCoderBeta.as_str());
    InitProp::new(
        cookie_path,
        ColorMode::NoColor,
        Duration::from_secs(10),
        credentials,
    )
}
