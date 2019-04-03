mod service;

use snowchains::app::{App, Download, ListLangs, Login, Opt, Restore, Submit};
use snowchains::config;
use snowchains::errors::{ServiceError, ServiceErrorKind};
use snowchains::service::ServiceKind;
use snowchains::terminal::{AnsiColorChoice, Term as _, TermImpl};

use difference::assert_diff;
use failure::Fallible;
use if_chain::if_chain;
use indexmap::IndexMap;
use pretty_assertions::assert_eq;
use serde_derive::Deserialize;

use std::fs::File;
use std::path::Path;
use std::{io, mem, str};

#[test]
fn it_logins() -> Fallible<()> {
    fn login(mut app: App<TermImpl<&[u8], Vec<u8>, Vec<u8>>>) -> Fallible<()> {
        app.run(Opt::Login(Login {
            json: true,
            color_choice: AnsiColorChoice::Never,
            service: ServiceKind::Atcoder,
        }))?;
        let stdout = String::from_utf8(mem::replace(app.term.stdout().get_mut(), vec![]))?;
        let stderr = String::from_utf8(mem::replace(app.term.stdout().get_mut(), vec![]))?;
        serde_json::from_str::<serde_json::Value>(&stdout)?;
        assert_diff!(&stderr, "", "\n", 0);
        Ok(())
    }

    let _ = env_logger::try_init();
    let stdin = credentials_as_input()?;
    service::test_in_tempdir("it_logins", &stdin, login)
}

#[test]
fn it_scrapes_samples_from_practice() -> Fallible<()> {
    let _ = env_logger::try_init();
    service::test_in_tempdir(
        "it_scrapes_samples_from_practice",
        &credentials_as_input()?,
        |mut app| -> snowchains::Result<()> {
            app.run(Opt::Download(Download {
                json: false,
                open: false,
                only_scraped: true,
                service: Some(ServiceKind::Atcoder),
                contest: Some("practice".to_owned()),
                problems: vec![],
                color_choice: AnsiColorChoice::Never,
            }))?;
            let download_dir = app
                .working_dir
                .join(".snowchains")
                .join("tests")
                .join("atcoder")
                .join("practice");
            check_yaml_md5(&download_dir, "a", "f9da086de05e439ebe3bac66cfc1ef89")?;
            check_yaml_md5(&download_dir, "b", "4cccced6eee33d234bc084c12b2db7c2")?;
            Ok(())
        },
    )
}

#[test]
fn it_scrapes_samples_from_abc100() -> Fallible<()> {
    let _ = env_logger::try_init();
    service::test_in_tempdir(
        "it_scrapes_samples_from_abc100",
        &credentials_as_input()?,
        |mut app| -> snowchains::Result<()> {
            app.run(Opt::Download(Download {
                json: false,
                open: false,
                only_scraped: true,
                service: Some(ServiceKind::Atcoder),
                contest: Some("abc100".to_owned()),
                problems: vec![],
                color_choice: AnsiColorChoice::Never,
            }))?;
            let download_dir = app
                .working_dir
                .join(".snowchains")
                .join("tests")
                .join("atcoder")
                .join("abc100");
            check_yaml_md5(&download_dir, "a", "86531ee215ce7634434b1a0b8ed9d932")?;
            check_yaml_md5(&download_dir, "b", "8aa1291a4fdba2ebc2f1fdc6fc484394")?;
            check_yaml_md5(&download_dir, "c", "61e0e720317997d3f27a0fa4fed0bb51")?;
            check_yaml_md5(&download_dir, "d", "599ec4fb07dcc02532944cab6f8e49f8")?;
            Ok(())
        },
    )
}

#[test]
fn it_scrapes_samples_and_download_files_from_abc099_a() -> Fallible<()> {
    let _ = env_logger::try_init();
    service::test_in_tempdir(
        "it_scrapes_samples_and_download_files_from_abc099_a",
        &credentials_as_input()?,
        |mut app| -> Fallible<()> {
            app.run(Opt::Download(Download {
                json: false,
                open: false,
                only_scraped: false,
                service: Some(ServiceKind::Atcoder),
                contest: Some("abc099".to_owned()),
                problems: vec!["a".to_owned()],
                color_choice: AnsiColorChoice::Never,
            }))?;
            // "ARC058_ABC042"
            let download_dir = app
                .working_dir
                .join(".snowchains")
                .join("tests")
                .join("atcoder")
                .join("abc099");
            just_confirm_num_samples_and_timelimit(&download_dir, "a", 9, "2000ms")
        },
    )
}

fn check_yaml_md5(dir: &Path, name: &str, expected: &str) -> io::Result<()> {
    let path = dir.join(name).with_extension("yml");
    let yaml = std::fs::read_to_string(&path)?;
    assert_eq!(format!("{:x}", md5::compute(&yaml)), expected);
    Ok(())
}

fn just_confirm_num_samples_and_timelimit(
    dir: &Path,
    name: &str,
    n: usize,
    t: &str,
) -> Fallible<()> {
    #[derive(Deserialize)]
    #[serde(tag = "type", rename_all = "lowercase")]
    enum TestSuite {
        Batch {
            timelimit: String,
            cases: Vec<serde_yaml::Mapping>,
        },
        Interactive {
            timelimit: String,
            each_args: Vec<serde_yaml::Sequence>,
        },
    }
    let path = dir.join(name).with_extension("yml");
    let file = File::open(&path)?;
    match serde_yaml::from_reader::<_, TestSuite>(file)? {
        TestSuite::Batch { timelimit, cases } => {
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
    Ok(())
}

#[test]
fn restore_command_works_without_error() -> Fallible<()> {
    let _ = env_logger::try_init();
    service::test_in_tempdir(
        "it_restores_source_code",
        &credentials_as_input()?,
        |mut app| -> Fallible<()> {
            app.run(Opt::Restore(Restore {
                json: true,
                service: Some(ServiceKind::Atcoder),
                contest: Some("practice".to_owned()),
                mode: config::Mode::Debug,
                problems: vec![],
                color_choice: AnsiColorChoice::Never,
            }))?;
            let stdout = String::from_utf8(app.term.stdout().get_ref().to_owned())?;
            let stderr = String::from_utf8(app.term.stderr().get_ref().to_owned())?;
            serde_json::from_str::<serde_json::Value>(&stdout)?;
            assert!(stderr.starts_with(
                r#"Targets: practice contest/*
GET https://atcoder.jp/contests/practice/submissions/me?page=1 ... 302 Found
GET https://atcoder.jp/contests/practice ... 200 OK
GET https://atcoder.jp/settings ... 302 Found
GET https://atcoder.jp/login ... 200 OK
Username: Password: POST https://atcoder.jp/login ... 302 Found
GET https://atcoder.jp/settings ... 200 OK
Successfully logged in.
GET https://atcoder.jp/contests/practice ... 200 OK
"#,
            ));
            Ok(())
        },
    )
}

#[test]
fn it_fails_to_submit_if_the_lang_name_is_invalid() -> Fallible<()> {
    let _ = env_logger::try_init();
    service::test_in_tempdir(
        "it_fails_to_submit_if_the_lang_name_is_invalid",
        &credentials_as_input()?,
        |mut app| -> Fallible<()> {
            static CODE: &[u8] = b"#";
            let wd = app.working_dir.join("atcoder").join("practice").join("py");
            std::fs::create_dir_all(&wd)?;
            std::fs::write(&wd.join("a.py"), CODE)?;
            let err = app
                .run(Opt::Submit(Submit {
                    json: false,
                    open: false,
                    force_compile: false,
                    only_transpile: false,
                    no_judge: true,
                    debug: false,
                    no_check_duplication: false,
                    service: Some(ServiceKind::Atcoder),
                    contest: Some("practice".to_owned()),
                    language: Some("python3-with-invalid-lang-names".to_owned()),
                    mode: config::Mode::Release,
                    jobs: None,
                    color_choice: AnsiColorChoice::Never,
                    problem: "a".to_owned(),
                }))
                .unwrap_err();
            if_chain! {
                if let snowchains::Error::Service(ServiceError::Context(ctx)) = &err;
                if let ServiceErrorKind::NoSuchLang(lang_name) = ctx.get_context();
                then {
                    assert_eq!(lang_name, "invalid");
                    Ok(())
                } else {
                    Err(err.into())
                }
            }
        },
    )
}

#[test]
#[ignore]
fn it_submits_to_practice_a() -> Fallible<()> {
    let _ = env_logger::try_init();
    service::test_in_tempdir(
        "it_submits_to_practice_a",
        &credentials_as_input()?,
        |mut app| -> Fallible<()> {
            static CODE: &[u8] = br#"def main():
    (a, (b, c), s) = (int(input()), map(int, input().split()), input())
    print('{} {}'.format(a + b + c, s))


if __name__ == '__main__':
    main()
"#;
            let wd = app.working_dir.join("atcoder").join("practice").join("py");
            std::fs::create_dir_all(&wd)?;
            std::fs::write(&wd.join("a.py"), CODE)?;
            app.run(Opt::Submit(Submit {
                json: false,
                open: false,
                force_compile: false,
                only_transpile: false,
                no_judge: true,
                debug: false,
                no_check_duplication: false,
                service: Some(ServiceKind::Atcoder),
                contest: Some("practice".to_owned()),
                language: Some("python3".to_owned()),
                mode: config::Mode::Release,
                jobs: None,
                color_choice: AnsiColorChoice::Never,
                problem: "a".to_owned(),
            }))
            .map_err(Into::into)
        },
    )
}

#[test]
fn it_lists_languages_in_practice() -> Fallible<()> {
    let _ = env_logger::try_init();
    service::test_in_tempdir(
        "it_lists_languages_in_practice",
        &credentials_as_input()?,
        |mut app| -> Fallible<()> {
            app.run(Opt::ListLangs(ListLangs {
                json: true,
                service: Some(ServiceKind::Atcoder),
                contest: Some("practice".to_owned()),
                color_choice: AnsiColorChoice::Never,
                problem: None,
            }))?;
            let stdout = String::from_utf8(app.term.stdout().get_ref().to_owned())?;
            let stderr = String::from_utf8(app.term.stderr().get_ref().to_owned())?;
            let stdout = serde_json::from_str::<ListLangsStdout>(&stdout)?;
            assert_eq!(stdout.available_languages.len(), 56);
            assert_diff!(
                &stderr,
                r#"Targets: practice contest/*
GET https://atcoder.jp/login ... 200 OK
Username: Password: POST https://atcoder.jp/login ... 302 Found
GET https://atcoder.jp/settings ... 200 OK
Successfully logged in.
GET https://atcoder.jp/contests/practice/submit ... 200 OK
+---------------------------------+------+
| Name                            | ID   |
+---------------------------------+------+
| C++14 (GCC 5.4.1)               | 3003 |
+---------------------------------+------+
| Bash (GNU bash v4.3.11)         | 3001 |
+---------------------------------+------+
| C (GCC 5.4.1)                   | 3002 |
+---------------------------------+------+
| C (Clang 3.8.0)                 | 3004 |
+---------------------------------+------+
| C++14 (Clang 3.8.0)             | 3005 |
+---------------------------------+------+
| C# (Mono 4.6.2.0)               | 3006 |
+---------------------------------+------+
| Clojure (1.8.0)                 | 3007 |
+---------------------------------+------+
| Common Lisp (SBCL 1.1.14)       | 3008 |
+---------------------------------+------+
| D (DMD64 v2.070.1)              | 3009 |
+---------------------------------+------+
| D (LDC 0.17.0)                  | 3010 |
+---------------------------------+------+
| D (GDC 4.9.4)                   | 3011 |
+---------------------------------+------+
| Fortran (gfortran v4.8.4)       | 3012 |
+---------------------------------+------+
| Go (1.6)                        | 3013 |
+---------------------------------+------+
| Haskell (GHC 7.10.3)            | 3014 |
+---------------------------------+------+
| Java7 (OpenJDK 1.7.0)           | 3015 |
+---------------------------------+------+
| Java8 (OpenJDK 1.8.0)           | 3016 |
+---------------------------------+------+
| JavaScript (node.js v5.12)      | 3017 |
+---------------------------------+------+
| OCaml (4.02.3)                  | 3018 |
+---------------------------------+------+
| Pascal (FPC 2.6.2)              | 3019 |
+---------------------------------+------+
| Perl (v5.18.2)                  | 3020 |
+---------------------------------+------+
| PHP (5.6.30)                    | 3021 |
+---------------------------------+------+
| Python2 (2.7.6)                 | 3022 |
+---------------------------------+------+
| Python3 (3.4.3)                 | 3023 |
+---------------------------------+------+
| Ruby (2.3.3)                    | 3024 |
+---------------------------------+------+
| Scala (2.11.7)                  | 3025 |
+---------------------------------+------+
| Scheme (Gauche 0.9.3.3)         | 3026 |
+---------------------------------+------+
| Text (cat)                      | 3027 |
+---------------------------------+------+
| Visual Basic (Mono 4.0.1)       | 3028 |
+---------------------------------+------+
| C++ (GCC 5.4.1)                 | 3029 |
+---------------------------------+------+
| C++ (Clang 3.8.0)               | 3030 |
+---------------------------------+------+
| Objective-C (GCC 5.3.0)         | 3501 |
+---------------------------------+------+
| Objective-C (Clang3.8.0)        | 3502 |
+---------------------------------+------+
| Swift (swift-2.2-RELEASE)       | 3503 |
+---------------------------------+------+
| Rust (1.15.1)                   | 3504 |
+---------------------------------+------+
| Sed (GNU sed 4.2.2)             | 3505 |
+---------------------------------+------+
| Awk (mawk 1.3.3)                | 3506 |
+---------------------------------+------+
| Brainfuck (bf 20041219)         | 3507 |
+---------------------------------+------+
| Standard ML (MLton 20100608)    | 3508 |
+---------------------------------+------+
| PyPy2 (5.6.0)                   | 3509 |
+---------------------------------+------+
| PyPy3 (2.4.0)                   | 3510 |
+---------------------------------+------+
| Crystal (0.20.5)                | 3511 |
+---------------------------------+------+
| F# (Mono 4.0)                   | 3512 |
+---------------------------------+------+
| Unlambda (0.1.3)                | 3513 |
+---------------------------------+------+
| Lua (5.3.2)                     | 3514 |
+---------------------------------+------+
| LuaJIT (2.0.4)                  | 3515 |
+---------------------------------+------+
| MoonScript (0.5.0)              | 3516 |
+---------------------------------+------+
| Ceylon (1.2.1)                  | 3517 |
+---------------------------------+------+
| Julia (0.5.0)                   | 3518 |
+---------------------------------+------+
| Octave (4.0.2)                  | 3519 |
+---------------------------------+------+
| Nim (0.13.0)                    | 3520 |
+---------------------------------+------+
| TypeScript (2.1.6)              | 3521 |
+---------------------------------+------+
| Perl6 (rakudo-star 2016.01)     | 3522 |
+---------------------------------+------+
| Kotlin (1.0.0)                  | 3523 |
+---------------------------------+------+
| PHP7 (7.0.15)                   | 3524 |
+---------------------------------+------+
| COBOL - Fixed (OpenCOBOL 1.1.0) | 3525 |
+---------------------------------+------+
| COBOL - Free (OpenCOBOL 1.1.0)  | 3526 |
+---------------------------------+------+
"#,
                "\n",
                0
            );
            Ok(())
        },
    )
}

#[test]
fn it_lists_languages_in_tenka1_2016_final_open_a() -> Fallible<()> {
    let _ = env_logger::try_init();
    service::test_in_tempdir(
        "it_lists_languages_in_tenka1_2016_final_open_a",
        &credentials_as_input()?,
        |mut app| -> Fallible<()> {
            app.run(Opt::ListLangs(ListLangs {
                json: true,
                service: Some(ServiceKind::Atcoder),
                contest: Some("tenka1-2016-final-open".to_owned()),
                color_choice: AnsiColorChoice::Never,
                problem: Some("a".to_owned()),
            }))?;
            let stdout = String::from_utf8(app.term.stdout().get_ref().to_owned())?;
            let stderr = String::from_utf8(app.term.stderr().get_ref().to_owned())?;
            let stdout = serde_json::from_str::<ListLangsStdout>(&stdout)?;
            assert_eq!(stdout.available_languages.len(), 1);
            assert_diff!(
                &stderr,
                r#"Target: tenka1-2016-final-open/A
GET https://atcoder.jp/login ... 200 OK
Username: Password: POST https://atcoder.jp/login ... 302 Found
GET https://atcoder.jp/settings ... 200 OK
Successfully logged in.
GET https://atcoder.jp/contests/tenka1-2016-final-open/tasks ... 200 OK
GET https://atcoder.jp/contests/tenka1-2016-final-open/tasks/tenka1_2016_final_a ... 200 OK
+------------+----+
| Name       | ID |
+------------+----+
| Text (cat) | 17 |
+------------+----+
"#,
                "\n",
                0
            );
            Ok(())
        },
    )
}

fn credentials_as_input() -> Fallible<String> {
    let username = service::env_var("ATCODER_USERNAME")?;
    let password = service::env_var("ATCODER_PASSWORD")?;
    Ok(format!("{}\n{}\n", username, password))
}

#[derive(Deserialize)]
struct ListLangsStdout {
    available_languages: IndexMap<String, String>,
}
