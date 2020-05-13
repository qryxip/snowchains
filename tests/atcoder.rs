mod common;

use crate::common::service;

use snowchains::config;
use snowchains::errors::{ServiceError, ServiceErrorKind};
use snowchains::service::ServiceKind;
use snowchains::terminal::AnsiColorChoice;
use snowchains::{
    Login, Opt, OutputKind, Retrieve, RetrieveLanguages, RetrieveSubmissions, RetrieveTestcases,
    Submit,
};

use difference::assert_diff;
use failure::Fallible;
use if_chain::if_chain;
use indexmap::IndexMap;
use pretty_assertions::assert_eq;
use serde::Deserialize;

use std::convert::TryFrom as _;
use std::fs::File;
use std::path::Path;
use std::{io, str};

#[test]
fn it_logins() -> Fallible<()> {
    let stdin = credentials_as_input()?;
    service::test_in_tempdir("it_logins", &stdin, |mut ctx| {
        let opt = Opt::Login(Login {
            json: false,
            colorize: false,
            output: OutputKind::Json,
            color_choice: AnsiColorChoice::Never,
            service: ServiceKind::Atcoder,
        });
        let code = snowchains::run(opt, &mut ctx)?;
        assert_eq!(code, 0);
        let stdout = String::try_from(ctx.stdout)?;
        let stderr = String::try_from(ctx.stderr)?;
        serde_json::from_str::<serde_json::Value>(&stdout)?;
        assert_diff!(
            &stderr,
            &r#"
Username: Password: GET https://atcoder.jp/login ... 200 OK
POST https://atcoder.jp/login ... 302 Found
GET https://atcoder.jp/settings ... 200 OK
Successfully logged in.
POST https://api.dropboxapi.com/2/users/get_current_account ... 200 OK
Already authorized.
"#[1..],
            "\n",
            0
        );
        Ok(())
    })
}

#[test]
fn it_scrapes_samples_from_practice() -> Fallible<()> {
    service::test_in_tempdir(
        "it_scrapes_samples_from_practice",
        &credentials_as_input()?,
        |mut ctx| -> _ {
            let opt = Opt::Retrieve(Retrieve::Testcases(RetrieveTestcases {
                full: false,
                no_save: false,
                open: false,
                verbose: false,
                json: false,
                colorize: false,
                service: Some(ServiceKind::Atcoder),
                contest: Some("practice".to_owned()),
                problems: vec![],
                output: OutputKind::Pretty,
                color_choice: AnsiColorChoice::Never,
            }));
            let code = snowchains::run(opt, &mut ctx)?;
            assert_eq!(code, 0);
            let dir = ctx
                .cwd
                .join(".snowchains")
                .join("tests")
                .join("atcoder")
                .join("practice");
            check_yaml_md5(&dir, "a", "f9da086de05e439ebe3bac66cfc1ef89")?;
            check_yaml_md5(&dir, "b", "4cccced6eee33d234bc084c12b2db7c2")?;
            Ok(())
        },
    )
}

#[test]
fn it_scrapes_samples_from_abc100() -> Fallible<()> {
    service::test_in_tempdir(
        "it_scrapes_samples_from_abc100",
        &credentials_as_input()?,
        |mut ctx| -> _ {
            let opt = Opt::Retrieve(Retrieve::Testcases(RetrieveTestcases {
                full: false,
                no_save: false,
                open: false,
                verbose: false,
                json: false,
                colorize: false,
                service: Some(ServiceKind::Atcoder),
                contest: Some("abc100".to_owned()),
                problems: vec![],
                output: OutputKind::Pretty,
                color_choice: AnsiColorChoice::Never,
            }));
            let code = snowchains::run(opt, &mut ctx)?;
            assert_eq!(code, 0);
            let dir = ctx
                .cwd
                .join(".snowchains")
                .join("tests")
                .join("atcoder")
                .join("abc100");
            check_yaml_md5(&dir, "a", "86531ee215ce7634434b1a0b8ed9d932")?;
            check_yaml_md5(&dir, "b", "8aa1291a4fdba2ebc2f1fdc6fc484394")?;
            check_yaml_md5(&dir, "c", "61e0e720317997d3f27a0fa4fed0bb51")?;
            check_yaml_md5(&dir, "d", "599ec4fb07dcc02532944cab6f8e49f8")?;
            Ok(())
        },
    )
}

#[test]
fn it_scrapes_samples_and_download_files_from_abc099_a() -> Fallible<()> {
    service::test_in_tempdir(
        "it_scrapes_samples_and_download_files_from_abc099_a",
        &credentials_as_input()?,
        |mut ctx| -> _ {
            let opt = Opt::Retrieve(Retrieve::Testcases(RetrieveTestcases {
                full: true,
                no_save: false,
                open: false,
                verbose: false,
                json: false,
                colorize: false,
                service: Some(ServiceKind::Atcoder),
                contest: Some("abc099".to_owned()),
                problems: vec!["a".to_owned()],
                output: OutputKind::Pretty,
                color_choice: AnsiColorChoice::Never,
            }));
            let code = snowchains::run(opt, &mut ctx)?;
            assert_eq!(code, 0);
            // "ARC058_ABC042"
            let dir = ctx
                .cwd
                .join(".snowchains")
                .join("tests")
                .join("atcoder")
                .join("abc099");
            just_confirm_num_samples_and_timelimit(&dir, "a", 9, "2000ms")
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
            assert_eq!(timelimit, t);
            assert_eq!(cases.len(), n)
        }
        TestSuite::Interactive {
            timelimit,
            each_args,
        } => {
            assert_eq!(timelimit, t);
            assert_eq!(each_args.len(), n)
        }
    }
    Ok(())
}

#[test]
fn retrieve_submissions_command_works_without_error() -> Fallible<()> {
    service::test_in_tempdir(
        "retrieve_submissions_command_works_without_error",
        &credentials_as_input()?,
        |mut ctx| -> _ {
            let opt = Opt::Retrieve(Retrieve::Submissions(RetrieveSubmissions {
                fetch_all: false,
                no_save: true,
                verbose: false,
                json: false,
                colorize: false,
                service: Some(ServiceKind::Atcoder),
                contest: Some("practice".to_owned()),
                mode: config::Mode::Debug,
                problems: vec![],
                output: OutputKind::Json,
                color_choice: AnsiColorChoice::Never,
            }));
            let code = snowchains::run(opt, &mut ctx)?;
            assert_eq!(code, 0);
            let stdout = String::try_from(ctx.stdout)?;
            let stderr = String::try_from(ctx.stderr)?;
            serde_json::from_str::<serde_json::Value>(&stdout)?;
            assert!(stderr.starts_with(
                r#"GET https://atcoder.jp/contests/practice/submissions/me?page=1 ... 302 Found
GET https://atcoder.jp/contests/practice ... 200 OK
GET https://atcoder.jp/settings ... 302 Found
Username: Password: GET https://atcoder.jp/login ... 200 OK
POST https://atcoder.jp/login ... 302 Found
GET https://atcoder.jp/settings ... 200 OK
Successfully logged in.
GET https://atcoder.jp/contests/practice ... 200 OK
GET https://atcoder.jp/contests/practice/submissions/me?page=1 ... 200 OK
"#,
            ));
            Ok(())
        },
    )
}

#[test]
fn it_fails_to_submit_if_the_lang_name_is_invalid() -> Fallible<()> {
    service::test_in_tempdir(
        "it_fails_to_submit_if_the_lang_name_is_invalid",
        &credentials_as_input()?,
        |mut ctx| -> _ {
            static CODE: &[u8] = b"#";
            let wd = ctx.cwd.join("atcoder").join("practice").join("py");
            std::fs::create_dir_all(&wd)?;
            std::fs::write(&wd.join("a.py"), CODE)?;
            let opt = Opt::Submit(Submit {
                open: false,
                force_compile: false,
                only_transpile: false,
                no_judge: true,
                debug: false,
                no_check_duplication: false,
                verbose: false,
                json: false,
                colorize: false,
                service: Some(ServiceKind::Atcoder),
                contest: Some("practice".to_owned()),
                language: Some("python3-with-invalid-lang-names".to_owned()),
                mode: config::Mode::Release,
                jobs: None,
                output: OutputKind::Pretty,
                color_choice: AnsiColorChoice::Never,
                problem: "a".to_owned(),
            });
            let err = snowchains::run(opt, &mut ctx).unwrap_err();
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
    service::test_in_tempdir(
        "it_submits_to_practice_a",
        &credentials_as_input()?,
        |mut ctx| -> _ {
            static CODE: &[u8] = br#"def main():
    (a, (b, c), s) = (int(input()), map(int, input().split()), input())
    print('{} {}'.format(a + b + c, s))


if __name__ == '__main__':
    main()
"#;
            let wd = ctx.cwd.join("atcoder").join("practice").join("py");
            std::fs::create_dir_all(&wd)?;
            std::fs::write(&wd.join("a.py"), CODE)?;
            let opt = Opt::Submit(Submit {
                open: false,
                force_compile: false,
                only_transpile: false,
                no_judge: true,
                debug: false,
                no_check_duplication: false,
                verbose: false,
                json: false,
                colorize: false,
                service: Some(ServiceKind::Atcoder),
                contest: Some("practice".to_owned()),
                language: Some("python3".to_owned()),
                mode: config::Mode::Release,
                jobs: None,
                output: OutputKind::Pretty,
                color_choice: AnsiColorChoice::Never,
                problem: "a".to_owned(),
            });
            let code = snowchains::run(opt, &mut ctx)?;
            assert_eq!(code, 0);
            Ok(())
        },
    )
}

#[test]
fn it_retrieves_languages_in_practice() -> Fallible<()> {
    service::test_in_tempdir(
        "it_retrieves_languages_in_practice",
        &credentials_as_input()?,
        |mut ctx| -> _ {
            let opt = Opt::Retrieve(Retrieve::Languages(RetrieveLanguages {
                json: false,
                colorize: false,
                service: Some(ServiceKind::Atcoder),
                contest: Some("practice".to_owned()),
                output: OutputKind::Pretty,
                color_choice: AnsiColorChoice::Never,
                problem: None,
            }));
            let code = snowchains::run(opt, &mut ctx)?;
            assert_eq!(code, 0);
            assert_diff!(
                &String::try_from(ctx.stdout)?,
                &r#"
┌──────────────────────────────────┬──────┐
│ Name                             │ ID   │
├──────────────────────────────────┼──────┤
│ C (GCC 9.2.1)                    │ 4001 │
├──────────────────────────────────┼──────┤
│ C (Clang 10.0.0)                 │ 4002 │
├──────────────────────────────────┼──────┤
│ C++ (GCC 9.2.1)                  │ 4003 │
├──────────────────────────────────┼──────┤
│ C++ (Clang 10.0.0)               │ 4004 │
├──────────────────────────────────┼──────┤
│ Java (OpenJDK 11.0.6)            │ 4005 │
├──────────────────────────────────┼──────┤
│ Python (3.8.2)                   │ 4006 │
├──────────────────────────────────┼──────┤
│ Bash (5.0.11)                    │ 4007 │
├──────────────────────────────────┼──────┤
│ bc (1.07.1)                      │ 4008 │
├──────────────────────────────────┼──────┤
│ Awk (GNU Awk 4.1.4)              │ 4009 │
├──────────────────────────────────┼──────┤
│ C# (.NET Core 3.1.201)           │ 4010 │
├──────────────────────────────────┼──────┤
│ C# (Mono-mcs 6.8.0.105)          │ 4011 │
├──────────────────────────────────┼──────┤
│ C# (Mono-csc 3.5.0)              │ 4012 │
├──────────────────────────────────┼──────┤
│ Clojure (1.10.1.536)             │ 4013 │
├──────────────────────────────────┼──────┤
│ Crystal (0.33.0)                 │ 4014 │
├──────────────────────────────────┼──────┤
│ D (DMD 2.091.0)                  │ 4015 │
├──────────────────────────────────┼──────┤
│ D (GDC 9.2.1)                    │ 4016 │
├──────────────────────────────────┼──────┤
│ D (LDC 1.20.1)                   │ 4017 │
├──────────────────────────────────┼──────┤
│ Dart (2.7.2)                     │ 4018 │
├──────────────────────────────────┼──────┤
│ dc (1.4.1)                       │ 4019 │
├──────────────────────────────────┼──────┤
│ Erlang (22.3)                    │ 4020 │
├──────────────────────────────────┼──────┤
│ Elixir (1.10.2)                  │ 4021 │
├──────────────────────────────────┼──────┤
│ F# (.NET Core 3.1.201)           │ 4022 │
├──────────────────────────────────┼──────┤
│ F# (Mono 10.2.3)                 │ 4023 │
├──────────────────────────────────┼──────┤
│ Forth (gforth 0.7.3)             │ 4024 │
├──────────────────────────────────┼──────┤
│ Fortran(GNU Fortran 9.2.1)       │ 4025 │
├──────────────────────────────────┼──────┤
│ Go (1.14.1)                      │ 4026 │
├──────────────────────────────────┼──────┤
│ Haskell (GHC 8.8.3)              │ 4027 │
├──────────────────────────────────┼──────┤
│ Haxe (4.0.3); js                 │ 4028 │
├──────────────────────────────────┼──────┤
│ Haxe (4.0.3); Java               │ 4029 │
├──────────────────────────────────┼──────┤
│ JavaScript (Node.js 12.16.1)     │ 4030 │
├──────────────────────────────────┼──────┤
│ Julia (1.4.0)                    │ 4031 │
├──────────────────────────────────┼──────┤
│ Kotlin (1.3.71)                  │ 4032 │
├──────────────────────────────────┼──────┤
│ Lua (Lua 5.3.5)                  │ 4033 │
├──────────────────────────────────┼──────┤
│ Lua (LuaJIT 2.1.0)               │ 4034 │
├──────────────────────────────────┼──────┤
│ Dash (0.5.8)                     │ 4035 │
├──────────────────────────────────┼──────┤
│ Nim (1.0.6)                      │ 4036 │
├──────────────────────────────────┼──────┤
│ Objective-C (Clang 10.0.0)       │ 4037 │
├──────────────────────────────────┼──────┤
│ Common Lisp (SBCL 2.0.3)         │ 4038 │
├──────────────────────────────────┼──────┤
│ OCaml (4.10.0)                   │ 4039 │
├──────────────────────────────────┼──────┤
│ Octave (5.2.0)                   │ 4040 │
├──────────────────────────────────┼──────┤
│ Pascal (FPC 3.0.4)               │ 4041 │
├──────────────────────────────────┼──────┤
│ Perl (5.26.1)                    │ 4042 │
├──────────────────────────────────┼──────┤
│ Raku (Rakudo 2020.02.1)          │ 4043 │
├──────────────────────────────────┼──────┤
│ PHP (7.4.4)                      │ 4044 │
├──────────────────────────────────┼──────┤
│ Prolog (SWI-Prolog 8.0.3)        │ 4045 │
├──────────────────────────────────┼──────┤
│ PyPy2 (7.3.0)                    │ 4046 │
├──────────────────────────────────┼──────┤
│ PyPy3 (7.3.0)                    │ 4047 │
├──────────────────────────────────┼──────┤
│ Racket (7.6)                     │ 4048 │
├──────────────────────────────────┼──────┤
│ Ruby (2.7.1)                     │ 4049 │
├──────────────────────────────────┼──────┤
│ Rust (1.42.0)                    │ 4050 │
├──────────────────────────────────┼──────┤
│ Scala (2.13.1)                   │ 4051 │
├──────────────────────────────────┼──────┤
│ Java (OpenJDK 1.8.0)             │ 4052 │
├──────────────────────────────────┼──────┤
│ Scheme (Gauche 0.9.9)            │ 4053 │
├──────────────────────────────────┼──────┤
│ Standard ML (MLton 20130715)     │ 4054 │
├──────────────────────────────────┼──────┤
│ Swift (5.2.1)                    │ 4055 │
├──────────────────────────────────┼──────┤
│ Text (cat 8.28)                  │ 4056 │
├──────────────────────────────────┼──────┤
│ TypeScript (3.8)                 │ 4057 │
├──────────────────────────────────┼──────┤
│ Visual Basic (.NET Core 3.1.101) │ 4058 │
├──────────────────────────────────┼──────┤
│ Zsh (5.4.2)                      │ 4059 │
├──────────────────────────────────┼──────┤
│ COBOL - Fixed (OpenCOBOL 1.1.0)  │ 4060 │
├──────────────────────────────────┼──────┤
│ COBOL - Free (OpenCOBOL 1.1.0)   │ 4061 │
├──────────────────────────────────┼──────┤
│ Brainfuck (bf 20041219)          │ 4062 │
├──────────────────────────────────┼──────┤
│ Ada2012 (GNAT 9.2.1)             │ 4063 │
├──────────────────────────────────┼──────┤
│ Unlambda (2.0.0)                 │ 4064 │
├──────────────────────────────────┼──────┤
│ Cython (0.29.16)                 │ 4065 │
├──────────────────────────────────┼──────┤
│ Sed (4.4)                        │ 4066 │
├──────────────────────────────────┼──────┤
│ Vim (8.2.0460)                   │ 4067 │
└──────────────────────────────────┴──────┘
"#[1..],
                "\n",
                0
            );
            assert_diff!(
                &String::try_from(ctx.stderr)?,
                &r#"
Username: Password: GET https://atcoder.jp/login ... 200 OK
POST https://atcoder.jp/login ... 302 Found
GET https://atcoder.jp/settings ... 200 OK
Successfully logged in.
GET https://atcoder.jp/contests/practice/submit ... 200 OK

"#[1..],
                "\n",
                0
            );
            Ok(())
        },
    )
}

#[test]
fn it_retrieves_languages_in_tenka1_2016_final_open_a() -> Fallible<()> {
    service::test_in_tempdir(
        "it_retrieves_languages_in_tenka1_2016_final_open_a",
        &credentials_as_input()?,
        |mut ctx| -> _ {
            let opt = Opt::Retrieve(Retrieve::Languages(RetrieveLanguages {
                json: false,
                colorize: false,
                service: Some(ServiceKind::Atcoder),
                contest: Some("tenka1-2016-final-open".to_owned()),
                output: OutputKind::Pretty,
                color_choice: AnsiColorChoice::Never,
                problem: Some("a".to_owned()),
            }));
            let code = snowchains::run(opt, &mut ctx)?;
            assert_eq!(code, 0);
            assert_diff!(
                &String::try_from(ctx.stdout)?,
                &r#"
┌────────────┬────┐
│ Name       │ ID │
├────────────┼────┤
│ Text (cat) │ 17 │
└────────────┴────┘
"#[1..],
                "\n",
                0
            );
            assert_diff!(
                &String::try_from(ctx.stderr)?,
                &r#"
Username: Password: GET https://atcoder.jp/login ... 200 OK
POST https://atcoder.jp/login ... 302 Found
GET https://atcoder.jp/settings ... 200 OK
Successfully logged in.
GET https://atcoder.jp/contests/tenka1-2016-final-open/tasks ... 200 OK
GET https://atcoder.jp/contests/tenka1-2016-final-open/tasks/tenka1_2016_final_a ... 200 OK

"#[1..],
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

#[derive(Debug, Deserialize)]
struct RetrieveLangsStdout {
    available_languages: IndexMap<String, String>,
}
