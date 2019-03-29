use snowchains::app::{App, Opt};
use snowchains::config;
use snowchains::errors::{JudgeError, JudgeErrorKind};
use snowchains::path::AbsPathBuf;
use snowchains::service::ServiceKind;
use snowchains::terminal::{AnsiColorChoice, Term, TermImpl};

use failure::Fallible;
use if_chain::if_chain;
use tempdir::TempDir;

use std::path::Path;

#[test]
fn it_works_for_atcoder_practice_a() -> Fallible<()> {
    static SUITE: &str = r#"---
type: batch
match: exact
cases:
  - name: Sample 1
    in: |
      1
      2 3
      test
    out: |
      6 test
  - name: Sample 2
    in: |
      72
      128 256
      myonmyon
    out: |
      456 myonmyon
"#;
    static SUITE_WITH_TIMELIMIT: &str = r#"---
type: batch
timelimit: 100ms
match: exact
cases:
  - in: ""
    out: ""
"#;
    static CODE: &str = r#"use std::io::{self, Read};

fn main() {
    let mut input = "".to_owned();
    io::stdin().read_to_string(&mut input).unwrap();
    let mut input = input.split(char::is_whitespace);
    let a = input.next().unwrap().parse::<u32>().unwrap();
    let b = input.next().unwrap().parse::<u32>().unwrap();
    let c = input.next().unwrap().parse::<u32>().unwrap();
    let s = input.next().unwrap();
    println!("{} {}", a + b + c, s);
}
"#;
    static INVLID_CODE: &str = "print('Hello!')";
    static WRONG_CODE: &str = "fn main() {}";
    static FREEZING_CODE: &str = r#"use std::thread;
use std::time::Duration;

fn main() {
    thread::sleep(Duration::from_secs(10));
}
"#;

    let _ = env_logger::try_init();

    let tempdir = TempDir::new("batch_it_works")?;

    let dir = tempdir.path().join("atcoder").join("practice");
    let src_dir = dir.join("rs").join("src").join("bin");
    let src_path = src_dir.join("a.rs");
    let suite_dir = dir.join("tests");
    let suite_path = suite_dir.join("a.yml");

    std::fs::write(
        tempdir.path().join("snowchains.toml"),
        include_bytes!("./snowchains.toml").as_ref(),
    )?;
    std::fs::create_dir_all(&src_dir)?;
    std::fs::create_dir_all(&suite_dir)?;
    std::fs::write(&suite_path, SUITE)?;

    let mut app = App {
        working_dir: AbsPathBuf::try_new(tempdir.path()).unwrap(),
        login_retries: Some(0),
        term: TermImpl::null(),
    };

    app.test(&src_path, CODE)?;

    if_chain! {
        let err = app.test(&src_path, INVLID_CODE).unwrap_err();
        if let snowchains::Error::Judge(JudgeError::Context(ctx)) = &err;
        if let JudgeErrorKind::Build { .. } = ctx.get_context();
        then {} else { return Err(err.into()) }
    }

    if_chain! {
        let err = app.test(&src_path, WRONG_CODE).unwrap_err();
        if let snowchains::Error::Judge(JudgeError::Context(ctx)) = &err;
        if let JudgeErrorKind::TestFailed(n, d) = ctx.get_context();
        if (n.get(), d.get()) == (2, 2);
        then {} else { return Err(err.into()) }
    }

    std::fs::write(&suite_path, SUITE_WITH_TIMELIMIT)?;

    if_chain! {
        let err = app.test(&src_path, FREEZING_CODE).unwrap_err();
        if let snowchains::Error::Judge(JudgeError::Context(ctx)) = &err;
        if let JudgeErrorKind::TestFailed(n, d) = ctx.get_context();
        if (n.get(), d.get()) == (1, 1);
        then { Ok(()) } else { Err(err.into()) }
    }
}

trait AppExt {
    fn test(&mut self, src_path: &Path, code: &str) -> snowchains::Result<()>;
}

impl<T: Term> AppExt for App<T> {
    fn test(&mut self, src_path: &Path, code: &str) -> snowchains::Result<()> {
        std::fs::write(src_path, code)?;
        self.run(Opt::Judge {
            force_compile: false,
            release: false,
            service: Some(ServiceKind::Atcoder),
            contest: Some("practice".to_owned()),
            language: Some("rust".to_owned()),
            mode: config::Mode::Debug,
            jobs: None,
            color_choice: AnsiColorChoice::Never,
            problem: "a".to_owned(),
        })
    }
}
