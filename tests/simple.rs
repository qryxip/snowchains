use snowchains::app::{App, Opt};
use snowchains::path::AbsPathBuf;
use snowchains::service::{Credentials, ServiceName};
use snowchains::terminal::{AnsiColorChoice, TermImpl};

use tempdir::TempDir;

use std::path::PathBuf;

#[test]
fn it_works() {
    let _ = env_logger::try_init();

    let tempdir = TempDir::new("simple_it_works").unwrap();

    let src_dir = tempdir.path().join("rs").join("src").join("bin");
    let src_path = src_dir.join("a.rs");
    let suite_dir = tempdir.path().join("tests").join("other").join("practice");
    let suite_path = suite_dir.join("a.yaml");
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
    static WRONG_CODE: &str = "fn main {}";
    static SUITE: &str = r#"---
type: simple
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

    std::fs::create_dir_all(&src_dir).unwrap();
    std::fs::create_dir_all(&suite_dir).unwrap();
    std::fs::write(&suite_path, SUITE).unwrap();

    let mut app = App {
        working_dir: AbsPathBuf::try_new(tempdir.path().to_owned()).unwrap(),
        cookies_on_init: "$service".to_owned(),
        dropbox_auth_on_init: "dropbox.json".to_owned(),
        enable_dropbox_on_init: false,
        credentials: Credentials::default(),
        term: TermImpl::null(),
    };
    app.run(Opt::Init {
        color_choice: AnsiColorChoice::Never,
        directory: PathBuf::from("."),
    })
    .unwrap();

    let mut test = |code: &str| -> snowchains::Result<()> {
        std::fs::write(&src_path, code)?;
        app.run(Opt::Judge {
            force_compile: false,
            service: Some(ServiceName::Other),
            contest: Some("practice".to_owned()),
            language: Some("rust".to_owned()),
            jobs: None,
            color_choice: AnsiColorChoice::Never,
            problem: "a".to_owned(),
        })
    };
    test(CODE).unwrap();
    test(WRONG_CODE).unwrap_err();
}
