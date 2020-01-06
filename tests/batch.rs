mod common;

use snowchains::config;
use snowchains::errors::{JudgeError, JudgeErrorKind};
use snowchains::path::AbsPathBuf;
use snowchains::service::ServiceKind;
use snowchains::terminal::{
    AnsiColorChoice, AttemptEnableColor, Dumb, Input, ModifyTermProps, TtyOrPiped,
};
use snowchains::{Judge, Opt, OutputKind};

use failure::Fallible;
use if_chain::if_chain;
use once_cell::sync::Lazy;
use retry::OperationResult;
use tempdir::TempDir;

use std::path::Path;
use std::time::Duration;
use std::{env, io, iter};

#[test]
fn it_works_for_atcoder_practice_a() -> Fallible<()> {
    static CONFIG: Lazy<String> = Lazy::new(|| {
        format!(
            r#"
target = ".snowchains/target.json"

[testfiles]
path = ".snowchains/tests/${{service}}/${{snake_case(contest)}}/${{snake_case(problem)}}.${{extension}}"

[session]
timeout = "0s"
silent = false
robots = false
cookies = "/dev/null"
api_tokens = "/dev/null"
dropbox = false

[session.retrieve]
extension = "yml"
text_file_dir = "/dev/null"

[judge]
testfile_extensions = ["yml"]

[env."(equal service 'atcoder)"]
RUST_VERSION = "stable"
RUST_OPT_LEVEL = "0"

[languages.rust]
src = "${{service}}/${{snake_case(contest)}}/rs/src/bin/${{kebab_case(problem)}}.rs"
bin = "${{service}}/${{snake_case(contest)}}/rs/target/manually/${{kebab_case(problem)}}{}"
compile = ["rustc", "+${{env:RUST_VERSION}}", "-C", "opt-level=${{env:RUST_OPT_LEVEL}}", "-o", "${{bin}}", "${{src}}"]
run = ["${{bin}}"]
working_directory = "${{service}}/${{contest}}/rs"
"#,
            if cfg!(windows) { ".exe" } else { "" },
        )
    });

    static TARGET: &str = r#"{
  "service": "atcoder",
  "contest": "practice",
  "language": "rust"
}
"#;

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

    static INVALID_CODE: &str = "print('Hello!')";

    static WRONG_CODE: &str = "fn main() {}";

    static FREEZING_CODE: &str = r#"use std::thread;
use std::time::Duration;

fn main() {
    thread::sleep(Duration::from_secs(10));
}
"#;

    let tempdir = dunce::canonicalize(&env::temp_dir())?;
    let tempdir = TempDir::new_in(&tempdir, "it_works")?;

    let src_dir = tempdir
        .path()
        .join("atcoder")
        .join("practice")
        .join("rs")
        .join("src")
        .join("bin");
    let src_path = src_dir.join("a.rs");
    let suite_dir = tempdir
        .path()
        .join(".snowchains")
        .join("tests")
        .join("atcoder")
        .join("practice");
    let suite_path = suite_dir.join("a.yml");

    std::fs::write(tempdir.path().join("snowchains.toml"), &*CONFIG)?;
    std::fs::create_dir_all(tempdir.path().join(".snowchains"))?;
    std::fs::write(
        tempdir.path().join(".snowchains").join("target.json"),
        TARGET,
    )?;
    std::fs::create_dir_all(&src_dir)?;
    std::fs::create_dir_all(&suite_dir)?;
    std::fs::write(&suite_path, SUITE)?;

    let mut ctx = snowchains::Context {
        cwd: AbsPathBuf::try_new(tempdir.path()).unwrap(),
        login_retries: Some(0),
        stdin: TtyOrPiped::Piped(io::empty()),
        stdout: Dumb::new(),
        stderr: Dumb::new(),
    };

    const RETRIES: usize = 2;
    const INTERVAL: Duration = Duration::from_secs(1);

    retry::retry(iter::repeat(INTERVAL).take(RETRIES), || {
        match test(&src_path, CODE, &mut ctx) {
            Ok(0) => OperationResult::Ok(()),
            Ok(n) => OperationResult::Retry(failure::err_msg(n.to_string())),
            Err(err) => OperationResult::Retry(failure::Error::from(err)),
        }
    })
    .map_err(|err| match err {
        retry::Error::Operation { error, .. } => error,
        retry::Error::Internal(s) => failure::err_msg(s),
    })?;

    retry::retry(iter::repeat(INTERVAL).take(RETRIES), || {
        match test(&src_path, INVALID_CODE, &mut ctx) {
            Ok(n) => OperationResult::Retry(failure::err_msg(n.to_string())),
            Err(err) => {
                if_chain! {
                    if let snowchains::Error::Judge(JudgeError::Context(ctx)) = &err;
                    if let JudgeErrorKind::Build { .. } = ctx.get_context();
                    then {
                        OperationResult::Ok(())
                    } else {
                        OperationResult::Retry(err.into())
                    }
                }
            }
        }
    })
    .map_err(|err| match err {
        retry::Error::Operation { error, .. } => error,
        retry::Error::Internal(s) => failure::err_msg(s),
    })?;

    retry::retry(iter::repeat(INTERVAL).take(RETRIES), || {
        match test(&src_path, WRONG_CODE, &mut ctx) {
            Ok(1) => OperationResult::Ok(()),
            Ok(n) => OperationResult::Retry(failure::err_msg(n.to_string())),
            Err(err) => OperationResult::Retry(err.into()),
        }
    })
    .map_err(|err| match err {
        retry::Error::Operation { error, .. } => error,
        retry::Error::Internal(s) => failure::err_msg(s),
    })?;

    std::fs::write(&suite_path, SUITE_WITH_TIMELIMIT)?;

    retry::retry(iter::repeat(INTERVAL).take(RETRIES), || {
        match test(&src_path, FREEZING_CODE, &mut ctx) {
            Ok(1) => OperationResult::Ok(()),
            Ok(n) => OperationResult::Retry(failure::err_msg(n.to_string())),
            Err(err) => OperationResult::Retry(err.into()),
        }
    })
    .map_err(|err| match err {
        retry::Error::Operation { error, .. } => error,
        retry::Error::Internal(s) => failure::err_msg(s),
    })
}

fn test<
    I: Input,
    O: AttemptEnableColor + ModifyTermProps,
    E: AttemptEnableColor + ModifyTermProps,
>(
    src_path: &Path,
    code: &str,
    ctx: &mut snowchains::Context<I, O, E>,
) -> snowchains::Result<i32> {
    std::fs::write(src_path, code)?;
    let opt = Opt::Judge(Judge {
        force_compile: false,
        release: false,
        verbose: false,
        json: false,
        colorize: false,
        service: Some(ServiceKind::Atcoder),
        contest: Some("practice".to_owned()),
        language: Some("rust".to_owned()),
        mode: config::Mode::Debug,
        jobs: None,
        output: OutputKind::Pretty,
        color_choice: AnsiColorChoice::Never,
        problem: "a".to_owned(),
    });
    snowchains::run(opt, ctx)
}
