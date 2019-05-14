#![allow(dead_code)]

use snowchains::app::{App, Login, Opt, OutputKind, Retrieve, RetrieveTestcases};
use snowchains::path::{AbsPath, AbsPathBuf};
use snowchains::service::ServiceKind;
use snowchains::terminal::{AnsiColorChoice, Dumb, TtyOrPiped};

use failure::Fallible;
use pretty_assertions::assert_eq;
use serde_derive::Deserialize;
use serde_json::json;
use tempdir::TempDir;

use std::fs::File;
use std::panic::UnwindSafe;
use std::{env, panic};

pub(crate) fn test_in_tempdir(
    tempdir_prefix: &str,
    stdin: &str,
    f: impl FnOnce(App<TtyOrPiped<&[u8]>, Dumb, Dumb>) -> Fallible<()> + UnwindSafe,
) -> Fallible<()> {
    static CONFIG: &str = r#"
target = ".snowchains/target.json"

[testfiles]
path = ".snowchains/tests/${service}/${snake_case(contest)}/${snake_case(problem)}.${extension}"

[session]
timeout = "60s"
silent = false
robots = false
cookies = "local/cookies/${service}.json"
api_tokens = "local/api_tokens/${service}.json"
dropbox = { auth = "local/dropbox.json" }

[session.retrieve]
extension = "yml"
text_file_dir = "${service}/${snake_case(contest)}/tests/${snake_case(problem)}"

[judge]
testfile_extensions = ["yml"]

[languages.python3]
src = "${service}/${snake_case(contest)}/py/${kebab_case(problem)}.py"
run = ["false"]

[languages.python3.names]
atcoder = "Python3 (3.4.3)"
yukicoder = "Python3 (3.7.1 + numpy 1.14.5 + scipy 1.1.0)"

[languages.python3-with-invalid-lang-names]
src = "${service}/${snake_case(contest)}/py/${kebab_case(problem)}.py"
run = ["false"]

[languages.python3-with-invalid-lang-names.names]
atcoder = "invalid"
codeforces = "invalid"
yukicoder = "invalid"

[languages.text]
src = "${service}/${snake_case(contest)}/txt/${kebab_case(problem)}.txt"
run = ["false"]

[languages.text.names]
yukicoder = "Text (cat 8.22)"
"#;

    static TARGET: &str = r#"{
  "service": "other",
  "contest": "",
  "language": "text"
}
"#;

    let tempdir = dunce::canonicalize(&env::temp_dir())?;
    let tempdir = TempDir::new_in(&tempdir, tempdir_prefix)?;
    let tempdir_path = tempdir.path().to_owned();
    let result = panic::catch_unwind(move || -> Fallible<()> {
        std::fs::write(tempdir_path.join("snowchains.toml"), CONFIG)?;
        std::fs::create_dir(tempdir_path.join(".snowchains"))?;
        std::fs::write(tempdir_path.join(".snowchains").join("target.json"), TARGET)?;
        std::fs::create_dir(tempdir_path.join("local"))?;
        serde_json::to_writer(
            File::create(tempdir_path.join("local").join("dropbox.json"))?,
            &json!({ "access_token": env_var("DROPBOX_ACCESS_TOKEN")? }),
        )?;
        f(App {
            working_dir: AbsPathBuf::try_new(&tempdir_path).unwrap(),
            login_retries: Some(0),
            stdin: TtyOrPiped::Piped(stdin.as_ref()),
            stdout: Dumb::new(),
            stderr: Dumb::new(),
        })
    });
    tempdir.close()?;
    match result {
        Ok(result) => result,
        Err(panic) => panic::resume_unwind(panic),
    }
}

pub(crate) fn env_var(name: &'static str) -> Fallible<String> {
    env::var(name).map_err(|err| failure::err_msg(format!("Failed to read {:?}: {}", name, err)))
}

pub(crate) fn login(
    mut app: App<TtyOrPiped<&[u8]>, Dumb, Dumb>,
    service: ServiceKind,
) -> snowchains::Result<()> {
    let code = app.run(Opt::Login(Login {
        json: true,
        output: OutputKind::None,
        color_choice: AnsiColorChoice::Never,
        service,
    }))?;
    assert_eq!(code, 0);
    Ok(())
}

pub(crate) fn retrieve_testcases(
    mut app: App<TtyOrPiped<&[u8]>, Dumb, Dumb>,
    service: ServiceKind,
    contest: &str,
    problems: &[&str],
) -> snowchains::Result<()> {
    let code = app.run(Opt::Retrieve(Retrieve::Testcases(RetrieveTestcases {
        open: false,
        json: true,
        only_scraped: false,
        service: Some(service),
        contest: Some(contest.to_owned()),
        problems: problems.iter().map(|&s| s.to_owned()).collect(),
        output: OutputKind::Pretty,
        color_choice: AnsiColorChoice::Never,
    })))?;
    assert_eq!(code, 0);
    Ok(())
}

pub(crate) fn confirm_num_cases(
    wd: &AbsPath,
    service: ServiceKind,
    contest: &str,
    pairs: &[(&str, usize)],
) -> Fallible<()> {
    #[derive(Deserialize)]
    struct BatchSuite {
        cases: Vec<serde_yaml::Value>,
    }

    for &(problem, expected_num_cases) in pairs {
        let path = wd
            .join(".snowchains")
            .join("tests")
            .join(Into::<&str>::into(service))
            .join(contest)
            .join(problem)
            .with_extension("yml");
        let file = File::open(&path)?;
        let suite = serde_yaml::from_reader::<_, BatchSuite>(file)?;
        assert_eq!(suite.cases.len(), expected_num_cases);
    }
    Ok(())
}
