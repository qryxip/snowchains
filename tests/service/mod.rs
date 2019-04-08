#![allow(dead_code)]

use snowchains::app::{App, Login, Opt, Retrieve, RetrieveTestcases};
use snowchains::path::{AbsPath, AbsPathBuf};
use snowchains::service::ServiceKind;
use snowchains::terminal::{AnsiColorChoice, TermImpl};

use failure::Fallible;
use pretty_assertions::assert_eq;
use serde_derive::Deserialize;
use serde_json::json;
use tempdir::TempDir;

use std::fs::File;
use std::panic::UnwindSafe;
use std::{env, panic};

pub fn test_in_tempdir<E: Into<failure::Error>>(
    tempdir_prefix: &str,
    stdin: &str,
    f: impl FnOnce(App<TermImpl<&[u8], Vec<u8>, Vec<u8>>>) -> Result<(), E> + UnwindSafe,
) -> Fallible<()> {
    let tempdir = dunce::canonicalize(&env::temp_dir())?;
    let tempdir = TempDir::new_in(&tempdir, tempdir_prefix)?;
    let tempdir_path = tempdir.path().to_owned();
    let result = panic::catch_unwind(move || -> Fallible<()> {
        std::fs::write(
            tempdir_path.join("snowchains.toml"),
            &include_bytes!("../snowchains.toml")[..],
        )?;
        std::fs::create_dir(tempdir_path.join(".snowchains"))?;
        std::fs::write(
            tempdir_path.join(".snowchains").join("target.json"),
            &include_bytes!("../target.json")[..],
        )?;
        std::fs::create_dir(tempdir_path.join("local"))?;
        serde_json::to_writer(
            File::create(tempdir_path.join("local").join("dropbox.json"))?,
            &json!({ "access_token": env_var("DROPBOX_ACCESS_TOKEN")? }),
        )?;
        let app = App {
            working_dir: AbsPathBuf::try_new(&tempdir_path).unwrap(),
            login_retries: Some(0),
            term: TermImpl::new(stdin.as_bytes(), vec![], vec![]),
        };
        f(app).map_err(Into::into)
    });
    tempdir.close()?;
    match result {
        Err(panic) => panic::resume_unwind(panic),
        Ok(result) => result,
    }
}

pub fn env_var(name: &'static str) -> Fallible<String> {
    env::var(name).map_err(|err| failure::err_msg(format!("Failed to read {:?}: {}", name, err)))
}

pub fn login(
    mut app: App<TermImpl<&[u8], Vec<u8>, Vec<u8>>>,
    service: ServiceKind,
) -> snowchains::Result<()> {
    app.run(Opt::Login(Login {
        json: true,
        color_choice: AnsiColorChoice::Never,
        service,
    }))
}

pub fn retrieve_testcases(
    mut app: App<TermImpl<&[u8], Vec<u8>, Vec<u8>>>,
    service: ServiceKind,
    contest: &str,
    problems: &[&str],
) -> snowchains::Result<()> {
    app.run(Opt::Retrieve(Retrieve::Testcases(RetrieveTestcases {
        open: false,
        json: true,
        only_scraped: false,
        service: Some(service),
        contest: Some(contest.to_owned()),
        problems: problems.iter().map(|&s| s.to_owned()).collect(),
        color_choice: AnsiColorChoice::Never,
    })))
}

pub fn confirm_num_cases(
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
        assert_eq!(expected_num_cases, suite.cases.len());
    }
    Ok(())
}
