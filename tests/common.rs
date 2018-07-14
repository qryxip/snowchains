extern crate snowchains;

extern crate failure;
extern crate serde;
extern crate serde_yaml;
extern crate tempdir;

use snowchains::palette::ColorChoice;
use snowchains::path::AbsPathBuf;
use snowchains::{Credentials, Opt, Prop, ServiceName};

use serde::{de, Deserialize, Deserializer};
use tempdir::TempDir;

use std::borrow::Cow;
use std::fs::File;
use std::io;
use std::path::PathBuf;
use std::rc::Rc;

pub fn test<E: Into<failure::Error>>(
    tempdir_prefix: &str,
    credentials: Credentials,
    f: impl FnOnce(&Prop) -> Result<(), E>,
) -> Result<(), failure::Error> {
    let tempdir = TempDir::new(tempdir_prefix)?;
    let result = (|| -> Result<(), failure::Error> {
        let prop = Prop {
            working_dir: AbsPathBuf::new_or_panic(tempdir.path()),
            cookies_on_init: Cow::from("$service"),
            credentials,
            suppress_download_bars: true,
        };
        Opt::Init {
            color_choice: ColorChoice::Never,
            directory: PathBuf::from("."),
        }.run(&prop)?;
        f(&prop).map_err(Into::into)
    })();
    tempdir.close()?;
    result
}

pub fn dummy_credentials() -> Credentials {
    Credentials::UserNameAndPassword(Rc::new("".to_owned()), Rc::new("".to_owned()))
}

pub fn login(prop: &Prop, service: ServiceName) -> snowchains::Result<()> {
    Opt::Login {
        color_choice: ColorChoice::Never,
        service,
    }.run(prop)
}

pub fn download(
    prop: &Prop,
    service: ServiceName,
    contest: &str,
    problems: &[&str],
) -> snowchains::Result<()> {
    Opt::Download {
        open_browser: false,
        service: Some(service),
        contest: Some(contest.to_owned()),
        problems: problems.iter().map(|&s| s.to_owned()).collect(),
        color_choice: ColorChoice::Never,
    }.run(prop)
}

pub fn confirm_num_cases(
    prop: &Prop,
    service: ServiceName,
    contest: &str,
    pairs: &[(&str, usize)],
) {
    struct SimpleSuite {
        cases: Vec<serde_yaml::Value>,
    }

    // TODO: use `serde_derive`
    impl<'de> Deserialize<'de> for SimpleSuite {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            let m = serde_yaml::Mapping::deserialize(deserializer)?;
            let cases = m
                .get(&serde_yaml::Value::String("cases".to_owned()))
                .cloned()
                .ok_or_else(|| de::Error::custom("expected \"cases\""))?;
            match cases {
                serde_yaml::Value::Sequence(cases) => Ok(Self { cases }),
                _ => Err(de::Error::custom("expected sequence")),
            }
        }
    }

    for &(problem, expected_num_cases) in pairs {
        let path = prop
            .working_dir
            .join("snowchains")
            .join(service.as_str())
            .join(contest)
            .join(format!("{}.yaml", problem));
        let file = File::open(&path).unwrap();
        let suite = serde_yaml::from_reader::<_, SimpleSuite>(file).unwrap();
        assert_eq!(expected_num_cases, suite.cases.len());
    }
}

pub fn confirm_zip_exists(prop: &Prop, contest: &str, problem: &str) -> io::Result<()> {
    let path = prop
        .working_dir
        .join("snowchains")
        .join("hackerrank")
        .join(contest)
        .join(format!("{}.zip", problem));
    ::std::fs::metadata(&path).map(|_| ())
}
