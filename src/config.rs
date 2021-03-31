// https://github.com/Nadrieril/dhall-rust/pull/215
#![allow(redundant_semicolons)]

use anyhow::{anyhow, bail, ensure, Context as _};
use dhall::syntax::InterpolatedText;
use heck::{CamelCase as _, KebabCase as _, MixedCase as _, SnakeCase as _};
use indexmap::IndexMap;
use itertools::Itertools as _;
use maplit::hashmap;
use serde::Deserialize;
use serde_dhall::{SimpleType, StaticType};
use snowchains_core::web::PlatformKind;
use std::{
    collections::BTreeMap,
    convert::Infallible,
    fmt,
    path::{Path, PathBuf},
};

pub(crate) fn detect_target(
    cwd: &Path,
    rel_path: Option<&Path>,
) -> anyhow::Result<(Detected, PathBuf)> {
    let path = find_snowchains_dhall(cwd, rel_path)?;
    let detected = Detected::load_and_eval(cwd, &path)?;
    let dir = Path::new(&path)
        .parent()
        .unwrap_or_else(|| path.as_ref())
        .to_owned();
    Ok((detected, dir))
}

pub(crate) fn target_and_language(
    cwd: &Path,
    rel_path: Option<&Path>,
    cli_opt_service: Option<PlatformKind>,
    cli_opt_contest: Option<&str>,
    cli_opt_problem: Option<&str>,
    cli_opt_language: Option<&str>,
    cli_opt_mode: Mode,
) -> anyhow::Result<(Target, Language, PathBuf)> {
    let path = find_snowchains_dhall(cwd, rel_path)?;

    let (target, language_name) = Detected::load_and_eval(cwd, &path)?.merge_with_cli_options(
        cli_opt_service,
        cli_opt_contest,
        cli_opt_problem,
        cli_opt_language,
        cli_opt_mode,
    )?;

    let mut languages = serde_dhall::from_str(&format!(
        "let target = {} let config = {} in config.languages target",
        target.to_dhall_expr(),
        path,
    ))
    .parse::<BTreeMap<String, Language>>()
    .with_context(|| format!("Could not evaluate `{}`", path))?;

    let expected_names = languages.keys().join(", ");

    let language = languages.remove(&language_name).with_context(|| {
        format!(
            "The language `{}` not found. Expected one of [{}]",
            language_name, expected_names,
        )
    })?;

    let dir = Path::new(&path)
        .parent()
        .unwrap_or_else(|| path.as_ref())
        .to_owned();

    Ok((target, language, dir))
}

pub(crate) fn xtask(cwd: &Path, rel_path: Option<&Path>, name: &str) -> anyhow::Result<Script> {
    let path = find_snowchains_dhall(cwd, rel_path)?;

    let xtask = serde_dhall::from_str(&format!("let config = {} in config.xtask", path))
        .type_annotation(&map_annot(SimpleType::Text, Script::static_type()))
        .parse::<IndexMap<String, _>>()
        .with_context(|| format!("Could not evalute `{}`", path))?;

    xtask.get(name).cloned().with_context(|| {
        format!(
            "No such xtask subcommand: `{}` (found [{}])",
            name,
            xtask
                .keys()
                .format_with(", ", |s, f| f(&format_args!("`{}`", s)))
        )
    })
}

fn find_snowchains_dhall(cwd: &Path, rel_path: Option<&Path>) -> anyhow::Result<String> {
    let path = if let Some(rel_path) = rel_path {
        let rel_path = rel_path.strip_prefix(".").unwrap_or(rel_path);
        let path = cwd.join(rel_path);
        ensure!(path.exists(), "`{}` does not exist", path.display());
        path
    } else {
        cwd.ancestors()
            .map(|p| p.join("snowchains.dhall"))
            .find(|p| p.exists())
            .with_context(|| {
                format!(
                    "Could not find `snowchains.dhall` in `{}` or any parent directory",
                    cwd.display(),
                )
            })?
    };

    let path = path
        .into_os_string()
        .into_string()
        .map_err(|path| anyhow!("The config path must be valid UTF-8: {:?}", path))?;

    if path.chars().any(|c| c.is_whitespace() || c.is_control()) {
        bail!(
            "The config path must not contain whitespace and control characters: {:?}",
            path,
        );
    }

    Ok(path)
}

fn quote(s: impl AsRef<str>) -> impl fmt::Display {
    InterpolatedText::<Infallible>::from(s.as_ref().to_owned())
}

fn map_annot(key: SimpleType, value: SimpleType) -> SimpleType {
    SimpleType::List(Box::new(SimpleType::Record(
        hashmap!("mapKey".to_owned() => key, "mapValue".to_owned() => value),
    )))
}

#[derive(Debug, Deserialize, StaticType)]
pub(crate) struct Detected {
    pub(crate) service: Option<String>,
    pub(crate) contest: Option<String>,
    pub(crate) problem: Option<String>,
    pub(crate) language: Option<String>,
}

impl Detected {
    fn load_and_eval(cwd: &Path, path: &str) -> anyhow::Result<Self> {
        let rel_path_components = cwd
            .strip_prefix({
                let path = Path::new(path);
                path.parent().unwrap_or(path)
            })
            .map(|rel_path| {
                rel_path
                    .iter()
                    .map(|comp| {
                        quote(
                            comp.to_str()
                                .expect("components of a UTF-8 path should also be UTF-8"),
                        )
                    })
                    .join(", ")
            })
            .unwrap_or_default();

        let rel_path_components = if rel_path_components.is_empty() {
            "[] : List Text".to_owned()
        } else {
            format!("[{}]", rel_path_components)
        };

        serde_dhall::from_str(&format!(
            r"let relativePathSegments = {}

let config = {}

in  {{ service = config.detectServiceFromRelativePathSegments relativePathSegments
    , contest = config.detectContestFromRelativePathSegments relativePathSegments
    , problem = config.detectProblemFromRelativePathSegments relativePathSegments
    , language = config.detectLanguageFromRelativePathSegments relativePathSegments
    }}
",
            rel_path_components, path,
        ))
        .static_type_annotation()
        .parse()
        .with_context(|| format!("Could not evalute `{}`", path))
    }

    fn merge_with_cli_options(
        &self,
        service: Option<PlatformKind>,
        contest: Option<&str>,
        problem: Option<&str>,
        language: Option<&str>,
        mode: Mode,
    ) -> anyhow::Result<(Target, String)> {
        let service = service.map(Ok).unwrap_or_else(|| {
            self.service
                .as_deref()
                .with_context(|| "`service` was not detected. Specify with `--service`")?
                .parse()
                .with_context(|| {
                    "`detectServiceFromRelativePathSegments` returned unrecognized `service`. \
                     Specify the correct `service` with `--service`"
                })
        })?;

        let contest = contest
            .or_else(|| self.contest.as_deref())
            .map(ToOwned::to_owned);

        let problem = problem
            .map(Ok)
            .unwrap_or_else(|| {
                self.problem
                    .as_deref()
                    .with_context(|| "`problem` was not detected. Specify with `--problem`")
            })?
            .to_owned();

        let language = language
            .map(Ok)
            .unwrap_or_else(|| {
                self.language
                    .as_deref()
                    .with_context(|| "`language` was not detected. Specify with `--language`")
            })?
            .to_owned();

        let target = Target {
            service,
            contest,
            problem,
            mode,
        };

        Ok((target, language))
    }

    pub(crate) fn parse_service(&self) -> anyhow::Result<Option<PlatformKind>> {
        self.service
            .as_deref()
            .map(str::parse)
            .transpose()
            .with_context(|| {
                "Specified invalid `service` by `detectServiceFromRelativePathSegments`"
            })
    }
}

#[derive(Debug, Deserialize, StaticType, Clone)]
pub(crate) enum Command {
    Args(Vec<String>),
    Script(Script),
}

#[derive(Debug, Deserialize, StaticType, Clone)]
pub(crate) struct Script {
    pub(crate) program: String,
    pub(crate) extension: String,
    pub(crate) content: String,
}

#[allow(non_snake_case)] // for `StaticType`
#[derive(Debug, Deserialize, StaticType)]
pub(crate) struct Language {
    pub(crate) src: String,
    pub(crate) transpile: Option<Compile>,
    pub(crate) compile: Option<Compile>,
    pub(crate) run: Command,
    pub(crate) languageId: Option<String>,
}

#[derive(Debug, Deserialize, StaticType)]
pub(crate) struct Compile {
    pub(crate) command: Command,
    pub(crate) output: String,
}

#[derive(Debug)]
pub(crate) struct Target {
    pub(crate) service: PlatformKind,
    pub(crate) contest: Option<String>,
    pub(crate) problem: String,
    pub(crate) mode: Mode,
}

impl Target {
    fn to_dhall_expr(&self) -> String {
        format!(
            r"let Service = < Atcoder | Codeforces | Yukicoder >

let CaseConvertedText =
      {{ lowercase : Text
      , uppercase : Text
      , snakeCase : Text
      , kebabCase : Text
      , mixedCase : Text
      , pascalCase : Text
      }}

let Mode = < Debug | Release >

in  {{ service = Service.{}
    , contest = {}
    , problem =
        {{ lowercase = {}
        , uppercase = {}
        , snakeCase = {}
        , kebabCase = {}
        , mixedCase = {}
        , pascalCase = {}
        }}
    , mode = Mode.{}
    }}
",
            self.service.to_pascal_case_str(),
            if let Some(contest) = &self.contest {
                format!(
                    r"Some {{ lowercase =  {}, uppercase =  {}, snakeCase =  {}, kebabCase =  {}, mixedCase =  {}, pascalCase = {} }}",
                    quote(contest.to_lowercase()),
                    quote(contest.to_uppercase()),
                    quote(contest.to_snake_case()),
                    quote(contest.to_kebab_case()),
                    quote(contest.to_mixed_case()),
                    quote(contest.to_camel_case()),
                )
            } else {
                "None CaseConvertedText".to_owned()
            },
            quote(self.problem.to_lowercase()),
            quote(self.problem.to_uppercase()),
            quote(self.problem.to_snake_case()),
            quote(self.problem.to_kebab_case()),
            quote(self.problem.to_mixed_case()),
            quote(self.problem.to_camel_case()),
            match self.mode {
                Mode::Debug => "Debug",
                Mode::Release => "Release",
            },
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Mode {
    Debug,
    Release,
}
