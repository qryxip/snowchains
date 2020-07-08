use crate::shell::Shell;
use anyhow::{ensure, Context as _};
use dhall::syntax::InterpolatedText;
use heck::{CamelCase as _, KebabCase as _, MixedCase as _, SnakeCase as _};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use snowchains_core::web::PlatformVariant;
use std::collections::BTreeMap;
use std::convert::Infallible;
use std::{fmt, fs, io::BufRead, path::Path};
use termcolor::WriteColor;

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Config {
    pub(crate) custom_subcommands: BTreeMap<String, RunCommand>,
    pub(crate) default_language: Language,
    pub(crate) languages: IndexMap<String, Language>,
}

impl Config {
    pub(crate) fn load(
        shell: &mut Shell<impl BufRead, impl WriteColor>,
        cwd: &Path,
        path: Option<&Path>,
        target_service: Option<PlatformVariant>,
        target_contest: Option<&str>,
        target_problem: Option<&str>,
        target_mode: Mode,
    ) -> anyhow::Result<Config> {
        let dhall_path = if let Some(path) = path {
            let path = cwd.join(path.strip_prefix(".").unwrap_or(path));
            ensure!(path.exists(), "`{}` does not exist", path.display());
            path
        } else {
            cwd.ancestors()
                .map(|p| p.join("snowchains.dhall"))
                .find(|p| p.exists())
                .with_context(|| {
                    format!(
                        "Could not find `snowchains.toml` in `{}` or any parent directory",
                        cwd.display()
                    )
                })?
        };

        let dhall_path = dhall_path
            .to_str()
            .with_context(|| format!("`{}` is not valid UTF-8 path", dhall_path.display()))?;

        let target = Target::load_or_create(
            shell,
            Path::new(dhall_path)
                .with_file_name(".snowchains")
                .join("target.json"),
            target_service,
            target_contest,
            target_problem,
            target_mode,
        )?
        .to_dhall_expr();

        serde_dhall::from_str(&format!("{} ({})", dhall_path, target))
            .parse()
            .with_context(|| format!("Could not evaluate `{}`", dhall_path))
    }
}

#[derive(Debug, Deserialize)]
pub(crate) enum RunCommand {
    Args(Vec<String>),
    Shell(ShellCommand),
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct ShellCommand {
    pub(crate) shell: ShellRunner,
    pub(crate) code: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct ShellRunner {
    pub(crate) runner: String,
    pub(crate) extension: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Language {
    pub(crate) src: String,
    pub(crate) transpile: Option<Compile>,
    pub(crate) compile: Option<Compile>,
    pub(crate) run: RunCommand,
    pub(crate) language_id: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Compile {
    pub(crate) command: RunCommand,
    pub(crate) output: String,
}

#[derive(Debug)]
struct Target {
    service: PlatformVariant,
    contest: Option<String>,
    problem: String,
    mode: Mode,
}

impl Target {
    fn load_or_create(
        shell: &mut Shell<impl BufRead, impl WriteColor>,
        path: impl AsRef<Path>,
        service: Option<PlatformVariant>,
        contest: Option<impl AsRef<str>>,
        problem: Option<impl AsRef<str>>,
        mode: Mode,
    ) -> anyhow::Result<Self> {
        return {
            let path = path.as_ref();

            let repr = if path.exists() {
                let repr = fs::read_to_string(path)
                    .with_context(|| format!("Could not read `{}`", path.display()))?;

                serde_json::from_str::<Repr>(&repr).with_context(|| {
                    format!("Could not parse the JSON file at `{}`", path.display())
                })?
            } else {
                if let Some(parent) = path.parent() {
                    fs::create_dir_all(parent)
                        .with_context(|| format!("Could not create `{}`", parent.display()))?;
                }

                let repr = Repr {
                    service,
                    contest: contest.as_ref().map(AsRef::as_ref).map(ToOwned::to_owned),
                    problem: problem.as_ref().map(AsRef::as_ref).map(ToOwned::to_owned),
                };

                fs::write(path, repr.to_json())
                    .with_context(|| format!("Could not write `{}`", path.display()))?;
                shell.info(format!("Wrote `{}`", path.display()))?;

                repr
            };

            let service = service
                .or(repr.service)
                .with_context(|| "Missing `service`")?;

            let contest = contest.map(|s| s.as_ref().to_owned());

            let problem = problem.map(|s| s.as_ref().to_owned()).with_context(|| "")?;

            Ok(Self {
                service,
                contest,
                problem,
                mode,
            })
        };

        #[derive(Deserialize, Serialize)]
        struct Repr {
            service: Option<PlatformVariant>,
            contest: Option<String>,
            problem: Option<String>,
        }

        impl Repr {
            fn to_json(&self) -> String {
                serde_json::to_string(self).expect("should not fail")
            }
        }
    }

    fn to_dhall_expr(&self) -> String {
        return format!(
            r"let Service = < Atcoder | Codeforces | Yukicoder >
let Mode = < Debug | Release >
let CaseConvertedText = {{ original : Text, lowercase : Text, uppercase : Text, snakeCase : Text, kebabCase : Text, mixedCase : Text, pascalCase : Text }}
in  {{ service = Service.{}
    , contest = {}
    , problem = {{ original = {}, lowercase = {}, uppercase = {}, snakeCase = {}, kebabCase = {}, mixedCase = {}, pascalCase = {} }}
    , mode = Mode.Debug
    }}
",
            self.service,
            if let Some(contest) = &self.contest {
                format!(
                    r"Some {{ original = {}, lowercase =  {}, uppercase =  {}, snakeCase =  {}, kebabCase =  {}, mixedCase =  {}, pascalCase = {} }}",
                    quote(contest),
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
            quote(&self.problem),
            quote(self.problem.to_lowercase()),
            quote(self.problem.to_uppercase()),
            quote(self.problem.to_snake_case()),
            quote(self.problem.to_kebab_case()),
            quote(self.problem.to_mixed_case()),
            quote(self.problem.to_camel_case()),
        );

        fn quote(s: impl AsRef<str>) -> impl fmt::Display {
            InterpolatedText::<Infallible>::from(s.as_ref().to_owned())
        }
    }
}

#[derive(Debug)]
pub(crate) enum Mode {
    Debug,
    Release,
}
