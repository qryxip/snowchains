use crate::{shell::Shell, web::CaseConversions};
use anyhow::Context as _;
use maplit::btreeset;
use serde::Serialize;
use snowchains_core::{
    testsuite::{Additional, BatchTestSuite, TestSuite},
    web::{
        Atcoder, AtcoderRetrieveFullTestCasesCredentials,
        AtcoderRetrieveSampleTestCasesCredentials, AtcoderRetrieveTestCasesTargets, Codeforces,
        CodeforcesRetrieveSampleTestCasesCredentials, CodeforcesRetrieveTestCasesTargets,
        PlatformVariant, RetrieveFullTestCases, RetrieveTestCases, Yukicoder,
        YukicoderRetrieveFullTestCasesCredentials, YukicoderRetrieveTestCasesTargets,
    },
};
use std::{
    cell::RefCell,
    io::{BufRead, Write},
    path::PathBuf,
};
use structopt::StructOpt;
use strum::VariantNames as _;
use termcolor::{Color, ColorSpec, WriteColor};
use url::Url;

#[derive(StructOpt, Debug)]
pub struct OptRetrieveTestcases {
    /// Downloads full test cases
    #[structopt(long)]
    pub full: bool,

    /// Prints JSON data
    #[structopt(long)]
    pub json: bool,

    /// Path to `snowchains.dhall`
    #[structopt(long)]
    pub config: Option<PathBuf>,

    /// Coloring
    #[structopt(
        long,
        possible_values(crate::ColorChoice::VARIANTS),
        default_value("auto")
    )]
    pub color: crate::ColorChoice,

    /// Platform
    #[structopt(
        short,
        long,
        value_name("SERVICE"),
        possible_values(PlatformVariant::KEBAB_CASE_VARIANTS)
    )]
    pub service: Option<PlatformVariant>,

    /// Contest ID
    #[structopt(short, long, value_name("STRING"))]
    pub contest: Option<String>,

    /// Problem indexes (e.g. "a", "b", "c")
    #[structopt(short, long, value_name("STRING"))]
    pub problems: Option<Vec<String>>,
}

#[derive(Debug, Serialize)]
struct Outcome {
    contest: Option<OutcomeContest>,
    problems: Vec<OutcomeProblem>,
}

impl Outcome {
    fn to_json(&self) -> String {
        serde_json::to_string(self).expect("should not fail")
    }
}

#[derive(Debug, Serialize)]
struct OutcomeContest {
    id: CaseConversions,
    submissions_url: Url,
}

#[derive(Debug, Serialize)]
struct OutcomeProblem {
    index: CaseConversions,
    url: Url,
    screen_name: String,
    display_name: String,
    test_suite: OutcomeProblemTestSuite,
}

#[derive(Debug, Serialize)]
struct OutcomeProblemTestSuite {
    path: String,
    content: TestSuite,
}

pub(crate) fn run(
    opt: OptRetrieveTestcases,
    ctx: crate::Context<impl BufRead, impl Write, impl WriteColor>,
) -> anyhow::Result<()> {
    let OptRetrieveTestcases {
        full,
        json,
        config,
        color: _,
        service,
        contest,
        problems,
    } = opt;

    let crate::Context {
        cwd,
        mut stdin,
        mut stdout,
        mut stderr,
        stdin_process_redirection: _,
        stdout_process_redirection: _,
        stderr_process_redirection: _,
        draw_progress: _,
    } = ctx;

    let (detected_target, workspace) = crate::config::detect_target(&cwd, config.as_deref())?;

    let service = service
        .map(Ok)
        .or_else(|| detected_target.parse_service().transpose())
        .with_context(|| "`service` is not specified")??;

    let contest = contest.or(detected_target.contest);

    let problems = match (problems.as_deref().unwrap_or(&[]), &detected_target.problem) {
        ([], None) => None,
        ([], Some(problem)) => Some(btreeset!(problem.clone())),
        (problems, _) => Some(problems.iter().cloned().collect()),
    };

    let cookie_storage = crate::web::cookie_storage::cookie_storage()?;

    let timeout = Some(crate::web::SESSION_TIMEOUT);

    let outcome = match service {
        PlatformVariant::Atcoder => {
            let targets = {
                let contest = contest
                    .clone()
                    .with_context(|| "`contest` is required for AtCoder")?;
                AtcoderRetrieveTestCasesTargets { contest, problems }
            };

            let stderr = RefCell::new(&mut stderr);
            let shell = Shell::new(&stderr, true);

            let credentials = AtcoderRetrieveSampleTestCasesCredentials {
                username_and_password: &mut crate::web::credentials::atcoder_username_and_password(
                    stdin, &stderr,
                ),
            };

            let full = if full {
                Some(RetrieveFullTestCases {
                    credentials: AtcoderRetrieveFullTestCasesCredentials {
                        dropbox_access_token: crate::web::credentials::dropbox_access_token()?,
                    },
                })
            } else {
                None
            };

            Atcoder::exec(RetrieveTestCases {
                targets,
                credentials,
                full,
                cookie_storage,
                timeout,
                shell,
            })
        }
        PlatformVariant::Codeforces => {
            let targets = {
                let contest = contest
                    .clone()
                    .with_context(|| "`contest` is required for Codeforces")?;
                CodeforcesRetrieveTestCasesTargets { contest, problems }
            };

            let stderr = RefCell::new(&mut stderr);
            let shell = Shell::new(&stderr, true);

            let credentials = CodeforcesRetrieveSampleTestCasesCredentials {
                username_and_password:
                    &mut crate::web::credentials::codeforces_username_and_password(stdin, &stderr),
            };

            Codeforces::exec(RetrieveTestCases {
                targets,
                credentials,
                full: None,
                cookie_storage,
                timeout,
                shell,
            })
        }
        PlatformVariant::Yukicoder => {
            let targets = if let Some(contest) = &contest {
                YukicoderRetrieveTestCasesTargets::Contest(contest.clone(), problems)
            } else {
                let nos = problems
                    .with_context(|| "`contest` or `problem`s are required for yukicoder")?
                    .iter()
                    .map(|s| s.parse())
                    .collect::<Result<_, _>>()
                    .with_context(|| "`problem`s for yukicoder must be unsigned integer")?;
                YukicoderRetrieveTestCasesTargets::ProblemNos(nos)
            };

            let full = if full {
                Some(RetrieveFullTestCases {
                    credentials: YukicoderRetrieveFullTestCasesCredentials {
                        api_key: crate::web::credentials::yukicoder_api_key(
                            &mut stdin,
                            &mut stderr,
                        )?,
                    },
                })
            } else {
                None
            };

            let stderr = RefCell::new(&mut stderr);
            let shell = Shell::new(&stderr, true);

            Yukicoder::exec(RetrieveTestCases {
                targets,
                credentials: (),
                full,
                cookie_storage: (),
                timeout,
                shell,
            })
        }
    }?;

    let mut acc = Outcome {
        contest: outcome.contest.map(
            |snowchains_core::web::RetrieveTestCasesOutcomeContest {
                 id,
                 submissions_url,
             }| OutcomeContest {
                id: CaseConversions::new(id),
                submissions_url,
            },
        ),
        problems: vec![],
    };

    for snowchains_core::web::RetrieveTestCasesOutcomeProblem {
        index,
        url,
        screen_name,
        display_name,
        mut test_suite,
        text_files,
    } in outcome.problems
    {
        let index = CaseConversions::new(index);

        let path = workspace
            .join(".snowchains")
            .join("tests")
            .join(service.to_kebab_case_str())
            .join(contest.as_deref().unwrap_or(""))
            .join(&index.kebab)
            .with_extension("yml");

        let txt_path = |dir_file_name: &str, txt_file_name: &str| -> _ {
            path.with_file_name(&index.kebab)
                .join(dir_file_name)
                .join(txt_file_name)
                .with_extension("txt")
        };

        for (name, snowchains_core::web::RetrieveTestCasesOutcomeProblemTextFiles { r#in, out }) in
            &text_files
        {
            crate::fs::write(txt_path("in", name), &r#in, true)?;
            if let Some(out) = out {
                crate::fs::write(txt_path("out", name), out, true)?;
            }
        }

        if !text_files.is_empty() {
            if let TestSuite::Batch(BatchTestSuite { cases, extend, .. }) = &mut test_suite {
                cases.clear();

                extend.push(Additional::Text {
                    path: format!("./{}", index.kebab),
                    r#in: "/in/*.txt".to_owned(),
                    out: "/out/*.txt".to_owned(),
                    timelimit: None,
                    r#match: None,
                })
            }
        }

        crate::fs::write(&path, test_suite.to_yaml_pretty(), true)?;

        stderr.set_color(ColorSpec::new().set_reset(false).set_bold(true))?;
        write!(stderr, "{}:", index.original)?;
        stderr.reset()?;

        write!(stderr, " Saved to ")?;

        stderr.set_color(ColorSpec::new().set_reset(false).set_fg(Some(Color::Cyan)))?;
        if text_files.is_empty() {
            write!(stderr, "{}", path.display())
        } else {
            write!(
                stderr,
                "{}",
                path.with_file_name(format!("{{{index}.yml, {index}/}}", index = index.kebab))
                    .display(),
            )
        }?;
        stderr.reset()?;

        write!(stderr, " (")?;

        let (msg, color) = match &test_suite {
            TestSuite::Batch(BatchTestSuite { cases, .. }) => {
                match cases.len() + text_files.len() {
                    0 => ("no test cases".to_owned(), Color::Yellow),
                    1 => ("1 test case".to_owned(), Color::Green),
                    n => (format!("{} test cases", n), Color::Green),
                }
            }
            TestSuite::Interactive(_) => ("interactive problem".to_owned(), Color::Yellow),
            TestSuite::Unsubmittable => ("unsubmittable problem".to_owned(), Color::Yellow),
        };

        stderr.set_color(ColorSpec::new().set_reset(false).set_fg(Some(color)))?;
        write!(stderr, "{}", msg)?;
        stderr.reset()?;

        writeln!(stderr, ")")?;
        stderr.flush()?;

        acc.problems.push(OutcomeProblem {
            index,
            url,
            screen_name,
            display_name,
            test_suite: OutcomeProblemTestSuite {
                path: path
                    .into_os_string()
                    .into_string()
                    .expect("should be UTF-8"),
                content: test_suite,
            },
        });
    }

    if json {
        writeln!(stdout, "{}", acc.to_json())?;
        stdout.flush()?;
    }

    Ok(())
}
