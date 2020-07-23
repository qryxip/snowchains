use crate::{
    shell::Shell,
    web::{CaseConversions, LazyLockedFile},
};
use anyhow::Context as _;
use maplit::btreeset;
use serde::{Deserialize, Serialize};
use snowchains_core::{
    testsuite::{Additional, BatchTestSuite, TestSuite},
    web::{
        Atcoder, AtcoderRetrieveFullTestCasesCredentials,
        AtcoderRetrieveSampleTestCasesCredentials, AtcoderRetrieveTestCasesTargets, Codeforces,
        CodeforcesRetrieveSampleTestCasesCredentials, CodeforcesRetrieveTestCasesTargets, Cookies,
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

    /// Prints the output as a JSON value
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
    slug: CaseConversions,
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
        stderr,
        stdin_process_redirection: _,
        stdout_process_redirection: _,
        stderr_process_redirection: _,
        draw_progress: _,
    } = ctx;

    let (detected_target, workspace) = crate::config::detect_target(&cwd, config.as_deref())?;

    let service = service
        .map(Ok)
        .or_else(|| {
            detected_target.service.as_ref().map(|s| {
                s.parse().with_context(|| {
                    "Specified invalid `service` by `detectServiceFromRelativePathSegments`"
                })
            })
        })
        .with_context(|| "`service` is not specified")??;

    let contest = contest.or(detected_target.contest);
    let contest = contest.as_deref();

    let problems = match (problems.as_deref().unwrap_or(&[]), &detected_target.problem) {
        ([], None) => None,
        ([], Some(problem)) => Some(btreeset!(problem.clone())),
        (problems, _) => Some(problems.iter().cloned().collect()),
    };

    let timeout = Some(crate::web::SESSION_TIMEOUT);

    let cookies_path = crate::web::cookies_path()?;
    let cookies_file = LazyLockedFile::new(&cookies_path);

    let mut on_update_cookie_store =
        |cookie_store: &_| crate::web::save_cookie_store(cookie_store, &cookies_file);
    let cookies = Cookies {
        cookie_store: crate::web::load_cookie_store(cookies_file.path())?,
        on_update_cookie_store: &mut on_update_cookie_store,
    };

    let stderr = RefCell::new(stderr);
    let shell = Shell::new(&stderr, true);

    let mut username_and_password = || -> _ {
        let mut stderr = stderr.borrow_mut();

        write!(stderr, "Username: ")?;
        stderr.flush()?;
        let username = stdin.read_reply()?;

        write!(stderr, "Password: ")?;
        stderr.flush()?;
        let password = stdin.read_password()?;

        Ok((username, password))
    };
    let username_and_password = &mut username_and_password;

    let outcome = match service {
        PlatformVariant::Atcoder => {
            let targets = {
                let contest = contest
                    .with_context(|| "`contest` is required for AtCoder")?
                    .to_owned();
                AtcoderRetrieveTestCasesTargets { contest, problems }
            };

            let credentials = AtcoderRetrieveSampleTestCasesCredentials {
                username_and_password,
            };

            let full = if full {
                Some({
                    #[derive(Deserialize)]
                    struct Dropbox {
                        access_token: String,
                    }

                    let path = dirs::data_local_dir()
                        .with_context(|| "Could not find the local data directory")?
                        .join("snowchains")
                        .join("dropbox.json");

                    let Dropbox { access_token } =
                        crate::fs::read_json(&path).with_context(|| {
                            format!("First, save the access token to `{}`", path.display())
                        })?;

                    RetrieveFullTestCases {
                        credentials: AtcoderRetrieveFullTestCasesCredentials {
                            dropbox_access_token: access_token,
                        },
                    }
                })
            } else {
                None
            };

            Atcoder::exec(RetrieveTestCases {
                targets,
                timeout,
                cookies,
                shell,
                credentials,
                full,
            })
        }
        PlatformVariant::Codeforces => {
            let targets = {
                let contest = contest
                    .with_context(|| "`contest` is required for Codeforces")?
                    .parse()
                    .with_context(|| "`contest` for Codeforces must be 64-bit unsigned integer")?;
                CodeforcesRetrieveTestCasesTargets { contest, problems }
            };
            let credentials = CodeforcesRetrieveSampleTestCasesCredentials {
                username_and_password,
            };

            Codeforces::exec(RetrieveTestCases {
                targets,
                timeout,
                cookies,
                shell,
                credentials,
                full: None,
            })
        }
        PlatformVariant::Yukicoder => {
            let targets = if let Some(contest) = contest {
                let contest = contest
                    .parse()
                    .with_context(|| "`contest` for yukicoder must be 64-bit unsigned integer")?;
                YukicoderRetrieveTestCasesTargets::Contest(contest, problems)
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
                Some({
                    let path = dirs::data_local_dir()
                        .with_context(|| "Could not find the local data directory")?
                        .join("snowchains")
                        .join("yukicoder.json");

                    let api_key = if path.exists() {
                        crate::fs::read_json(path)?
                    } else {
                        let mut stderr = stderr.borrow_mut();
                        write!(stderr, "yukicoder API key: ")?;
                        stderr.flush()?;
                        let api_key = stdin.read_reply()?;
                        crate::fs::write_json(path, &api_key, true)?;
                        api_key
                    };

                    RetrieveFullTestCases {
                        credentials: YukicoderRetrieveFullTestCasesCredentials { api_key },
                    }
                })
            } else {
                None
            };

            Yukicoder::exec(RetrieveTestCases {
                targets,
                timeout,
                cookies: (),
                shell,
                credentials: (),
                full,
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
        slug,
        url,
        screen_name,
        display_name,
        mut test_suite,
        text_files,
    } in outcome.problems
    {
        let slug = CaseConversions::new(slug);

        let path = workspace
            .join(".snowchains")
            .join("tests")
            .join(service.to_kebab_case_str())
            .join(contest.as_deref().unwrap_or(""))
            .join(&slug.kebab)
            .with_extension("yml");

        let txt_path = |dir_file_name: &str, txt_file_name: &str| -> _ {
            path.with_file_name(&slug.kebab)
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
                    base: format!("./{}", slug.kebab),
                    r#in: "/in/*.txt".to_owned(),
                    out: "/out/*.txt".to_owned(),
                    timelimit: None,
                    r#match: None,
                })
            }
        }

        crate::fs::write(&path, test_suite.to_yaml_pretty(), true)?;

        let mut stderr = stderr.borrow_mut();

        stderr.set_color(ColorSpec::new().set_reset(false).set_bold(true))?;
        write!(stderr, "{}:", slug.original)?;
        stderr.reset()?;

        write!(stderr, " Saved to ")?;

        stderr.set_color(ColorSpec::new().set_reset(false).set_fg(Some(Color::Cyan)))?;
        if text_files.is_empty() {
            write!(stderr, "{}", path.display())
        } else {
            write!(
                stderr,
                "{}",
                path.with_file_name(format!("{{{slug}.yml, {slug}/}}", slug = slug.kebab))
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
            slug,
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
