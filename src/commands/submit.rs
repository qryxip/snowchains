use crate::config;
use eyre::{bail, ContextCompat as _};
use human_size::Size;
use snowchains_core::web::{
    Atcoder, AtcoderSubmitCredentials, Codeforces, CodeforcesSubmitCredentials, CookieStorage,
    PlatformKind, ProblemInContest, Submit, Yukicoder, YukicoderSubmitCredentials,
    YukicoderSubmitTarget,
};
use std::{cell::RefCell, env, io::BufRead, iter, path::PathBuf};
use structopt::StructOpt;
use strum::VariantNames as _;
use termcolor::WriteColor;

#[derive(StructOpt, Debug)]
pub struct OptSubmit {
    /// Do no watch the submission
    #[structopt(long)]
    pub no_watch: bool,

    /// Do not `compile` the code
    #[structopt(long)]
    pub no_judge: bool,

    /// Tests code in `Debug` mode
    #[structopt(long)]
    pub debug: bool,

    /// Prints JSON data
    #[structopt(long)]
    pub json: bool,

    /// Test for only the test cases
    #[structopt(long, value_name("NAME"))]
    pub testcases: Option<Vec<String>>,

    /// Display limit for the test
    #[structopt(long, value_name("SIZE"), default_value("4KiB"))]
    pub display_limit: Size,

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
        possible_values(PlatformKind::KEBAB_CASE_VARIANTS)
    )]
    pub service: Option<PlatformKind>,

    /// Contest ID
    #[structopt(short, long, value_name("STRING"))]
    pub contest: Option<String>,

    /// Language name
    #[structopt(short, long, value_name("STRING"))]
    pub language: Option<String>,

    /// Problem index (e.g. "a", "b", "c")
    pub problem: Option<String>,
}

pub(crate) fn run(
    opt: OptSubmit,
    ctx: crate::Context<impl BufRead, impl WriteColor, impl WriteColor>,
) -> eyre::Result<()> {
    let OptSubmit {
        no_watch,
        no_judge,
        debug,
        json,
        testcases,
        display_limit,
        config,
        color,
        service,
        contest,
        language,
        problem,
    } = opt;

    let crate::Context { cwd, mut shell } = ctx;

    let (
        config::Target {
            service,
            contest,
            problem,
            mode: _,
        },
        config::Language {
            src,
            transpile,
            compile: _,
            run: _,
            languageId: language_id,
        },
        base_dir,
    ) = config::target_and_language(
        &cwd,
        config.as_deref(),
        service,
        contest.as_deref(),
        problem.as_deref(),
        language.as_deref(),
        if debug {
            config::Mode::Debug
        } else {
            config::Mode::Release
        },
    )?;

    let code = crate::fs::read_to_string(base_dir.join(&src))?;
    let language_id = language_id.with_context(|| "Missing `languageId`")?;

    if no_judge {
        if let Some(transpile) = &transpile {
            crate::judge::transpile(
                &mut shell.stderr,
                &base_dir,
                &src,
                transpile,
                shell.stdin_process_redirection,
                shell.stdout_process_redirection,
                shell.stderr_process_redirection,
            )?;
        }
    } else {
        let status = std::process::Command::new(env::current_exe()?)
            .arg("j")
            .args(if debug { &[][..] } else { &["--release"] })
            .args(if let Some(testcases) = testcases {
                iter::once("--testcases".into()).chain(testcases).collect()
            } else {
                vec![]
            })
            .args(&["--display-limit", &display_limit.to_string()])
            .arg("--config")
            .arg(base_dir.join("snowchains.dhall"))
            .args(&["--color", &color.to_string()])
            .args(&["-s", service.to_kebab_case_str()])
            .args(if let Some(contest) = &contest {
                vec!["-c".to_owned(), contest.clone()]
            } else {
                vec![]
            })
            .args(if let Some(language) = &language {
                vec!["-l".to_owned(), language.clone()]
            } else {
                vec![]
            })
            .arg(&problem)
            .status()?;

        if !status.success() {
            bail!("`snowchains j ...` failed ({})", status);
        }
    }

    let watch_submission = !no_watch;

    let cookie_storage = CookieStorage::with_jsonl(crate::web::credentials::cookie_store_path()?)?;

    let timeout = Some(crate::web::SESSION_TIMEOUT);

    let outcome = match service {
        PlatformKind::Atcoder => {
            let shell = RefCell::new(&mut shell);

            let target = ProblemInContest::Index {
                contest: contest.with_context(|| "`contest` is required for AtCoder")?,
                problem,
            };

            let credentials = AtcoderSubmitCredentials {
                username_and_password: &mut crate::web::credentials::atcoder_username_and_password(
                    &shell,
                ),
            };

            Atcoder::exec(Submit {
                target,
                credentials,
                language_id,
                code,
                watch_submission,
                cookie_storage,
                timeout,
                shell: &shell,
            })
        }
        PlatformKind::Codeforces => {
            let target = ProblemInContest::Index {
                contest: contest.with_context(|| "`contest` is required for Codeforces")?,
                problem,
            };

            let (api_key, api_secret) =
                crate::web::credentials::codeforces_api_key_and_secret(&mut shell)?;

            let shell = RefCell::new(&mut shell);

            let credentials = CodeforcesSubmitCredentials {
                username_and_password:
                    &mut crate::web::credentials::codeforces_username_and_password(&shell),
                api_key,
                api_secret,
            };

            Codeforces::exec(Submit {
                target,
                credentials,
                language_id,
                code,
                watch_submission,
                cookie_storage,
                timeout,
                shell: &shell,
            })
        }
        PlatformKind::Yukicoder => {
            let target = if let Some(contest) = contest {
                YukicoderSubmitTarget::Contest(contest, problem)
            } else {
                YukicoderSubmitTarget::from_problem_no(&problem)
            };

            let credentials = YukicoderSubmitCredentials {
                api_key: crate::web::credentials::yukicoder_api_key(&mut shell)?,
            };

            let shell = RefCell::new(&mut shell);

            Yukicoder::exec(Submit {
                target,
                credentials,
                language_id,
                code,
                watch_submission,
                cookie_storage: (),
                timeout,
                shell,
            })
        }
    }?;

    if json {
        write!(shell.stdout, "{}", outcome.to_json())?;
        shell.stdout.flush()?;
    }

    Ok(())
}
