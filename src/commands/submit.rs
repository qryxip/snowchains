use crate::config;
use anyhow::Context as _;
use snowchains_core::web::{
    Atcoder, AtcoderSubmitCredentials, AtcoderSubmitTarget, Codeforces,
    CodeforcesSubmitCredentials, CodeforcesSubmitTarget, CookieStorage, PlatformKind, Submit,
    Yukicoder, YukicoderSubmitCredentials, YukicoderSubmitTarget,
};
use std::{cell::RefCell, io::BufRead, path::PathBuf};
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
) -> anyhow::Result<()> {
    let OptSubmit {
        no_watch,
        no_judge,
        debug,
        json,
        config,
        color: _,
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
            ..
        },
        config::Language {
            src,
            transpile,
            compile,
            run,
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
        crate::judge::judge(crate::judge::Args {
            progress_draw_target: shell.progress_draw_target(),
            stdout: &mut shell.stdout,
            stderr: &mut shell.stderr,
            stdin_process_redirection: shell.stdin_process_redirection,
            stdout_process_redirection: shell.stdout_process_redirection,
            stderr_process_redirection: shell.stderr_process_redirection,
            base_dir,
            service,
            contest: contest.clone(),
            problem: problem.clone(),
            src,
            transpile,
            compile,
            run,
        })?;
    }

    let watch_submission = !no_watch;

    let cookie_storage = CookieStorage::with_jsonl(crate::web::credentials::cookie_store_path()?)?;

    let timeout = Some(crate::web::SESSION_TIMEOUT);

    let outcome = match service {
        PlatformKind::Atcoder => {
            let shell = RefCell::new(&mut shell);

            let target = AtcoderSubmitTarget {
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
            let target = CodeforcesSubmitTarget {
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
                YukicoderSubmitTarget::ProblemNo(problem)
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
