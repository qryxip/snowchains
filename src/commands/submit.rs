use crate::{config, shell::Shell};
use anyhow::Context as _;
use snowchains_core::web::{
    Atcoder, AtcoderSubmitCredentials, AtcoderSubmitTarget, Codeforces,
    CodeforcesSubmitCredentials, CodeforcesSubmitTarget, PlatformVariant, Submit, Yukicoder,
    YukicoderSubmitCredentials, YukicoderSubmitTarget,
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
        possible_values(PlatformVariant::KEBAB_CASE_VARIANTS)
    )]
    pub service: Option<PlatformVariant>,

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

    let crate::Context {
        cwd,
        mut stdin,
        mut stdout,
        mut stderr,
        stdin_process_redirection,
        stdout_process_redirection,
        stderr_process_redirection,
        draw_progress,
    } = ctx;

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
                &mut stderr,
                &base_dir,
                &src,
                transpile,
                stdin_process_redirection,
                stdout_process_redirection,
                stderr_process_redirection,
            )?;
        }
    } else {
        crate::judge::judge(crate::judge::Args {
            stdout: &mut stdout,
            stderr: &mut stderr,
            stdin_process_redirection,
            stdout_process_redirection,
            stderr_process_redirection,
            draw_progress,
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

    let cookie_storage = crate::web::cookie_storage::cookie_storage()?;

    let timeout = Some(crate::web::SESSION_TIMEOUT);

    let outcome = match service {
        PlatformVariant::Atcoder => {
            let target = AtcoderSubmitTarget {
                contest: contest.with_context(|| "`contest` is required for AtCoder")?,
                problem,
            };

            let stderr = RefCell::new(&mut stderr);
            let shell = Shell::new(&stderr, true);

            let credentials = AtcoderSubmitCredentials {
                username_and_password: &mut crate::web::credentials::atcoder_username_and_password(
                    stdin, &stderr,
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
                shell,
            })
        }
        PlatformVariant::Codeforces => {
            let target = CodeforcesSubmitTarget {
                contest: contest.with_context(|| "`contest` is required for Codeforces")?,
                problem,
            };

            let (api_key, api_secret) =
                crate::web::credentials::codeforces_api_key_and_secret(&mut stdin, &mut stderr)?;

            let stderr = RefCell::new(&mut stderr);
            let shell = Shell::new(&stderr, true);

            let credentials = CodeforcesSubmitCredentials {
                username_and_password:
                    &mut crate::web::credentials::codeforces_username_and_password(stdin, &stderr),
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
                shell,
            })
        }
        PlatformVariant::Yukicoder => {
            let target = if let Some(contest) = contest {
                YukicoderSubmitTarget::Contest(contest, problem)
            } else {
                YukicoderSubmitTarget::ProblemNo(problem)
            };

            let credentials = YukicoderSubmitCredentials {
                api_key: crate::web::credentials::yukicoder_api_key(&mut stdin, &mut stderr)?,
            };

            let stderr = RefCell::new(&mut stderr);
            let shell = Shell::new(&stderr, true);

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
        write!(stdout, "{}", outcome.to_json())?;
        stdout.flush()?;
    }

    Ok(())
}
