use eyre::ContextCompat as _;
use snowchains_core::web::{
    Atcoder, AtcoderRetrieveLanguagesCredentials, AtcoderRetrieveLanguagesTarget, Codeforces,
    CodeforcesRetrieveLanguagesCredentials, CodeforcesRetrieveLanguagesTarget, CookieStorage,
    PlatformKind, RetrieveLanguages, Yukicoder,
};
use std::{
    cell::RefCell,
    io::{BufRead, Write},
    path::PathBuf,
};
use structopt::StructOpt;
use strum::VariantNames as _;
use termcolor::WriteColor;

#[derive(StructOpt, Debug)]
pub struct OptRetrieveLanguages {
    /// Prints the result as JSON
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

    /// Problem index (e.g. "a", "b", "c")
    #[structopt(short, long, value_name("STRING"))]
    pub problem: Option<String>,
}

pub(crate) fn run(
    opt: OptRetrieveLanguages,
    ctx: crate::Context<impl BufRead, impl Write, impl WriteColor>,
) -> eyre::Result<()> {
    let OptRetrieveLanguages {
        json,
        config,
        color: _,
        service,
        contest,
        problem,
    } = opt;

    let crate::Context { cwd, mut shell } = ctx;

    let (detected_target, _) = crate::config::detect_target(&cwd, config.as_deref())?;

    let service = service
        .map(Ok)
        .or_else(|| detected_target.parse_service().transpose())
        .with_context(|| {
            "`service` was not detected. To specify it, add `--service` to the arguments"
        })??;

    let contest = contest.or(detected_target.contest);
    let problem = problem.or(detected_target.problem);

    let cookie_storage = CookieStorage::with_jsonl(crate::web::credentials::cookie_store_path()?)?;

    let timeout = Some(crate::web::SESSION_TIMEOUT);

    let outcome =
        match service {
            PlatformKind::Atcoder => {
                let shell = RefCell::new(&mut shell);

                let target = AtcoderRetrieveLanguagesTarget {
                    contest_and_problem: contest.and_then(|c| problem.map(|p| (c, p))),
                };

                let credentials = AtcoderRetrieveLanguagesCredentials {
                    username_and_password:
                        &mut crate::web::credentials::atcoder_username_and_password(&shell),
                };

                Atcoder::exec(RetrieveLanguages {
                    target,
                    credentials,
                    cookie_storage,
                    timeout,
                    shell: &shell,
                })
            }
            PlatformKind::Codeforces => {
                let shell = RefCell::new(&mut shell);

                let target = CodeforcesRetrieveLanguagesTarget {
                    contest: contest.with_context(|| "`contest` is required for Codeforces")?,
                };

                let credentials = CodeforcesRetrieveLanguagesCredentials {
                    username_and_password:
                        &mut crate::web::credentials::atcoder_username_and_password(&shell),
                };

                Codeforces::exec(RetrieveLanguages {
                    target,
                    credentials,
                    cookie_storage,
                    timeout,
                    shell: &shell,
                })
            }
            PlatformKind::Yukicoder => Yukicoder::exec(RetrieveLanguages {
                target: (),
                credentials: (),
                cookie_storage: (),
                timeout,
                shell: &mut shell,
            }),
        }?;

    if json {
        writeln!(shell.stdout, "{}", outcome.to_json())
    } else {
        write!(shell.stdout, "{}", outcome.to_table())
    }?;

    shell.stdout.flush()?;
    Ok(())
}
