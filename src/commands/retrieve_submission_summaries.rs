use anyhow::Context as _;
use snowchains_core::web::{
    Atcoder, AtcoderRetrieveSubmissionSummariesCredentials,
    AtcoderRetrieveSubmissionSummariesTarget, CookieStorage, PlatformKind,
    RetrieveSubmissionSummaries,
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
pub struct OptRetrieveSubmissionSummaries {
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
    #[structopt(short, long, value_name("SERVICE"), possible_value("atcoder"))]
    pub service: Option<PlatformKind>,

    /// Contest ID
    #[structopt(short, long, value_name("STRING"))]
    pub contest: Option<String>,
}

pub(crate) fn run(
    opt: OptRetrieveSubmissionSummaries,
    ctx: crate::Context<impl BufRead, impl Write, impl WriteColor>,
) -> anyhow::Result<()> {
    let OptRetrieveSubmissionSummaries {
        config,
        color: _,
        service,
        contest,
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

    let cookie_storage = CookieStorage::with_jsonl(crate::web::credentials::cookie_store_path()?)?;
    let timeout = Some(crate::web::SESSION_TIMEOUT);

    match service {
        PlatformKind::Atcoder => {
            let outcome = {
                let shell = RefCell::new(&mut shell);

                let target = AtcoderRetrieveSubmissionSummariesTarget {
                    contest: contest.with_context(|| "`contest` is required for AtCoder")?,
                };

                let credentials = AtcoderRetrieveSubmissionSummariesCredentials {
                    username_and_password:
                        &mut crate::web::credentials::atcoder_username_and_password(&shell),
                };

                Atcoder::exec(RetrieveSubmissionSummaries {
                    target,
                    credentials,
                    cookie_storage,
                    timeout,
                    shell: &shell,
                })?
            };

            writeln!(shell.stdout, "{}", outcome.to_json())?;
            shell.stdout.flush()?;
            Ok(())
        }
        PlatformKind::Codeforces => {
            todo!("`retrieve submission-summaries` for Codeforces is not implemented");
        }
        PlatformKind::Yukicoder => {
            todo!("`retrieve submission-summaries` for yukicoder is not implemented");
        }
    }
}
