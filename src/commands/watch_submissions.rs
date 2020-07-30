use anyhow::Context as _;
use snowchains_core::web::{
    Atcoder, AtcoderWatchSubmissionsCredentials, AtcoderWatchSubmissionsTarget, CookieStorage,
    PlatformKind, WatchSubmissions,
};
use std::{cell::RefCell, io::BufRead, path::PathBuf};
use structopt::StructOpt;
use strum::VariantNames as _;
use termcolor::WriteColor;

#[derive(StructOpt, Debug)]
pub struct OptWatchSubmissions {
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

    /// Target platform
    #[structopt(short, long, value_name("SERVICE"), possible_value("atcoder"))]
    pub service: Option<PlatformKind>,

    /// Contest ID
    #[structopt(short, long, value_name("STRING"))]
    pub contest: Option<String>,
}

pub(crate) fn run(
    opt: OptWatchSubmissions,
    ctx: crate::Context<impl BufRead, impl Sized, impl WriteColor>,
) -> anyhow::Result<()> {
    let OptWatchSubmissions {
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
            let target = AtcoderWatchSubmissionsTarget {
                contest: contest.with_context(|| "`contest` is required for AtCoder")?,
            };

            let shell = RefCell::new(&mut shell);

            let credentials = AtcoderWatchSubmissionsCredentials {
                username_and_password: &mut crate::web::credentials::atcoder_username_and_password(
                    &shell,
                ),
            };

            Atcoder::exec(WatchSubmissions {
                target,
                credentials,
                cookie_storage,
                timeout,
                shell: &shell,
            })
        }
        PlatformKind::Codeforces => todo!(),
        PlatformKind::Yukicoder => todo!(),
    }
}
