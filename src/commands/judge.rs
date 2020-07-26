use crate::config;
use snowchains_core::web::PlatformKind;
use std::path::PathBuf;
use structopt::StructOpt;
use strum::VariantNames as _;
use termcolor::WriteColor;

#[derive(StructOpt, Debug)]
pub struct OptJudge {
    /// Build in `Release` mode
    #[structopt(long)]
    pub release: bool,

    ///// Prints the output as a JSON value
    //#[structopt(long)]
    //pub json: bool,
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
    opt: OptJudge,
    ctx: crate::Context<impl Sized, impl WriteColor, impl WriteColor>,
) -> anyhow::Result<()> {
    let OptJudge {
        release,
        //json,
        config,
        color: _,
        service,
        contest,
        language,
        problem,
    } = opt;

    let crate::Context {
        cwd,
        stdin: _,
        stdout,
        stderr,
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
            languageId: _,
        },
        base_dir,
    ) = config::target_and_language(
        &cwd,
        config.as_deref(),
        service,
        contest.as_deref(),
        problem.as_deref(),
        language.as_deref(),
        if release {
            config::Mode::Release
        } else {
            config::Mode::Debug
        },
    )?;

    crate::judge::judge(crate::judge::Args {
        stdout,
        stderr,
        stdin_process_redirection,
        stdout_process_redirection,
        stderr_process_redirection,
        draw_progress,
        base_dir,
        service,
        contest,
        problem,
        src,
        transpile,
        compile,
        run,
    })
}
