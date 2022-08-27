use crate::config;
use human_size::Size;
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

    /// Test for only the test cases
    #[structopt(long, value_name("NAME"))]
    pub testcases: Option<Vec<String>>,

    /// Display limit
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
    opt: OptJudge,
    ctx: crate::Context<impl Sized, impl WriteColor, impl WriteColor>,
) -> eyre::Result<()> {
    let OptJudge {
        release,
        testcases,
        display_limit,
        config,
        color: _,
        service,
        contest,
        language,
        problem,
    } = opt;

    let crate::Context { cwd, shell } = ctx;

    let progress_draw_target = shell.progress_draw_target();

    let crate::shell::Shell {
        stdout,
        stderr,
        stdin_process_redirection,
        stdout_process_redirection,
        stderr_process_redirection,
        ..
    } = shell;

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

    let test_case_names = testcases.map(|ss| ss.into_iter().collect());

    crate::judge::judge(crate::judge::Args {
        stdout,
        stderr,
        stdin_process_redirection,
        stdout_process_redirection,
        stderr_process_redirection,
        progress_draw_target,
        base_dir,
        service,
        contest,
        problem,
        src,
        transpile,
        compile,
        run,
        test_case_names,
        display_limit,
    })
}
