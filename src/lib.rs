mod commands;
mod config;
mod fs;
mod judge;
pub mod shell;
mod web;

pub use crate::commands::{
    init::OptInit, judge::OptJudge, login::OptLogin, participate::OptParticipate,
    retrieve_languages::OptRetrieveLanguages,
    retrieve_submission_summaries::OptRetrieveSubmissionSummaries,
    retrieve_testcases::OptRetrieveTestcases, submit::OptSubmit,
    watch_submissions::OptWatchSubmissions, xtask::OptXtask,
};
use std::{env, io::BufRead, path::PathBuf};
use structopt::{
    clap::{self, AppSettings},
    StructOpt,
};
use strum::{EnumString, EnumVariantNames};
use termcolor::WriteColor;

pub const STACK_SIZE: usize = 128 * 1024 * 1024;

#[derive(StructOpt, Debug)]
#[structopt(author, about, global_setting = AppSettings::DeriveDisplayOrder)]
pub enum Opt {
    /// Create a new config file
    #[structopt(author, visible_alias("i"))]
    Init(OptInit),

    /// Logges in to a service
    #[structopt(author, visible_alias("l"))]
    Login(OptLogin),

    /// Participates in a contest
    Participate(OptParticipate),

    /// Retrieves data
    #[structopt(author, visible_alias("r"))]
    Retrieve(OptRetrieve),

    /// Alias for `retrieve testcases`
    #[structopt(author, visible_alias("d"))]
    Download(OptRetrieveTestcases),

    /// Watches data
    #[structopt(author, visible_alias("w"))]
    Watch(OptWatch),

    /// Tests code
    #[structopt(author, visible_aliases(&["j", "test", "t"]))]
    Judge(OptJudge),

    /// Submits code
    #[structopt(author, visible_alias("s"))]
    Submit(OptSubmit),

    /// Runs a custom subcommand written in the config file
    #[structopt(author, visible_alias("x"), setting = AppSettings::TrailingVarArg)]
    Xtask(OptXtask),
}

#[derive(StructOpt, Debug)]
pub enum OptRetrieve {
    /// Retrieves list of languages
    #[structopt(author, visible_alias("l"))]
    Languages(OptRetrieveLanguages),

    /// Retrieves test cases
    #[structopt(author, visible_alias("t"))]
    Testcases(OptRetrieveTestcases),

    /// Retrieves submission summaries
    #[structopt(author, visible_alias("ss"))]
    SubmissionSummaries(OptRetrieveSubmissionSummaries),
}

#[derive(StructOpt, Debug)]
pub enum OptWatch {
    /// Watches your submissions
    #[structopt(author, visible_alias("s"))]
    Submissions(OptWatchSubmissions),
}

impl Opt {
    pub fn from_args_with_workaround_for_clap_issue_1538() -> Self {
        let mut args = env::args_os().collect::<Vec<_>>();

        Self::from_iter_safe(&args).unwrap_or_else(|clap::Error { kind, .. }| {
            if matches!(
                args.get(1).and_then(|s| s.to_str()),
                Some("x") | Some("xtask")
            ) && matches!(args.get(2).and_then(|s| s.to_str()), Some(s) if !s.starts_with('-'))
                && matches!(
                    kind,
                    clap::ErrorKind::UnknownArgument
                        | clap::ErrorKind::HelpDisplayed
                        | clap::ErrorKind::VersionDisplayed
                )
            {
                args.insert(3, "--".into());
            }

            Self::from_iter(args)
        })
    }

    pub fn color(&self) -> crate::ColorChoice {
        match *self {
            Self::Init(OptInit { color, .. })
            | Self::Login(OptLogin { color, .. })
            | Self::Participate(OptParticipate { color, .. })
            | Self::Retrieve(OptRetrieve::Languages(OptRetrieveLanguages { color, .. }))
            | Self::Retrieve(OptRetrieve::Testcases(OptRetrieveTestcases { color, .. }))
            | Self::Retrieve(OptRetrieve::SubmissionSummaries(OptRetrieveSubmissionSummaries {
                color,
                ..
            }))
            | Self::Download(OptRetrieveTestcases { color, .. })
            | Self::Watch(OptWatch::Submissions(OptWatchSubmissions { color, .. }))
            | Self::Judge(OptJudge { color, .. })
            | Self::Submit(OptSubmit { color, .. }) => color,
            Self::Xtask(_) => crate::ColorChoice::Auto,
        }
    }
}

#[derive(EnumVariantNames, EnumString, strum::Display, Debug, Clone, Copy)]
#[strum(serialize_all = "lowercase")]
pub enum ColorChoice {
    Auto,
    Always,
    Never,
}

pub struct Context<R, W1, W2> {
    pub cwd: PathBuf,
    pub shell: crate::shell::Shell<R, W1, W2>,
}

pub fn run<R: BufRead, W1: WriteColor, W2: WriteColor>(
    opt: Opt,
    ctx: Context<R, W1, W2>,
) -> anyhow::Result<()> {
    match opt {
        Opt::Init(opt) => commands::init::run(opt, ctx),
        Opt::Login(opt) => commands::login::run(opt, ctx),
        Opt::Participate(opt) => commands::participate::run(opt, ctx),
        Opt::Retrieve(OptRetrieve::Languages(opt)) => commands::retrieve_languages::run(opt, ctx),
        Opt::Retrieve(OptRetrieve::Testcases(opt)) => commands::retrieve_testcases::run(opt, ctx),
        Opt::Retrieve(OptRetrieve::SubmissionSummaries(opt)) => {
            commands::retrieve_submission_summaries::run(opt, ctx)
        }
        Opt::Download(opt) => commands::retrieve_testcases::run(opt, ctx),
        Opt::Watch(OptWatch::Submissions(opt)) => commands::watch_submissions::run(opt, ctx),
        Opt::Judge(opt) => commands::judge::run(opt, ctx),
        Opt::Submit(opt) => commands::submit::run(opt, ctx),
        Opt::Xtask(opt) => commands::xtask::run(opt, ctx),
    }
}
