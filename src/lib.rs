macro_rules! color_spec {
    ($($tt:tt)*) => {
        color_spec_inner!(@acc(::termcolor::ColorSpec::new().set_reset(false)), @rest($($tt)*))
    };
}

macro_rules! color_spec_inner {
    (@acc($acc:expr), @rest()) => {
        $acc
    };
    (@acc($acc:expr), @rest(, $($rest:tt)*)) => {
        color_spec_inner!(@acc($acc), @rest($($rest)*))
    };
    (@acc($acc:expr), @rest(bold $($rest:tt)*)) => {
        color_spec_inner!(@acc($acc.set_bold(true)), @rest($($rest)*))
    };
    (@acc($acc:expr), @rest(fg($color:expr) $($rest:tt)*)) => {
        color_spec_inner!(@acc($acc.set_fg(::std::option::Option::Some($color))), @rest($($rest)*))
    };
}

mod commands;
mod config;
mod fs;
mod judge;
mod shell;
mod web;

pub use crate::commands::{
    init::OptInit, login::OptLogin, retrieve_languages::OptRetrieveLanguages,
    retrieve_testcases::OptRetrieveTestcases, submit::OptSubmit, test::OptTest, xtask::OptXtask,
};
use std::{
    env,
    io::{self, BufRead, Stdin, StdinLock},
    path::PathBuf,
    process::Stdio,
};
use structopt::{
    clap::{self, AppSettings},
    StructOpt,
};
use strum::{EnumString, EnumVariantNames};
use termcolor::WriteColor;

#[derive(StructOpt, Debug)]
#[structopt(author, about, global_setting = AppSettings::DeriveDisplayOrder)]
pub enum Opt {
    /// Create a new config file
    #[structopt(author, visible_alias("i"))]
    Init(OptInit),

    /// Logges in to a service
    #[structopt(author, visible_alias("l"))]
    Login(OptLogin),

    /// Retrieves data
    #[structopt(author, visible_alias("r"))]
    Retrieve(OptRetrieve),

    /// Tests code
    #[structopt(author, visible_alias("t"))]
    Test(OptTest),

    /// Submits code
    #[structopt(author, visible_alias("s"))]
    Submit(OptSubmit),

    /// Run a custom subcommand written in the config file
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
            | Self::Retrieve(OptRetrieve::Languages(OptRetrieveLanguages { color, .. }))
            | Self::Retrieve(OptRetrieve::Testcases(OptRetrieveTestcases { color, .. }))
            | Self::Test(OptTest { color, .. })
            | Self::Submit(OptSubmit { color, .. }) => color,
            Self::Xtask(_) => crate::ColorChoice::Auto,
        }
    }
}

#[derive(EnumVariantNames, EnumString, Debug, Clone, Copy)]
#[strum(serialize_all = "lowercase")]
pub enum ColorChoice {
    Auto,
    Always,
    Never,
}

#[derive(Debug)]
pub struct Context<R, W1, W2> {
    pub cwd: PathBuf,
    pub stdin: TtyOrPiped<R>,
    pub stdout: W1,
    pub stderr: W2,
    pub stdin_process_redirection: fn() -> Stdio,
    pub stdout_process_redirection: fn() -> Stdio,
    pub stderr_process_redirection: fn() -> Stdio,
    pub draw_progress: bool,
}

#[derive(Debug)]
pub enum TtyOrPiped<R> {
    Tty,
    Piped(R),
}

impl<'a> TtyOrPiped<StdinLock<'a>> {
    /// Creates a new `TtyOrPiped`.
    ///
    /// Returns `Tty` if the stdin is a TTY, otherwise `Piped`.
    pub fn auto(stdin: &'a Stdin) -> Self {
        if atty::is(atty::Stream::Stdin) && !(cfg!(windows) && env::var_os("MSYSTEM").is_some()) {
            TtyOrPiped::Tty
        } else {
            TtyOrPiped::Piped(stdin.lock())
        }
    }
}

impl<R: BufRead> TtyOrPiped<R> {
    fn read_reply(&mut self) -> io::Result<String> {
        match self {
            Self::Tty => rprompt::read_reply(),
            Self::Piped(r) => rpassword::read_password_with_reader(Some(r)),
        }
    }

    fn read_password(&mut self) -> io::Result<String> {
        match self {
            Self::Tty => rpassword::read_password_from_tty(None),
            Self::Piped(r) => rpassword::read_password_with_reader(Some(r)),
        }
    }
}

pub fn run<R: BufRead, W1: WriteColor, W2: WriteColor>(
    opt: Opt,
    ctx: Context<R, W1, W2>,
) -> anyhow::Result<()> {
    match opt {
        Opt::Init(opt) => commands::init::run(opt, ctx),
        Opt::Login(opt) => commands::login::run(opt, ctx),
        Opt::Retrieve(OptRetrieve::Languages(opt)) => commands::retrieve_languages::run(opt, ctx),
        Opt::Retrieve(OptRetrieve::Testcases(opt)) => commands::retrieve_testcases::run(opt, ctx),
        Opt::Test(opt) => commands::test::run(opt, ctx),
        Opt::Submit(opt) => commands::submit::run(opt, ctx),
        Opt::Xtask(opt) => commands::xtask::run(opt, ctx),
    }
}
