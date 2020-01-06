#![recursion_limit = "128"]

#[macro_use]
mod macros;

pub mod config;
pub mod errors;
pub mod path;
pub mod service;
pub mod signal;
pub mod terminal;
pub mod testsuite;

mod command;
mod fs;
mod judging;
mod outcome;
mod template;
mod time;
mod util;

pub use crate::errors::{Error, ErrorKind, Result};

use crate::config::{Config, SubCommandKind};
use crate::outcome::Outcome;
use crate::path::{AbsPath, AbsPathBuf};
use crate::service::{
    RetrieveLangsProps, RetrieveSubmissionsProps, RetrieveTestCasesProps, ServiceKind, SubmitProps,
};
use crate::terminal::{AnsiColorChoice, AttemptEnableColor, HasTermProps, Input, ModifyTermProps};
use crate::util::collections::NonEmptyIndexSet;

use snowchains_proc_macros::ArgEnum;

use indexmap::IndexMap;
use serde::Serialize;
use structopt::clap::{AppSettings, Arg};
use structopt::StructOpt;
use termcolor::WriteColor;
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

use std::num::NonZeroUsize;
use std::path::PathBuf;

#[derive(Debug, StructOpt)]
#[structopt(
    author,
    about,
    global_setting(AppSettings::DeriveDisplayOrder),
    usage(
        "snowchains <i|init> [FLAGS] [OPTIONS] [directory]\
         \n    snowchains <w|switch|c|checkout> [FLAGS] [OPTIONS]\
         \n    snowchains <l|login> [FLAGS] [OPTIONS] <service>\
         \n    snowchains <p|participate> [FLAGS] [OPTIONS] <service> <contest>\
         \n    snowchains <d|download> [FLAGS] [OPTIONS]\
         \n    snowchains <r|retrieve> <t|testcases> [FLAGS] [OPTIONS]\
         \n    snowchains <r|retrieve> <l|languages> [FLAGS] [OPTIONS] [problem]\
         \n    snowchains <r|retrieve> <s|submissions> [FLAGS] [OPTIONS]\
         \n    snowchains <j|judge|t|test> [FLAGS] [OPTIONS] <problem>\
         \n    snowchains <s|submit> [FLAGS] [OPTIONS] <problem>"
    )
)]
pub enum Opt {
    #[structopt(
        visible_alias("i"),
        about("Creates a config file (\"snowchains.toml\")"),
        usage("snowchains <i|init> [FLAGS] [OPTIONS] [directory]")
    )]
    Init(Init),
    #[structopt(
        visible_aliases(&["w", "checkout", "c"]),
        about("Modifies values in a config file"),
        usage("snowchains <w|switch|c|checkout> [FLAGS] [OPTIONS]")
    )]
    Switch(Switch),
    #[structopt(
        visible_alias("l"),
        about("Logges in to a service"),
        usage("snowchains <l|login> [FLAGS] [OPTIONS] <service>")
    )]
    Login(Login),
    #[structopt(
        visible_alias("p"),
        about("Participates in a contest"),
        usage("snowchains <p|participate> [FLAGS] [OPTIONS] <service> <contest>")
    )]
    Participate(Participate),
    #[structopt(
        visible_alias("d"),
        about("An alias for `retrieve testcases`"),
        usage("snowchains <d|download> [FLAGS] [OPTIONS]")
    )]
    Download(RetrieveTestcases),
    #[structopt(
        visible_alias("r"),
        about("Retrieves data"),
        usage(
            "snowchains <r|retrieve> <t|testcases> [FLAGS] [OPTIONS]\
             \n    snowchains <r|retrieve> <l|languages> [FLAGS] [OPTIONS] [problem]\
             \n    snowchains <r|retrieve> <s|submissions> [FLAGS] [OPTIONS]"
        )
    )]
    Retrieve(Retrieve),
    #[structopt(
        visible_aliases(&["j", "test", "t"]),
        about("Tests a binary or script"),
        usage("snowchains <j|judge|t|test> [FLAGS] [OPTIONS] <problem>")
    )]
    Judge(Judge),
    #[structopt(
        visible_alias("s"),
        about("Submits a source file"),
        usage("snowchains <s|submit> [FLAGS] [OPTIONS] <problem>")
    )]
    Submit(Submit),
}

#[derive(Debug, Serialize, StructOpt)]
pub struct Init {
    #[structopt(short = "C", long, colorize())]
    colorize: bool,
    #[structopt(color_choice())]
    color_choice: AnsiColorChoice,
    #[structopt(
        default_value = ".",
        parse(from_os_str),
        help("Directory to create a \"snowchains.toml\"")
    )]
    directory: PathBuf,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct Switch {
    #[structopt(long, json())]
    json: bool,
    #[structopt(short = "C", long, colorize())]
    colorize: bool,
    #[structopt(service(&ServiceKind::variants(), true))]
    service: Option<ServiceKind>,
    #[structopt(contest(true))]
    contest: Option<String>,
    #[structopt(language())]
    language: Option<String>,
    #[structopt(output("pretty", &["pretty", "json"]))]
    output: OutputKind,
    #[structopt(color_choice())]
    color_choice: AnsiColorChoice,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct Login {
    #[structopt(long, json())]
    pub json: bool,
    #[structopt(short = "C", long, colorize())]
    pub colorize: bool,
    #[structopt(output("pretty", &["pretty", "json"]))]
    pub output: OutputKind,
    #[structopt(color_choice())]
    pub color_choice: AnsiColorChoice,
    #[structopt(service(&ServiceKind::variants_except_other(), false))]
    pub service: ServiceKind,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct Participate {
    #[structopt(long, json())]
    json: bool,
    #[structopt(short = "C", long, colorize())]
    colorize: bool,
    #[structopt(output("pretty", &["pretty", "json"]))]
    output: OutputKind,
    #[structopt(color_choice())]
    color_choice: AnsiColorChoice,
    #[structopt(service(&["atcoder", "codeforces"], false))]
    service: ServiceKind,
    #[structopt(contest(false))]
    contest: String,
}

#[derive(Debug, Serialize, StructOpt)]
pub enum Retrieve {
    #[structopt(
        visible_alias("t"),
        about("Retrieves test cases"),
        usage("snowchains <r|retrieve> <t|testcases> [FLAGS] [OPTIONS]")
    )]
    Testcases(RetrieveTestcases),
    #[structopt(
        visible_alias("l"),
        about("Retrieves available languages"),
        usage("snowchains <r|retrieve> <l|languages> [FLAGS] [OPTIONS] [problem]")
    )]
    Languages(RetrieveLanguages),
    #[structopt(
        visible_alias("s"),
        about("Retrieves source files you have submitted"),
        usage("snowchains <r|retrieve> <s|submissions> [FLAGS] [OPTIONS]")
    )]
    Submissions(RetrieveSubmissions),
}

#[derive(Debug, Serialize, StructOpt)]
pub struct RetrieveTestcases {
    #[structopt(long, help("Downloads full test cases"))]
    pub full: bool,
    #[structopt(long, no_save())]
    pub no_save: bool,
    #[structopt(short, long, open())]
    pub open: bool,
    #[structopt(long, verbose())]
    pub verbose: bool,
    #[structopt(long, json())]
    pub json: bool,
    #[structopt(short = "C", long, colorize())]
    pub colorize: bool,
    #[structopt(service(&ServiceKind::variants_except_other(), true))]
    pub service: Option<ServiceKind>,
    #[structopt(contest(true))]
    pub contest: Option<String>,
    #[structopt(problems())]
    pub problems: Vec<String>,
    #[structopt(output("pretty", &["pretty", "json"]))]
    pub output: OutputKind,
    #[structopt(color_choice())]
    pub color_choice: AnsiColorChoice,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct RetrieveLanguages {
    #[structopt(long, json())]
    pub json: bool,
    #[structopt(short = "C", long, colorize())]
    pub colorize: bool,
    #[structopt(service(&ServiceKind::variants_except_other(), true))]
    pub service: Option<ServiceKind>,
    #[structopt(contest(true))]
    pub contest: Option<String>,
    #[structopt(output("pretty", &["pretty", "json"]))]
    pub output: OutputKind,
    #[structopt(color_choice())]
    pub color_choice: AnsiColorChoice,
    #[structopt(problem())]
    pub problem: Option<String>,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct RetrieveSubmissions {
    #[structopt(long, help("Fetches all of the code"))]
    pub fetch_all: bool,
    #[structopt(long, no_save())]
    pub no_save: bool,
    #[structopt(long, verbose())]
    pub verbose: bool,
    #[structopt(long, json())]
    pub json: bool,
    #[structopt(short = "C", long, colorize())]
    pub colorize: bool,
    #[structopt(service(&ServiceKind::variants_except_other(), true))]
    pub service: Option<ServiceKind>,
    #[structopt(contest(true))]
    pub contest: Option<String>,
    #[structopt(mode("debug"))]
    pub mode: config::Mode,
    #[structopt(problems())]
    pub problems: Vec<String>,
    #[structopt(output("pretty", &["pretty", "pretty-verbose", "json"]))]
    pub output: OutputKind,
    #[structopt(color_choice())]
    pub color_choice: AnsiColorChoice,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct Judge {
    #[structopt(long, force_compile())]
    pub force_compile: bool,
    #[structopt(long, conflicts_with("mode"), help("Equivalents to `--mode release`"))]
    pub release: bool,
    #[structopt(long, verbose())]
    pub verbose: bool,
    #[structopt(long, json())]
    pub json: bool,
    #[structopt(short = "C", long, colorize())]
    pub colorize: bool,
    #[structopt(service(&ServiceKind::variants(), true))]
    pub service: Option<ServiceKind>,
    #[structopt(contest(true))]
    pub contest: Option<String>,
    #[structopt(language())]
    pub language: Option<String>,
    #[structopt(mode("debug"))]
    pub mode: config::Mode,
    #[structopt(parse(try_from_str = parse_non_zero_usize), jobs())]
    pub jobs: Option<NonZeroUsize>,
    #[structopt(output("pretty", &["pretty", "pretty-verbose", "json"]))]
    pub output: OutputKind,
    #[structopt(color_choice())]
    pub color_choice: AnsiColorChoice,
    #[structopt(problem())]
    pub problem: String,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct Submit {
    #[structopt(short, long, open())]
    pub open: bool,
    #[structopt(long, conflicts_with("no_judge"), force_compile())]
    pub force_compile: bool,
    #[structopt(
        long,
        conflicts_with("no_judge"),
        help("Transpile the source code but not compile")
    )]
    pub only_transpile: bool,
    #[structopt(
        long,
        conflicts_with_all(&["force_compile", "only_transpile"]),
        help("Skips testing")
    )]
    pub no_judge: bool,
    #[structopt(long, conflicts_with("mode"), help("Equivalents to `--mode debug`"))]
    pub debug: bool,
    #[structopt(
        long,
        help("Submits even if the contest is active and you have already solved the problem")
    )]
    pub no_check_duplication: bool,
    #[structopt(long, verbose())]
    pub verbose: bool,
    #[structopt(long, json())]
    pub json: bool,
    #[structopt(short = "C", long, colorize())]
    pub colorize: bool,
    #[structopt(service(&ServiceKind::variants_except_other(), true))]
    pub service: Option<ServiceKind>,
    #[structopt(contest(true))]
    pub contest: Option<String>,
    #[structopt(language())]
    pub language: Option<String>,
    #[structopt(mode("release"))]
    pub mode: config::Mode,
    #[structopt(parse(try_from_str = parse_non_zero_usize), jobs())]
    pub jobs: Option<NonZeroUsize>,
    #[structopt(output("pretty", &["pretty", "pretty-verbose", "json"]))]
    pub output: OutputKind,
    #[structopt(color_choice())]
    pub color_choice: AnsiColorChoice,
    #[structopt(problem())]
    pub problem: String,
}

trait ArgExt {
    fn verbose(self) -> Self;
    fn json(self) -> Self;
    fn colorize(self) -> Self;
    fn force_compile(self) -> Self;
    fn no_save(self) -> Self;
    fn open(self) -> Self;
    fn language(self) -> Self;
    fn problems(self) -> Self;
    fn mode(self, default: &'static str) -> Self;
    fn jobs(self) -> Self;
    fn output(self, default: &'static str, possible: &'static [&str]) -> Self;
    fn color_choice(self) -> Self;
    fn problem(self) -> Self;
    fn nth(self) -> Self;
    fn service(self, values: &[&'static str], option: bool) -> Self;
    fn contest(self, option: bool) -> Self;
}

impl ArgExt for Arg<'static, 'static> {
    fn verbose(self) -> Self {
        self.help("Equivalents to `--output pretty-verbose`")
            .conflicts_with_all(&["json", "output"])
    }

    fn json(self) -> Self {
        self.help("Equivalents to `--output json`")
            .conflicts_with_all(&["verbose", "output"])
    }

    fn colorize(self) -> Self {
        self.help("Equivalents to `--color always`")
            .conflicts_with("color")
    }

    fn force_compile(self) -> Self {
        self.help("Force to transpile and to compile")
    }

    fn no_save(self) -> Self {
        self.help("Does not save files")
    }

    fn open(self) -> Self {
        self.help("Opens the pages in your default browser")
    }

    fn language(self) -> Self {
        self.short("l")
            .long("language")
            .help("Language name")
            .value_name("STRING")
    }

    fn problems(self) -> Self {
        self.short("p")
            .long("problems")
            .help("Problem names")
            .value_name("STRING")
    }

    fn mode(self, default: &'static str) -> Self {
        self.short("m")
            .long("mode")
            .help("Mode")
            .required(false)
            .possible_values(&config::Mode::variants())
            .value_name("MODE")
            .default_value(default)
    }

    fn jobs(self) -> Self {
        self.short("j")
            .long("jobs")
            .help("Number of jobs")
            .value_name("NUMBER")
    }

    fn output(self, default: &'static str, possible: &'static [&str]) -> Self {
        self.long("output")
            .help("Output")
            .value_name("OUTPUT")
            .required(false)
            .default_value(default)
            .possible_values(possible)
    }

    fn color_choice(self) -> Self {
        self.long("color")
            .help("Coloring")
            .required(false)
            .possible_values(&AnsiColorChoice::variants())
            .value_name("WHEN")
            .default_value("auto")
    }

    fn problem(self) -> Self {
        self.help("Problem name")
    }

    fn nth(self) -> Self {
        self.help("0-based index")
    }

    fn service(mut self, values: &[&'static str], option: bool) -> Self {
        self = self.help("Service name").possible_values(values);
        if option {
            self = self
                .short("s")
                .long("service")
                .help("Service name")
                .value_name("SERVICE")
        }
        self
    }

    fn contest(mut self, option: bool) -> Self {
        self = self.help("Contest name");
        if option {
            self = self.short("c").long("contest").value_name("STRING")
        }
        self
    }
}

fn parse_non_zero_usize(s: &str) -> std::result::Result<NonZeroUsize, String> {
    let n = s.parse::<usize>().map_err(|e| e.to_string())?;
    NonZeroUsize::new(n).ok_or_else(|| "must be non-zero".to_owned())
}

#[derive(Debug)]
pub struct Context<
    I: Input,
    O: AttemptEnableColor + ModifyTermProps,
    E: AttemptEnableColor + ModifyTermProps,
> {
    pub cwd: AbsPathBuf,
    pub login_retries: Option<u32>,
    pub stdin: I,
    pub stdout: O,
    pub stderr: E,
}

impl<
        I: Input,
        O: AttemptEnableColor + ModifyTermProps,
        E: AttemptEnableColor + ModifyTermProps,
    > Context<I, O, E>
{
    fn service_context(
        &mut self,
        config: &Config,
    ) -> crate::Result<service::Context<&mut I, &mut E>> {
        let cookies_path = config.session_cookies().expand(None)?;
        let api_token_path = config.session_api_tokens().expand(None)?;

        service::ContextBuilder {
            stdin: &mut self.stdin,
            stderr: &mut self.stderr,
            base_url: config.service().base_url(),
            cookies_path: Some(&cookies_path),
            api_token_path: Some(&api_token_path),
            timeout: config.session_timeout(),
            http_silent: config.session_silent(),
            robots: config.session_robots(),
            retries_on_get: config.session_retries_on_get(),
            login_retries: self.login_retries,
        }
        .build()
        .map_err(Into::into)
    }

    fn attempt_enable_color(&mut self, choice: AnsiColorChoice) {
        self.stdout.attempt_enable_color(choice);
        self.stderr.attempt_enable_color(choice);
    }

    fn apply_console_conf(&mut self, conf: &config::Console) {
        let char_width: fn(char) -> Option<usize> = if conf.cjk {
            UnicodeWidthChar::width_cjk
        } else {
            UnicodeWidthChar::width
        };
        let str_width: fn(&str) -> usize = if conf.cjk {
            UnicodeWidthStr::width_cjk
        } else {
            UnicodeWidthStr::width
        };

        self.stdout.modify_term_props(|props| {
            props.char_width = char_width;
            props.str_width = str_width
        });
        self.stderr.modify_term_props(|props| {
            props.char_width = char_width;
            props.str_width = str_width
        });
    }
}

pub(crate) trait HasContext {
    type Stdin: Input;
    type Stdout: AttemptEnableColor + ModifyTermProps;
    type Stderr: AttemptEnableColor + ModifyTermProps;

    fn context(&self) -> &Context<Self::Stdin, Self::Stdout, Self::Stderr>;

    fn cwd(&self) -> &AbsPath {
        &self.context().cwd
    }
}

impl<C: HasContext> HasContext for &'_ C {
    type Stdin = C::Stdin;
    type Stdout = C::Stdout;
    type Stderr = C::Stderr;

    fn context(&self) -> &Context<C::Stdin, C::Stdout, C::Stderr> {
        (**self).context()
    }
}

impl<C: HasContext> HasContext for &'_ mut C {
    type Stdin = C::Stdin;
    type Stdout = C::Stdout;
    type Stderr = C::Stderr;

    fn context(&self) -> &Context<C::Stdin, C::Stdout, C::Stderr> {
        (**self).context()
    }
}

impl<
        I: Input,
        O: AttemptEnableColor + ModifyTermProps,
        E: AttemptEnableColor + ModifyTermProps,
    > HasContext for Context<I, O, E>
{
    type Stdin = I;
    type Stdout = O;
    type Stderr = E;

    fn context(&self) -> &Self {
        self
    }
}

pub(crate) trait HasContextMut: HasContext {
    fn context_mut(&mut self) -> &mut Context<Self::Stdin, Self::Stdout, Self::Stderr>;

    fn stdin(&mut self) -> &mut Self::Stdin {
        &mut self.context_mut().stdin
    }

    fn stdout(&mut self) -> &mut Self::Stdout {
        &mut self.context_mut().stdout
    }

    fn stderr(&mut self) -> &mut Self::Stderr {
        &mut self.context_mut().stderr
    }

    fn stdio(&mut self) -> (&mut Self::Stdin, &mut Self::Stdout, &mut Self::Stderr) {
        let ctx = self.context_mut();
        (&mut ctx.stdin, &mut ctx.stdout, &mut ctx.stderr)
    }
}

impl<C: HasContextMut> HasContextMut for &'_ mut C {
    fn context_mut(&mut self) -> &mut Context<C::Stdin, C::Stdout, C::Stderr> {
        (**self).context_mut()
    }
}

impl<
        I: Input,
        O: AttemptEnableColor + ModifyTermProps,
        E: AttemptEnableColor + ModifyTermProps,
    > HasContextMut for Context<I, O, E>
{
    fn context_mut(&mut self) -> &mut Self {
        self
    }
}

pub fn run<
    I: Input,
    O: AttemptEnableColor + ModifyTermProps,
    E: AttemptEnableColor + ModifyTermProps,
>(
    opt: Opt,
    mut ctx: &mut Context<I, O, E>,
) -> crate::Result<i32> {
    let wd = ctx.cwd.clone();

    match &opt {
        Opt::Init(cli_args) => {
            let Init {
                colorize,
                color_choice,
                directory,
            } = cli_args;
            ctx.attempt_enable_color(color_choice.with(*colorize));
            config::init(&directory, ctx)?;
            Ok(0)
        }
        Opt::Switch(cli_args) => {
            let Switch {
                json,
                colorize,
                service,
                contest,
                language,
                output,
                color_choice,
            } = cli_args;
            ctx.attempt_enable_color(color_choice.with(*colorize));
            let (config, outcome) =
                config::switch(*service, contest.as_deref(), language.as_deref(), &mut ctx)?;
            ctx.apply_console_conf(config.console());
            finish(
                outcome,
                cli_args,
                &config,
                SubCommandKind::Switch,
                output.with(false, *json),
                &mut ctx.stdout,
                &mut ctx.stderr,
            )
        }
        Opt::Login(cli_args) => {
            let Login {
                json,
                colorize,
                output,
                color_choice,
                service,
            } = cli_args;
            ctx.attempt_enable_color(color_choice.with(*colorize));
            let config = Config::load(Some(*service), None, None, &ctx.cwd)?;
            let dropbox_path = config
                .session_dropbox_auth()
                .map(|p| p.expand(None))
                .transpose()?;
            ctx.apply_console_conf(config.console());
            let outcome = service::login(
                *service,
                dropbox_path.as_deref(),
                ctx.service_context(&config)?,
            )?;
            finish(
                outcome,
                cli_args,
                &config,
                SubCommandKind::Login,
                output.with(false, *json),
                &mut ctx.stdout,
                &mut ctx.stderr,
            )
        }
        Opt::Participate(cli_args) => {
            let Participate {
                json,
                colorize,
                output,
                color_choice,
                service,
                contest,
            } = cli_args;
            ctx.attempt_enable_color(color_choice.with(*colorize));
            let config = Config::load(Some(*service), Some(contest), None, &wd)?;
            ctx.apply_console_conf(config.console());
            let outcome = service::participate(*service, contest, ctx.service_context(&config)?)?;
            finish(
                outcome,
                cli_args,
                &config,
                SubCommandKind::Participate,
                output.with(false, *json),
                &mut ctx.stdout,
                &mut ctx.stderr,
            )
        }
        Opt::Download(cli_args) | Opt::Retrieve(Retrieve::Testcases(cli_args)) => {
            let RetrieveTestcases {
                full,
                no_save,
                open,
                verbose,
                json,
                colorize,
                service,
                contest,
                problems,
                output,
                color_choice,
            } = cli_args;
            let contest = contest.as_deref();
            ctx.attempt_enable_color(color_choice.with(*colorize));
            let config = Config::load(*service, contest, None, &ctx.cwd)?;
            ctx.apply_console_conf(config.console());
            let outcome = service::retrieve_testcases(
                config.service(),
                RetrieveTestCasesProps {
                    contest: config.contest().to_owned(),
                    problems: NonEmptyIndexSet::try_new(problems.iter().cloned().collect()),
                    destinations: config.destinations(None),
                    open_in_browser: *open,
                    attempt_full: *full,
                    save_files: !no_save,
                    dropbox_path: config
                        .session_dropbox_auth()
                        .map(|p| p.expand(None))
                        .transpose()?,
                },
                ctx.service_context(&config)?,
            )?;
            finish(
                outcome,
                cli_args,
                &config,
                SubCommandKind::RetrieveTestcases,
                output.with(*verbose, *json),
                &mut ctx.stdout,
                &mut ctx.stderr,
            )
        }
        Opt::Retrieve(Retrieve::Languages(cli_args)) => {
            let RetrieveLanguages {
                json,
                colorize,
                service,
                contest,
                output,
                color_choice,
                problem,
            } = cli_args;
            let contest = contest.as_deref();
            let problem = problem.clone();
            ctx.attempt_enable_color(color_choice.with(*colorize));
            let config = Config::load(*service, contest, None, &wd)?;
            ctx.apply_console_conf(config.console());
            let contest = config.contest().to_owned();
            let outcome = service::retrieve_langs(
                config.service(),
                RetrieveLangsProps { contest, problem },
                ctx.service_context(&config)?,
            )?;
            finish(
                outcome,
                cli_args,
                &config,
                SubCommandKind::RetrieveLanguages,
                output.with(false, *json),
                &mut ctx.stdout,
                &mut ctx.stderr,
            )
        }
        Opt::Retrieve(Retrieve::Submissions(cli_args)) => {
            let RetrieveSubmissions {
                fetch_all,
                no_save,
                verbose,
                json,
                colorize,
                service,
                contest,
                mode,
                problems,
                output,
                color_choice,
            } = cli_args;
            let contest = contest.as_deref();
            let problems = problems.clone();
            ctx.attempt_enable_color(color_choice.with(*colorize));
            let config = Config::load(*service, contest, None, &wd)?;
            ctx.apply_console_conf(config.console());
            let outcome = service::retrieve_submissions(
                config.service(),
                RetrieveSubmissionsProps::new(&config, *mode, problems, *fetch_all, !no_save)?,
                ctx.service_context(&config)?,
            )?;
            finish(
                outcome,
                cli_args,
                &config,
                SubCommandKind::RetrieveSubmissions,
                output.with(*verbose, *json),
                &mut ctx.stdout,
                &mut ctx.stderr,
            )
        }
        Opt::Judge(cli_args) => {
            let Judge {
                force_compile,
                release,
                verbose,
                json,
                colorize,
                service,
                contest,
                language,
                mode,
                jobs,
                output,
                color_choice,
                problem,
            } = cli_args;
            let (contest, language) = (contest.as_deref(), language.as_deref());
            let mode = if *release {
                config::Mode::Release
            } else {
                *mode
            };
            ctx.attempt_enable_color(color_choice.with(*colorize));
            let config = Config::load(*service, contest, language, &wd)?;
            ctx.apply_console_conf(config.console());
            let outcome = judging::judge(&config, mode, &problem, *force_compile, *jobs, &mut ctx)?;
            finish(
                outcome,
                cli_args,
                &config,
                SubCommandKind::Judge,
                output.with(*verbose, *json),
                &mut ctx.stdout,
                &mut ctx.stderr,
            )
        }
        Opt::Submit(cli_args) => {
            let Submit {
                open,
                force_compile,
                only_transpile,
                no_judge,
                debug,
                no_check_duplication,
                verbose,
                json,
                colorize,
                language,
                service,
                contest,
                mode,
                jobs,
                output,
                color_choice,
                problem,
            } = cli_args;
            let contest = contest.as_deref();
            let language = language.as_deref();
            let mode = if *debug { config::Mode::Debug } else { *mode };
            ctx.attempt_enable_color(color_choice.with(*colorize));
            let config = Config::load(*service, contest, language, &wd)?;
            ctx.apply_console_conf(config.console());
            if *only_transpile {
                if judging::only_transpile(
                    &ctx.stdout,
                    &mut ctx.stderr,
                    &config,
                    mode,
                    &problem,
                    *force_compile,
                )? {
                    writeln!(ctx.stderr)?;
                }
            } else if !no_judge {
                let outcome =
                    judging::judge(&config, mode, &problem, *force_compile, *jobs, &mut ctx)?;
                if !outcome.is_success() {
                    return finish(
                        outcome,
                        cli_args,
                        &config,
                        SubCommandKind::Submit,
                        output.with(*verbose, *json),
                        &mut ctx.stdout,
                        &mut ctx.stderr,
                    );
                }
                writeln!(ctx.stderr)?;
            }
            let outcome = service::submit(
                config.service(),
                SubmitProps::try_new(&config, mode, problem.clone(), *open, *no_check_duplication)?,
                ctx.service_context(&config)?,
            )?;
            finish(
                outcome,
                cli_args,
                &config,
                SubCommandKind::Submit,
                output.with(*verbose, *json),
                &mut ctx.stdout,
                &mut ctx.stderr,
            )
        }
    }
}

fn finish(
    outcome: impl Outcome,
    command_line_arguments: impl Serialize,
    config: &Config,
    subcommand: SubCommandKind,
    output_kind: OutputKind,
    mut stdout: impl WriteColor + HasTermProps,
    mut stderr: impl WriteColor + HasTermProps,
) -> crate::Result<i32> {
    #[derive(Serialize)]
    struct WithCliArgsAndConfig<'a, A: Serialize, T: Serialize> {
        command_line_arguments: A,
        config: &'a config::Inner,
        target: IndexMap<&'static str, String>,
        base_directory: &'a AbsPath,
        #[serde(flatten)]
        outcome: T,
    }

    let outcome_json = serde_json::to_string(&WithCliArgsAndConfig {
        command_line_arguments,
        config: config.inner(),
        target: config.target_with_case_converted_names(),
        base_directory: config.base_dir(),
        outcome: &outcome,
    })?;

    match output_kind {
        OutputKind::None => {}
        OutputKind::Pretty => {
            writeln!(stderr)?;
            stderr.flush()?;
            outcome.print_pretty(false, &mut stdout)?;
        }
        OutputKind::PrettyVerbose => {
            writeln!(stderr)?;
            stderr.flush()?;
            outcome.print_pretty(true, &mut stdout)?;
        }
        OutputKind::Json => {
            writeln!(stdout, "{}", outcome_json)?;
            stdout.flush()?;
        }
    }

    let hooks = config.hooks(subcommand).expand()?;
    hooks.run(&outcome_json, &mut stdout, stderr)?;

    Ok(if outcome.is_success() { 0 } else { 1 })
}

#[derive(Debug, Clone, Copy, PartialEq, ArgEnum, Serialize)]
#[arg_enum(rename_all = "kebab-case")]
#[serde(rename_all = "kebab-case")]
pub enum OutputKind {
    None,
    Pretty,
    PrettyVerbose,
    Json,
}

impl OutputKind {
    fn with(self, verbose: bool, json: bool) -> Self {
        match (self, verbose, json) {
            (_, true, false) => OutputKind::PrettyVerbose,
            (_, false, true) => OutputKind::Json,
            (k, _, _) => k,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{ArgExt as _, OutputKind};

    use derive_new::new;
    use pretty_assertions::assert_eq;
    use structopt::{clap, StructOpt};

    #[test]
    fn verbose_json_and_output_conflict_with_each_other() {
        #[derive(Debug, StructOpt, new)]
        struct Opt {
            #[structopt(long, verbose())]
            verbose: bool,
            #[structopt(long, json())]
            json: bool,
            #[structopt(output("none", &["none", "pretty", "pretty-verbose", "json"]))]
            output: OutputKind,
        }

        macro_rules! test {
            () => {};
            ($args:expr => Ok($expected:ident)) => {{
                let result = Opt::from_iter_safe($args)
                    .map(|o| o.output.with(o.verbose, o.json))
                    .map_err(|e| e.kind);
                assert_eq!(result, Ok(OutputKind::$expected));
            }};
            ($args:expr => Ok($expected:ident), $($rest:tt)*) => {
                test!($args => Ok($expected));
                test!($($rest)*);
            };
            ($args:expr => Err($kind:ident)) => {{
                let result = Opt::from_iter_safe($args)
                    .map(|o| o.output.with(o.verbose, o.json))
                    .map_err(|e| e.kind);
                assert_eq!(result, Err(clap::ErrorKind::$kind));
            }};
            ($args:expr => Err($expected:ident), $($rest:tt)*) => {
                test!($args => Err($expected));
                test!($($rest)*);
            };
        }

        test!(
            &[""]                                            => Ok(None),
            &["", "--verbose"]                               => Ok(PrettyVerbose),
            &["", "--json"]                                  => Ok(Json),
            &["", "--output", "none"]                        => Ok(None),
            &["", "--output", "pretty"]                      => Ok(Pretty),
            &["", "--output", "pretty-verbose"]              => Ok(PrettyVerbose),
            &["", "--output", "json"]                        => Ok(Json),
            &["", "--verbose", "--json"]                     => Err(ArgumentConflict),
            &["", "--verbose", "--output", "none"]           => Err(ArgumentConflict),
            &["", "--json", "--output", "none"]              => Err(ArgumentConflict),
            &["", "--verbose", "--json", "--output", "none"] => Err(ArgumentConflict),
        );
    }
}
