use crate::config::{self, Config, SubCommandKind};
use crate::errors::ExpandTemplateResult;
use crate::judging::{self, JudgeParams};
use crate::outcome::Outcome;
use crate::path::{AbsPath, AbsPathBuf};
use crate::service::{
    self, RetrieveLangsProps, RetrieveSubmissionsProps, RetrieveTestCasesProps, ServiceKind,
    SessionProps, SubmitProps,
};
use crate::terminal::{AnsiColorChoice, AttemptEnableColor, HasTermProps, Input, ModifyTermProps};
use crate::util::collections::NonEmptyIndexSet;

use serde::Serialize;
use structopt::clap::Arg;
use structopt::StructOpt;
use strum_macros::EnumString;
use termcolor::WriteColor;

use std::num::NonZeroUsize;
use std::path::PathBuf;

#[derive(Debug, StructOpt)]
#[structopt(usage = "snowchains <i|init> [FLAGS] [OPTIONS] [directory]\
                     \n    snowchains <w|switch|c|checkout> [FLAGS] [OPTIONS]\
                     \n    snowchains <l|login> [FLAGS] [OPTIONS] <service>\
                     \n    snowchains <p|participate> [FLAGS] [OPTIONS] <service> <contest>\
                     \n    snowchains <d|download> [FLAGS] [OPTIONS]\
                     \n    snowchains <r|retrieve> <t|testcases> [FLAGS] [OPTIONS]\
                     \n    snowchains <r|retrieve> <l|languages> [FLAGS] [OPTIONS] [problem]\
                     \n    snowchains <r|retrieve> <s|submissions> [FLAGS] [OPTIONS]\
                     \n    snowchains <j|judge|t|test> [FLAGS] [OPTIONS] <problem>\
                     \n    snowchains <s|submit> [FLAGS] [OPTIONS] <problem>")]
pub enum Opt {
    #[structopt(
        about = "Creates a config file (\"snowchains.toml\")",
        name = "init",
        usage = "snowchains <i|init> [FLAGS] [OPTIONS] [directory]",
        raw(visible_alias = "\"i\"", display_order = "1")
    )]
    Init(Init),
    #[structopt(
        about = "Modifies values in a config file",
        name = "switch",
        usage = "snowchains <w|switch|c|checkout> [FLAGS] [OPTIONS]",
        raw(visible_aliases = r#"&["w", "checkout", "c"]"#, display_order = "2")
    )]
    Switch(Switch),
    #[structopt(
        about = "Logges in to a service",
        name = "login",
        usage = "snowchains <l|login> [FLAGS] [OPTIONS] <service>",
        raw(visible_alias = "\"l\"", display_order = "3")
    )]
    Login(Login),
    #[structopt(
        about = "Participates in a contest",
        name = "participate",
        usage = "snowchains <p|participate> [FLAGS] [OPTIONS] <service> <contest>",
        raw(visible_alias = "\"p\"", display_order = "4")
    )]
    Participate(Participate),
    #[structopt(
        about = "An alias for `retrieve testcases`",
        name = "download",
        usage = "snowchains <d|download> [FLAGS] [OPTIONS]",
        raw(visible_alias = "\"d\"", display_order = "5")
    )]
    Download(RetrieveTestcases),
    #[structopt(
        about = "Retrieves data",
        name = "retrieve",
        usage = "snowchains <r|retrieve> <t|testcases> [FLAGS] [OPTIONS]\
                 \n    snowchains <r|retrieve> <l|languages> [FLAGS] [OPTIONS] [problem]\
                 \n    snowchains <r|retrieve> <s|submissions> [FLAGS] [OPTIONS]",
        raw(visible_alias = "\"r\"", display_order = "6")
    )]
    Retrieve(Retrieve),
    #[structopt(
        about = "Tests a binary or script",
        name = "judge",
        usage = "snowchains <j|judge|t|test> [FLAGS] [OPTIONS] <problem>",
        raw(visible_aliases = r#"&["j", "test", "t"]"#, display_order = "7")
    )]
    Judge(Judge),
    #[structopt(
        about = "Submits a source file",
        name = "submit",
        usage = "snowchains <s|submit> [FLAGS] [OPTIONS] <problem>",
        raw(visible_alias = "\"s\"", display_order = "8")
    )]
    Submit(Submit),
}

#[derive(Debug, Serialize, StructOpt)]
pub struct Init {
    #[structopt(raw(colorize = "1"))]
    colorize: bool,
    #[structopt(raw(color_choice = "1"))]
    color_choice: AnsiColorChoice,
    #[structopt(
        help = "Directory to create a \"snowchains.toml\"",
        default_value = ".",
        parse(from_os_str)
    )]
    directory: PathBuf,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct Switch {
    #[structopt(raw(json = "1"))]
    json: bool,
    #[structopt(raw(colorize = "2"))]
    colorize: bool,
    #[structopt(raw(service = r#"SERVICE_VALUES, Kind::Option(1)"#))]
    service: Option<ServiceKind>,
    #[structopt(raw(contest = "Kind::Option(2)"))]
    contest: Option<String>,
    #[structopt(raw(language = "3"))]
    language: Option<String>,
    #[structopt(raw(output = r#""pretty", &["pretty", "json"], 4"#))]
    output: OutputKind,
    #[structopt(raw(color_choice = "5"))]
    color_choice: AnsiColorChoice,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct Login {
    #[structopt(raw(json = "1"))]
    pub json: bool,
    #[structopt(raw(colorize = "2"))]
    pub colorize: bool,
    #[structopt(raw(output = r#""pretty", &["pretty", "json"], 1"#))]
    pub output: OutputKind,
    #[structopt(raw(color_choice = "2"))]
    pub color_choice: AnsiColorChoice,
    #[structopt(raw(service = r#"EXCEPT_OTHER, Kind::Arg"#))]
    pub service: ServiceKind,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct Participate {
    #[structopt(raw(json = "1"))]
    json: bool,
    #[structopt(raw(colorize = "2"))]
    colorize: bool,
    #[structopt(raw(output = r#""pretty", &["pretty", "json"], 1"#))]
    output: OutputKind,
    #[structopt(raw(color_choice = "2"))]
    color_choice: AnsiColorChoice,
    #[structopt(raw(service = r#"&["atcoder"], Kind::Arg"#))]
    service: ServiceKind,
    #[structopt(raw(contest = "Kind::Arg"))]
    contest: String,
}

#[derive(Debug, Serialize, StructOpt)]
pub enum Retrieve {
    #[structopt(
        about = "Retrieves test cases",
        name = "testcases",
        usage = "snowchains <r|retrieve> <t|testcases> [FLAGS] [OPTIONS]",
        raw(visible_alias = "\"t\"", display_order = "1")
    )]
    Testcases(RetrieveTestcases),
    #[structopt(
        about = "Retrieves available languages",
        name = "languages",
        usage = "snowchains <r|retrieve> <l|languages> [FLAGS] [OPTIONS] [problem]",
        raw(visible_alias = "\"l\"", display_order = "2")
    )]
    Languages(RetrieveLanguages),
    #[structopt(
        about = "Retrieves source files you have submitted",
        name = "submissions",
        usage = "snowchains <r|retrieve> <s|submissions> [FLAGS] [OPTIONS]",
        raw(visible_alias = "\"s\"", display_order = "3")
    )]
    Submissions(RetrieveSubmissions),
}

#[derive(Debug, Serialize, StructOpt)]
pub struct RetrieveTestcases {
    #[structopt(
        long = "full",
        help = "Downloads full test cases",
        raw(display_order = "1")
    )]
    pub full: bool,
    #[structopt(raw(no_save = "2"))]
    pub no_save: bool,
    #[structopt(raw(open = "3"))]
    pub open: bool,
    #[structopt(raw(verbose = "4"))]
    pub verbose: bool,
    #[structopt(raw(json = "5"))]
    pub json: bool,
    #[structopt(raw(colorize = "6"))]
    pub colorize: bool,
    #[structopt(raw(service = r#"EXCEPT_OTHER, Kind::Option(1)"#))]
    pub service: Option<ServiceKind>,
    #[structopt(raw(contest = "Kind::Option(2)"))]
    pub contest: Option<String>,
    #[structopt(raw(problems = "3"))]
    pub problems: Vec<String>,
    #[structopt(raw(output = r#""pretty", &["pretty", "json"], 4"#))]
    pub output: OutputKind,
    #[structopt(raw(color_choice = "5"))]
    pub color_choice: AnsiColorChoice,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct RetrieveLanguages {
    #[structopt(raw(json = "1"))]
    pub json: bool,
    #[structopt(raw(colorize = "2"))]
    pub colorize: bool,
    #[structopt(raw(service = "EXCEPT_OTHER, Kind::Option(1)"))]
    pub service: Option<ServiceKind>,
    #[structopt(raw(contest = "Kind::Option(2)"))]
    pub contest: Option<String>,
    #[structopt(raw(output = r#""pretty", &["pretty", "json"], 3"#))]
    pub output: OutputKind,
    #[structopt(raw(color_choice = "4"))]
    pub color_choice: AnsiColorChoice,
    #[structopt(raw(problem = ""))]
    pub problem: Option<String>,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct RetrieveSubmissions {
    #[structopt(
        long = "fetch-all",
        help = "Fetches all of the code",
        raw(display_order = "1")
    )]
    pub fetch_all: bool,
    #[structopt(raw(no_save = "2"))]
    pub no_save: bool,
    #[structopt(raw(verbose = "3"))]
    pub verbose: bool,
    #[structopt(raw(json = "4"))]
    pub json: bool,
    #[structopt(raw(colorize = "5"))]
    pub colorize: bool,
    #[structopt(raw(service = "&[\"atcoder\"], Kind::Option(1)"))]
    pub service: Option<ServiceKind>,
    #[structopt(raw(contest = "Kind::Option(2)"))]
    pub contest: Option<String>,
    #[structopt(raw(mode = "3, \"debug\""))]
    pub mode: config::Mode,
    #[structopt(raw(problems = "4"))]
    pub problems: Vec<String>,
    #[structopt(raw(output = r#""none", &["none", "pretty-verbose", "json"], 5"#))]
    pub output: OutputKind,
    #[structopt(raw(color_choice = "6"))]
    pub color_choice: AnsiColorChoice,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct Judge {
    #[structopt(raw(force_compile = "1"))]
    pub force_compile: bool,
    #[structopt(
        long = "release",
        raw(
            help = "\"Equivalents to `--mode release`\"",
            conflicts_with = "\"mode\"",
            display_order = "2",
        )
    )]
    pub release: bool,
    #[structopt(raw(verbose = "2"))]
    pub verbose: bool,
    #[structopt(raw(json = "3"))]
    pub json: bool,
    #[structopt(raw(colorize = "4"))]
    pub colorize: bool,
    #[structopt(raw(service = "SERVICE_VALUES, Kind::Option(1)"))]
    pub service: Option<ServiceKind>,
    #[structopt(raw(contest = "Kind::Option(2)"))]
    pub contest: Option<String>,
    #[structopt(raw(language = "3"))]
    pub language: Option<String>,
    #[structopt(raw(mode = "4, \"debug\""))]
    pub mode: config::Mode,
    #[structopt(parse(try_from_str = "parse_non_zero_usize"), raw(jobs = "5"))]
    pub jobs: Option<NonZeroUsize>,
    #[structopt(raw(output = r#""pretty", &["pretty", "pretty-verbose", "json"], 6"#))]
    pub output: OutputKind,
    #[structopt(raw(color_choice = "7"))]
    pub color_choice: AnsiColorChoice,
    #[structopt(raw(problem = ""))]
    pub problem: String,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct Submit {
    #[structopt(raw(open = "1"))]
    pub open: bool,
    #[structopt(raw(conflicts_with = "\"no_judge\"", force_compile = "2"))]
    pub force_compile: bool,
    #[structopt(
        long = "only-transpile",
        help = "Transpile the source code but not compile",
        raw(conflicts_with = "\"no_judge\"", display_order = "3")
    )]
    pub only_transpile: bool,
    #[structopt(
        long = "no-judge",
        help = "Skips testing",
        raw(
            conflicts_with_all = r#"&["force_compile", "only_transpile"]"#,
            display_order = "4"
        )
    )]
    pub no_judge: bool,
    #[structopt(
        long = "debug",
        raw(
            help = "\"Equivalents to `--mode debug`\"",
            conflicts_with = "\"mode\"",
            display_order = "5",
        )
    )]
    pub debug: bool,
    #[structopt(
        long = "no-check-duplication",
        help = "Submits even if the contest is active and you have already solved the problem",
        raw(display_order = "6")
    )]
    pub no_check_duplication: bool,
    #[structopt(raw(verbose = "7"))]
    pub verbose: bool,
    #[structopt(raw(json = "8"))]
    pub json: bool,
    #[structopt(raw(colorize = "9"))]
    pub colorize: bool,
    #[structopt(raw(service = "EXCEPT_OTHER, Kind::Option(1)"))]
    pub service: Option<ServiceKind>,
    #[structopt(raw(contest = "Kind::Option(2)"))]
    pub contest: Option<String>,
    #[structopt(raw(language = "3"))]
    pub language: Option<String>,
    #[structopt(raw(mode = "4, \"release\""))]
    pub mode: config::Mode,
    #[structopt(parse(try_from_str = "parse_non_zero_usize"), raw(jobs = "5"))]
    pub jobs: Option<NonZeroUsize>,
    #[structopt(raw(output = r#""pretty", &["pretty", "pretty-verbose", "json"], 6"#))]
    pub output: OutputKind,
    #[structopt(raw(color_choice = "7"))]
    pub color_choice: AnsiColorChoice,
    #[structopt(raw(problem = ""))]
    pub problem: String,
}

static SERVICE_VALUES: &[&str] = &["atcoder", "codeforces", "yukicoder", "other"];

static EXCEPT_OTHER: &[&str] = &["atcoder", "codeforces", "yukicoder"];

#[derive(Debug)]
enum Kind {
    Option(usize),
    Arg,
}

trait ArgExt {
    fn verbose(self, order: usize) -> Self;
    fn json(self, order: usize) -> Self;
    fn colorize(self, order: usize) -> Self;
    fn force_compile(self, order: usize) -> Self;
    fn no_save(self, order: usize) -> Self;
    fn open(self, order: usize) -> Self;
    fn language(self, order: usize) -> Self;
    fn problems(self, order: usize) -> Self;
    fn mode(self, order: usize, default: &'static str) -> Self;
    fn jobs(self, order: usize) -> Self;
    fn output(self, default: &'static str, possible: &'static [&str], order: usize) -> Self;
    fn color_choice(self, order: usize) -> Self;
    fn problem(self) -> Self;
    fn nth(self) -> Self;
    fn extension(self, values: &'static [&'static str]) -> Self;
    fn service(self, values: &'static [&'static str], kind: Kind) -> Self;
    fn contest(self, kind: Kind) -> Self;
}

impl ArgExt for Arg<'static, 'static> {
    fn verbose(self, order: usize) -> Self {
        self.long("verbose")
            .help("Equivalents to `--output pretty-verbose`")
            .conflicts_with_all(&["json", "output"])
            .display_order(order)
    }

    fn json(self, order: usize) -> Self {
        self.long("json")
            .help("Equivalents to `--output json`")
            .conflicts_with_all(&["verbose", "output"])
            .display_order(order)
    }

    fn colorize(self, order: usize) -> Self {
        self.short("C")
            .long("colorize")
            .help("Equivalents to `--color always`")
            .conflicts_with("color")
            .display_order(order)
    }

    fn force_compile(self, order: usize) -> Self {
        self.long("force-compile")
            .help("Force to transpile and to compile")
            .display_order(order)
    }

    fn no_save(self, order: usize) -> Self {
        self.long("no-save")
            .help("Does not save files")
            .display_order(order)
    }

    fn open(self, order: usize) -> Self {
        self.short("o")
            .long("--open")
            .help("Opens the pages in your default browser")
            .display_order(order)
    }

    fn language(self, order: usize) -> Self {
        self.short("l")
            .long("language")
            .help("Language name")
            .value_name("STRING")
            .display_order(order)
    }

    fn problems(self, order: usize) -> Self {
        self.short("p")
            .long("problems")
            .help("Problem names")
            .value_name("STRING")
            .display_order(order)
    }

    fn mode(self, order: usize, default: &'static str) -> Self {
        self.short("m")
            .long("mode")
            .help("Mode")
            .required(false)
            .possible_values(&["debug", "release"])
            .value_name("MODE")
            .default_value(default)
            .display_order(order)
    }

    fn jobs(self, order: usize) -> Self {
        self.short("j")
            .long("jobs")
            .help("Number of jobs")
            .value_name("NUMBER")
            .display_order(order)
    }

    fn output(self, default: &'static str, possible: &'static [&str], order: usize) -> Self {
        self.long("output")
            .help("Output")
            .value_name("OUTPUT")
            .required(false)
            .default_value(default)
            .possible_values(possible)
            .display_order(order)
    }

    fn color_choice(self, order: usize) -> Self {
        self.long("color")
            .help("Coloring")
            .required(false)
            .possible_values(&["never", "auto", "always"])
            .value_name("WHEN")
            .default_value("auto")
            .display_order(order)
    }

    fn problem(self) -> Self {
        self.help("Problem name")
    }

    fn nth(self) -> Self {
        self.help("0-based index")
    }

    fn extension(self, values: &'static [&'static str]) -> Self {
        self.help("Extension").possible_values(values)
    }

    fn service(mut self, values: &'static [&'static str], kind: Kind) -> Self {
        self = self.help("Service name").possible_values(values);
        if let Kind::Option(order) = kind {
            self = self
                .short("s")
                .long("service")
                .help("Service name")
                .value_name("SERVICE")
                .display_order(order);
        }
        self
    }

    fn contest(mut self, kind: Kind) -> Self {
        self = self.help("Contest name");
        if let Kind::Option(order) = kind {
            self = self
                .short("c")
                .long("contest")
                .value_name("STRING")
                .display_order(order);
        }
        self
    }
}

fn parse_non_zero_usize(s: &str) -> std::result::Result<NonZeroUsize, String> {
    let n = s.parse::<usize>().map_err(|e| e.to_string())?;
    NonZeroUsize::new(n).ok_or_else(|| "must be non-zero".to_owned())
}

#[derive(Debug)]
pub struct App<
    I: Input,
    O: AttemptEnableColor + ModifyTermProps,
    E: AttemptEnableColor + ModifyTermProps,
> {
    pub working_dir: AbsPathBuf,
    pub login_retries: Option<u32>,
    pub stdin: I,
    pub stdout: O,
    pub stderr: E,
}

impl<
        I: Input,
        O: AttemptEnableColor + ModifyTermProps,
        E: AttemptEnableColor + ModifyTermProps,
    > App<I, O, E>
{
    pub fn run(&mut self, opt: Opt) -> crate::Result<i32> {
        let wd = self.working_dir.clone();

        match &opt {
            Opt::Init(cli_args) => {
                let Init {
                    colorize,
                    color_choice,
                    directory,
                } = cli_args;
                self.attempt_enable_color(color_choice.with(*colorize));
                let wd = wd.join_canonicalizing_lossy(&directory);
                config::init(&mut self.stderr, &wd)?;
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
                let contest = contest.as_ref().map(AsRef::as_ref);
                let language = language.as_ref().map(AsRef::as_ref);
                self.attempt_enable_color(color_choice.with(*colorize));
                let (config, outcome) = config::switch(
                    &mut self.stdout,
                    &mut self.stderr,
                    &wd,
                    *service,
                    contest,
                    language,
                )?;
                self.apply_console_conf(config.console());
                finish(
                    outcome,
                    cli_args,
                    &config,
                    SubCommandKind::Switch,
                    output.with(false, *json),
                    &mut self.stdout,
                    &mut self.stderr,
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
                self.attempt_enable_color(color_choice.with(*colorize));
                let config = Config::load(Some(*service), None, None, &wd)?;
                self.apply_console_conf(config.console());
                let outcome = service::login(
                    *service,
                    self.sess_props(&config)?,
                    &mut self.stdin,
                    &mut self.stderr,
                )?;
                finish(
                    outcome,
                    cli_args,
                    &config,
                    SubCommandKind::Login,
                    output.with(false, *json),
                    &mut self.stdout,
                    &mut self.stderr,
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
                self.attempt_enable_color(color_choice.with(*colorize));
                let config = Config::load(Some(*service), Some(contest), None, &wd)?;
                self.apply_console_conf(config.console());
                let outcome = service::participate(
                    *service,
                    self.sess_props(&config)?,
                    contest,
                    &mut self.stdin,
                    &mut self.stderr,
                )?;
                finish(
                    outcome,
                    cli_args,
                    &config,
                    SubCommandKind::Participate,
                    output.with(false, *json),
                    &mut self.stdout,
                    &mut self.stderr,
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
                let contest = contest.as_ref().map(AsRef::as_ref);
                self.attempt_enable_color(color_choice.with(*colorize));
                let config = Config::load(*service, contest, None, &self.working_dir)?;
                self.apply_console_conf(config.console());
                let outcome = service::retrieve_testcases(
                    config.service(),
                    self.sess_props(&config)?,
                    RetrieveTestCasesProps {
                        contest: config.contest().to_owned(),
                        problems: NonEmptyIndexSet::try_new(problems.iter().cloned().collect()),
                        destinations: config.destinations(None),
                        open_in_browser: *open,
                        attempt_full: *full,
                        save_files: !no_save,
                    },
                    &mut self.stdin,
                    &mut self.stderr,
                )?;
                finish(
                    outcome,
                    cli_args,
                    &config,
                    SubCommandKind::RetrieveTestcases,
                    output.with(*verbose, *json),
                    &mut self.stdout,
                    &mut self.stderr,
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
                let contest = contest.as_ref().map(AsRef::as_ref);
                let problem = problem.clone();
                self.attempt_enable_color(color_choice.with(*colorize));
                let config = Config::load(*service, contest, None, &wd)?;
                self.apply_console_conf(config.console());
                let contest = config.contest().to_owned();
                let outcome = service::retrieve_langs(
                    config.service(),
                    self.sess_props(&config)?,
                    RetrieveLangsProps { contest, problem },
                    &mut self.stdin,
                    &mut self.stderr,
                )?;
                finish(
                    outcome,
                    cli_args,
                    &config,
                    SubCommandKind::RetrieveLanguages,
                    output.with(false, *json),
                    &mut self.stdout,
                    &mut self.stderr,
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
                let contest = contest.as_ref().map(AsRef::as_ref);
                let problems = problems.clone();
                self.attempt_enable_color(color_choice.with(*colorize));
                let config = Config::load(*service, contest, None, &wd)?;
                self.apply_console_conf(config.console());
                let outcome = service::retrieve_submissions(
                    config.service(),
                    self.sess_props(&config)?,
                    RetrieveSubmissionsProps::new(&config, *mode, problems, *fetch_all, !no_save)?,
                    &mut self.stdin,
                    &mut self.stderr,
                )?;
                finish(
                    outcome,
                    cli_args,
                    &config,
                    SubCommandKind::RetrieveSubmissions,
                    output.with(*verbose, *json),
                    &mut self.stdout,
                    &mut self.stderr,
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
                let contest = contest.as_ref().map(AsRef::as_ref);
                let language = language.as_ref().map(AsRef::as_ref);
                let mode = if *release {
                    config::Mode::Release
                } else {
                    *mode
                };
                self.attempt_enable_color(color_choice.with(*colorize));
                let config = Config::load(*service, contest, language, &wd)?;
                self.apply_console_conf(config.console());
                let outcome = judging::judge(JudgeParams {
                    stdout: &self.stdout,
                    stderr: &mut self.stderr,
                    config: &config,
                    mode,
                    problem: &problem,
                    force_compile: *force_compile,
                    jobs: *jobs,
                })?;
                finish(
                    outcome,
                    cli_args,
                    &config,
                    SubCommandKind::Judge,
                    output.with(*verbose, *json),
                    &mut self.stdout,
                    &mut self.stderr,
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
                let contest = contest.as_ref().map(AsRef::as_ref);
                let language = language.as_ref().map(AsRef::as_ref);
                let mode = if *debug { config::Mode::Debug } else { *mode };
                self.attempt_enable_color(color_choice.with(*colorize));
                let config = Config::load(*service, contest, language, &wd)?;
                self.apply_console_conf(config.console());
                if *only_transpile {
                    if judging::only_transpile(
                        &self.stdout,
                        &mut self.stderr,
                        &config,
                        mode,
                        &problem,
                        *force_compile,
                    )? {
                        writeln!(self.stderr)?;
                    }
                } else if !no_judge {
                    let outcome = judging::judge(JudgeParams {
                        stdout: &self.stdout,
                        stderr: &mut self.stderr,
                        config: &config,
                        mode,
                        problem: &problem,
                        force_compile: *force_compile,
                        jobs: *jobs,
                    })?;
                    if !outcome.is_success() {
                        return finish(
                            outcome,
                            cli_args,
                            &config,
                            SubCommandKind::Submit,
                            output.with(*verbose, *json),
                            &mut self.stdout,
                            &mut self.stderr,
                        );
                    }
                    writeln!(self.stderr)?;
                }
                let outcome = service::submit(
                    config.service(),
                    self.sess_props(&config)?,
                    SubmitProps::try_new(
                        &config,
                        mode,
                        problem.clone(),
                        *open,
                        *no_check_duplication,
                    )?,
                    &mut self.stdin,
                    &mut self.stderr,
                )?;
                finish(
                    outcome,
                    cli_args,
                    &config,
                    SubCommandKind::Submit,
                    output.with(*verbose, *json),
                    &mut self.stdout,
                    &mut self.stderr,
                )
            }
        }
    }

    fn sess_props(&mut self, config: &Config) -> ExpandTemplateResult<SessionProps> {
        let cookies_path = config.session_cookies().expand(None)?;
        let api_token_path = config.session_api_tokens().expand(None)?;
        let dropbox_path = config
            .session_dropbox_auth()
            .map(|p| p.expand(None))
            .transpose()?;
        Ok(SessionProps {
            domain: config.service().domain(),
            cookies_path,
            api_token_path,
            dropbox_path,
            timeout: config.session_timeout(),
            login_retries: self.login_retries,
            http_silent: config.session_silent(),
            robots: config.session_robots(),
        })
    }

    fn attempt_enable_color(&mut self, choice: AnsiColorChoice) {
        self.stdout.attempt_enable_color(choice);
        self.stderr.attempt_enable_color(choice);
    }

    fn apply_console_conf(&mut self, conf: &config::Console) {
        conf.modify_term_props(&mut self.stdout, &mut self.stderr);
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
        target: &'a config::Target,
        base_directory: &'a AbsPath,
        #[serde(flatten)]
        outcome: T,
    }

    let outcome_json = serde_json::to_string(&WithCliArgsAndConfig {
        command_line_arguments,
        config: config.inner(),
        target: config.target(),
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

    let hooks = config.hooks(subcommand, outcome_json).expand()?;
    hooks.run(&mut stdout, stderr)?;

    Ok(if outcome.is_success() { 0 } else { 1 })
}

#[derive(Debug, Clone, Copy, PartialEq, EnumString, Serialize)]
#[strum(serialize_all = "kebab_case")]
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
    use crate::app::{ArgExt as _, OutputKind};

    use derive_new::new;
    use pretty_assertions::assert_eq;
    use structopt::{clap, StructOpt};

    #[test]
    fn verbose_json_and_output_conflict_with_each_other() {
        #[derive(Debug, StructOpt, new)]
        struct Opt {
            #[structopt(raw(verbose = "1"))]
            verbose: bool,
            #[structopt(raw(json = "2"))]
            json: bool,
            #[structopt(raw(
                output = r#""none", &["none", "pretty", "pretty-verbose", "json"], 1"#
            ))]
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
