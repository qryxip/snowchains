use crate::config::{self, Config, SubCommandKind};
use crate::errors::ExpandTemplateResult;
use crate::judging::{self, JudgeParams};
use crate::path::{AbsPath, AbsPathBuf};
use crate::service::{
    atcoder, codeforces, yukicoder, RetrieveLangsProps, RetrieveSubmissionsProps,
    RetrieveTestCasesProps, ServiceKind, SessionProps, SubmitProps,
};
use crate::terminal::{AnsiColorChoice, Term};
use crate::util::collections::NonEmptyVec;

use serde::Serialize;
use serde_derive::Serialize;
use structopt::clap::Arg;
use structopt::StructOpt;

use std::io::Write as _;
use std::num::NonZeroUsize;
use std::path::PathBuf;

#[derive(Debug, StructOpt)]
#[structopt(usage = "snowchains <i|init> [OPTIONS] [directory]\
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
        usage = "snowchains <i|init> [OPTIONS] [directory]",
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
        about = "Equivalents to `retrieve testcases`",
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
    #[structopt(raw(service = r#"SERVICE_VALUES, Kind::Option(1)"#))]
    service: Option<ServiceKind>,
    #[structopt(raw(contest = "Kind::Option(2)"))]
    contest: Option<String>,
    #[structopt(raw(language = "3"))]
    language: Option<String>,
    #[structopt(raw(color_choice = "4"))]
    color_choice: AnsiColorChoice,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct Login {
    #[structopt(raw(json = "1"))]
    pub json: bool,
    #[structopt(raw(color_choice = "1"))]
    pub color_choice: AnsiColorChoice,
    #[structopt(raw(service = r#"EXCEPT_OTHER, Kind::Arg"#))]
    pub service: ServiceKind,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct Participate {
    #[structopt(raw(json = "1"))]
    json: bool,
    #[structopt(raw(color_choice = "1"))]
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
    #[structopt(raw(open = "1"))]
    pub open: bool,
    #[structopt(raw(json = "2"))]
    pub json: bool,
    #[structopt(
        long = "only-scraped",
        help = "Does not download official test cases",
        raw(display_order = "2")
    )]
    pub only_scraped: bool,
    #[structopt(raw(service = r#"EXCEPT_OTHER, Kind::Option(1)"#))]
    pub service: Option<ServiceKind>,
    #[structopt(raw(contest = "Kind::Option(2)"))]
    pub contest: Option<String>,
    #[structopt(raw(problems = "3"))]
    pub problems: Vec<String>,
    #[structopt(raw(color_choice = "4"))]
    pub color_choice: AnsiColorChoice,
}

#[derive(Debug, Serialize, StructOpt)]
pub struct RetrieveLanguages {
    #[structopt(raw(json = "1"))]
    pub json: bool,
    #[structopt(raw(service = "EXCEPT_OTHER, Kind::Option(1)"))]
    pub service: Option<ServiceKind>,
    #[structopt(raw(contest = "Kind::Option(2)"))]
    pub contest: Option<String>,
    #[structopt(raw(color_choice = "3"))]
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
    #[structopt(raw(json = "2"))]
    pub json: bool,
    #[structopt(raw(service = "&[\"atcoder\"], Kind::Option(1)"))]
    pub service: Option<ServiceKind>,
    #[structopt(raw(contest = "Kind::Option(2)"))]
    pub contest: Option<String>,
    #[structopt(raw(mode = "3, \"debug\""))]
    pub mode: config::Mode,
    #[structopt(raw(problems = "4"))]
    pub problems: Vec<String>,
    #[structopt(raw(color_choice = "5"))]
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
    #[structopt(raw(json = "3"))]
    pub json: bool,
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
    #[structopt(raw(color_choice = "6"))]
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
    #[structopt(raw(json = "7"))]
    pub json: bool,
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
    #[structopt(raw(color_choice = "6"))]
    pub color_choice: AnsiColorChoice,
    #[structopt(raw(problem = ""))]
    pub problem: String,
}

static SERVICE_VALUES: &[&str] = &["atcoder", "codeforces", "yukicoder", "other"];

static EXCEPT_OTHER: &[&str] = &["atcoder", "codeforces", "yukicoder"];

enum Kind {
    Option(usize),
    Arg,
}

trait ArgExt {
    fn json(self, order: usize) -> Self;
    fn force_compile(self, order: usize) -> Self;
    fn open(self, order: usize) -> Self;
    fn language(self, order: usize) -> Self;
    fn problems(self, order: usize) -> Self;
    fn mode(self, order: usize, default: &'static str) -> Self;
    fn jobs(self, order: usize) -> Self;
    fn color_choice(self, order: usize) -> Self;
    fn problem(self) -> Self;
    fn nth(self) -> Self;
    fn extension(self, values: &'static [&'static str]) -> Self;
    fn service(self, values: &'static [&'static str], kind: Kind) -> Self;
    fn contest(self, kind: Kind) -> Self;
}

impl ArgExt for Arg<'static, 'static> {
    fn json(self, order: usize) -> Self {
        self.long("json")
            .help("Prints the result as a JSON")
            .display_order(order)
    }

    fn force_compile(self, order: usize) -> Self {
        self.long("force-compile")
            .help("Force to transpile and to compile")
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

    fn color_choice(self, order: usize) -> Self {
        self.short("C")
            .long("color")
            .help("Use colors")
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

pub struct App<T: Term> {
    pub working_dir: AbsPathBuf,
    pub login_retries: Option<u32>,
    pub term: T,
}

impl<T: Term> App<T> {
    pub fn run(&mut self, opt: Opt) -> crate::Result<()> {
        let wd = self.working_dir.clone();
        match &opt {
            Opt::Init(cli_args) => {
                let Init {
                    color_choice,
                    directory,
                } = cli_args;
                let wd = wd.join_canonicalizing_lossy(&directory);
                self.term.attempt_enable_ansi(*color_choice);
                config::init(self.term.stderr(), &wd)?;
            }
            Opt::Switch(cli_args) => {
                let Switch {
                    json,
                    service,
                    contest,
                    language,
                    color_choice,
                } = cli_args;
                let contest = contest.as_ref().map(AsRef::as_ref);
                let language = language.as_ref().map(AsRef::as_ref);
                self.term.attempt_enable_ansi(*color_choice);
                let (_, stdout, stderr) = self.term.split_mut();
                let (config, outcome) =
                    config::switch(stdout, stderr, &wd, *service, contest, language)?;
                finish(
                    &outcome,
                    cli_args,
                    &config,
                    SubCommandKind::Switch,
                    *json,
                    &mut self.term,
                )?;
            }
            Opt::Login(cli_args) => {
                let Login {
                    json,
                    color_choice,
                    service,
                } = cli_args;
                self.term.attempt_enable_ansi(*color_choice);
                let config = Config::load(Some(*service), None, None, &wd)?;
                self.term.apply_conf(config.console());
                let props = self.sess_props(&config)?;
                let term = &mut self.term;
                let outcome = match service {
                    ServiceKind::Atcoder => atcoder::login(props, term),
                    ServiceKind::Codeforces => codeforces::login(props, term),
                    ServiceKind::Yukicoder => yukicoder::login(props, term),
                    ServiceKind::Other => unreachable!(),
                }?;
                finish(
                    &outcome,
                    cli_args,
                    &config,
                    SubCommandKind::Login,
                    *json,
                    &mut self.term,
                )?;
            }
            Opt::Participate(cli_args) => {
                let Participate {
                    json,
                    color_choice,
                    service,
                    contest,
                } = cli_args;
                self.term.attempt_enable_ansi(*color_choice);
                let config = Config::load(Some(*service), Some(contest), None, &wd)?;
                self.term.apply_conf(config.console());
                let props = self.sess_props(&config)?;
                let term = &mut self.term;
                let outcome = match service {
                    ServiceKind::Atcoder => atcoder::participate(contest, props, term),
                    _ => unreachable!(),
                }?;
                finish(
                    &outcome,
                    cli_args,
                    &config,
                    SubCommandKind::Participate,
                    *json,
                    &mut self.term,
                )?;
            }
            Opt::Download(cli_args) => {
                self.on_retrieve_testcases(cli_args, SubCommandKind::Download)?
            }
            Opt::Retrieve(Retrieve::Testcases(cli_args)) => {
                self.on_retrieve_testcases(cli_args, SubCommandKind::RetrieveTestcases)?
            }
            Opt::Retrieve(Retrieve::Languages(cli_args)) => {
                let RetrieveLanguages {
                    json,
                    service,
                    contest,
                    color_choice,
                    problem,
                } = cli_args;
                let contest = contest.as_ref().map(AsRef::as_ref);
                let problem = problem.clone();
                self.term.attempt_enable_ansi(*color_choice);
                let config = Config::load(*service, contest, None, &wd)?;
                let contest = config.contest().to_owned();
                let sess_props = self.sess_props(&config)?;
                let retrieve_props = RetrieveLangsProps { contest, problem };
                let props = (sess_props, retrieve_props);
                let term = &mut self.term;
                let outcome = match config.service() {
                    ServiceKind::Atcoder => atcoder::retrieve_langs(props, term)?,
                    ServiceKind::Codeforces => codeforces::retrieve_langs(props, term)?,
                    ServiceKind::Yukicoder => yukicoder::retrieve_langs(props, term)?,
                    _ => return Err(crate::ErrorKind::Unimplemented.into()),
                };
                finish(
                    &outcome,
                    cli_args,
                    &config,
                    SubCommandKind::RetrieveLanguages,
                    *json,
                    &mut self.term,
                )?;
            }
            Opt::Retrieve(Retrieve::Submissions(cli_args)) => {
                let RetrieveSubmissions {
                    fetch_all,
                    json,
                    service,
                    contest,
                    mode,
                    problems,
                    color_choice,
                } = cli_args;
                let contest = contest.as_ref().map(AsRef::as_ref);
                let problems = problems.clone();
                self.term.attempt_enable_ansi(*color_choice);
                let config = Config::load(*service, contest, None, &wd)?;
                self.term.apply_conf(config.console());
                let sess_props = self.sess_props(&config)?;
                let retrieve_props =
                    RetrieveSubmissionsProps::new(&config, *mode, problems, *fetch_all)?;
                let props = (sess_props, retrieve_props);
                let term = &mut self.term;
                let outcome = match config.service() {
                    ServiceKind::Atcoder => atcoder::retrieve_submissions(props, term)?,
                    ServiceKind::Codeforces => codeforces::retrieve_submissiosn(props, term)?,
                    _ => return Err(crate::ErrorKind::Unimplemented.into()),
                };
                finish(
                    &outcome,
                    cli_args,
                    &config,
                    SubCommandKind::RetrieveSubmissions,
                    *json,
                    &mut self.term,
                )?;
            }
            Opt::Judge(cli_args) => {
                let Judge {
                    json,
                    force_compile,
                    release,
                    service,
                    contest,
                    language,
                    mode,
                    jobs,
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
                self.term.attempt_enable_ansi(*color_choice);
                let config = Config::load(*service, contest, language, &wd)?;
                self.term.apply_conf(config.console());
                let outcome = judging::judge::<T::Stdout, _>(JudgeParams {
                    stderr: self.term.stderr(),
                    config: &config,
                    mode,
                    problem: &problem,
                    force_compile: *force_compile,
                    jobs: *jobs,
                })?;
                finish(
                    &outcome,
                    cli_args,
                    &config,
                    SubCommandKind::Judge,
                    *json,
                    &mut self.term,
                )?;
            }
            Opt::Submit(cli_args) => {
                let Submit {
                    json,
                    open,
                    force_compile,
                    only_transpile,
                    no_judge,
                    debug,
                    no_check_duplication,
                    language,
                    service,
                    contest,
                    mode,
                    jobs,
                    color_choice,
                    problem,
                } = cli_args;
                let contest = contest.as_ref().map(AsRef::as_ref);
                let language = language.as_ref().map(AsRef::as_ref);
                let mode = if *debug { config::Mode::Debug } else { *mode };
                self.term.attempt_enable_ansi(*color_choice);
                let config = Config::load(*service, contest, language, &wd)?;
                self.term.apply_conf(config.console());
                if *only_transpile {
                    let mut stderr = self.term.stderr();
                    if judging::only_transpile::<T::Stdout, _>(
                        &mut stderr,
                        &config,
                        mode,
                        &problem,
                        *force_compile,
                    )? {
                        writeln!(stderr)?;
                    }
                } else if !no_judge {
                    judging::judge::<T::Stdout, _>(JudgeParams {
                        stderr: self.term.stderr(),
                        config: &config,
                        mode,
                        problem: &problem,
                        force_compile: *force_compile,
                        jobs: *jobs,
                    })?;
                    writeln!(self.term.stderr())?;
                }
                let sess_props = self.sess_props(&config)?;
                let submit_props = SubmitProps::try_new(
                    &config,
                    mode,
                    problem.clone(),
                    *open,
                    *no_check_duplication,
                )?;
                let props = (sess_props, submit_props);
                let term = &mut self.term;
                let outcome = match config.service() {
                    ServiceKind::Atcoder => atcoder::submit(props, term)?,
                    ServiceKind::Codeforces => codeforces::submit(props, term)?,
                    ServiceKind::Yukicoder => yukicoder::submit(props, term)?,
                    _ => return Err(crate::ErrorKind::Unimplemented.into()),
                };
                finish(
                    &outcome,
                    cli_args,
                    &config,
                    SubCommandKind::Submit,
                    *json,
                    &mut self.term,
                )?;
            }
        }
        Ok(())
    }

    fn on_retrieve_testcases(
        &mut self,
        cli_args: &RetrieveTestcases,
        kind: SubCommandKind,
    ) -> crate::Result<()> {
        let RetrieveTestcases {
            json,
            open,
            only_scraped,
            service,
            contest,
            problems,
            color_choice,
        } = cli_args;
        let contest = contest.as_ref().map(AsRef::as_ref);
        self.term.attempt_enable_ansi(*color_choice);
        let config = Config::load(*service, contest, None, &self.working_dir)?;
        self.term.apply_conf(config.console());
        let sess_props = self.sess_props(&config)?;
        let retrieve_props = RetrieveTestCasesProps {
            contest: config.contest().to_owned(),
            problems: NonEmptyVec::try_new(problems.clone()),
            destinations: config.destinations(None),
            open_in_browser: *open,
            only_scraped: *only_scraped,
        };
        let props = (sess_props, retrieve_props);
        let term = &mut self.term;
        let outcome = match config.service() {
            ServiceKind::Atcoder => atcoder::retrieve_testcases(props, term),
            ServiceKind::Codeforces => codeforces::retrieve_testcases(props, term),
            ServiceKind::Yukicoder => yukicoder::retrieve_testcases(props, term),
            ServiceKind::Other => return Err(crate::ErrorKind::Unimplemented.into()),
        }?;
        finish(&outcome, cli_args, &config, kind, *json, &mut self.term)
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
            silent: config.session_silent(),
            robots: config.session_robots(),
        })
    }
}

fn finish<O: Serialize, A: Serialize, T: Term>(
    outcome: &O,
    command_line_arguments: &A,
    config: &Config,
    subcommand: SubCommandKind,
    json: bool,
    mut term: T,
) -> crate::Result<()> {
    #[derive(Serialize)]
    struct WithCliArgsAndConfig<'a, A: Serialize, T: Serialize> {
        command_line_arguments: &'a A,
        config: &'a config::Inner,
        target: &'a config::Target,
        base_directory: &'a AbsPath,
        #[serde(flatten)]
        outcome: T,
    }

    let outcome = WithCliArgsAndConfig {
        command_line_arguments,
        config: config.inner(),
        target: config.target(),
        base_directory: config.base_dir(),
        outcome,
    };
    let hooks = config.hooks(subcommand, &outcome).expand()?;
    hooks.run::<T::Stdout, _>(term.stderr())?;
    if json {
        let json = serde_json::to_string_pretty(&outcome)?;
        writeln!(term.stdout(), "{}", json)?;
        term.stdout().flush()?;
    }
    Ok(())
}
