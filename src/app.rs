use crate::config::{self, Config};
use crate::errors::ExpandTemplateResult;
use crate::judging::{self, JudgeParams};
use crate::path::AbsPathBuf;
use crate::service::{
    atcoder, yukicoder, DownloadProps, RestoreProps, ServiceName, SessionProps, SubmitProps,
};
use crate::terminal::{AnsiColorChoice, Term};
use crate::testsuite::{self, SuiteFileExtension};
use crate::time;
use crate::util::num::PositiveFinite;
use crate::util::std_unstable::Transpose_;

use log::info;
use structopt::clap::Arg;
use structopt::StructOpt;
use strum_macros::EnumString;

use std::io::Write;
use std::num::NonZeroUsize;
use std::path::PathBuf;
use std::time::Duration;

#[derive(Debug, StructOpt)]
#[structopt(usage = "snowchains <i|init> [OPTIONS] [directory]\
               \n    snowchains <w|switch|c|checkout> [OPTIONS]\
               \n    snowchains <l|login> [OPTIONS] <service>\
               \n    snowchains <p|participate> [OPTIONS] <service> <contest>\
               \n    snowchains <d|download> [FLAGS] [OPTIONS]\
               \n    snowchains <r|restore> [OPTIONS]\
               \n    snowchains <j|judge|t|test> [FLAGS] [OPTIONS] <problem>\
               \n    snowchains <s|submit> [FLAGS] [OPTIONS] <problem>\
               \n    snowchains show num-cases [OPTIONS] <problem> <extension>\
               \n    snowchains show timelimit-millis [OPTIONS] <problem> <nth>\
               \n    snowchains show in [OPTIONS] <problem> <nth>\
               \n    snowchains show accepts [OPTIONS] <problem> <nth>\
               \n    snowchains modify timelimit [OPTIONS] <problem> <nth> [timelimit]\
               \n    snowchains modify append [OPTIONS] <problem> <extensioon> <input> [output]\
               \n    snowchains modify match [OPTIONS] <problem> <extension> <match>")]
pub enum Opt {
    #[structopt(
        about = "Creates a config file (\"snowchains.toml\")",
        name = "init",
        usage = "snowchains <i|init> [OPTIONS] [directory]",
        raw(alias = "\"i\"", display_order = "1")
    )]
    Init {
        #[structopt(raw(color_choice = "1"))]
        color_choice: AnsiColorChoice,
        #[structopt(
            help = "Directory to create a \"snowchains.toml\"",
            default_value = ".",
            parse(from_os_str)
        )]
        directory: PathBuf,
    },

    #[structopt(
        about = "Modifies values in a config file",
        name = "switch",
        usage = "snowchains <w|switch|c|checkout> [OPTIONS]",
        raw(aliases = r#"&["w", "checkout", "c"]"#, display_order = "2")
    )]
    Switch {
        #[structopt(raw(service = r#"&["atcoder", "yukicoder", "other"], Kind::Option(1)"#))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(language = "3"))]
        language: Option<String>,
        #[structopt(raw(color_choice = "4"))]
        color_choice: AnsiColorChoice,
    },

    #[structopt(
        about = "Logges in to a service",
        name = "login",
        usage = "snowchains <l|login> [OPTIONS] <service>",
        raw(alias = "\"l\"", display_order = "3")
    )]
    Login {
        #[structopt(raw(color_choice = "1"))]
        color_choice: AnsiColorChoice,
        #[structopt(raw(service = r#"&["atcoder", "yukicoder"], Kind::Arg"#))]
        service: ServiceName,
    },

    #[structopt(
        about = "Participates in a contest",
        name = "participate",
        usage = "snowchains <p|participate> [OPTIONS] <service> <contest>",
        raw(alias = "\"p\"", display_order = "4")
    )]
    Participate {
        #[structopt(raw(color_choice = "1"))]
        color_choice: AnsiColorChoice,
        #[structopt(raw(service = r#"&["atcoder"], Kind::Arg"#))]
        service: ServiceName,
        #[structopt(raw(contest = "Kind::Arg"))]
        contest: String,
    },

    #[structopt(
        about = "Downloads test cases",
        name = "download",
        usage = "snowchains <d|download> [FLAGS] [OPTIONS]",
        raw(alias = "\"d\"", display_order = "5")
    )]
    Download {
        #[structopt(raw(open = "1"))]
        open: bool,
        #[structopt(
            long = "only-scraped",
            help = "Does not download official test cases",
            raw(display_order = "2")
        )]
        only_scraped: bool,
        #[structopt(raw(service = r#"&["atcoder", "yukicoder"], Kind::Option(1)"#))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(problems = "3"))]
        problems: Vec<String>,
        #[structopt(raw(color_choice = "4"))]
        color_choice: AnsiColorChoice,
    },

    #[structopt(
        about = "Downloads source files you have submitted",
        name = "restore",
        usage = "snowchains <r|restore> [OPTIONS]",
        raw(alias = "\"r\"", display_order = "6")
    )]
    Restore {
        #[structopt(raw(service = "&[\"atcoder\"], Kind::Option(1)"))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(problems = "3"))]
        problems: Vec<String>,
        #[structopt(raw(color_choice = "4"))]
        color_choice: AnsiColorChoice,
    },

    #[structopt(
        about = "Tests a binary or script",
        name = "judge",
        usage = "snowchains <j|judge|t|test> [FLAGS] [OPTIONS] <problem>",
        raw(aliases = r#"&["j", "test", "t"]"#, display_order = "7")
    )]
    Judge {
        #[structopt(raw(force_compile = "1"))]
        force_compile: bool,
        #[structopt(raw(service = "SERVICE_VALUES, Kind::Option(1)"))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(language = "3"))]
        language: Option<String>,
        #[structopt(parse(try_from_str = "parse_non_zero_usize"), raw(jobs = "4"))]
        jobs: Option<NonZeroUsize>,
        #[structopt(raw(color_choice = "4"))]
        color_choice: AnsiColorChoice,
        #[structopt(raw(problem = ""))]
        problem: String,
    },

    #[structopt(
        about = "Submits a source file",
        name = "submit",
        usage = "snowchains <s|submit> [FLAGS] [OPTIONS] <problem>",
        raw(alias = "\"s\"", display_order = "8")
    )]
    Submit {
        #[structopt(raw(open = "1"))]
        open: bool,
        #[structopt(raw(conflicts_with = "\"no_judge\"", force_compile = "2"))]
        force_compile: bool,
        #[structopt(
            long = "only-transpile",
            help = "Transpile the source code but not compile",
            raw(conflicts_with = "\"no_judge\"", display_order = "3")
        )]
        only_transpile: bool,
        #[structopt(
            long = "no-judge",
            help = "Skips testing",
            raw(
                conflicts_with_all = r#"&["force_compile", "only_transpile"]"#,
                display_order = "4"
            )
        )]
        no_judge: bool,
        #[structopt(
            long = "no-check-duplication",
            help = "Submits even if the contest is active and you have already solved the problem",
            raw(display_order = "5")
        )]
        no_check_duplication: bool,
        #[structopt(raw(service = "&[\"atcoder\", \"yukicoder\"], Kind::Option(1)"))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(language = "3"))]
        language: Option<String>,
        #[structopt(parse(try_from_str = "parse_non_zero_usize"), raw(jobs = "4"))]
        jobs: Option<NonZeroUsize>,
        #[structopt(raw(color_choice = "5"))]
        color_choice: AnsiColorChoice,
        #[structopt(raw(problem = ""))]
        problem: String,
    },

    #[structopt(
        about = "Prints information",
        name = "show",
        usage = "snowchains show num-cases [OPTIONS] <problem> <extension>\
                 \n    snowchains show timelimit-millis [OPTIONS] <problem> <nth>\
                 \n    snowchains show in [OPTIONS] <problem> <nth>\
                 \n    snowchains show accepts [OPTIONS] <problem> <nth>",
        raw(display_order = "9")
    )]
    Show(Show),

    #[structopt(
        about = "Modifies values in a config file or test files",
        name = "modify",
        usage = "snowchains modify timelimit [OPTIONS] <problem> <nth> [timelimit]\
                 \n    snowchains modify append [OPTIONS] <problem> <extensioon> <input> [output]\
                 \n    snowchains modify match [OPTIONS] <problem> <extension> <match>",
        raw(display_order = "10")
    )]
    Modify(Modify),
}

#[derive(Debug, StructOpt)]
pub enum Show {
    #[structopt(
        about = "Prints number of test cases (without EOL)",
        name = "num-cases",
        raw(display_order = "1")
    )]
    NumCases {
        #[structopt(raw(service = "SERVICE_VALUES, Kind::Option(1)"))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(problem = ""))]
        problem: String,
    },

    #[structopt(
        about = "Prints timelimit (without EOL)",
        name = "timelimit-millis",
        raw(display_order = "2")
    )]
    TimelimitMillis {
        #[structopt(raw(service = "SERVICE_VALUES, Kind::Option(1)"))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(problem = ""))]
        problem: String,
        #[structopt(raw(nth = ""))]
        nth: usize,
    },

    #[structopt(about = "Prints \"in\" value", name = "in", raw(display_order = "3"))]
    In {
        #[structopt(raw(service = "SERVICE_VALUES, Kind::Option(1)"))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(problem = ""))]
        problem: String,
        #[structopt(raw(nth = ""))]
        nth: usize,
    },

    #[structopt(
        about = "Tests for a value from stdin (without timelimit)",
        name = "accepts",
        raw(display_order = "4")
    )]
    Accepts {
        #[structopt(raw(service = "SERVICE_VALUES, Kind::Option(1)"))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(color_choice = "3"))]
        color_choice: AnsiColorChoice,
        #[structopt(raw(problem = ""))]
        problem: String,
        #[structopt(raw(nth = ""))]
        nth: usize,
    },
}

#[derive(Debug, StructOpt)]
pub enum Modify {
    #[structopt(
        about = "Modifies a `timellimit`",
        name = "timelimit",
        raw(display_order = "1")
    )]
    Timelimit {
        #[structopt(raw(service = "SERVICE_VALUES, Kind::Option(1)"))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(color_choice = "3"))]
        color_choice: AnsiColorChoice,
        #[structopt(raw(problem = ""))]
        problem: String,
        #[structopt(raw(extension = r#"&["json", "toml", "yaml", "yml"]"#))]
        extension: SuiteFileExtension,
        #[structopt(help = "Timelimit", parse(try_from_str = "time::parse_secs"))]
        timelimit: Option<Duration>,
    },

    #[structopt(
        about = "Appends a test case to a test suite file",
        name = "append",
        raw(display_order = "2")
    )]
    Append {
        #[structopt(raw(service = "SERVICE_VALUES, Kind::Option(1)"))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(color_choice = "3"))]
        color_choice: AnsiColorChoice,
        #[structopt(raw(problem = ""))]
        problem: String,
        #[structopt(raw(extension = r#"&["json", "toml", "yaml", "yml"]"#))]
        extension: SuiteFileExtension,
        #[structopt(help = "\"input\" value to append")]
        input: String,
        #[structopt(help = "\"expected\" value to append")]
        output: Option<String>,
    },

    #[structopt(about = "Modifies a `match`", name = "match", raw(display_order = "3"))]
    Match {
        #[structopt(raw(service = "SERVICE_VALUES, Kind::Option(1)"))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(color_choice = "3"))]
        color_choice: AnsiColorChoice,
        #[structopt(raw(problem = ""))]
        problem: String,
        #[structopt(raw(extension = r#"&["json", "toml", "yaml", "yml"]"#))]
        extension: SuiteFileExtension,
        #[structopt(flatten)]
        match_opts: MatchOpts,
    },
}

static SERVICE_VALUES: &[&str] = &["atcoder", "yukicoder", "other"];

#[derive(StructOpt, Debug)]
pub struct MatchOpts {
    #[structopt(
        name = "match",
        help = "`match` type",
        raw(possible_values = r#"&["any", "exact", "float"]"#)
    )]
    kind: MatchKind,
    #[structopt(
        long = "relative",
        help = "Relative error (ignored if <match> is not \"float\")",
        raw(value_name = "\"POSITIVE_FINITE_FLOAT64\"", display_order = "4")
    )]
    relative_error: Option<PositiveFinite<f64>>,
    #[structopt(
        long = "absolute",
        help = "Absolute error (ignored if <match> is not \"float\")",
        raw(value_name = "\"POSITIVE_FINITE_FLOAT64\"", display_order = "5")
    )]
    absolute_error: Option<PositiveFinite<f64>>,
}

impl Into<testsuite::Match> for MatchOpts {
    fn into(self) -> testsuite::Match {
        match self.kind {
            MatchKind::Any => testsuite::Match::Any,
            MatchKind::Exact => testsuite::Match::Exact,
            MatchKind::Float => testsuite::Match::Float {
                relative_error: self.relative_error,
                absolute_error: self.absolute_error,
            },
        }
    }
}

#[derive(Debug, EnumString)]
#[strum(serialize_all = "snake_case")]
enum MatchKind {
    Any,
    Exact,
    Float,
}

enum Kind {
    Option(usize),
    Arg,
}

trait ArgExt {
    fn force_compile(self, order: usize) -> Self;
    fn open(self, order: usize) -> Self;
    fn language(self, order: usize) -> Self;
    fn problems(self, order: usize) -> Self;
    fn jobs(self, order: usize) -> Self;
    fn color_choice(self, order: usize) -> Self;
    fn problem(self) -> Self;
    fn nth(self) -> Self;
    fn extension(self, values: &'static [&'static str]) -> Self;
    fn service(self, values: &'static [&'static str], kind: Kind) -> Self;
    fn contest(self, kind: Kind) -> Self;
}

impl ArgExt for Arg<'static, 'static> {
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
        info!("Opt = {:?}", opt);
        let working_dir = self.working_dir.clone();
        match opt {
            Opt::Init {
                color_choice,
                directory,
            } => {
                let wd = working_dir.join_canonicalizing_lossy(&directory);
                self.term.attempt_enable_ansi(color_choice);
                config::init(self.term.stdout(), &wd)?;
            }
            Opt::Switch {
                service,
                contest,
                language,
                color_choice,
            } => {
                self.term.attempt_enable_ansi(color_choice);
                let (_, stdout, stderr) = self.term.split_mut();
                let (config, outcome) =
                    config::switch(stdout, stderr, &working_dir, service, contest, language)?;
                let hooks = config.switch_hooks(&outcome).expand()?;
                hooks.run::<_, T::Stderr>(self.term.stdout())?;
            }
            Opt::Login {
                color_choice,
                service,
            } => {
                self.term.attempt_enable_ansi(color_choice);
                let config = Config::load(service, None, None, &working_dir)?;
                self.term.apply_conf(config.console());
                let sess_props = self.sess_props(&config)?;
                match service {
                    ServiceName::Atcoder => atcoder::login(sess_props),
                    ServiceName::Yukicoder => yukicoder::login(sess_props),
                    ServiceName::Other => unreachable!(),
                }?;
            }
            Opt::Participate {
                color_choice,
                service,
                contest,
            } => {
                self.term.attempt_enable_ansi(color_choice);
                let config = Config::load(service, contest.clone(), None, &working_dir)?;
                self.term.apply_conf(config.console());
                let sess_props = self.sess_props(&config)?;
                match service {
                    ServiceName::Atcoder => atcoder::participate(&contest, sess_props),
                    _ => unreachable!(),
                }?;
            }
            Opt::Download {
                open,
                only_scraped,
                service,
                contest,
                problems,
                color_choice,
            } => {
                self.term.attempt_enable_ansi(color_choice);
                let config = Config::load(service, contest, None, &working_dir)?;
                self.term.apply_conf(config.console());
                let sess_props = self.sess_props(&config)?;
                let download_props = DownloadProps {
                    contest: config.contest().to_owned(),
                    problems: if problems.is_empty() {
                        None
                    } else {
                        Some(problems)
                    },
                    destinations: config.download_destinations(None),
                    open_in_browser: open,
                    only_scraped,
                };
                let outcome = match config.service() {
                    ServiceName::Atcoder => atcoder::download(sess_props, download_props),
                    ServiceName::Yukicoder => yukicoder::download(sess_props, download_props),
                    ServiceName::Other => return Err(crate::ErrorKind::Unimplemented.into()),
                }?;
                let hooks = config.download_hooks(&outcome).expand()?;
                hooks.run::<_, T::Stderr>(self.term.stdout())?;
            }
            Opt::Restore {
                service,
                contest,
                problems,
                color_choice,
            } => {
                self.term.attempt_enable_ansi(color_choice);
                let config = Config::load(service, contest, None, &working_dir)?;
                self.term.apply_conf(config.console());
                let sess_props = self.sess_props(&config)?;
                let restore_props = RestoreProps::new(&config, problems);
                match config.service() {
                    ServiceName::Atcoder => atcoder::restore(sess_props, restore_props)?,
                    _ => return Err(crate::ErrorKind::Unimplemented.into()),
                };
            }
            Opt::Judge {
                force_compile,
                service,
                contest,
                language,
                jobs,
                color_choice,
                problem,
            } => {
                self.term.attempt_enable_ansi(color_choice);
                let config = Config::load(service, contest, language.clone(), &working_dir)?;
                self.term.apply_conf(config.console());
                let (_, stdout, stderr) = self.term.split_mut();
                judging::judge(JudgeParams {
                    stdout,
                    stderr,
                    config: &config,
                    problem: &problem,
                    force_compile,
                    jobs,
                })?;
            }
            Opt::Submit {
                open,
                force_compile,
                only_transpile,
                no_judge,
                no_check_duplication,
                language,
                service,
                contest,
                jobs,
                color_choice,
                problem,
            } => {
                self.term.attempt_enable_ansi(color_choice);
                let config = Config::load(service, contest, language.clone(), &working_dir)?;
                self.term.apply_conf(config.console());
                if only_transpile {
                    let (_, mut stdout, stderr) = self.term.split_mut();
                    if judging::only_transpile(
                        &mut stdout,
                        stderr,
                        &config,
                        &problem,
                        force_compile,
                    )? {
                        writeln!(stdout)?;
                    }
                } else if !no_judge {
                    let (_, mut stdout, stderr) = self.term.split_mut();
                    judging::judge(JudgeParams {
                        stdout: &mut stdout,
                        stderr,
                        config: &config,
                        problem: &problem,
                        force_compile,
                        jobs,
                    })?;
                    writeln!(stdout)?;
                }
                let sess_props = self.sess_props(&config)?;
                let submit_props =
                    SubmitProps::try_new(&config, problem.clone(), open, no_check_duplication)?;
                match config.service() {
                    ServiceName::Atcoder => atcoder::submit(sess_props, submit_props)?,
                    ServiceName::Yukicoder => yukicoder::submit(sess_props, submit_props)?,
                    _ => return Err(crate::ErrorKind::Unimplemented.into()),
                };
            }
            Opt::Show(Show::NumCases {
                service,
                contest,
                problem,
            }) => {
                let config = Config::load(service, contest, None, &working_dir)?;
                let num_cases = judging::num_cases(&config, &problem)?;
                write!(self.term.stdout(), "{}", num_cases)?;
                self.term.stdout().flush()?;
            }
            Opt::Show(Show::TimelimitMillis {
                service,
                contest,
                problem,
                nth,
            }) => {
                let config = Config::load(service, contest, None, &working_dir)?;
                let timelimit = judging::timelimit_millis(&config, &problem, nth)?;
                write!(self.term.stdout(), "{}", timelimit)?;
                self.term.stdout().flush()?;
            }
            Opt::Show(Show::In {
                service,
                contest,
                problem,
                nth,
            }) => {
                let config = Config::load(service, contest, None, &working_dir)?;
                let input = judging::input(&config, &problem, nth)?;
                write!(self.term.stdout(), "{}", input)?;
                self.term.stdout().flush()?;
            }
            Opt::Show(Show::Accepts {
                service,
                contest,
                color_choice,
                problem,
                nth,
            }) => {
                self.term.attempt_enable_ansi(color_choice);
                let config = Config::load(service, contest, None, &working_dir)?;
                self.term.apply_conf(config.console());
                let (stdin, _, stderr) = self.term.split_mut();
                judging::accepts(&config, &problem, nth, stdin, stderr)?;
            }
            Opt::Modify(Modify::Timelimit {
                service,
                contest,
                color_choice,
                problem,
                extension,
                timelimit,
            }) => {
                self.term.attempt_enable_ansi(color_choice);
                let config = Config::load(service, contest, None, &working_dir)?;
                self.term.apply_conf(config.console());
                let path = config
                    .download_destinations(Some(extension))
                    .expand(&problem)?;
                testsuite::modify_timelimit(self.term.stdout(), &problem, &path, timelimit)?;
            }
            Opt::Modify(Modify::Append {
                service,
                contest,
                color_choice,
                problem,
                extension,
                input,
                output,
            }) => {
                self.term.attempt_enable_ansi(color_choice);
                let config = Config::load(service, contest, None, &working_dir)?;
                self.term.apply_conf(config.console());
                let path = config
                    .download_destinations(Some(extension))
                    .expand(&problem)?;
                let output = output.as_ref().map(String::as_str);
                testsuite::modify_append(&problem, &path, &input, output, self.term.stdout())?;
            }
            Opt::Modify(Modify::Match {
                service,
                contest,
                color_choice,
                problem,
                extension,
                match_opts,
            }) => {
                self.term.attempt_enable_ansi(color_choice);
                let config = Config::load(service, contest, None, &working_dir)?;
                self.term.apply_conf(config.console());
                let path = config
                    .download_destinations(Some(extension))
                    .expand(&problem)?;
                let output_match = match_opts.into();
                testsuite::modify_match(self.term.stdout(), &problem, &path, output_match)?;
            }
        }
        Ok(())
    }

    fn sess_props(&mut self, config: &Config) -> ExpandTemplateResult<SessionProps<&mut T>> {
        let cookies_path = config.session_cookies().expand(None)?;
        let dropbox_path = config
            .session_dropbox_auth()
            .map(|p| p.expand(None))
            .transpose_()?;
        Ok(SessionProps {
            term: &mut self.term,
            domain: config.service().domain(),
            cookies_path,
            dropbox_path,
            timeout: config.session_timeout(),
            login_retries: self.login_retries,
            silent: config.session_silent(),
        })
    }
}
