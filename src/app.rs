use config::{self, Config};
use console::{self, ColorChoice, ConsoleReadWrite, Printer};
use errors::ExpandTemplateResult;
use judging::{self, JudgeParams};
use path::AbsPathBuf;
use service::{
    atcoder, hackerrank, yukicoder, Credentials, DownloadProp, RestoreProp, ServiceName,
    SessionProp, SubmitProp,
};
use testsuite::{self, SerializableExtension};

use structopt::clap::Arg;

use std;
use std::borrow::Cow;
use std::io::Write as _Write;
use std::num::NonZeroUsize;
use std::path::PathBuf;

#[derive(Debug, StructOpt)]
#[structopt(
    usage = "snowchains <i|init> [OPTIONS] [directory]\
             \n    snowchains <w|switch> [OPTIONS]\
             \n    snowchains <l|login> [OPTIONS] <service>\
             \n    snowchains <p|participate> [OPTIONS] <service> <contest>\
             \n    snowchains <d|download> [FLAGS] [OPTIONS]\
             \n    snowchains <r|restore> [OPTIONS]\
             \n    snowchains <a|append> [OPTIONS] <problem> <extension> <input> [output]\
             \n    snowchains <j|judge> [FLAGS] [OPTIONS] <problem>\
             \n    snowchains <s|submit> [FLAGS] [OPTIONS] <problem>\
             \n    snowchains show path [FLAGS] [OPTIONS] <problem> <extension>"
)]
pub enum Opt {
    #[structopt(
        about = "Creates a \"snowchains.yaml\"",
        name = "init",
        usage = "snowchains <i|init> [OPTIONS] [directory]",
        raw(alias = "\"i\"", display_order = "1"),
    )]
    Init {
        #[structopt(raw(color_choice = "1"))]
        color_choice: ColorChoice,
        #[structopt(
            help = "Directory to create a \"snowchains.yaml\"",
            default_value = ".",
            parse(from_os_str),
        )]
        directory: PathBuf,
    },

    #[structopt(
        about = "Changes attribute values of a \"snowchains.yaml\"",
        name = "switch",
        usage = "snowchains <w|switch> [OPTIONS]",
        raw(alias = "\"w\"", display_order = "2"),
    )]
    Switch {
        #[structopt(
            raw(service = r#"&["atcoder", "hackerrank", "yukicoder", "other"], Kind::Option(1)"#),
        )]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(language = "3"))]
        language: Option<String>,
        #[structopt(raw(color_choice = "4"))]
        color_choice: ColorChoice,
    },

    #[structopt(
        about = "Logges in to a service",
        name = "login",
        usage = "snowchains <l|login> [OPTIONS] <service>",
        raw(alias = "\"l\"", display_order = "3"),
    )]
    Login {
        #[structopt(raw(color_choice = "1"))]
        color_choice: ColorChoice,
        #[structopt(raw(service = r#"&["atcoder", "hackerrank", "yukicoder"], Kind::Arg"#))]
        service: ServiceName,
    },

    #[structopt(
        about = "Participates in a contest",
        name = "participate",
        usage = "snowchains <p|participate> [OPTIONS] <service> <contest>",
        raw(alias = "\"p\"", display_order = "4"),
    )]
    Participate {
        #[structopt(raw(color_choice = "1"))]
        color_choice: ColorChoice,
        #[structopt(raw(service = r#"&["atcoder"], Kind::Arg"#))]
        service: ServiceName,
        #[structopt(raw(contest = "Kind::Arg"))]
        contest: String,
    },

    #[structopt(
        about = "Downloads test cases",
        name = "download",
        usage = "snowchains <d|download> [FLAGS] [OPTIONS]",
        raw(alias = "\"d\"", display_order = "5"),
    )]
    Download {
        #[structopt(raw(open_browser = "1"))]
        open_browser: bool,
        #[structopt(raw(service = r#"&["atcoder", "hackerrank", "yukicoder"], Kind::Option(1)"#))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(problems = "3"))]
        problems: Vec<String>,
        #[structopt(raw(color_choice = "4"))]
        color_choice: ColorChoice,
    },

    #[structopt(
        about = "Downloads the source codes you've submitted",
        name = "restore",
        usage = "snowchains <r|restore> [OPTIONS]",
        raw(alias = "\"r\"", display_order = "6"),
    )]
    Restore {
        #[structopt(raw(service = "&[\"atcoder\"], Kind::Option(1)"))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(problems = "3"))]
        problems: Vec<String>,
        #[structopt(raw(color_choice = "4"))]
        color_choice: ColorChoice,
    },

    #[structopt(
        about = "Appends a test case to a test suite file",
        name = "append",
        usage = "snowchains <a|append> [OPTIONS] <problem> <extension> <input> [output]",
        raw(alias = "\"a\"", display_order = "7"),
    )]
    Append {
        #[structopt(raw(service = "SERVICE_VALUES, Kind::Option(1)"))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(color_choice = "3"))]
        color_choice: ColorChoice,
        #[structopt(raw(problem = ""))]
        problem: String,
        #[structopt(raw(extension = r#"&["json", "toml", "yaml", "yml"]"#))]
        extension: SerializableExtension,
        #[structopt(help = "\"input\" value to append")]
        input: String,
        #[structopt(help = "\"expected\" value to append")]
        output: Option<String>,
    },

    #[structopt(
        about = "Tests a binary or script",
        name = "judge",
        usage = "snowchains <j|judge> [FLAGS] [OPTIONS] <problem>",
        raw(alias = "\"j\"", display_order = "8"),
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
        #[structopt(
            parse(try_from_str = "parse_non_zero_usize"),
            raw(jobs = "4"),
        )]
        jobs: Option<NonZeroUsize>,
        #[structopt(raw(color_choice = "4"))]
        color_choice: ColorChoice,
        #[structopt(raw(problem = ""))]
        problem: String,
    },

    #[structopt(
        about = "Submits a source code",
        name = "submit",
        usage = "snowchains <s|submit> [FLAGS] [OPTIONS] <problem>",
        raw(alias = "\"s\"", display_order = "9"),
    )]
    Submit {
        #[structopt(raw(open_browser = "1"))]
        open_browser: bool,
        #[structopt(raw(force_compile = "2", conflicts_with = "\"skip_judging\""))]
        force_compile: bool,
        #[structopt(
            long = "skip-judging",
            help = "Skips judging",
            raw(conflicts_with = "\"force_compile\"", display_order = "3"),
        )]
        skip_judging: bool,
        #[structopt(
            long = "skip-checking-duplication",
            help = "Submits even if the contest is active and your code is already accepted",
            raw(display_order = "4"),
        )]
        skip_checking_duplication: bool,
        #[structopt(raw(service = "&[\"atcoder\", \"yukicoder\"], Kind::Option(1)"))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(language = "3"))]
        language: Option<String>,
        #[structopt(
            parse(try_from_str = "parse_non_zero_usize"),
            raw(jobs = "4"),
        )]
        jobs: Option<NonZeroUsize>,
        #[structopt(raw(color_choice = "5"))]
        color_choice: ColorChoice,
        #[structopt(raw(problem = ""))]
        problem: String,
    },

    #[structopt(
        about = "Shows information",
        name = "show",
        usage = "snowchains show path [FLAGS] [OPTIONS] <problem> <extension>",
        raw(display_order = "10"),
    )]
    Show(Show),
}

#[derive(Debug, StructOpt)]
pub enum Show {
    #[structopt(about = "Number of test cases", name = "num-cases")]
    NumCases {
        #[structopt(raw(service = "SERVICE_VALUES, Kind::Option(1)"))]
        service: Option<ServiceName>,
        #[structopt(raw(contest = "Kind::Option(2)"))]
        contest: Option<String>,
        #[structopt(raw(problem = ""))]
        problem: String,
    },
}

static SERVICE_VALUES: &[&str] = &["atcoder", "hackerrank", "yukicoder", "other"];

enum Kind {
    Option(usize),
    Arg,
}

trait ArgExt {
    fn force_compile(self, order: usize) -> Self;
    fn open_browser(self, order: usize) -> Self;
    fn language(self, order: usize) -> Self;
    fn problems(self, order: usize) -> Self;
    fn jobs(self, order: usize) -> Self;
    fn color_choice(self, order: usize) -> Self;
    fn problem(self) -> Self;
    fn extension(self, values: &'static [&'static str]) -> Self;
    fn service(self, values: &'static [&'static str], kind: Kind) -> Self;
    fn contest(self, kind: Kind) -> Self;
}

impl ArgExt for Arg<'static, 'static> {
    fn force_compile(self, order: usize) -> Self {
        self.long("force-compile")
            .help("Force to compile")
            .display_order(order)
    }

    fn open_browser(self, order: usize) -> Self {
        self.short("b")
            .long("--open-browser")
            .help("Opens the pages with your default browser")
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

pub struct App<RW: ConsoleReadWrite> {
    pub working_dir: AbsPathBuf,
    pub cookies_on_init: Cow<'static, str>,
    pub credentials: Credentials,
    pub console: RW,
}

impl<RW: ConsoleReadWrite> App<RW> {
    pub fn run(&mut self, opt: Opt) -> ::Result<()> {
        info!("Opt = {:?}", opt);
        let working_dir = self.working_dir.clone();
        let cookies_on_init = self.cookies_on_init.clone();
        match opt {
            Opt::Init {
                color_choice,
                directory,
            } => {
                self.console
                    .fill_palettes(color_choice, &console::Conf::default())?;
                let wd = working_dir.join(&directory);
                config::init(self.console.stdout(), &wd, &cookies_on_init)?;
            }
            Opt::Switch {
                service,
                contest,
                language,
                color_choice,
            } => {
                self.console
                    .fill_palettes(color_choice, &console::Conf::default())?;
                config::switch(
                    self.console.stdout_and_stderr(),
                    &working_dir,
                    service,
                    contest,
                    language,
                )?;
            }
            Opt::Login {
                color_choice,
                service,
            } => {
                self.console
                    .fill_palettes(color_choice, &console::Conf::default())?;
                let config = Config::load(self.console.stdout(), service, None, &working_dir)?;
                self.console.fill_palettes(color_choice, config.console())?;
                let sess_prop = self.sess_prop(&config)?;
                match service {
                    ServiceName::Atcoder => atcoder::login(sess_prop),
                    ServiceName::Hackerrank => hackerrank::login(sess_prop),
                    ServiceName::Yukicoder => yukicoder::login(sess_prop),
                    ServiceName::Other => unreachable!(),
                }?;
            }
            Opt::Participate {
                color_choice,
                service,
                contest,
            } => {
                self.console
                    .fill_palettes(color_choice, &console::Conf::default())?;
                let config = Config::load(
                    self.console.stdout(),
                    service,
                    contest.clone(),
                    &working_dir,
                )?;
                self.console.fill_palettes(color_choice, config.console())?;
                let sess_prop = self.sess_prop(&config)?;
                match service {
                    ServiceName::Atcoder => atcoder::participate(&contest, sess_prop),
                    _ => unreachable!(),
                }?;
            }
            Opt::Download {
                open_browser,
                service,
                contest,
                problems,
                color_choice,
            } => {
                self.console
                    .fill_palettes(color_choice, &console::Conf::default())?;
                let config = Config::load(self.console.stdout(), service, contest, &working_dir)?;
                self.console.fill_palettes(color_choice, config.console())?;
                let sess_prop = self.sess_prop(&config)?;
                let download_prop = DownloadProp::new(&config, open_browser, problems)?;
                match config.service() {
                    ServiceName::Atcoder => atcoder::download(sess_prop, download_prop),
                    ServiceName::Hackerrank => hackerrank::download(sess_prop, download_prop),
                    ServiceName::Yukicoder => yukicoder::download(sess_prop, download_prop),
                    ServiceName::Other => return Err(::Error::Unimplemented),
                }?;
            }
            Opt::Restore {
                service,
                contest,
                problems,
                color_choice,
            } => {
                self.console
                    .fill_palettes(color_choice, &console::Conf::default())?;
                let config = Config::load(self.console.stdout(), service, contest, &working_dir)?;
                self.console.fill_palettes(color_choice, config.console())?;
                let sess_prop = self.sess_prop(&config)?;
                let restore_prop = RestoreProp::new(&config, problems)?;
                match config.service() {
                    ServiceName::Atcoder => atcoder::restore(sess_prop, restore_prop)?,
                    _ => return Err(::Error::Unimplemented),
                };
            }
            Opt::Append {
                service,
                contest,
                color_choice,
                problem,
                extension,
                input,
                output,
            } => {
                self.console
                    .fill_palettes(color_choice, &console::Conf::default())?;
                let config = Config::load(self.console.stdout(), service, contest, &working_dir)?;
                self.console.fill_palettes(color_choice, config.console())?;
                let path = config
                    .download_destinations(Some(extension))
                    .scraping(&problem)?;
                let output = output.as_ref().map(String::as_str);
                testsuite::append(&problem, &path, &input, output, self.console.stdout())?;
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
                self.console
                    .fill_palettes(color_choice, &console::Conf::default())?;
                let config = Config::load(self.console.stdout(), service, contest, &working_dir)?;
                self.console.fill_palettes(color_choice, config.console())?;
                let (stdout, stderr) = self.console.stdout_and_stderr();
                judging::judge(JudgeParams {
                    stdout,
                    stderr,
                    config: &config,
                    problem: &problem,
                    language: language.as_ref().map(String::as_ref),
                    force_compile,
                    jobs,
                })?;
            }
            Opt::Submit {
                open_browser,
                force_compile,
                skip_judging,
                skip_checking_duplication,
                language,
                service,
                contest,
                jobs,
                color_choice,
                problem,
            } => {
                self.console
                    .fill_palettes(color_choice, &console::Conf::default())?;
                let language = language.as_ref().map(String::as_str);
                let config = Config::load(self.console.stdout(), service, contest, &working_dir)?;
                self.console.fill_palettes(color_choice, config.console())?;
                if !skip_judging {
                    let (mut stdout, stderr) = self.console.stdout_and_stderr();
                    judging::judge(JudgeParams {
                        stdout: &mut stdout,
                        stderr,
                        config: &config,
                        problem: &problem,
                        language,
                        force_compile,
                        jobs,
                    })?;
                    writeln!(stdout)?;
                }
                let sess_prop = self.sess_prop(&config)?;
                let submit_prop = SubmitProp::new(
                    &config,
                    problem.clone(),
                    language,
                    open_browser,
                    skip_checking_duplication,
                )?;
                match config.service() {
                    ServiceName::Atcoder => atcoder::submit(sess_prop, submit_prop)?,
                    ServiceName::Yukicoder => yukicoder::submit(sess_prop, submit_prop)?,
                    _ => return Err(::Error::Unimplemented),
                };
            }
            Opt::Show(Show::NumCases {
                service,
                contest,
                problem,
            }) => {
                let config = Config::load(Printer::null(), service, contest, &working_dir)?;
                let num_cases = judging::num_cases(&config, &problem)?;
                write!(self.console.stdout(), "{}", num_cases)?;
                self.console.stdout().flush()?;
            }
        }
        Ok(())
    }

    fn sess_prop(&mut self, config: &Config) -> ExpandTemplateResult<SessionProp<&mut RW>> {
        let cookies_path = config.session_cookies().expand("")?;
        Ok(SessionProp {
            console: &mut self.console,
            domain: config.service().domain(),
            cookies_path,
            timeout: config.session_timeout(),
            credentials: self.credentials.clone(),
        })
    }
}
