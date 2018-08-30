use config::{self, Config};
use console::{self, ColorChoice, ConsoleReadWrite};
use errors::ExpandTemplateResult;
use judging::{self, JudgeProp};
use path::AbsPathBuf;
use service::{
    atcoder, hackerrank, yukicoder, Credentials, DownloadProp, RestoreProp, SessionProp, SubmitProp,
};
use testsuite::{self, SerializableExtension, SuiteFilePath};
use ServiceName;

use std::borrow::Cow;
use std::io::Write as _Write;
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
             \n    snowchains <j|judge> [OPTIONS] <problem>\
             \n    snowchains <s|submit> [FLAGS] [OPTIONS] <problem>"
)]
pub enum Opt {
    #[structopt(
        name = "init",
        about = "Creates a \"snowchains.yaml\"",
        usage = "snowchains <i|init> [OPTIONS] [directory]",
        raw(display_order = "1", aliases = r#"&["i"]"#)
    )]
    Init {
        #[structopt(
            short = "C",
            long = "color",
            name = "WHEN",
            raw(
                help = "ColorChoice::clap_help()",
                default_value = "ColorChoice::clap_default_value()",
                possible_values = "ColorChoice::clap_possible_values()"
            )
        )]
        color_choice: ColorChoice,
        #[structopt(
            name = "directory",
            help = "Directory to create a \"snowchains.yaml\"",
            parse(from_os_str),
            default_value = "."
        )]
        directory: PathBuf,
    },

    #[structopt(
        name = "switch",
        about = "Changes attribute values of a \"snowchains.yaml\"",
        usage = "snowchains <w|switch> [OPTIONS]",
        raw(display_order = "2", aliases = r#"&["w"]"#)
    )]
    Switch {
        #[structopt(
            short = "s",
            long = "service",
            help = "Service name",
            raw(
                possible_values = r#"&["atcoder", "hackerrank", "yukicoder", "other"]"#,
                display_order = "1"
            )
        )]
        service: Option<ServiceName>,
        #[structopt(
            short = "c",
            long = "contest",
            help = "Contest name",
            raw(display_order = "2")
        )]
        contest: Option<String>,
        #[structopt(
            short = "l",
            long = "language",
            help = "Language name",
            raw(display_order = "3")
        )]
        language: Option<String>,
        #[structopt(
            short = "C",
            long = "color",
            name = "WHEN",
            raw(
                help = "ColorChoice::clap_help()",
                default_value = "ColorChoice::clap_default_value()",
                possible_values = "ColorChoice::clap_possible_values()",
                display_order = "4"
            )
        )]
        color_choice: ColorChoice,
    },

    #[structopt(
        name = "login",
        about = "Logges in to a service",
        usage = "snowchains <l|login> [OPTIONS] <service>",
        raw(display_order = "3", aliases = r#"&["l"]"#)
    )]
    Login {
        #[structopt(
            short = "C",
            long = "color",
            name = "WHEN",
            raw(
                help = "ColorChoice::clap_help()",
                default_value = "ColorChoice::clap_default_value()",
                possible_values = "ColorChoice::clap_possible_values()"
            )
        )]
        color_choice: ColorChoice,
        #[structopt(
            name = "service",
            help = "Service name",
            raw(possible_values = r#"&["atcoder", "hackerrank", "yukicoder"]"#)
        )]
        service: ServiceName,
    },

    #[structopt(
        name = "participate",
        about = "Participates in a contest",
        usage = "snowchains <p|participate> [OPTIONS] <service> <contest>",
        raw(display_order = "4", aliases = r#"&["p"]"#)
    )]
    Participate {
        #[structopt(
            short = "C",
            long = "color",
            name = "WHEN",
            raw(
                help = "ColorChoice::clap_help()",
                default_value = "ColorChoice::clap_default_value()",
                possible_values = "ColorChoice::clap_possible_values()"
            )
        )]
        color_choice: ColorChoice,
        #[structopt(
            name = "service",
            help = "Service name",
            raw(possible_values = r#"&["atcoder"]"#)
        )]
        service: ServiceName,
        #[structopt(name = "contest", help = "Contest name")]
        contest: String,
    },

    #[structopt(
        name = "download",
        about = "Downloads test cases",
        usage = "snowchains <d|download> [FLAGS] [OPTIONS]",
        raw(display_order = "5", aliases = r#"&["d"]"#)
    )]
    Download {
        #[structopt(
            short = "b",
            long = "open-browser",
            help = "Opens the pages with your default browser"
        )]
        open_browser: bool,
        #[structopt(
            short = "s",
            long = "service",
            help = "Service name",
            raw(
                possible_values = r#"&["atcoder", "hackerrank", "yukicoder"]"#,
                display_order = "1"
            )
        )]
        service: Option<ServiceName>,
        #[structopt(
            short = "c",
            long = "contest",
            help = "Contest name",
            raw(display_order = "2")
        )]
        contest: Option<String>,
        #[structopt(
            short = "p",
            long = "problems",
            help = "Problem names",
            raw(display_order = "3")
        )]
        problems: Vec<String>,
        #[structopt(
            short = "C",
            long = "color",
            name = "WHEN",
            raw(
                help = "ColorChoice::clap_help()",
                default_value = "ColorChoice::clap_default_value()",
                possible_values = "ColorChoice::clap_possible_values()",
                display_order = "4"
            )
        )]
        color_choice: ColorChoice,
    },

    #[structopt(
        name = "restore",
        about = "Downloads the source codes you've submitted",
        usage = "snowchains <r|restore> [OPTIONS]",
        raw(display_order = "6", aliases = r#"&["r"]"#)
    )]
    Restore {
        #[structopt(
            short = "s",
            long = "service",
            help = "Service name",
            raw(display_order = "1", possible_values = "&[\"atcoder\"]")
        )]
        service: Option<ServiceName>,
        #[structopt(
            short = "c",
            long = "contest",
            help = "Contest name",
            raw(display_order = "2")
        )]
        contest: Option<String>,
        #[structopt(
            short = "p",
            long = "problems",
            help = "Problem names",
            raw(display_order = "3")
        )]
        problems: Vec<String>,
        #[structopt(
            short = "C",
            long = "color",
            name = "WHEN",
            raw(
                help = "ColorChoice::clap_help()",
                default_value = "ColorChoice::clap_default_value()",
                possible_values = "ColorChoice::clap_possible_values()",
                display_order = "4"
            )
        )]
        color_choice: ColorChoice,
    },

    #[structopt(
        name = "append",
        about = "Appends a test case to a test suite file",
        usage = "snowchains <a|append> [OPTIONS] <problem> <extension> <input> [output]",
        raw(display_order = "7", aliases = r#"&["a"]"#)
    )]
    Append {
        #[structopt(
            short = "s",
            long = "service",
            help = "Service name",
            raw(
                display_order = "1",
                possible_values = r#"&["atcoder", "hackerrank", "other"]"#
            )
        )]
        service: Option<ServiceName>,
        #[structopt(
            short = "c",
            long = "contest",
            help = "Contest name",
            raw(display_order = "2")
        )]
        contest: Option<String>,
        #[structopt(
            short = "C",
            long = "color",
            name = "WHEN",
            raw(
                help = "ColorChoice::clap_help()",
                default_value = "ColorChoice::clap_default_value()",
                possible_values = "ColorChoice::clap_possible_values()",
                display_order = "3"
            )
        )]
        color_choice: ColorChoice,
        #[structopt(name = "problem", help = "Problem name")]
        problem: String,
        #[structopt(
            name = "extension",
            help = "Extension",
            raw(possible_values = r#"&["json", "toml", "yaml", "yml"]"#)
        )]
        extension: SerializableExtension,
        #[structopt(name = "input", help = "\"input\" value to append")]
        input: String,
        #[structopt(name = "output", help = "\"expected\" value to append")]
        output: Option<String>,
    },

    #[structopt(
        name = "judge",
        about = "Tests a binary or script",
        usage = "snowchains <j|judge> [OPTIONS] <problem>",
        raw(display_order = "8", aliases = r#"&["j"]"#)
    )]
    Judge {
        #[structopt(long = "force-compile", help = "Force to compile")]
        force_compile: bool,
        #[structopt(
            short = "s",
            long = "service",
            help = "Service name",
            raw(
                display_order = "1",
                possible_values = r#"&["atcoder", "hackerrank", "yukicoder", "other"]"#
            )
        )]
        service: Option<ServiceName>,
        #[structopt(
            short = "c",
            long = "contest",
            help = "Contest name",
            raw(display_order = "2")
        )]
        contest: Option<String>,
        #[structopt(
            short = "l",
            long = "language",
            help = "Language name",
            raw(display_order = "3")
        )]
        language: Option<String>,
        #[structopt(
            short = "C",
            long = "color",
            name = "WHEN",
            raw(
                help = "ColorChoice::clap_help()",
                default_value = "ColorChoice::clap_default_value()",
                possible_values = "ColorChoice::clap_possible_values()",
                display_order = "4"
            )
        )]
        color_choice: ColorChoice,
        #[structopt(name = "problem", help = "Problem name")]
        problem: String,
    },

    #[structopt(
        name = "submit",
        about = "Submits a source code",
        usage = "snowchains <s|submit> [FLAGS] [OPTIONS] <problem>",
        raw(display_order = "9", aliases = r#"&["s"]"#)
    )]
    Submit {
        #[structopt(
            short = "b",
            long = "open-browser",
            help = "Opens the pages with your default browser"
        )]
        open_browser: bool,
        #[structopt(
            long = "force-compile",
            help = "Force to compile",
            raw(conflicts_with = "\"skip_judging\"")
        )]
        force_compile: bool,
        #[structopt(
            short = "j",
            long = "skip-judging",
            help = "Skips judging",
            raw(conflicts_with = "\"force_compile\"")
        )]
        skip_judging: bool,
        #[structopt(
            short = "d",
            long = "skip-checking-duplication",
            help = "Submits even if the contest is active and your code is already accepted"
        )]
        skip_checking_duplication: bool,
        #[structopt(
            short = "s",
            long = "service",
            help = "Service name",
            raw(
                possible_values = "&[\"atcoder\", \"yukicoder\"]",
                display_order = "1"
            )
        )]
        service: Option<ServiceName>,
        #[structopt(
            short = "c",
            long = "contest",
            help = "Contest name",
            raw(display_order = "2")
        )]
        contest: Option<String>,
        #[structopt(
            short = "l",
            long = "language",
            help = "Language name",
            raw(display_order = "3")
        )]
        language: Option<String>,
        #[structopt(
            short = "C",
            long = "color",
            name = "WHEN",
            raw(
                help = "ColorChoice::clap_help()",
                default_value = "ColorChoice::clap_default_value()",
                possible_values = "ColorChoice::clap_possible_values()",
                display_order = "4"
            )
        )]
        color_choice: ColorChoice,
        #[structopt(name = "problem", help = "Problem name")]
        problem: String,
    },
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
                let dir = config.testfiles_dir().expand("")?;
                let path = SuiteFilePath::new(&dir, &problem, extension);
                let output = output.as_ref().map(String::as_str);
                testsuite::append(&path, &input, output, self.console.stdout())?;
            }
            Opt::Judge {
                force_compile,
                service,
                contest,
                language,
                color_choice,
                problem,
            } => {
                self.console
                    .fill_palettes(color_choice, &console::Conf::default())?;
                let language = language.as_ref().map(String::as_str);
                let config = Config::load(self.console.stdout(), service, contest, &working_dir)?;
                self.console.fill_palettes(color_choice, config.console())?;
                let judge_prop = JudgeProp::new(
                    self.console.stdout_and_stderr(),
                    &config,
                    &problem,
                    language,
                    force_compile,
                )?;
                judging::judge(judge_prop)?;
            }
            Opt::Submit {
                open_browser,
                force_compile,
                skip_judging,
                skip_checking_duplication,
                language,
                service,
                contest,
                color_choice,
                problem,
            } => {
                self.console
                    .fill_palettes(color_choice, &console::Conf::default())?;
                let language = language.as_ref().map(String::as_str);
                let config = Config::load(self.console.stdout(), service, contest, &working_dir)?;
                self.console.fill_palettes(color_choice, config.console())?;
                if !skip_judging {
                    judging::judge(JudgeProp::new(
                        self.console.stdout_and_stderr(),
                        &config,
                        &problem,
                        language,
                        force_compile,
                    )?)?;
                    writeln!(self.console.stdout())?;
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
