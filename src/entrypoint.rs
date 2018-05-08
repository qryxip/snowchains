use config::{self, Config};
use judging::{self, JudgeProp};
use service::{
    atcoder, hackerrank, Credentials, DownloadProp, RestoreProp, SessionProp, SubmitProp,
};
use testsuite::{self, SerializableExtension, SuiteFilePath};
use ServiceName;

use httpsession::ColorMode;

use std::env;
use std::path::PathBuf;

#[derive(StructOpt)]
#[structopt(
    usage = "snowchains <i|init> [directory]\n    \
             snowchains <w|switch> <service> <contest>\n    \
             snowchains <l|login> <service>\n    \
             snowchains <p|participate> <service> <contest>\n    \
             snowchains <d|download> [-s|--service <service>]\
             \n                            [-c|--contest <contest>]\
             \n                            [-b|--open-browser]\n    \
             snowchains <r|restore> [-s|--service <service>]\
             \n                           [-c|--contest <contest>]\n    \
             snowchains <a|append> <target> <extension> <input> [output]\
             \n                                                       [-s|--service <service>]\
             \n                                                       \
             [-c|--contest <contest>]\n    \
             snowchains <j|judge> <target> [-l|--language <language>]\
             \n                                  [-s|--service <service>]\
             \n                                  [-c|--contest <contest>]\n    \
             snowchains <s|submit> <target> [-l|--language <language>]\
             \n                                   [-s|--service <service>]\
             \n                                   [-c|--contest <contest>]\
             \n                                   [-b|--open-browser]\
             \n                                   [-j|--skip-judging]\
             \n                                   [-d|--skip-checking-duplication]"
)]
pub enum Opt {
    #[structopt(
        name = "init",
        about = "Creates a \"snowchains.yaml\"",
        usage = "snowchains <i|init> [directory]",
        raw(display_order = "1", aliases = r#"&["i"]"#)
    )]
    Init {
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
        about = "Changes <service> and <contest> of the \"snowchains.yaml\"",
        usage = "snowchains <w|switch> <service> <contest>",
        raw(display_order = "2", aliases = r#"&["w"]"#)
    )]
    Switch {
        #[structopt(
            name = "service",
            help = "Service name",
            raw(possible_values = r#"&["atcoder", "hackerrank", "other"]"#)
        )]
        service: ServiceName,
        #[structopt(name = "contest", help = "Contest name")]
        contest: String,
    },

    #[structopt(
        name = "login",
        about = "Logges in to a service",
        usage = "snowchains <l|login> <service>",
        raw(display_order = "3", aliases = r#"&["l"]"#)
    )]
    Login {
        #[structopt(
            name = "service",
            help = "Service name",
            raw(possible_values = r#"&["atcoder", "hackerrank"]"#)
        )]
        service: ServiceName,
    },

    #[structopt(
        name = "participate",
        about = "Participates in a contest",
        usage = "snowchains <p|participate> <service> <contest>",
        raw(display_order = "4", aliases = r#"&["p"]"#)
    )]
    Participate {
        #[structopt(
            name = "service", help = "Service name", raw(possible_values = r#"&["atcoder"]"#)
        )]
        service: ServiceName,
        #[structopt(name = "contest", help = "Contest name")]
        contest: String,
    },

    #[structopt(
        name = "download",
        about = "Downloads test cases",
        usage = "snowchains <d|download> [-s|--service <service>]\
                 \n                            [-c|--contest <contest>]\
                 \n                            [-b|--open-browser]",
        raw(display_order = "5", aliases = r#"&["d"]"#)
    )]
    Download {
        #[structopt(
            short = "s",
            long = "service",
            help = "Service name",
            raw(display_order = "1", possible_values = r#"&["atcoder", "hackerrank"]"#)
        )]
        service: Option<ServiceName>,
        #[structopt(short = "c", long = "contest", help = "Contest name", raw(display_order = "2"))]
        contest: Option<String>,
        #[structopt(
            short = "b",
            long = "open-browser",
            help = "Opens the pages with your default browser",
            raw(display_order = "1")
        )]
        open_browser: bool,
    },

    #[structopt(
        name = "restore",
        about = "Downloads the source codes you've submitted",
        usage = "snowchains <r|restore> [-s|--service <service>]\
                 \n                           [-c|--contest <contest>]",
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
        #[structopt(short = "c", long = "contest", help = "Contest name", raw(display_order = "2"))]
        contest: Option<String>,
    },

    #[structopt(
        name = "append",
        about = "Appends a test case to a test suite file",
        usage = "snowchains <a|append> <target> <extension> <input> [output]\
                 \n                                                       [-s|--service <service>]\
                 \n                                                       [-c|--contest <contest>]",
        raw(display_order = "7", aliases = r#"&["a"]"#)
    )]
    Append {
        #[structopt(name = "target", help = "Target name")]
        target: String,
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
        #[structopt(
            short = "s",
            long = "service",
            help = "Service name",
            raw(display_order = "1", possible_values = r#"&["atcoder", "hackerrank", "other"]"#)
        )]
        service: Option<ServiceName>,
        #[structopt(short = "c", long = "contest", help = "Contest name", raw(display_order = "2"))]
        contest: Option<String>,
    },

    #[structopt(
        name = "judge",
        about = "Tests a binary or script",
        usage = "snowchains <j|judge> <target> [-l|--language <language>]\
                 \n                                  [-s|--service <service>]\
                 \n                                  [-c|--contest <contest>]",
        raw(display_order = "8", aliases = r#"&["j"]"#)
    )]
    Judge {
        #[structopt(name = "target", help = "Target name")]
        target: String,
        #[structopt(
            short = "l", long = "language", help = "Language name", raw(display_order = "1")
        )]
        language: Option<String>,
        #[structopt(
            short = "s",
            long = "service",
            help = "Service name",
            raw(display_order = "2", possible_values = r#"&["atcoder", "hackerrank", "other"]"#)
        )]
        service: Option<ServiceName>,
        #[structopt(short = "c", long = "contest", help = "Contest name", raw(display_order = "3"))]
        contest: Option<String>,
    },

    #[structopt(
        name = "submit",
        about = "Submits a source code",
        usage = "snowchains <s|submit> <target> [-l|--language <language>]\
                 \n                                   [-s|--service <service>]\
                 \n                                   [-c|--contest <contest>]\
                 \n                                   [-b|--open-browser]\
                 \n                                   [-j|--skip-judging]\
                 \n                                   [-d|--skip-checking-duplication]",
        raw(display_order = "9", aliases = r#"&["s"]"#)
    )]
    Submit {
        #[structopt(name = "target", help = "Target name")]
        target: String,
        #[structopt(
            short = "l", long = "language", help = "Language name", raw(display_order = "1")
        )]
        language: Option<String>,
        #[structopt(
            short = "s",
            long = "service",
            help = "Service name",
            raw(display_order = "2", possible_values = "&[\"atcoder\"]")
        )]
        service: Option<ServiceName>,
        #[structopt(short = "c", long = "contest", help = "Contest name", raw(display_order = "3"))]
        contest: Option<String>,
        #[structopt(
            short = "b",
            long = "open-browser",
            help = "Opens the pages with your default browser",
            raw(display_order = "1")
        )]
        open_browser: bool,
        #[structopt(
            short = "j", long = "skip-judging", help = "Skips judging", raw(display_order = "2")
        )]
        skip_judging: bool,
        #[structopt(
            short = "d",
            long = "skip-checking-duplication",
            help = "Submits even if the contest is active and your code is already accepted",
            raw(display_order = "3")
        )]
        skip_checking_duplication: bool,
    },
}

impl Opt {
    pub fn run(self, prop: &Prop) -> ::Result<()> {
        match self {
            Opt::Init { directory } => {
                info!("Running \"init\" command");
                let directory = prop.working_dir.join(&directory);
                config::init(&directory, prop.default_lang_on_init)?;
            }
            Opt::Switch { service, contest } => {
                info!("Running \"switch\" command");
                config::switch(service, &contest, &prop.working_dir)?;
            }
            Opt::Login { service } => {
                info!("Running \"login\" command");
                let config = Config::load_from_file(None, None, &prop.working_dir).ok();
                let sess_prop = prop.sess_prop(service, config.as_ref());
                match service {
                    ServiceName::AtCoder => atcoder::login(&sess_prop),
                    ServiceName::HackerRank => hackerrank::login(&sess_prop),
                    ServiceName::Other => unreachable!(),
                }?;
            }
            Opt::Participate { service, contest } => {
                info!("Running \"participate\" command");
                let config = Config::load_from_file(None, None, &prop.working_dir).ok();
                let sess_prop = prop.sess_prop(service, config.as_ref());
                match service {
                    ServiceName::AtCoder => atcoder::participate(&contest, &sess_prop),
                    _ => unreachable!(),
                }?;
            }
            Opt::Download {
                service,
                contest,
                open_browser,
            } => {
                info!("Running \"download\" command");
                let config = Config::load_from_file(service, contest, &prop.working_dir)?;
                let sess_prop = prop.sess_prop(config.service(), &config);
                let download_prop = DownloadProp::new(&config, open_browser)?;
                match config.service() {
                    ServiceName::AtCoder => atcoder::download(&sess_prop, download_prop),
                    ServiceName::HackerRank => hackerrank::download(&sess_prop, &download_prop),
                    ServiceName::Other => bail!(::ErrorKind::Unimplemented),
                }?;
            }
            Opt::Restore { service, contest } => {
                info!("Running \"restore\" command");
                let config = Config::load_from_file(service, contest, &prop.working_dir)?;
                let sess_prop = prop.sess_prop(config.service(), &config);
                let restore_prop = RestoreProp::new(&config)?;
                match config.service() {
                    ServiceName::AtCoder => atcoder::restore(&sess_prop, restore_prop)?,
                    _ => bail!(::ErrorKind::Unimplemented),
                };
            }
            Opt::Append {
                target,
                extension,
                input,
                output,
                service,
                contest,
            } => {
                info!("Running \"append\" command");
                let config = Config::load_from_file(service, contest, &prop.working_dir)?;
                let dir = config.testfiles_dir().expand("")?;
                let path = SuiteFilePath::new(&dir, &target, extension);
                testsuite::append(&path, &input, output.as_ref().map(String::as_str))?;
            }
            Opt::Judge {
                target,
                language,
                service,
                contest,
            } => {
                let language = language.as_ref().map(String::as_str);
                info!("Running \"judge\" command");
                let config = Config::load_from_file(service, contest, &prop.working_dir)?;
                let judge_prop = JudgeProp::new(&config, &target, language)?;
                judging::judge(judge_prop)?;
            }
            Opt::Submit {
                target,
                language,
                service,
                contest,
                open_browser,
                skip_judging,
                skip_checking_duplication,
            } => {
                let language = language.as_ref().map(String::as_str);
                info!("Running \"submit\" command");
                let config = Config::load_from_file(service, contest, &prop.working_dir)?;
                let sess_prop = prop.sess_prop(config.service(), &config);
                let submit_prop = SubmitProp::new(
                    &config,
                    target.clone(),
                    language,
                    open_browser,
                    skip_checking_duplication,
                )?;
                if !skip_judging {
                    judging::judge(JudgeProp::new(&config, &target, language)?)?;
                    println!();
                }
                match config.service() {
                    ServiceName::AtCoder => atcoder::submit(&sess_prop, submit_prop)?,
                    _ => bail!(::ErrorKind::Unimplemented),
                };
            }
        }
        Ok(())
    }
}

pub struct Prop {
    pub working_dir: PathBuf,
    pub default_lang_on_init: Option<&'static str>,
    pub cookie_dir: PathBuf,
    pub color_mode: ColorMode,
    pub credentials: Credentials,
}

impl Prop {
    pub fn new() -> ::Result<Self> {
        let working_dir = env::current_dir()?;
        let cookie_dir = env::home_dir()
            .ok_or_else(|| ::Error::from(::ErrorKind::HomeDirNotFound))?
            .join(".local")
            .join("share")
            .join("snowchains");
        Ok(Self {
            working_dir,
            default_lang_on_init: None,
            cookie_dir,
            color_mode: ColorMode::Prefer256,
            credentials: Credentials::None,
        })
    }

    fn sess_prop<'a, C: Into<Option<&'a Config>>>(
        &self,
        service: ServiceName,
        config: C,
    ) -> SessionProp {
        SessionProp::new(
            service.domain(),
            self.cookie_dir.join(service.as_str()),
            self.color_mode,
            self.credentials.clone(),
            config.into().map(Config::session),
        )
    }
}
