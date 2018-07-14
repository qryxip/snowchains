pub mod session;

pub(crate) mod atcoder;
pub(crate) mod hackerrank;
pub(crate) mod yukicoder;

pub(self) mod downloader;

use config::Config;
use errors::SessionResult;
use palette::Palette;
use path::{AbsPath, AbsPathBuf};
use replacer::CodeReplacer;
use service::downloader::ZipDownloader;
use service::session::{HttpSession, UrlBase};
use template::{BaseDirNone, BaseDirSome, PathTemplate};
use testsuite::SerializableExtension;
use {util, ServiceName};

use itertools::Itertools as _Itertools;
use reqwest::header::{Headers, UserAgent};
use reqwest::{self, RedirectPolicy};
use tokio_core::reactor::Core;
use url::Host;
use {rpassword, rprompt};

use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;
use std::time::Duration;
use std::{self, env, fmt, io};

pub(self) static USER_AGENT: &str = "snowchains <https://github.com/wariuni/snowchains>";

pub(self) fn ask_yes_or_no(mes: &str, default: bool) -> io::Result<bool> {
    let prompt = format!("{}{} ", mes, if default { "(Y/n)" } else { "(y/N)" });
    loop {
        match &rprompt::prompt_reply_stderr(&prompt)? {
            s if s.is_empty() => break Ok(default),
            s if s.eq_ignore_ascii_case("y") || s.eq_ignore_ascii_case("yes") => break Ok(true),
            s if s.eq_ignore_ascii_case("n") || s.eq_ignore_ascii_case("no") => break Ok(false),
            _ => eprintln!("Answer \"y\", \"yes\", \"n\", \"no\", or \"\"."),
        }
    }
}

/// Asks username and password.
pub(self) fn ask_credentials(username_prompt: &str) -> io::Result<(String, String)> {
    let username = rprompt::prompt_reply_stderr(username_prompt)?;
    let password = rpassword::prompt_password_stderr("Password: ").or_else(|e| match e.kind() {
        io::ErrorKind::BrokenPipe => {
            eprintln!("{}", Palette::Warning.paint("broken pipe"));
            rprompt::prompt_reply_stderr("Password (not hidden): ")
        }
        _ => Err(e),
    })?;
    Ok((username, password))
}

#[derive(Clone)]
pub enum Credentials {
    None,
    RevelSession(Rc<String>),
    UserNameAndPassword(Rc<String>, Rc<String>),
}

impl Credentials {
    pub fn from_env_vars(
        username: &str,
        password: &str,
    ) -> std::result::Result<Self, env::VarError> {
        let (username, password) = (Rc::new(env::var(username)?), Rc::new(env::var(password)?));
        Ok(Credentials::UserNameAndPassword(username, password))
    }

    pub(self) fn or_ask(&self, username_prompt: &str) -> io::Result<(Rc<String>, Rc<String>)> {
        if let Credentials::UserNameAndPassword(username, password) = self {
            Ok((username.clone(), password.clone()))
        } else {
            let (username, password) = ask_credentials(username_prompt)?;
            Ok((Rc::new(username), Rc::new(password)))
        }
    }

    pub(self) fn not_none(&self) -> bool {
        match self {
            Credentials::None => false,
            _ => true,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub(crate) struct SessionConfig {
    #[serde(serialize_with = "util::ser::secs", deserialize_with = "util::de::non_zero_secs")]
    timeout: Option<Duration>,
    cookies: PathTemplate<BaseDirNone>,
}

impl SessionConfig {
    pub(crate) fn timeout(&self) -> Option<Duration> {
        self.timeout
    }

    pub(crate) fn cookies<'a>(
        &self,
        base: AbsPath<'a>,
        service: ServiceName,
    ) -> PathTemplate<BaseDirSome<'a>> {
        self.cookies
            .base_dir(base)
            .embed_strings(&hashmap!("service" => service.as_str()))
    }
}

pub(crate) struct SessionProp {
    pub domain: Option<&'static str>,
    pub cookies_path: AbsPathBuf,
    pub timeout: Option<Duration>,
    pub credentials: Credentials,
}

impl SessionProp {
    pub(self) fn start_session(&self) -> SessionResult<HttpSession> {
        let client = reqwest_client(self.timeout)?;
        let base = self
            .domain
            .map(|domain| UrlBase::new(Host::Domain(domain), true, None));
        HttpSession::new(client, base, self.cookies_path.clone())
    }

    pub(self) fn zip_downloader(&self) -> SessionResult<ZipDownloader> {
        let core = Core::new()?;
        let mut builder = reqwest::unstable::async::Client::builder();
        if let Some(timeout) = self.timeout {
            builder.timeout(timeout);
        }
        let client = builder
            .redirect(RedirectPolicy::none())
            .referer(false)
            .default_headers({
                let mut headers = Headers::new();
                headers.set(UserAgent::new(USER_AGENT));
                headers
            })
            .build(&core.handle())?;
        Ok(ZipDownloader::new(client, core))
    }
}

pub(self) fn reqwest_client(
    timeout: impl Into<Option<Duration>>,
) -> reqwest::Result<reqwest::Client> {
    reqwest::Client::builder()
        .redirect(RedirectPolicy::none())
        .timeout(timeout)
        .referer(false)
        .default_headers({
            let mut headers = Headers::new();
            headers.set(UserAgent::new(USER_AGENT));
            headers
        })
        .build()
}

pub(crate) struct DownloadProp<C: Contest> {
    pub contest: C,
    pub problems: Option<Vec<String>>,
    pub download_dir: AbsPathBuf,
    pub extension: SerializableExtension,
    pub open_browser: bool,
}

impl DownloadProp<String> {
    pub fn new(config: &Config, open_browser: bool, problems: Vec<String>) -> ::Result<Self> {
        let download_dir = config.testfiles_dir().expand("")?;
        Ok(Self {
            contest: config.contest().to_owned(),
            problems: if problems.is_empty() {
                None
            } else {
                Some(problems)
            },
            download_dir,
            extension: config.extension_on_scrape(),
            open_browser,
        })
    }

    pub(self) fn parse_contest<C: Contest>(self) -> DownloadProp<C> {
        DownloadProp {
            contest: C::from_string(self.contest),
            problems: self.problems,
            download_dir: self.download_dir,
            extension: self.extension,
            open_browser: self.open_browser,
        }
    }
}

impl<C: Contest> PrintTargets for DownloadProp<C> {
    type Contest = C;

    fn contest(&self) -> &C {
        &self.contest
    }

    fn problems(&self) -> Option<&[String]> {
        self.problems.as_ref().map(Deref::deref)
    }
}

impl<C: Contest> DownloadProp<C> {
    pub(self) fn lowerize_problems(self) -> Self {
        Self {
            problems: self
                .problems
                .map(|ps| ps.into_iter().map(|p| p.to_lowercase()).collect()),
            ..self
        }
    }
}

pub(crate) struct RestoreProp<'a, C: Contest> {
    pub contest: C,
    pub problems: Option<Vec<String>>,
    pub src_paths: HashMap<&'a str, PathTemplate<BaseDirSome<'a>>>,
    pub replacers: HashMap<&'a str, CodeReplacer>,
}

impl<'a> RestoreProp<'a, String> {
    pub fn new(config: &'a Config, problems: Vec<String>) -> ::Result<Self> {
        let replacers = config.code_replacers_on_atcoder()?;
        Ok(Self {
            contest: config.contest().to_owned(),
            problems: if problems.is_empty() {
                None
            } else {
                Some(problems)
            },
            src_paths: config.src_paths(),
            replacers,
        })
    }

    pub(self) fn parse_contest<C: Contest>(self) -> RestoreProp<'a, C> {
        RestoreProp {
            contest: C::from_string(self.contest),
            problems: self.problems,
            src_paths: self.src_paths,
            replacers: self.replacers,
        }
    }
}

impl<'a, C: Contest> RestoreProp<'a, C> {
    pub(self) fn upperize_problems(self) -> Self {
        Self {
            problems: self
                .problems
                .map(|ps| ps.into_iter().map(|p| p.to_uppercase()).collect()),
            ..self
        }
    }
}

pub(crate) struct SubmitProp<C: Contest> {
    pub contest: C,
    pub problem: String,
    pub lang_id: String,
    pub src_path: AbsPathBuf,
    pub replacer: Option<CodeReplacer>,
    pub open_browser: bool,
    pub skip_checking_if_accepted: bool,
}

impl SubmitProp<String> {
    pub fn new(
        config: &Config,
        problem: String,
        language: Option<&str>,
        open_browser: bool,
        skip_checking_if_accepted: bool,
    ) -> ::Result<Self> {
        let service = config.service();
        let contest = config.contest().to_owned();
        let src_path = config.src_to_submit(language)?.expand(&problem)?;
        let replacer = config.code_replacer(language)?;
        let lang_id = config.lang_id(service, language)?.to_owned();
        Ok(Self {
            contest,
            problem,
            lang_id,
            src_path,
            replacer,
            open_browser,
            skip_checking_if_accepted,
        })
    }

    pub(self) fn parse_contest<C: Contest>(self) -> SubmitProp<C> {
        SubmitProp {
            contest: C::from_string(self.contest),
            problem: self.problem,
            lang_id: self.lang_id,
            src_path: self.src_path,
            replacer: self.replacer,
            open_browser: self.open_browser,
            skip_checking_if_accepted: self.skip_checking_if_accepted,
        }
    }
}

pub(crate) trait Contest: fmt::Display {
    fn from_string(s: String) -> Self;
}

impl Contest for String {
    fn from_string(s: String) -> Self {
        s
    }
}

pub(self) trait PrintTargets {
    type Contest: Contest;

    fn contest(&self) -> &Self::Contest;
    fn problems(&self) -> Option<&[String]>;

    fn print_targets(&self) {
        print!("Targets: {}/", self.contest());
        match self.problems() {
            None => println!("*"),
            Some(problems) => println!("{{{}}}", problems.iter().join(", ")),
        }
    }
}
