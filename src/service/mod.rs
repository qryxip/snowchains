pub mod session;

pub(crate) mod atcoder;
pub(crate) mod hackerrank;
pub(crate) mod yukicoder;

pub(self) mod downloader;

use config::Config;
use console::{ConsoleReadWrite, Printer};
use errors::SessionResult;
use path::{AbsPath, AbsPathBuf};
use replacer::CodeReplacer;
use service::session::{HttpSession, UrlBase};
use template::{Template, TemplateBuilder};
use testsuite::SerializableExtension;
use {util, Never};

use itertools::Itertools as _Itertools;
use reqwest::header::{Headers, UserAgent};
use reqwest::{self, RedirectPolicy, Response};
use select::document::Document;
use url::Host;

use std::collections::HashMap;
use std::io::{self, Write};
use std::ops::Deref;
use std::rc::Rc;
use std::str::FromStr;
use std::time::Duration;
use std::{self, fmt};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ServiceName {
    Atcoder,
    Hackerrank,
    Yukicoder,
    Other,
}

impl Default for ServiceName {
    fn default() -> Self {
        ServiceName::Other
    }
}

impl fmt::Display for ServiceName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl FromStr for ServiceName {
    type Err = Never;

    fn from_str(s: &str) -> std::result::Result<Self, Never> {
        match s {
            s if s.eq_ignore_ascii_case("atcoder") => Ok(ServiceName::Atcoder),
            s if s.eq_ignore_ascii_case("hackerrank") => Ok(ServiceName::Hackerrank),
            s if s.eq_ignore_ascii_case("yukicoder") => Ok(ServiceName::Yukicoder),
            s if s.eq_ignore_ascii_case("other") => Ok(ServiceName::Other),
            _ => Err(Never),
        }
    }
}

impl ServiceName {
    pub fn to_str(self) -> &'static str {
        match self {
            ServiceName::Atcoder => "atcoder",
            ServiceName::Hackerrank => "hackerrank",
            ServiceName::Yukicoder => "yukicoder",
            ServiceName::Other => "other",
        }
    }

    pub(crate) fn domain(self) -> Option<&'static str> {
        match self {
            ServiceName::Atcoder => Some("beta.atcoder.jp"),
            ServiceName::Hackerrank => Some("www.hackerrank.com"),
            ServiceName::Yukicoder => Some("yukicoder.me"),
            ServiceName::Other => None,
        }
    }
}

pub(self) static USER_AGENT: &str = "snowchains <https://github.com/wariuni/snowchains>";

pub(self) trait Service {
    type Console: ConsoleReadWrite;

    fn session_and_stdout(
        &mut self,
    ) -> (
        &mut HttpSession,
        Printer<&mut <Self::Console as ConsoleReadWrite>::Stdout>,
    );

    fn console(&mut self) -> &mut Self::Console;

    fn stdout(&mut self) -> Printer<&mut <Self::Console as ConsoleReadWrite>::Stdout> {
        self.console().stdout()
    }

    fn stderr(&mut self) -> Printer<&mut <Self::Console as ConsoleReadWrite>::Stderr> {
        self.console().stderr()
    }

    fn get(
        &mut self,
        url: &str,
    ) -> session::Request<Printer<&mut <Self::Console as ConsoleReadWrite>::Stdout>> {
        let (session, stdout) = self.session_and_stdout();
        session.get(url, stdout)
    }

    fn post(
        &mut self,
        url: &str,
    ) -> session::Request<Printer<&mut <Self::Console as ConsoleReadWrite>::Stdout>> {
        let (session, stdout) = self.session_and_stdout();
        session.post(url, stdout)
    }

    fn open_in_browser(&mut self, url: &str) -> SessionResult<()> {
        let (session, stdout) = self.session_and_stdout();
        session.open_in_browser(url, stdout)
    }
}

pub(self) trait TryIntoDocument {
    fn try_into_document(self) -> reqwest::Result<Document>;
}

impl TryIntoDocument for Response {
    fn try_into_document(mut self) -> reqwest::Result<Document> {
        Ok(Document::from(self.text()?.as_str()))
    }
}

#[derive(Clone)]
pub struct Credentials {
    pub atcoder: UserNameAndPassword,
    pub hackerrank: UserNameAndPassword,
    pub yukicoder: RevelSession,
}

impl Default for Credentials {
    fn default() -> Self {
        Self {
            atcoder: UserNameAndPassword::None,
            hackerrank: UserNameAndPassword::None,
            yukicoder: RevelSession::None,
        }
    }
}

#[derive(Clone)]
pub enum UserNameAndPassword {
    None,
    Some(Rc<String>, Rc<String>),
}

impl UserNameAndPassword {
    pub(self) fn is_some(&self) -> bool {
        match self {
            UserNameAndPassword::None => false,
            UserNameAndPassword::Some(..) => true,
        }
    }
}

#[derive(Clone)]
pub enum RevelSession {
    None,
    Some(Rc<String>),
}

#[derive(Serialize, Deserialize)]
pub(crate) struct SessionConfig {
    #[serde(
        serialize_with = "util::ser::secs",
        deserialize_with = "util::de::non_zero_secs"
    )]
    timeout: Option<Duration>,
    cookies: TemplateBuilder<AbsPathBuf>,
}

impl SessionConfig {
    pub(crate) fn timeout(&self) -> Option<Duration> {
        self.timeout
    }

    pub(crate) fn cookies(&self, base_dir: AbsPath, service: ServiceName) -> Template<AbsPathBuf> {
        self.cookies
            .build(base_dir)
            .strings(hashmap!("service".to_owned() => service.to_string()))
    }
}

pub(crate) struct SessionProp<RW: ConsoleReadWrite> {
    pub console: RW,
    pub domain: Option<&'static str>,
    pub cookies_path: AbsPathBuf,
    pub timeout: Option<Duration>,
    pub credentials: Credentials,
}

impl<RW: ConsoleReadWrite> SessionProp<RW> {
    pub(self) fn start_session(&mut self) -> SessionResult<HttpSession> {
        let client = reqwest_client(self.timeout)?;
        let base = self
            .domain
            .map(|domain| UrlBase::new(Host::Domain(domain), true, None));
        HttpSession::new(
            self.console.stdout(),
            client,
            base,
            self.cookies_path.clone(),
        )
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
        }).build()
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
    pub src_paths: HashMap<&'a str, Template<AbsPathBuf>>,
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

    fn write_targets(&self, mut wrt: impl Write) -> io::Result<()> {
        write!(wrt, "Targets: {}/", self.contest())?;
        match self.problems() {
            None => writeln!(wrt, "*"),
            Some(problems) => writeln!(wrt, "{{{}}}", problems.iter().join(", ")),
        }?;
        writeln!(wrt)
    }
}
