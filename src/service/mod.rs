pub mod session;

pub(crate) mod atcoder;
pub(crate) mod hackerrank;
pub(crate) mod yukicoder;

pub(self) mod downloader;

use config::Config;
use errors::SessionResult;
use path::{AbsPath, AbsPathBuf};
use replacer::CodeReplacer;
use service::session::{HttpSession, UrlBase};
use template::{Template, TemplateBuilder};
use terminal::{Term, WriteAnsi};
use testsuite::DownloadDestinations;
use {time, Never};

use heck::KebabCase as _KebabCase;
use maplit::hashmap;
use reqwest::header::{self, HeaderMap};
use reqwest::{RedirectPolicy, Response};
use select::document::Document;
use serde_derive::{Deserialize, Serialize};
use url::Host;

use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;
use std::str::FromStr;
use std::time::Duration;
use std::{fmt, io, slice};

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
            _ => unreachable!(),
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
    type Term: Term;

    fn session_and_term(&mut self) -> (&mut HttpSession, &mut Self::Term);

    fn stdout(&mut self) -> &mut <Self::Term as Term>::Stdout {
        self.session_and_term().1.stdout()
    }

    fn stderr(&mut self) -> &mut <Self::Term as Term>::Stderr {
        self.session_and_term().1.stderr()
    }

    fn get(&mut self, url: &str) -> session::Request<&mut <Self::Term as Term>::Stdout> {
        let (sess, term) = self.session_and_term();
        sess.get(url, term.stdout())
    }

    fn post(&mut self, url: &str) -> session::Request<&mut <Self::Term as Term>::Stdout> {
        let (sess, term) = self.session_and_term();
        sess.post(url, term.stdout())
    }

    fn open_in_browser(&mut self, url: &str) -> SessionResult<()> {
        let (sess, term) = self.session_and_term();
        sess.open_in_browser(url, term.stdout())
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
        serialize_with = "time::ser_secs",
        deserialize_with = "time::de_secs",
    )]
    timeout: Option<Duration>,
    cookies: TemplateBuilder<AbsPathBuf>,
}

impl SessionConfig {
    pub(crate) fn timeout(&self) -> Option<Duration> {
        self.timeout
    }

    pub(crate) fn cookies(&self, base_dir: &AbsPath, service: ServiceName) -> Template<AbsPathBuf> {
        self.cookies
            .build(base_dir)
            .strings(hashmap!("service".to_owned() => service.to_string()))
    }
}

pub(crate) struct SessionProp<T: Term> {
    pub term: T,
    pub domain: Option<&'static str>,
    pub cookies_path: AbsPathBuf,
    pub timeout: Option<Duration>,
    pub credentials: Credentials,
}

impl<T: Term> SessionProp<T> {
    pub(self) fn start_session(&mut self) -> SessionResult<HttpSession> {
        let client = reqwest_client(self.timeout)?;
        let base = self
            .domain
            .map(|domain| UrlBase::new(Host::Domain(domain), true, None));
        HttpSession::try_new(self.term.stdout(), client, base, self.cookies_path.clone())
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
            let mut headers = HeaderMap::new();
            headers.insert(header::USER_AGENT, USER_AGENT.parse().unwrap());
            headers
        }).build()
}

pub(crate) struct DownloadProp<C: Contest> {
    pub contest: C,
    pub problems: Option<Vec<String>>,
    pub destinations: DownloadDestinations,
    pub open_browser: bool,
}

impl DownloadProp<String> {
    pub fn try_new(config: &Config, open_browser: bool, problems: Vec<String>) -> ::Result<Self> {
        let destinations = config.download_destinations(None);
        Ok(Self {
            contest: config.contest().to_owned(),
            problems: if problems.is_empty() {
                None
            } else {
                Some(problems)
            },
            destinations,
            open_browser,
        })
    }

    pub(self) fn convert_contest_and_problems<C: Contest>(
        self,
        conversion: ProblemNameConversion,
    ) -> DownloadProp<C> {
        DownloadProp {
            contest: C::from_string(self.contest),
            problems: self
                .problems
                .map(|ps| ps.into_iter().map(|p| conversion.convert(&p)).collect()),
            destinations: self.destinations,
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

pub(crate) struct RestoreProp<'a, C: Contest> {
    pub contest: C,
    pub problems: Option<Vec<String>>,
    pub src_paths: HashMap<&'a str, Template<AbsPathBuf>>,
    pub replacers: HashMap<&'a str, CodeReplacer>,
}

impl<'a> RestoreProp<'a, String> {
    pub fn try_new(config: &'a Config, problems: Vec<String>) -> ::Result<Self> {
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

    pub(self) fn convert_contest_and_problems<C: Contest>(
        self,
        conversion: ProblemNameConversion,
    ) -> RestoreProp<'a, C> {
        RestoreProp {
            contest: C::from_string(self.contest),
            problems: self
                .problems
                .map(|ps| ps.into_iter().map(|p| conversion.convert(&p)).collect()),
            src_paths: self.src_paths,
            replacers: self.replacers,
        }
    }
}

impl<'a, C: Contest> PrintTargets for RestoreProp<'a, C> {
    type Contest = C;

    fn contest(&self) -> &Self::Contest {
        &self.contest
    }

    fn problems(&self) -> Option<&[String]> {
        self.problems.as_ref().map(Deref::deref)
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
    pub fn try_new(
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

    pub(self) fn convert_contest_and_problem<C: Contest>(
        self,
        conversion: ProblemNameConversion,
    ) -> SubmitProp<C> {
        SubmitProp {
            contest: C::from_string(self.contest),
            problem: conversion.convert(&self.problem),
            lang_id: self.lang_id,
            src_path: self.src_path,
            replacer: self.replacer,
            open_browser: self.open_browser,
            skip_checking_if_accepted: self.skip_checking_if_accepted,
        }
    }
}

impl<C: Contest> PrintTargets for SubmitProp<C> {
    type Contest = C;

    fn contest(&self) -> &Self::Contest {
        &self.contest
    }

    fn problems(&self) -> Option<&[String]> {
        Some(slice::from_ref(&self.problem))
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

#[derive(Clone, Copy)]
pub(self) enum ProblemNameConversion {
    Upper,
    Kebab,
}

impl ProblemNameConversion {
    fn convert(self, s: &str) -> String {
        match self {
            ProblemNameConversion::Upper => s.to_uppercase(),
            ProblemNameConversion::Kebab => s.to_kebab_case(),
        }
    }
}

pub(self) trait PrintTargets {
    type Contest: Contest;

    fn contest(&self) -> &Self::Contest;
    fn problems(&self) -> Option<&[String]>;

    fn print_targets(&self, mut out: impl WriteAnsi) -> io::Result<()> {
        fn print_common(
            mut out: impl WriteAnsi,
            multi: bool,
            contest: impl fmt::Display,
        ) -> io::Result<()> {
            let s = if multi { "Targets" } else { "Target" };
            out.with_reset(|o| o.fg(13)?.bold()?.write_str(s))?;
            out.with_reset(|o| o.fg(13)?.write_str(":"))?;
            write!(out, " {}/", contest)
        }

        match self.problems() {
            None => {
                print_common(&mut out, true, self.contest())?;
                out.with_reset(|o| o.bold()?.write_str("*"))?;
            }
            Some([problem]) => {
                print_common(&mut out, false, self.contest())?;
                out.with_reset(|o| o.bold()?.write_str(problem))?;
            }
            Some(problems) => {
                print_common(&mut out, true, self.contest())?;
                out.write_str("{")?;
                for (i, problem) in problems.iter().enumerate() {
                    if i > 0 {
                        out.write_all(b", ")?;
                    }
                    out.with_reset(|o| o.bold()?.write_str(problem))?;
                }
                out.write_str("}")?;
            }
        }
        writeln!(out)
    }
}
