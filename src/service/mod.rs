pub mod session;

pub(crate) mod atcoder;
pub(crate) mod hackerrank;
pub(crate) mod yukicoder;

pub(self) mod downloader;

use crate::config::Config;
use crate::errors::ServiceResult;
use crate::path::AbsPathBuf;
use crate::replacer::CodeReplacer;
use crate::service::session::{HttpSession, UrlBase};
use crate::template::Template;
use crate::terminal::{Term, WriteAnsi};
use crate::testsuite::DownloadDestinations;

use heck::KebabCase as _KebabCase;
use reqwest::header::{self, HeaderMap};
use reqwest::RedirectPolicy;
use serde_derive::{Deserialize, Serialize};
use strum_macros::{AsStaticStr, EnumString};
use tokio::runtime::Runtime;
use url::Host;

use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;
use std::time::Duration;
use std::{fmt, io, slice};

#[derive(
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Debug,
    strum_macros::Display,
    AsStaticStr,
    EnumString,
    Serialize,
    Deserialize,
)]
#[serde(rename_all = "lowercase")]
pub enum ServiceName {
    #[strum(to_string = "atcoder")]
    Atcoder,
    #[strum(to_string = "hackerrank")]
    Hackerrank,
    #[strum(to_string = "yukicoder")]
    Yukicoder,
    #[strum(to_string = "other")]
    Other,
}

impl Default for ServiceName {
    fn default() -> Self {
        ServiceName::Other
    }
}

impl ServiceName {
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

    fn requirements(&mut self) -> (&mut Self::Term, &mut HttpSession, &mut Runtime);

    fn stdout(&mut self) -> &mut <Self::Term as Term>::Stdout {
        self.requirements().0.stdout()
    }

    fn stderr(&mut self) -> &mut <Self::Term as Term>::Stderr {
        self.requirements().0.stderr()
    }

    fn get(&mut self, url: &str) -> session::Request<&mut <Self::Term as Term>::Stdout> {
        let (term, sess, runtime) = self.requirements();
        sess.get(url, term.stdout(), runtime)
    }

    fn post(&mut self, url: &str) -> session::Request<&mut <Self::Term as Term>::Stdout> {
        let (term, sess, runtime) = self.requirements();
        sess.post(url, term.stdout(), runtime)
    }

    fn open_in_browser(&mut self, url: &str) -> ServiceResult<()> {
        let (term, sess, _) = self.requirements();
        sess.open_in_browser(url, term.stdout())
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

pub(crate) struct SessionProps<T: Term> {
    pub(crate) term: T,
    pub(crate) domain: Option<&'static str>,
    pub(crate) cookies_path: AbsPathBuf,
    pub(crate) timeout: Option<Duration>,
    pub(crate) silent: bool,
    pub(crate) credentials: Credentials,
}

impl<T: Term> SessionProps<T> {
    pub(self) fn start_session(&mut self, runtime: &mut Runtime) -> ServiceResult<HttpSession> {
        let client = reqwest_client(self.timeout)?;
        let base = self
            .domain
            .map(|domain| UrlBase::new(Host::Domain(domain), true, None));
        HttpSession::try_new(
            self.term.stdout(),
            runtime,
            client,
            base,
            self.cookies_path.as_path(),
            self.silent,
        )
    }
}

pub(self) fn reqwest_client(
    timeout: impl Into<Option<Duration>>,
) -> reqwest::Result<reqwest::r#async::Client> {
    let builder = reqwest::r#async::Client::builder()
        .redirect(RedirectPolicy::none())
        .referer(false)
        .default_headers({
            let mut headers = HeaderMap::new();
            headers.insert(header::USER_AGENT, USER_AGENT.parse().unwrap());
            headers
        });
    match timeout.into() {
        None => builder,
        Some(timeout) => builder.timeout(timeout),
    }.build()
}

pub(crate) struct DownloadProps<C: Contest> {
    pub(self) contest: C,
    pub(self) problems: Option<Vec<String>>,
    pub(self) destinations: DownloadDestinations,
    pub(self) open_browser: bool,
}

impl DownloadProps<String> {
    pub(crate) fn try_new(
        config: &Config,
        open_browser: bool,
        problems: Vec<String>,
    ) -> crate::Result<Self> {
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
    ) -> DownloadProps<C> {
        DownloadProps {
            contest: C::from_string(self.contest),
            problems: self
                .problems
                .map(|ps| ps.into_iter().map(|p| conversion.convert(&p)).collect()),
            destinations: self.destinations,
            open_browser: self.open_browser,
        }
    }
}

impl<C: Contest> PrintTargets for DownloadProps<C> {
    type Contest = C;

    fn contest(&self) -> &C {
        &self.contest
    }

    fn problems(&self) -> Option<&[String]> {
        self.problems.as_ref().map(Deref::deref)
    }
}

pub(crate) struct RestoreProps<'a, C: Contest> {
    pub(self) contest: C,
    pub(self) problems: Option<Vec<String>>,
    pub(self) src_paths: HashMap<&'a str, Template<AbsPathBuf>>,
    pub(self) replacers: HashMap<&'a str, CodeReplacer>,
}

impl<'a> RestoreProps<'a, String> {
    pub(crate) fn try_new(config: &'a Config, problems: Vec<String>) -> crate::Result<Self> {
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
    ) -> RestoreProps<'a, C> {
        RestoreProps {
            contest: C::from_string(self.contest),
            problems: self
                .problems
                .map(|ps| ps.into_iter().map(|p| conversion.convert(&p)).collect()),
            src_paths: self.src_paths,
            replacers: self.replacers,
        }
    }
}

impl<'a, C: Contest> PrintTargets for RestoreProps<'a, C> {
    type Contest = C;

    fn contest(&self) -> &Self::Contest {
        &self.contest
    }

    fn problems(&self) -> Option<&[String]> {
        self.problems.as_ref().map(Deref::deref)
    }
}

pub(crate) struct SubmitProps<C: Contest> {
    pub(self) contest: C,
    pub(self) problem: String,
    pub(self) lang_id: String,
    pub(self) src_path: AbsPathBuf,
    pub(self) replacer: Option<CodeReplacer>,
    pub(self) open_browser: bool,
    pub(self) skip_checking_if_accepted: bool,
}

impl SubmitProps<String> {
    pub(crate) fn try_new(
        config: &Config,
        problem: String,
        language: Option<&str>,
        open_browser: bool,
        skip_checking_if_accepted: bool,
    ) -> crate::Result<Self> {
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
    ) -> SubmitProps<C> {
        SubmitProps {
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

impl<C: Contest> PrintTargets for SubmitProps<C> {
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
        writeln!(out)?;
        out.flush()
    }
}
