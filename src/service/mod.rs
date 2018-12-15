pub mod session;

pub(crate) mod atcoder;
pub(crate) mod hackerrank;
pub(crate) mod yukicoder;

pub(self) mod download;

use crate::config::Config;
use crate::errors::{FileErrorKind, FileResult, ServiceResult};
use crate::path::{AbsPath, AbsPathBuf};
use crate::service::session::{HttpSession, UrlBase};
use crate::template::Template;
use crate::terminal::{Term, WriteAnsi};
use crate::testsuite::DownloadDestinations;
use crate::util;

use failure::ResultExt as _ResultExt;
use heck::KebabCase as _KebabCase;
use maplit::hashmap;
use rayon::iter::{
    IntoParallelIterator as _IntoParalleIterator, ParallelIterator as _ParallelIterator,
};
use regex::Regex;
use reqwest::header::{self, HeaderMap};
use reqwest::RedirectPolicy;
use serde_derive::{Deserialize, Serialize};
use strum_macros::{AsStaticStr, EnumString};
use tokio::runtime::Runtime;
use url::Host;
use zip::result::ZipResult;
use zip::ZipArchive;

use std::collections::HashMap;
use std::io::{self, Cursor, Write as _Write};
use std::ops::Deref;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use std::{cmp, fmt, mem, slice};

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
    type Write: WriteAnsi;

    fn requirements(&mut self) -> (&mut Self::Write, &mut HttpSession, &mut Runtime);

    fn get(&mut self, url: &str) -> session::Request<&mut Self::Write> {
        let (out, sess, runtime) = self.requirements();
        sess.get(url, out, runtime)
    }

    fn post(&mut self, url: &str) -> session::Request<&mut Self::Write> {
        let (out, sess, runtime) = self.requirements();
        sess.post(url, out, runtime)
    }

    fn open_in_browser(&mut self, url: &str) -> ServiceResult<()> {
        let (out, sess, _) = self.requirements();
        sess.open_in_browser(url, out)
    }
}

pub(self) trait ExtractZip {
    type Write: WriteAnsi;

    fn out(&mut self) -> &mut Self::Write;

    fn extract_zip(
        &mut self,
        name: &str,
        zip: &[u8],
        dir: &AbsPath,
        entries: &'static ZipEntries,
    ) -> FileResult<Vec<(String, AbsPathBuf, AbsPathBuf)>> {
        let out = self.out();
        let ZipEntries {
            in_entry,
            in_match_group,
            in_crlf_to_lf,
            out_entry,
            out_match_group,
            out_crlf_to_lf,
            sortings,
        } = entries;

        out.with_reset(|o| o.bold()?.write_str(name))?;
        out.write_str(": Unzipping...\n")?;
        out.flush()?;

        let zip = ZipArchive::new(Cursor::new(zip)).with_context(|_| FileErrorKind::ReadZip)?;
        let pairs = Arc::new(Mutex::new(hashmap!()));

        (0..zip.len())
            .into_par_iter()
            .map(|i| {
                let mut zip = zip.clone();
                let (filename, content) = {
                    let file = zip.by_index(i)?;
                    let filename = file.sanitized_name().to_string_lossy().into_owned();
                    let cap = file.size() as usize + 1;
                    let content = util::string_from_read(file, cap)?;
                    (filename, content)
                };
                if let Some(caps) = in_entry.captures(&filename) {
                    let name = caps.get(*in_match_group).unwrap().as_str().to_owned();
                    let content = if *in_crlf_to_lf && content.contains("\r\n") {
                        content.replace("\r\n", "\n")
                    } else {
                        content
                    };
                    let filename = PathBuf::from(&filename);
                    let mut pairs = pairs.lock().unwrap();
                    if let Some((_, output)) = pairs.remove(&name) {
                        pairs.insert(name, (Some((filename, content)), output));
                    } else {
                        pairs.insert(name, (Some((filename, content)), None));
                    }
                } else if let Some(caps) = out_entry.captures(&filename) {
                    let name = caps.get(*out_match_group).unwrap().as_str().to_owned();
                    let content = if *out_crlf_to_lf && content.contains("\r\n") {
                        content.replace("\r\n", "\n")
                    } else {
                        content
                    };
                    let filename = PathBuf::from(filename);
                    let mut pairs = pairs.lock().unwrap();
                    if let Some((input, _)) = pairs.remove(&name) {
                        pairs.insert(name, (input, Some((filename, content))));
                    } else {
                        pairs.insert(name, (None, Some((filename, content))));
                    }
                }
                Ok(())
            })
            .collect::<ZipResult<()>>()
            .with_context(|_| FileErrorKind::ReadZip)?;

        let mut cases = mem::replace::<HashMap<_, _>>(&mut pairs.lock().unwrap(), hashmap!())
            .into_iter()
            .filter_map(|(name, (input, output))| match (input, output) {
                (Some(input), Some(output)) => Some((name, input, output)),
                _ => None,
            })
            .collect::<Vec<_>>();

        for sorting in sortings {
            match sorting {
                ZipEntriesSorting::Dictionary => cases.sort_by(|(s1, _, _), (s2, _, _)| s1.cmp(s2)),
                ZipEntriesSorting::Number => cases.sort_by(|(s1, _, _), (s2, _, _)| {
                    match (s1.parse::<usize>(), s2.parse::<usize>()) {
                        (Ok(n1), Ok(n2)) => n1.cmp(&n2),
                        (Ok(_), Err(_)) => cmp::Ordering::Less,
                        (Err(_), Ok(_)) => cmp::Ordering::Greater,
                        (Err(_), Err(_)) => cmp::Ordering::Equal,
                    }
                }),
            }
        }

        let ret = cases
            .into_iter()
            .map(|(name, (in_path, in_content), (out_path, out_content))| {
                let (in_path, out_path) = (dir.join(in_path), dir.join(out_path));
                crate::fs::write(&in_path, in_content.as_ref())?;
                crate::fs::write(&out_path, out_content.as_ref())?;
                Ok((name, in_path, out_path))
            })
            .collect::<FileResult<Vec<_>>>()?;

        out.with_reset(|o| o.bold()?.write_str(name))?;
        writeln!(
            out,
            ": Saved {} to {}",
            plural!(ret.len(), "file", "files"),
            dir.display(),
        )?;

        Ok(ret)
    }
}

pub(self) struct ZipEntries {
    pub(self) in_entry: Regex,
    pub(self) in_match_group: usize,
    pub(self) in_crlf_to_lf: bool,
    pub(self) out_entry: Regex,
    pub(self) out_match_group: usize,
    pub(self) out_crlf_to_lf: bool,
    pub(self) sortings: Vec<ZipEntriesSorting>,
}

pub(self) enum ZipEntriesSorting {
    Dictionary,
    Number,
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
    Some(String, String),
}

impl UserNameAndPassword {
    pub(self) fn is_some(&self) -> bool {
        match self {
            UserNameAndPassword::None => false,
            UserNameAndPassword::Some(..) => true,
        }
    }

    pub(self) fn take(&mut self) -> Self {
        mem::replace(self, UserNameAndPassword::None)
    }
}

#[derive(Clone)]
pub enum RevelSession {
    None,
    Some(String),
}

impl RevelSession {
    pub(self) fn take(&mut self) -> Self {
        mem::replace(self, RevelSession::None)
    }
}

pub(crate) struct SessionProps<T: Term> {
    pub(crate) term: T,
    pub(crate) domain: Option<&'static str>,
    pub(crate) cookies_path: AbsPathBuf,
    pub(crate) dropbox_path: Option<AbsPathBuf>,
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
    let mut builder = reqwest::r#async::Client::builder()
        .redirect(RedirectPolicy::none())
        .referer(false)
        .default_headers({
            let mut headers = HeaderMap::new();
            headers.insert(header::USER_AGENT, USER_AGENT.parse().unwrap());
            headers
        });
    if let Some(timeout) = timeout.into() {
        builder = builder.timeout(timeout);
    }
    builder.build()
}

pub(crate) struct DownloadProps<C: Contest> {
    pub(self) contest: C,
    pub(self) problems: Option<Vec<String>>,
    pub(self) destinations: DownloadDestinations,
    pub(self) open_browser: bool,
}

impl DownloadProps<String> {
    pub(crate) fn new(config: &Config, open_browser: bool, problems: Vec<String>) -> Self {
        Self {
            contest: config.contest().to_owned(),
            problems: if problems.is_empty() {
                None
            } else {
                Some(problems)
            },
            destinations: config.download_destinations(None),
            open_browser,
        }
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
}

impl<'a> RestoreProps<'a, String> {
    pub(crate) fn new(config: &'a Config, problems: Vec<String>) -> Self {
        Self {
            contest: config.contest().to_owned(),
            problems: if problems.is_empty() {
                None
            } else {
                Some(problems)
            },
            src_paths: config.src_paths(),
        }
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
    pub(self) lang_id: Option<String>,
    pub(self) src_path: AbsPathBuf,
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
        let lang_id = config.lang_id(service, language).map(ToOwned::to_owned);
        Ok(Self {
            contest,
            problem,
            lang_id,
            src_path,
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
