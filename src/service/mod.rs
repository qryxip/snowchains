pub mod session;

pub(crate) mod atcoder;
pub(crate) mod codeforces;
pub(crate) mod yukicoder;

pub(self) mod download;

use crate::config::{self, Config};
use crate::errors::{ConfigResult, FileErrorKind, FileResult, ServiceResult};
use crate::path::{AbsPath, AbsPathBuf};
use crate::service::session::{HttpSession, HttpSessionInitParams, UrlBase};
use crate::template::Template;
use crate::terminal::WriteAnsi;
use crate::testsuite::{DownloadDestinations, SuiteFilePath, TestSuite};
use crate::util;
use crate::util::collections::{NonEmptyIndexMap, NonEmptyVec};
use crate::util::str::CaseConversion;

use failure::ResultExt as _;
use heck::{CamelCase as _, KebabCase as _, MixedCase as _, SnakeCase as _};
use maplit::hashmap;
use prettytable::{cell, row, Table};
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};
use regex::Regex;
use reqwest::header::{self, HeaderMap};
use reqwest::RedirectPolicy;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_derive::Serialize;
use strum_macros::IntoStaticStr;
use tokio::runtime::Runtime;
use url::{Host, Url};
use zip::result::ZipResult;
use zip::ZipArchive;

use std::borrow::Cow;
use std::collections::HashMap;
use std::io::{self, Cursor, Write as _};
use std::ops::Deref;
use std::str::FromStr;
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
    IntoStaticStr,
    Serialize,
)]
#[serde(rename_all = "lowercase")]
#[strum(serialize_all = "snake_case")]
pub enum ServiceKind {
    Atcoder,
    Codeforces,
    Yukicoder,
    Other,
}

impl ServiceKind {
    pub(crate) fn domain(self) -> Option<&'static str> {
        match self {
            ServiceKind::Atcoder => Some("atcoder.jp"),
            ServiceKind::Codeforces => Some("codeforces.com"),
            ServiceKind::Yukicoder => Some("yukicoder.me"),
            ServiceKind::Other => None,
        }
    }
}

impl Default for ServiceKind {
    fn default() -> Self {
        ServiceKind::Other
    }
}

impl FromStr for ServiceKind {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, &'static str> {
        match s {
            "atcoder" => Ok(ServiceKind::Atcoder),
            "codeforces" => Ok(ServiceKind::Codeforces),
            "yukicoder" => Ok(ServiceKind::Yukicoder),
            "other" => Ok(ServiceKind::Other),
            _ => Err(r#"expected "atcoder", "codeforces", "yukicoder", or "other""#),
        }
    }
}

impl<'de> Deserialize<'de> for ServiceKind {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        String::deserialize(deserializer)?
            .parse::<Self>()
            .map_err(serde::de::Error::custom)
    }
}

pub(self) static USER_AGENT: &str = "snowchains <https://github.com/qryxip/snowchains>";

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

    fn print_lang_list(&mut self, lang_list: &NonEmptyIndexMap<String, String>) -> io::Result<()> {
        let (out, _, _) = self.requirements();

        let mut table = Table::new();
        table.add_row(row!["Name", "ID"]);
        for (name, id) in lang_list {
            table.add_row(row![name, id]);
        }

        write!(out, "{}", table)?;
        out.flush()
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
                let (filename, filename_sanitized, content) = {
                    let file = zip.by_index(i)?;
                    let filename = file.name().to_owned();
                    let filename_sanitized = file.sanitized_name();
                    let cap = file.size() as usize + 1;
                    let content = util::string_from_read(file, cap)?;
                    (filename, filename_sanitized, content)
                };
                if let Some(caps) = in_entry.captures(&filename) {
                    let name = caps[*in_match_group].to_owned();
                    let content = if *in_crlf_to_lf && content.contains("\r\n") {
                        content.replace("\r\n", "\n")
                    } else {
                        content
                    };
                    let mut pairs = pairs.lock().unwrap();
                    if let Some((_, output)) = pairs.remove(&name) {
                        pairs.insert(name, (Some((filename_sanitized, content)), output));
                    } else {
                        pairs.insert(name, (Some((filename_sanitized, content)), None));
                    }
                } else if let Some(caps) = out_entry.captures(&filename) {
                    let name = caps[*out_match_group].to_owned();
                    let content = if *out_crlf_to_lf && content.contains("\r\n") {
                        content.replace("\r\n", "\n")
                    } else {
                        content
                    };
                    let mut pairs = pairs.lock().unwrap();
                    if let Some((input, _)) = pairs.remove(&name) {
                        pairs.insert(name, (input, Some((filename_sanitized, content))));
                    } else {
                        pairs.insert(name, (None, Some((filename_sanitized, content))));
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
            plural!(2 * ret.len(), "file", "files"),
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

#[derive(Serialize)]
pub(crate) struct DownloadOutcome {
    service: ServiceKind,
    open_in_browser: bool,
    contest: DownloadOutcomeContest,
    pub(self) problems: Vec<DownloadOutcomeProblem>,
}

#[derive(Serialize)]
struct DownloadOutcomeContest {
    slug: String,
    slug_lower_case: String,
    slug_upper_case: String,
    slug_snake_case: String,
    slug_kebab_case: String,
    slug_mixed_case: String,
    slug_pascal_case: String,
}

#[derive(Serialize)]
pub(self) struct DownloadOutcomeProblem {
    #[serde(serialize_with = "ser_as_str")]
    pub(self) url: Url,
    pub(self) name: String,
    name_lower_case: String,
    name_upper_case: String,
    name_snake_case: String,
    name_kebab_case: String,
    name_mixed_case: String,
    name_pascal_case: String,
    #[serde(serialize_with = "ser_as_path")]
    pub(self) test_suite_path: SuiteFilePath,
    pub(self) test_suite: TestSuite,
}

fn ser_as_str<S: Serializer>(url: &Url, serializer: S) -> std::result::Result<S::Ok, S::Error> {
    url.as_str().serialize(serializer)
}

fn ser_as_path<S: Serializer>(
    path: &SuiteFilePath,
    serializer: S,
) -> std::result::Result<S::Ok, S::Error> {
    AsRef::<AbsPath>::as_ref(path).serialize(serializer)
}

impl DownloadOutcome {
    pub(self) fn new(service: ServiceKind, contest: &impl Contest, open_in_browser: bool) -> Self {
        Self {
            service,
            open_in_browser,
            contest: DownloadOutcomeContest {
                slug: contest.slug().into(),
                slug_lower_case: contest.slug().to_lowercase(),
                slug_upper_case: contest.slug().to_uppercase(),
                slug_snake_case: contest.slug().to_snake_case(),
                slug_kebab_case: contest.slug().to_kebab_case(),
                slug_mixed_case: contest.slug().to_mixed_case(),
                slug_pascal_case: contest.slug().to_camel_case(),
            },
            problems: vec![],
        }
    }

    pub(self) fn push_problem(
        &mut self,
        name: String,
        url: Url,
        suite: TestSuite,
        path: SuiteFilePath,
    ) {
        self.problems.push(DownloadOutcomeProblem {
            url,
            name_lower_case: name.to_lowercase(),
            name_upper_case: name.to_uppercase(),
            name_snake_case: name.to_snake_case(),
            name_kebab_case: name.to_kebab_case(),
            name_mixed_case: name.to_mixed_case(),
            name_pascal_case: name.to_camel_case(),
            name,
            test_suite_path: path,
            test_suite: suite,
        })
    }
}

pub(crate) struct SessionProps {
    pub(crate) domain: Option<&'static str>,
    pub(crate) cookies_path: AbsPathBuf,
    pub(crate) api_token_path: AbsPathBuf,
    pub(crate) dropbox_path: Option<AbsPathBuf>,
    pub(crate) timeout: Option<Duration>,
    pub(crate) login_retries: Option<u32>,
    pub(crate) silent: bool,
    pub(crate) robots: bool,
}

impl SessionProps {
    pub(self) fn start_session(
        &self,
        out: impl WriteAnsi,
        runtime: &mut Runtime,
    ) -> ServiceResult<HttpSession> {
        let client = reqwest_async_client(self.timeout)?;
        let base = self
            .domain
            .map(|domain| UrlBase::new(Host::Domain(domain), true, None));
        HttpSession::try_new(HttpSessionInitParams {
            out,
            runtime,
            robots: self.robots,
            client,
            base,
            cookies_path: Some(self.cookies_path.as_path()),
            api_token_path: Some(self.api_token_path.as_path()),
            silent: self.silent,
        })
    }
}

pub(self) fn reqwest_async_client(
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

#[cfg(test)]
pub(self) fn reqwest_sync_client(
    timeout: impl Into<Option<Duration>>,
) -> reqwest::Result<reqwest::Client> {
    let mut builder = reqwest::Client::builder()
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
    pub(crate) contest: C,
    pub(crate) problems: Option<NonEmptyVec<String>>,
    pub(crate) destinations: DownloadDestinations,
    pub(crate) open_in_browser: bool,
    pub(crate) only_scraped: bool,
}

impl DownloadProps<String> {
    pub(self) fn convert_problems(self, conversion: CaseConversion) -> Self {
        Self {
            problems: self.problems.map(|ps| ps.map(|p| conversion.apply(&p))),
            ..self
        }
    }

    pub(self) fn parse_contest<C: Contest>(
        self,
    ) -> std::result::Result<DownloadProps<C>, <C as FromStr>::Err> {
        Ok(DownloadProps {
            contest: self.contest.parse()?,
            problems: self.problems,
            destinations: self.destinations,
            open_in_browser: self.open_in_browser,
            only_scraped: self.only_scraped,
        })
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
    pub(self) problems: Option<NonEmptyVec<String>>,
    pub(self) src_paths: HashMap<&'a str, Template<AbsPathBuf>>,
}

impl<'a> RestoreProps<'a, String> {
    pub(crate) fn new(
        config: &'a Config,
        mode: config::Mode,
        problems: Vec<String>,
    ) -> ConfigResult<Self> {
        let src_paths = config.src_paths(mode)?;
        Ok(Self {
            contest: config.contest().to_owned(),
            problems: NonEmptyVec::try_new(problems),
            src_paths,
        })
    }

    pub(self) fn convert_problems(self, conversion: CaseConversion) -> Self {
        Self {
            problems: self.problems.map(|ps| ps.map(|p| conversion.apply(&p))),
            ..self
        }
    }

    pub(self) fn parse_contest<C: Contest>(
        self,
    ) -> std::result::Result<RestoreProps<'a, C>, <C as FromStr>::Err> {
        Ok(RestoreProps {
            contest: self.contest.parse()?,
            problems: self.problems,
            src_paths: self.src_paths,
        })
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
    pub(self) lang_name: String,
    pub(self) src_path: AbsPathBuf,
    pub(self) open_in_browser: bool,
    pub(self) skip_checking_if_accepted: bool,
}

impl SubmitProps<String> {
    pub(crate) fn try_new(
        config: &Config,
        mode: config::Mode,
        problem: String,
        open_in_browser: bool,
        skip_checking_if_accepted: bool,
    ) -> crate::Result<Self> {
        let contest = config.contest().to_owned();
        let src_path = config.src_to_submit(mode)?.expand(Some(&problem))?;
        let lang_name = config.lang_name()?.to_owned();
        Ok(Self {
            contest,
            problem,
            lang_name,
            src_path,
            open_in_browser,
            skip_checking_if_accepted,
        })
    }

    pub(self) fn convert_problem(self, conversion: CaseConversion) -> Self {
        Self {
            problem: conversion.apply(&self.problem),
            ..self
        }
    }

    pub(self) fn parse_contest<C: Contest>(
        self,
    ) -> std::result::Result<SubmitProps<C>, <C as FromStr>::Err> {
        Ok(SubmitProps {
            contest: self.contest.parse()?,
            problem: self.problem,
            lang_name: self.lang_name,
            src_path: self.src_path,
            open_in_browser: self.open_in_browser,
            skip_checking_if_accepted: self.skip_checking_if_accepted,
        })
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

pub(crate) struct ListLangsProps<C: Contest> {
    pub(crate) contest: C,
    pub(crate) problem: Option<String>,
}

impl ListLangsProps<String> {
    pub(self) fn convert_problem(self, conversion: CaseConversion) -> Self {
        Self {
            contest: self.contest,
            problem: self.problem.map(|p| conversion.apply(&p)),
        }
    }

    pub(self) fn parse_contest<C: Contest>(
        self,
    ) -> std::result::Result<ListLangsProps<C>, <C as FromStr>::Err> {
        Ok(ListLangsProps {
            contest: self.contest.parse()?,
            problem: self.problem,
        })
    }
}

impl<C: Contest> PrintTargets for ListLangsProps<C> {
    type Contest = C;

    fn contest(&self) -> &C {
        &self.contest
    }

    fn problems(&self) -> Option<&[String]> {
        self.problem.as_ref().map(slice::from_ref)
    }
}

pub(crate) trait Contest: fmt::Display + FromStr {
    fn slug(&self) -> Cow<str>;
}

impl Contest for String {
    fn slug(&self) -> Cow<str> {
        self.as_str().into()
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
