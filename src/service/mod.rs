pub mod session;

pub(crate) mod atcoder;
pub(crate) mod codeforces;
pub(crate) mod yukicoder;

pub(self) mod download;

use crate::config::{self, Config};
use crate::errors::{ConfigResult, FileErrorKind, FileResult, ServiceResult};
use crate::path::{AbsPath, AbsPathBuf};
use crate::service::session::{
    HttpSession, HttpSessionInitParams, IntoRelativeOrAbsoluteUrl, UrlBase,
};
use crate::template::Template;
use crate::terminal::WriteAnsi;
use crate::testsuite::{Destinations, SuiteFilePath, TestSuite};
use crate::util;
use crate::util::collections::{NonEmptyIndexMap, NonEmptyIndexSet, NonEmptyVec};
use crate::util::str::CaseConversion;

use chrono::{DateTime, FixedOffset};
use failure::ResultExt as _;
use heck::{CamelCase as _, KebabCase as _, MixedCase as _, SnakeCase as _};
use indexmap::IndexMap;
use maplit::hashmap;
use prettytable::{cell, row, Table};
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};
use regex::Regex;
use reqwest::header::{self, HeaderMap};
use reqwest::RedirectPolicy;
use serde::{Deserialize, Deserializer};
use serde_derive::Serialize;
use strum_macros::IntoStaticStr;
use tokio::runtime::Runtime;
use url::{Host, Url};
use zip::result::ZipResult;
use zip::ZipArchive;

use std::borrow::Cow;
use std::collections::HashMap;
use std::io::{self, Cursor, Write as _};
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use std::{cmp, fmt, mem};

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
    type Stderr: WriteAnsi;

    fn requirements(&mut self) -> (&mut Self::Stderr, &mut HttpSession, &mut Runtime);

    fn get(&mut self, url: impl IntoRelativeOrAbsoluteUrl) -> session::Request<&mut Self::Stderr> {
        let (stderr, sess, runtime) = self.requirements();
        sess.get(url, stderr, runtime)
    }

    fn post(&mut self, url: impl IntoRelativeOrAbsoluteUrl) -> session::Request<&mut Self::Stderr> {
        let (stderr, sess, runtime) = self.requirements();
        sess.post(url, stderr, runtime)
    }

    fn open_in_browser(&mut self, url: impl IntoRelativeOrAbsoluteUrl) -> ServiceResult<()> {
        let (stderr, sess, _) = self.requirements();
        sess.open_in_browser(url, stderr)
    }

    fn print_lang_list(&mut self, lang_list: &NonEmptyIndexMap<String, String>) -> io::Result<()> {
        let (stderr, _, _) = self.requirements();

        let mut table = Table::new();
        table.add_row(row!["Name", "ID"]);
        for (name, id) in lang_list {
            table.add_row(row![name, id]);
        }

        write!(stderr, "{}", table)?;
        stderr.flush()
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
pub(crate) struct LoginOutcome {}

#[derive(Serialize)]
pub(crate) struct ParticipateOutcome {}

#[derive(Serialize)]
pub(crate) struct RetrieveTestCasesOutcome {
    contest: RetrieveTestCasesOutcomeContest,
    pub(self) problems: Vec<RetrieveTestCasesOutcomeProblem>,
}

#[derive(Serialize)]
struct RetrieveTestCasesOutcomeContest {
    slug: String,
    slug_lower_case: String,
    slug_upper_case: String,
    slug_snake_case: String,
    slug_kebab_case: String,
    slug_mixed_case: String,
    slug_pascal_case: String,
}

#[derive(Serialize)]
pub(self) struct RetrieveTestCasesOutcomeProblem {
    #[serde(serialize_with = "util::serde::ser_as_ref_str")]
    pub(self) url: Url,
    pub(self) slug: String,
    slug_lower_case: String,
    slug_upper_case: String,
    slug_snake_case: String,
    slug_kebab_case: String,
    slug_mixed_case: String,
    slug_pascal_case: String,
    #[serde(serialize_with = "util::serde::ser_as_ref_path")]
    pub(self) test_suite_path: SuiteFilePath,
    pub(self) test_suite: TestSuite,
}

impl RetrieveTestCasesOutcome {
    pub(self) fn new(contest: &impl Contest) -> Self {
        Self {
            contest: RetrieveTestCasesOutcomeContest {
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
        slug: String,
        url: Url,
        suite: TestSuite,
        path: SuiteFilePath,
    ) {
        self.problems.push(RetrieveTestCasesOutcomeProblem {
            url,
            slug_lower_case: slug.to_lowercase(),
            slug_upper_case: slug.to_uppercase(),
            slug_snake_case: slug.to_snake_case(),
            slug_kebab_case: slug.to_kebab_case(),
            slug_mixed_case: slug.to_mixed_case(),
            slug_pascal_case: slug.to_camel_case(),
            slug,
            test_suite_path: path,
            test_suite: suite,
        })
    }
}

#[derive(Default, Serialize)]
pub(crate) struct RetrieveSubmissionsOutcome {
    #[serde(serialize_with = "util::serde::ser_indexmap_with_as_ref_str_keys")]
    submissions: IndexMap<Url, RetrieveSubmissionsOutcomeSubmission>,
}

impl RetrieveSubmissionsOutcome {
    pub(self) fn new() -> Self {
        Self::default()
    }
}

#[derive(Serialize)]
pub(self) struct RetrieveSubmissionsOutcomeSubmission {
    pub(self) problem: RetrieveSubmissionsOutcomeSubmissionProblem,
    pub(self) language: String,
    pub(self) date_time: DateTime<FixedOffset>,
    pub(self) verdict: RetrieveSubmissionsOutcomeSubmissionVerdict,
    pub(self) saved_as: Option<AbsPathBuf>,
    pub(self) detail: Option<RetrieveSubmissionsOutcomeSubmissionDetail>,
}

#[derive(Serialize)]
pub(self) struct RetrieveSubmissionsOutcomeSubmissionProblem {
    #[serde(serialize_with = "util::serde::ser_as_ref_str")]
    url: Url,
    pub(self) slug: String,
    slug_lower_case: String,
    slug_upper_case: String,
    slug_snake_case: String,
    slug_kebab_case: String,
    slug_mixed_case: String,
    slug_pascal_case: String,
    display_name: String,
    screen_name: String,
}

impl RetrieveSubmissionsOutcomeSubmissionProblem {
    pub(self) fn new(url: &Url, slug: &str, display: &str, screen: &str) -> Self {
        Self {
            url: url.clone(),
            slug: slug.to_owned(),
            slug_lower_case: slug.to_lowercase(),
            slug_upper_case: slug.to_uppercase(),
            slug_snake_case: slug.to_snake_case(),
            slug_kebab_case: slug.to_kebab_case(),
            slug_mixed_case: slug.to_mixed_case(),
            slug_pascal_case: slug.to_camel_case(),
            display_name: display.to_owned(),
            screen_name: screen.to_owned(),
        }
    }
}

#[derive(Serialize)]
pub(self) struct RetrieveSubmissionsOutcomeSubmissionVerdict {
    pub(self) is_ok: bool,
    pub(self) string: String,
}

#[derive(Serialize)]
pub(self) struct RetrieveSubmissionsOutcomeSubmissionDetail {
    pub(self) code: String,
}

#[derive(Serialize)]
pub(crate) struct RetrieveLangsOutcome {
    #[serde(serialize_with = "util::serde::ser_as_ref_str")]
    url: Url,
    available_languages: NonEmptyIndexMap<String, String>,
}

impl RetrieveLangsOutcome {
    fn new(url: Url, available_languages: NonEmptyIndexMap<String, String>) -> Self {
        Self {
            url,
            available_languages,
        }
    }
}

#[derive(Serialize)]
pub(crate) struct SubmitOutcome {}

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

pub(crate) struct RetrieveTestCasesProps<C: Contest> {
    pub(crate) contest: C,
    pub(crate) problems: Option<NonEmptyVec<String>>,
    pub(crate) destinations: Destinations,
    pub(crate) open_in_browser: bool,
    pub(crate) only_scraped: bool,
}

impl RetrieveTestCasesProps<String> {
    pub(self) fn convert_problems(self, conversion: CaseConversion) -> Self {
        RetrieveTestCasesProps {
            problems: self.problems.map(|ps| ps.map(|p| conversion.apply(&p))),
            ..self
        }
    }

    pub(self) fn parse_contest<C: Contest>(
        self,
    ) -> std::result::Result<RetrieveTestCasesProps<C>, <C as FromStr>::Err> {
        Ok(RetrieveTestCasesProps {
            contest: self.contest.parse()?,
            problems: self.problems,
            destinations: self.destinations,
            open_in_browser: self.open_in_browser,
            only_scraped: self.only_scraped,
        })
    }
}

pub(crate) struct RetrieveSubmissionsProps<'a, C: Contest> {
    pub(self) contest: C,
    pub(self) problems: Option<NonEmptyIndexSet<String>>,
    pub(self) src_paths: HashMap<&'a str, Template<AbsPathBuf>>,
    pub(self) fetch_all: bool,
}

impl<'a> RetrieveSubmissionsProps<'a, String> {
    pub(crate) fn new(
        config: &'a Config,
        mode: config::Mode,
        problems: Vec<String>,
        fetch_all: bool,
    ) -> ConfigResult<Self> {
        let src_paths = config.src_paths(mode)?;
        Ok(RetrieveSubmissionsProps {
            contest: config.contest().to_owned(),
            problems: NonEmptyIndexSet::try_new(problems.into_iter().collect()),
            src_paths,
            fetch_all,
        })
    }

    pub(self) fn convert_problems(self, conversion: CaseConversion) -> Self {
        RetrieveSubmissionsProps {
            problems: self.problems.map(|ps| ps.ref_map(|p| conversion.apply(&p))),
            ..self
        }
    }

    pub(self) fn parse_contest<C: Contest>(
        self,
    ) -> std::result::Result<RetrieveSubmissionsProps<'a, C>, <C as FromStr>::Err> {
        Ok(RetrieveSubmissionsProps {
            contest: self.contest.parse()?,
            problems: self.problems,
            src_paths: self.src_paths,
            fetch_all: self.fetch_all,
        })
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

pub(crate) struct RetrieveLangsProps<C: Contest> {
    pub(crate) contest: C,
    pub(crate) problem: Option<String>,
}

impl RetrieveLangsProps<String> {
    pub(self) fn convert_problem(self, conversion: CaseConversion) -> Self {
        Self {
            contest: self.contest,
            problem: self.problem.map(|p| conversion.apply(&p)),
        }
    }

    pub(self) fn parse_contest<C: Contest>(
        self,
    ) -> std::result::Result<RetrieveLangsProps<C>, <C as FromStr>::Err> {
        Ok(RetrieveLangsProps {
            contest: self.contest.parse()?,
            problem: self.problem,
        })
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
