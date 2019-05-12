pub mod session;

pub(crate) mod atcoder;
pub(crate) mod codeforces;
pub(crate) mod yukicoder;

pub(self) mod download;

use crate::config::{self, Config};
use crate::errors::{ConfigResult, FileErrorKind, FileResult, ServiceErrorKind, ServiceResult};
use crate::outcome::Outcome;
use crate::path::{AbsPath, AbsPathBuf};
use crate::service::session::{
    HttpSession, HttpSessionInitParams, IntoRelativeOrAbsoluteUrl, UrlBase,
};
use crate::template::Template;
use crate::terminal::{HasTermProps, Input, WriteExt as _};
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
use reqwest::{RedirectPolicy, StatusCode};
use serde::{Deserialize, Deserializer};
use serde_derive::Serialize;
use strum_macros::IntoStaticStr;
use termcolor::WriteColor;
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

pub(crate) fn login(
    service: ServiceKind,
    props: SessionProps,
    stdin: impl Input,
    stderr: impl WriteColor + HasTermProps,
) -> ServiceResult<LoginOutcome> {
    match service {
        ServiceKind::Atcoder => atcoder::login(props, stdin, stderr),
        ServiceKind::Codeforces => codeforces::login(props, stdin, stderr),
        ServiceKind::Yukicoder => yukicoder::login(props, stdin, stderr),
        ServiceKind::Other => Err(ServiceErrorKind::ServiceIsOther.into()),
    }
}

pub(crate) fn participate(
    service: ServiceKind,
    sess_props: SessionProps,
    contest: &str,
    stdin: impl Input,
    stderr: impl WriteColor + HasTermProps,
) -> ServiceResult<ParticipateOutcome> {
    let props = (sess_props, contest);
    match service {
        ServiceKind::Atcoder => atcoder::participate(props, stdin, stderr),
        ServiceKind::Codeforces => unimplemented!(),
        ServiceKind::Yukicoder => unimplemented!(),
        ServiceKind::Other => Err(ServiceErrorKind::ServiceIsOther.into()),
    }
}

pub(crate) fn retrieve_testcases(
    service: ServiceKind,
    sess_props: SessionProps,
    retrieve_props: RetrieveTestCasesProps<String>,
    stdin: impl Input,
    stderr: impl WriteColor + HasTermProps,
) -> ServiceResult<RetrieveTestCasesOutcome> {
    let props = (sess_props, retrieve_props);
    match service {
        ServiceKind::Atcoder => atcoder::retrieve_testcases(props, stdin, stderr),
        ServiceKind::Codeforces => codeforces::retrieve_testcases(props, stdin, stderr),
        ServiceKind::Yukicoder => yukicoder::retrieve_testcases(props, stdin, stderr),
        ServiceKind::Other => Err(ServiceErrorKind::ServiceIsOther.into()),
    }
}

pub(crate) fn retrieve_langs(
    service: ServiceKind,
    sess_props: SessionProps,
    retrieve_props: RetrieveLangsProps<String>,
    stdin: impl Input,
    stderr: impl WriteColor + HasTermProps,
) -> ServiceResult<RetrieveLangsOutcome> {
    let props = (sess_props, retrieve_props);
    match service {
        ServiceKind::Atcoder => atcoder::retrieve_langs(props, stdin, stderr),
        ServiceKind::Codeforces => codeforces::retrieve_langs(props, stdin, stderr),
        ServiceKind::Yukicoder => yukicoder::retrieve_langs(props, stdin, stderr),
        ServiceKind::Other => Err(ServiceErrorKind::ServiceIsOther.into()),
    }
}

/// Downloads submitted source codes.
pub(crate) fn retrieve_submissions(
    service: ServiceKind,
    sess_props: SessionProps,
    retrieve_props: RetrieveSubmissionsProps<String>,
    stdin: impl Input,
    stderr: impl WriteColor + HasTermProps,
) -> ServiceResult<RetrieveSubmissionsOutcome> {
    let props = (sess_props, retrieve_props);
    match service {
        ServiceKind::Atcoder => atcoder::retrieve_submissions(props, stdin, stderr),
        ServiceKind::Codeforces => codeforces::retrieve_submissions(props, stdin, stderr),
        ServiceKind::Yukicoder => unimplemented!(),
        ServiceKind::Other => Err(ServiceErrorKind::ServiceIsOther.into()),
    }
}

/// Submits a source code.
pub(crate) fn submit(
    service: ServiceKind,
    sess_props: SessionProps,
    submit_props: SubmitProps<String>,
    stdin: impl Input,
    stderr: impl WriteColor + HasTermProps,
) -> ServiceResult<SubmitOutcome> {
    let props = (sess_props, submit_props);
    match service {
        ServiceKind::Atcoder => atcoder::submit(props, stdin, stderr),
        ServiceKind::Codeforces => codeforces::submit(props, stdin, stderr),
        ServiceKind::Yukicoder => yukicoder::submit(props, stdin, stderr),
        ServiceKind::Other => Err(ServiceErrorKind::ServiceIsOther.into()),
    }
}

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
    type Stdin: Input;
    type Stderr: WriteColor;

    fn requirements(
        &mut self,
    ) -> (
        &mut Self::Stdin,
        &mut Self::Stderr,
        &mut HttpSession,
        &mut Runtime,
    );

    fn get(&mut self, url: impl IntoRelativeOrAbsoluteUrl) -> session::Request<&mut Self::Stderr> {
        let (_, stderr, sess, runtime) = self.requirements();
        sess.get(url, stderr, runtime)
    }

    fn post(&mut self, url: impl IntoRelativeOrAbsoluteUrl) -> session::Request<&mut Self::Stderr> {
        let (_, stderr, sess, runtime) = self.requirements();
        sess.post(url, stderr, runtime)
    }

    fn open_in_browser(&mut self, url: impl IntoRelativeOrAbsoluteUrl) -> ServiceResult<()> {
        let (_, stderr, sess, _) = self.requirements();
        sess.open_in_browser(url, stderr)
    }

    fn prompt_reply_stderr(&mut self, prompt: &str) -> io::Result<String> {
        let (stdin, stderr, _, _) = self.requirements();
        stderr.write_str(prompt)?;
        stderr.flush()?;
        stdin.read_reply()
    }

    fn prompt_password_stderr(&mut self, prompt: &str) -> io::Result<String> {
        let (stdin, stderr, _, _) = self.requirements();
        stderr.write_str(prompt)?;
        stderr.flush()?;
        stdin.read_password()
    }

    fn ask_yes_or_no(&mut self, mes: &str, default: bool) -> io::Result<bool> {
        let (stdin, stderr, _, _) = self.requirements();
        ask_yes_or_no(mes, default, stdin, stderr)
    }
}

fn ask_yes_or_no(
    mes: &str,
    default: bool,
    mut stdin: impl Input,
    mut stderr: impl WriteColor,
) -> io::Result<bool> {
    let prompt = format!("{}{} ", mes, if default { "(Y/n)" } else { "(y/N)" });
    loop {
        stderr.write_str(&prompt)?;
        stderr.flush()?;
        match &stdin.read_password()? {
            s if s.is_empty() => break Ok(default),
            s if s.eq_ignore_ascii_case("y") || s.eq_ignore_ascii_case("yes") => break Ok(true),
            s if s.eq_ignore_ascii_case("n") || s.eq_ignore_ascii_case("no") => break Ok(false),
            _ => {
                stderr.set_color(color!(fg(Yellow), intense))?;
                stderr.write_str(r#"Answer "y", "yes", "n", "no", or ""."#)?;
                stderr.reset()?;
            }
        }
    }
}

pub(self) trait ExtractZip {
    type Write: WriteColor;

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

        out.set_color(color!(bold))?;
        out.write_str(name)?;
        out.reset()?;
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
        out.set_color(color!(bold))?;
        out.write_str(name)?;
        out.reset()?;
        writeln!(
            out,
            ": Saved {} to {}",
            plural!(2 * ret.len(), "file", "files"),
            dir.display(),
        )?;
        Ok(ret)
    }
}

#[derive(Debug)]
pub(self) struct ZipEntries {
    pub(self) in_entry: Regex,
    pub(self) in_match_group: usize,
    pub(self) in_crlf_to_lf: bool,
    pub(self) out_entry: Regex,
    pub(self) out_match_group: usize,
    pub(self) out_crlf_to_lf: bool,
    pub(self) sortings: Vec<ZipEntriesSorting>,
}

#[derive(Debug)]
pub(self) enum ZipEntriesSorting {
    Dictionary,
    Number,
}

#[derive(Debug, Serialize)]
pub(crate) struct LoginOutcome {}

impl Outcome for LoginOutcome {
    fn is_success(&self) -> bool {
        true
    }

    fn print_pretty(&self, _: impl Sized) -> io::Result<()> {
        #[cfg(debug)]
        unreachable!();

        Ok(())
    }
}

#[derive(Debug, Serialize)]
pub(crate) struct ParticipateOutcome {}

impl Outcome for ParticipateOutcome {
    fn is_success(&self) -> bool {
        true
    }

    fn print_pretty(&self, _: impl Sized) -> io::Result<()> {
        #[cfg(debug)]
        unreachable!();

        Ok(())
    }
}

#[derive(Debug, Serialize)]
pub(crate) struct RetrieveTestCasesOutcome {
    contest: RetrieveTestCasesOutcomeContest,
    pub(self) problems: Vec<RetrieveTestCasesOutcomeProblem>,
}

#[derive(Debug, Serialize)]
struct RetrieveTestCasesOutcomeContest {
    slug: String,
    slug_lower_case: String,
    slug_upper_case: String,
    slug_snake_case: String,
    slug_kebab_case: String,
    slug_mixed_case: String,
    slug_pascal_case: String,
}

#[derive(Debug, Serialize)]
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

impl Outcome for RetrieveTestCasesOutcome {
    fn is_success(&self) -> bool {
        true
    }

    fn print_pretty(&self, mut stdout: impl WriteColor + HasTermProps) -> io::Result<()> {
        let columns = self
            .problems
            .iter()
            .map(|problem| {
                let path = problem.test_suite_path.display().to_string();
                (&problem.slug, path, &problem.test_suite)
            })
            .collect::<Vec<_>>();
        let str_width = stdout.str_width_fn();
        let slug_max_width = columns
            .iter()
            .map(|(s, _, _)| str_width(s))
            .max()
            .unwrap_or(0);
        let path_max_width = columns
            .iter()
            .map(|(_, p, _)| str_width(p))
            .max()
            .unwrap_or(0);
        for (slug, path, suite) in columns {
            stdout.set_color(color!(bold))?;
            write!(stdout, "{}:", slug)?;
            stdout.reset()?;
            stdout.write_spaces(slug_max_width - str_width(slug) + 1)?;
            write!(stdout, "Saved to {}", path)?;
            stdout.write_spaces(path_max_width - str_width(&path) + 1)?;
            match suite {
                TestSuite::Batch(suite) => match suite.num_cases() {
                    0 => {
                        stdout.set_color(color!(fg(Yellow), intense))?;
                        stdout.write_str("(no test case)")?;
                    }
                    1 => {
                        stdout.set_color(color!(fg(Green), intense))?;
                        stdout.write_str("(1 test case)")?;
                    }
                    n => {
                        stdout.set_color(color!(fg(Green), intense))?;
                        write!(stdout, "({} test cases)", n)?;
                    }
                },
                TestSuite::Interactive(_) => {
                    stdout.set_color(color!(fg(Green), intense))?;
                    stdout.write_str("(interactive problem)")?;
                }
                TestSuite::Unsubmittable => {
                    stdout.set_color(color!(fg(Green), intense))?;
                    stdout.write_str("(unsubmittable problem)")?;
                }
            }
            stdout.reset()?;
            writeln!(stdout)?;
        }
        stdout.flush()
    }
}

#[derive(Debug, Default, Serialize)]
pub(crate) struct RetrieveSubmissionsOutcome {
    #[serde(serialize_with = "util::serde::ser_indexmap_with_as_ref_str_keys")]
    submissions: IndexMap<Url, RetrieveSubmissionsOutcomeSubmission>,
}

impl RetrieveSubmissionsOutcome {
    pub(self) fn new() -> Self {
        Self::default()
    }
}

impl Outcome for RetrieveSubmissionsOutcome {
    fn is_success(&self) -> bool {
        true
    }

    fn print_pretty(&self, _: impl Sized) -> io::Result<()> {
        #[cfg(debug)]
        unreachable!();

        Ok(())
    }
}

#[derive(Debug, Serialize)]
pub(self) struct RetrieveSubmissionsOutcomeSubmission {
    pub(self) problem: RetrieveSubmissionsOutcomeSubmissionProblem,
    pub(self) language: String,
    pub(self) date_time: DateTime<FixedOffset>,
    pub(self) verdict: RetrieveSubmissionsOutcomeSubmissionVerdict,
    pub(self) saved_as: Option<AbsPathBuf>,
    pub(self) detail: Option<RetrieveSubmissionsOutcomeSubmissionDetail>,
}

#[derive(Debug, Serialize)]
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

#[derive(Debug, Serialize)]
pub(self) struct RetrieveSubmissionsOutcomeSubmissionVerdict {
    pub(self) is_ok: bool,
    pub(self) string: String,
}

#[derive(Debug, Serialize)]
pub(self) struct RetrieveSubmissionsOutcomeSubmissionDetail {
    pub(self) code: String,
}

#[derive(Debug, Serialize)]
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

impl Outcome for RetrieveLangsOutcome {
    fn is_success(&self) -> bool {
        true
    }

    fn print_pretty(&self, mut stdout: impl WriteColor) -> io::Result<()> {
        let mut table = Table::new();
        table.add_row(row!["Name", "ID"]);
        for (name, id) in &self.available_languages {
            table.add_row(row![name, id]);
        }
        write!(stdout, "{}", table)?;
        stdout.flush()
    }
}

#[derive(Debug, Serialize)]
pub(crate) struct SubmitOutcome {
    pub(self) rejected: bool,
    pub(self) response: SubmitOutcomeResponse,
    pub(self) language: SubmitOutcomeLanguage,
    pub(self) file: AbsPathBuf,
    pub(self) code: String,
}

#[derive(Debug, Serialize)]
pub(self) struct SubmitOutcomeResponse {
    #[serde(serialize_with = "util::serde::ser_http_status")]
    status: StatusCode,
    header_location: Option<String>,
}

#[derive(Debug, Serialize)]
pub(self) struct SubmitOutcomeLanguage {
    name: String,
    id: String,
}

impl Outcome for SubmitOutcome {
    fn is_success(&self) -> bool {
        !self.rejected
    }

    fn print_pretty(&self, mut stdout: impl WriteColor + HasTermProps) -> io::Result<()> {
        if self.rejected {
            stdout.set_color(color!(fg(Red), intense))?;
            stdout.write_str("Submission rejected.\n\n")?;
        } else {
            stdout.set_color(color!(fg(Green), intense))?;
            stdout.write_str("Successfully submitted.\n\n")?;
        }
        write!(
            stdout,
            "Response Status:            {}\n\
             Response \"Location\" Header: {}\n\
             Language Name:              {:?}\n\
             Language ID:                {:?}\n\
             File:                       {}\n\
             Code Size:                  {}B",
            self.response.status,
            match &self.response.header_location {
                None => "<none>".to_owned(),
                Some(l) => format!("{:?}", l),
            },
            self.language.name,
            self.language.id,
            self.file.display(),
            self.code.len(),
        )?;
        stdout.reset()?;
        writeln!(stdout)?;
        stdout.flush()
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
        out: impl WriteColor,
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

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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

#[cfg(test)]
mod tests {
    use crate::service::ask_yes_or_no;
    use crate::terminal::TtyOrPiped;

    use failure::Fallible;
    use once_cell::sync::Lazy;
    use once_cell::sync_lazy;
    use pretty_assertions::assert_eq;
    use termcolor::{Ansi, Color, ColorSpec, WriteColor as _};

    use std::io::{self, Write as _};
    use std::str;

    #[test]
    fn test_ask_yes_or_no() -> Fallible<()> {
        let mut rdr = "y\nn\n\ny\nn\nヌ\n\n".as_bytes();
        let mut stdin = TtyOrPiped::Piped(&mut rdr);
        let mut stderr = Ansi::new(vec![]);

        let mut ask = |mes: &str, default: bool| -> io::Result<_> {
            ask_yes_or_no(mes, default, &mut stdin, &mut stderr)
        };

        assert_eq!(ask("Yes?: ", true)?, true);
        assert_eq!(ask("Yes?: ", true)?, false);
        assert_eq!(ask("Yes?: ", true)?, true);
        assert_eq!(ask("No?: ", false)?, true);
        assert_eq!(ask("No?: ", false)?, false);
        assert_eq!(ask("No?: ", false)?, false);
        assert_eq!(ask("Yes?: ", true)?, true);

        assert_eq!(rdr, &b""[..]);

        static EXPECTED: Lazy<String> = sync_lazy! {
            let mut expected = Ansi::new(vec![]);
            expected
                .write_all(b"Yes?: (Y/n) Yes?: (Y/n) Yes?: (Y/n) No?: (y/N) No?: (y/N) No?: (y/N) ")
                .unwrap();
            expected.set_color(ColorSpec::new().set_fg(Some(Color::Ansi256(11)))).unwrap();
            expected.write_all(br#"Answer "y", "yes", "n", "no", or ""."#).unwrap();
            expected.reset().unwrap();
            expected.write_all(b"No?: (y/N) Yes?: (Y/n) ").unwrap();
            String::from_utf8(expected.into_inner()).unwrap()
        };
        assert_eq!(str::from_utf8(stderr.get_ref())?, &*EXPECTED);
        Ok(())
    }
}
