pub mod session;

pub(crate) mod atcoder;
pub(crate) mod codeforces;
pub(crate) mod yukicoder;

pub(self) mod download;

use crate::config::{self, Config};
use crate::errors::{ConfigResult, FileErrorKind, FileResult, ServiceErrorKind, ServiceResult};
use crate::outcome::Outcome;
use crate::path::{AbsPath, AbsPathBuf};
use crate::service::session::{State, StateStartArgs, UrlBase};
use crate::template::Template;
use crate::terminal::{HasTermProps, Input, WriteExt as _};
use crate::testsuite::{Destinations, SuiteFilePath, TestSuite};
use crate::util;
use crate::util::collections::{NonEmptyIndexMap, NonEmptyIndexSet, NonEmptyVec};
use crate::util::fmt::{OptionDebugExt as _, OptionDisplayExt as _};
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
use serde::{Deserialize, Deserializer, Serialize};
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

    fn print_pretty(&self, _: bool, _: impl Sized) -> io::Result<()> {
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

    fn print_pretty(&self, _: bool, _: impl Sized) -> io::Result<()> {
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
    display_name: String,
    screen_name: Option<String>,
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
        slug: &str,
        display_name: &str,
        screen_name: Option<&str>,
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
            slug: slug.to_owned(),
            display_name: display_name.to_owned(),
            screen_name: screen_name.map(ToOwned::to_owned),
            test_suite_path: path,
            test_suite: suite,
        })
    }
}

impl Outcome for RetrieveTestCasesOutcome {
    fn is_success(&self) -> bool {
        true
    }

    fn print_pretty(
        &self,
        verbose: bool,
        mut stdout: impl WriteColor + HasTermProps,
    ) -> io::Result<()> {
        if verbose {
            writeln!(stdout, "Contest slug: {:?}", self.contest.slug)?;
            for problem in &self.problems {
                writeln!(stdout)?;
                stdout.set_color(color!(bold))?;
                stdout.write_str(&problem.slug)?;
                stdout.reset()?;
                writeln!(
                    stdout,
                    " ({:?}):\n\
                     URL:         {}\n\
                     Screen name: {}\n\
                     Kind:        {}\n\
                     Timelimit:   {}\n\
                     Test cases:  {}\n\
                     Saved as:    {}",
                    problem.display_name,
                    problem.url,
                    problem.screen_name.fmt_debug_or("<none>"),
                    match &problem.test_suite {
                        TestSuite::Batch(_) => "Batch",
                        TestSuite::Interactive(_) => "Interactive",
                        TestSuite::Unsubmittable => "Unsubmittable",
                    },
                    problem.test_suite.timelimit().fmt_debug_or("<none>"),
                    match &problem.test_suite {
                        TestSuite::Batch(s) => s.num_cases().to_string(),
                        TestSuite::Interactive(_) | TestSuite::Unsubmittable => "-".to_owned(),
                    },
                    problem.test_suite_path.display(),
                )?;
            }
            Ok(())
        } else {
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

    fn print_pretty(&self, verbose: bool, mut stdout: impl WriteColor) -> io::Result<()> {
        if verbose {
            for (i, (url, submission)) in self.submissions.iter().enumerate() {
                if i > 0 {
                    writeln!(stdout)?;
                }
                stdout.set_color(color!(bold))?;
                stdout.write_str(url)?;
                stdout.reset()?;
                writeln!(
                    stdout,
                    "\n\
                     Problem URL:          {}\n\
                     Problem slug:         {:?}\n\
                     Problem display name: {:?}\n\
                     Problem screen name:  {}\n\
                     Language:             {:?}\n\
                     Date time:            {}\n\
                     Verdict:              {:?}\n\
                     Saved as:             {}\n\
                     Code size:            {}",
                    submission.problem.url,
                    submission.problem.slug,
                    submission.problem.display_name,
                    submission.problem.screen_name.fmt_debug_or("<none>"),
                    submission.language,
                    submission.date_time,
                    submission.verdict.string,
                    submission
                        .saved_as
                        .as_ref()
                        .map(|p| p.display())
                        .fmt_display_or("<none>"),
                    match &submission.detail {
                        None => "<none>".to_owned(),
                        Some(detail) => format!("{} B", detail.code.len()),
                    },
                )?;
            }
        }
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
    screen_name: Option<String>,
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
            screen_name: Some(screen.to_owned()),
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

    fn print_pretty(&self, _: bool, mut stdout: impl WriteColor) -> io::Result<()> {
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

    fn print_pretty(
        &self,
        verbose: bool,
        mut stdout: impl WriteColor + HasTermProps,
    ) -> io::Result<()> {
        if self.rejected {
            stdout.set_color(color!(fg(Red), intense))?;
            stdout.write_str("Submission rejected.\n\n")?;
        } else {
            stdout.set_color(color!(fg(Green), intense))?;
            stdout.write_str("Successfully submitted.\n\n")?;
        }
        if verbose || self.rejected {
            write!(
                stdout,
                "Response Status:            {}\n\
                 Response \"Location\" Header: {}",
                self.response.status,
                self.response.header_location.fmt_debug_or("<none>"),
            )?;
        }
        write!(
            stdout,
            "Language Name:              {:?}\n\
             Language ID:                {:?}\n\
             File:                       {}\n\
             Code Size:                  {} B",
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
    pub(crate) http_silent: bool,
    pub(crate) robots: bool,
}

impl SessionProps {
    pub(self) fn start_state<I: Input, E: WriteColor + HasTermProps>(
        &self,
        stdin: I,
        stderr: E,
    ) -> ServiceResult<State<I, E>> {
        let runtime = Runtime::new()?;
        let client = reqwest_async_client(self.timeout)?;
        let url_base = self
            .domain
            .map(|domain| UrlBase::new(Host::Domain(domain), true, None));
        State::start(StateStartArgs {
            stdin,
            stderr,
            runtime,
            robots: self.robots,
            client,
            url_base,
            cookies_path: Some(self.cookies_path.as_path()),
            api_token_path: Some(self.api_token_path.as_path()),
            http_silent: self.http_silent,
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
    pub(crate) attempt_full: bool,
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
            attempt_full: self.attempt_full,
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
