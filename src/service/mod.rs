pub mod session;

pub(crate) mod atcoder;
pub(crate) mod codeforces;
pub(crate) mod yukicoder;

pub(self) mod download;

use crate::config::{self, Config};
use crate::errors::{ConfigResult, FileErrorKind, FileResult, ServiceErrorKind, ServiceResult};
use crate::outcome::Outcome;
use crate::path::{AbsPath, AbsPathBuf};
use crate::service::session::{State, StateStartArgs};
use crate::template::Template;
use crate::terminal::{HasTermProps, Input, WriteExt as _};
use crate::testsuite::{Destinations, SuiteFilePath, TestSuite};
use crate::util;
use crate::util::collections::{NonEmptyIndexMap, NonEmptyIndexSet};
use crate::util::fmt::{OptionDebugExt as _, OptionDisplayExt as _};
use crate::util::str::CaseConversion;

use snowchains_proc_macros::{ArgEnum, DeserializeAsString};

use chrono::{DateTime, FixedOffset};
use derive_more::From;
use derive_new::new;
use failure::ResultExt as _;
use heck::{CamelCase as _, KebabCase as _, MixedCase as _, SnakeCase as _};
use http::header::{self, HeaderMap};
use http::{StatusCode, Uri};
use indexmap::{IndexMap, IndexSet};
use maplit::hashmap;
use prettytable::format::{FormatBuilder, LinePosition, LineSeparator};
use prettytable::{cell, row, Table};
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};
use regex::Regex;
use reqwest::RedirectPolicy;
use serde::ser::SerializeMap as _;
use serde::{Serialize, Serializer};
use strum::IntoStaticStr;
use termcolor::WriteColor;
use tokio::runtime::Runtime;
use url::Url;
use zip::result::ZipResult;
use zip::ZipArchive;

use std::borrow::Cow;
use std::collections::HashMap;
use std::io::{self, Cursor, Write as _};
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use std::{cmp, mem};

/// # Panics
///
/// Panics if `service` is `Other`.
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
        ServiceKind::Other => panic!("`service` must not be `Other`"),
    }
}

/// # Panics
///
/// Panics if `service` is not `Atcoder` or `Codeforces`.
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
        ServiceKind::Codeforces => codeforces::participate(props, stdin, stderr),
        _ => panic!("`service` must be `Atcoder` or `Codeforces`"),
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
        ServiceKind::Yukicoder => yukicoder::retrieve_submissions(props, stdin, stderr),
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
    ArgEnum,
    IntoStaticStr,
    Serialize,
    DeserializeAsString,
)]
#[arg_enum(rename_all = "lowercase")]
#[strum(serialize_all = "snake_case")]
#[serde(rename_all = "lowercase")]
#[deserialize_as_string(map_err_into_serde_style)]
pub enum ServiceKind {
    Atcoder,
    Codeforces,
    Yukicoder,
    Other,
}

impl ServiceKind {
    pub(crate) fn variants_except_other() -> [&'static str; 3] {
        ["atcoder", "codeforces", "yukicoder"]
    }

    pub(crate) fn base_url(self) -> Option<&'static Url> {
        match self {
            ServiceKind::Atcoder => Some(&atcoder::BASE_URL),
            ServiceKind::Codeforces => Some(&codeforces::BASE_URL),
            ServiceKind::Yukicoder => Some(&yukicoder::BASE_URL),
            ServiceKind::Other => None,
        }
    }
}

impl Default for ServiceKind {
    fn default() -> Self {
        ServiceKind::Other
    }
}

pub(self) static USER_AGENT: &str = "snowchains <https://github.com/qryxip/snowchains>";

pub(self) trait Url1Ext {
    fn to_url2(&self) -> Url;
}

impl Url1Ext for url1::Url {
    fn to_url2(&self) -> Url {
        self.as_str()
            .parse()
            .unwrap_or_else(|e| unimplemented!("{:?}", e))
    }
}

pub(self) trait ResponseExt {
    fn url2(&self) -> Url;
}

impl ResponseExt for reqwest::r#async::Response {
    fn url2(&self) -> Url {
        self.url().to_url2()
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
    ) -> FileResult<IndexMap<String, (AbsPathBuf, String, AbsPathBuf, String)>> {
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

        let mut cases = mem::take::<HashMap<_, _>>(&mut pairs.lock().unwrap())
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

        Ok(cases
            .into_iter()
            .map(|(name, (in_path, in_content), (out_path, out_content))| {
                let (in_path, out_path) = (dir.join(in_path), dir.join(out_path));
                (name, (in_path, in_content, out_path, out_content))
            })
            .collect())
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

#[derive(Debug, From, Serialize)]
pub(crate) struct ParticipateOutcome {
    kind: ParticipateOutcomeKind,
}

impl Outcome for ParticipateOutcome {
    fn is_success(&self) -> bool {
        true
    }

    fn print_pretty(&self, _: bool, mut wtr: impl WriteColor) -> io::Result<()> {
        match self.kind {
            ParticipateOutcomeKind::Success => {
                wtr.set_color(color!(fg(Green), intense))?;
                wtr.write_str("Successfully registered.")?;
            }
            ParticipateOutcomeKind::AlreadyParticipated => {
                wtr.set_color(color!(fg(Yellow), intense))?;
                wtr.write_str("You have already registered.")?;
            }
            ParticipateOutcomeKind::ContestIsFinished => {
                wtr.set_color(color!(fg(Yellow), intense))?;
                wtr.write_str("The contest is finished.")?;
            }
        }
        wtr.reset()?;
        writeln!(wtr)?;
        wtr.flush()
    }
}

#[derive(Debug, Clone, Copy, Serialize)]
pub(self) enum ParticipateOutcomeKind {
    Success,
    AlreadyParticipated,
    ContestIsFinished,
}

#[derive(Debug, Serialize)]
pub(crate) struct RetrieveTestCasesOutcome {
    contest: OutcomeContest,
    #[serde(serialize_with = "util::serde::ser_str_slice")]
    submissions_urls: Vec<Url>,
    pub(self) problems: Vec<RetrieveTestCasesOutcomeProblem>,
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
        fn common_ancestor<'a>(paths: &[&'a AbsPath]) -> Option<&'a AbsPath> {
            paths.get(0).map(|path| {
                let mut ret = path.parent().unwrap_or(paths[0]);
                while !paths.iter().all(|p| p.starts_with(ret)) {
                    match ret.parent() {
                        None => break,
                        Some(next) => ret = next,
                    }
                }
                ret
            })
        }

        if verbose {
            writeln!(
                stdout,
                "Contest slug:         {:?}\n\
                 Contest display name: {:?}",
                self.contest.slug, self.contest.display_name,
            )?;

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
                     Location:    {}\n\
                     Saved:       {:?}",
                    problem.display_name,
                    problem.url,
                    problem.screen_name.fmt_debug_or("<none>"),
                    match &problem.test_suite.contents {
                        TestSuite::Batch(_) => "Batch",
                        TestSuite::Interactive(_) => "Interactive",
                        TestSuite::Unsubmittable => "Unsubmittable",
                    },
                    problem
                        .test_suite
                        .contents
                        .timelimit()
                        .fmt_debug_or("<none>"),
                    match &problem.test_suite.contents {
                        TestSuite::Batch(s) => s.num_cases().to_string(),
                        TestSuite::Interactive(_) | TestSuite::Unsubmittable => "N/A".to_owned(),
                    },
                    problem.test_suite.location.display(),
                    problem.test_suite.saved,
                )?;
            }

            for problem in &self.problems {
                if !problem.text_files.is_empty() {
                    writeln!(stdout)?;
                    stdout.write_str("Text files of ")?;
                    stdout.set_color(color!(bold))?;
                    stdout.write_str(&problem.slug)?;
                    stdout.reset()?;
                    stdout.write_str(":\n")?;
                    for (name, text_files) in &problem.text_files {
                        writeln!(
                            stdout,
                            "  - Name:  {}\
                             \n    In:    {}\
                             \n    Out:   {}",
                            name,
                            text_files.r#in.location.display(),
                            text_files.out.location.display(),
                        )?;
                    }
                }
            }
        } else {
            writeln!(
                stdout,
                "Contest display name: {:?}",
                self.contest.display_name,
            )?;

            let lines = self
                .problems
                .iter()
                .map(|problem| {
                    let slug = &problem.slug;
                    let location = problem.test_suite.location.display().to_string();
                    let saved = problem.test_suite.saved;
                    let test_suite = &problem.test_suite.contents;
                    let text_files = &problem.text_files;
                    let text_dir = {
                        let paths = text_files
                            .iter()
                            .flat_map(|(_, t)| vec![&*t.r#in.location, &*t.out.location])
                            .collect::<Vec<_>>();
                        common_ancestor(&paths).map(|d| d.display().to_string())
                    };
                    (slug, location, saved, test_suite, text_files, text_dir)
                })
                .collect::<Vec<_>>();

            let str_width = stdout.str_width_fn();
            let slug_max_width = lines
                .iter()
                .map(|(s, _, _, _, _, _)| str_width(s))
                .max()
                .unwrap_or(0);
            let location_max_width = lines
                .iter()
                .map(|(_, s, _, _, _, _)| str_width(s))
                .max()
                .unwrap_or(0);
            let text_dir_max_width = lines
                .iter()
                .flat_map(|(_, _, _, _, _, s)| s.as_deref().map(str_width))
                .max()
                .unwrap_or(0);

            for (slug, location, saved, suite, _, _) in &lines {
                stdout.set_color(color!(bold))?;
                stdout.write_str(slug)?;
                stdout.reset()?;
                stdout.write_str(": ")?;
                stdout.write_spaces(slug_max_width - str_width(slug))?;

                if *saved {
                    stdout.write_str("Saved as ")?;
                    stdout.set_color(color!(bold))?;
                    stdout.write_str(&location)?;
                } else {
                    stdout.set_color(color!(fg(Yellow), intense))?;
                    stdout.write_str("NOT saved as ")?;
                    stdout.set_color(color!(fg(Yellow), intense, bold))?;
                    stdout.write_str(&location)?;
                }
                stdout.write_spaces(location_max_width - str_width(&location))?;

                match &suite {
                    TestSuite::Batch(suite) => match suite.num_cases() {
                        0 => {
                            stdout.set_color(color!(fg(Yellow), intense))?;
                            stdout.write_str(" (no test case)")?;
                        }
                        1 => {
                            stdout.set_color(color!(fg(Green), intense))?;
                            stdout.write_str(" (1 test case)")?;
                        }
                        n => {
                            stdout.set_color(color!(fg(Green), intense))?;
                            write!(stdout, " ({} test cases)", n)?;
                        }
                    },
                    TestSuite::Interactive(_) => {
                        stdout.set_color(color!(fg(Green), intense))?;
                        stdout.write_str(" (interactive problem)")?;
                    }
                    TestSuite::Unsubmittable => {
                        stdout.set_color(color!(fg(Green), intense))?;
                        stdout.write_str(" (unsubmittable problem)")?;
                    }
                }

                stdout.reset()?;
                writeln!(stdout)?;
            }

            for (slug, _, saved, _, text_files, text_dir) in lines {
                if let Some(text_dir) = text_dir {
                    let num_files = 2 * text_files.len();
                    let size_sum = text_files
                        .iter()
                        .map(|(_, t)| t.r#in.contents.len() + t.out.contents.len())
                        .sum::<usize>();

                    stdout.set_color(color!(bold))?;
                    stdout.write_str(slug)?;
                    stdout.reset()?;
                    stdout.write_str(": ")?;
                    stdout.write_spaces(slug_max_width - str_width(slug))?;

                    if saved {
                        stdout.write_str("Saved ")?;
                        stdout.set_color(color!(bold))?;
                        write!(stdout, "{}", num_files)?;
                        stdout.reset()?;
                        stdout.write_str(" files to ")?;
                        stdout.set_color(color!(bold))?;
                        stdout.write_str(&text_dir)?;
                        stdout.reset()?;
                        stdout.write_spaces(text_dir_max_width - str_width(&text_dir))?;
                        writeln!(stdout, " ({} B)", size_sum)?;
                    } else {
                        stdout.set_color(color!(fg(Yellow), intense))?;
                        stdout.write_str("NOT Saved ")?;
                        stdout.set_color(color!(fg(Yellow), intense, bold))?;
                        write!(stdout, "{}", num_files)?;
                        stdout.set_color(color!(fg(Yellow), intense))?;
                        stdout.write_str(" files to ")?;
                        stdout.set_color(color!(fg(Yellow), intense, bold))?;
                        stdout.write_str(&text_dir)?;
                        stdout.reset()?;
                        stdout.write_spaces(text_dir_max_width - str_width(&text_dir))?;
                        writeln!(stdout, " ({} B)", size_sum)?;
                        stdout.reset()?;
                        writeln!(stdout)?;
                    }
                }
            }
        }
        stdout.flush()
    }
}

#[derive(Debug, Serialize)]
pub(self) struct RetrieveTestCasesOutcomeProblem {
    #[serde(serialize_with = "util::serde::ser_as_ref_str")]
    url: Url,
    slug: String,
    slug_lower_case: String,
    slug_upper_case: String,
    slug_snake_case: String,
    slug_kebab_case: String,
    slug_mixed_case: String,
    slug_pascal_case: String,
    display_name: String,
    screen_name: Option<String>,
    test_suite: RetrieveTestCasesOutcomeProblemTestSuite,
    text_files: IndexMap<String, RetrieveTestCasesOutcomeProblemTextFiles>,
}

#[derive(Debug, Serialize, new)]
struct RetrieveTestCasesOutcomeProblemTestSuite {
    contents: TestSuite,
    #[serde(serialize_with = "util::serde::ser_as_ref_path")]
    location: SuiteFilePath,
    saved: bool,
}

#[derive(Debug, Serialize)]
struct RetrieveTestCasesOutcomeProblemTextFiles {
    r#in: RetrieveTestCasesOutcomeProblemTextFile,
    out: RetrieveTestCasesOutcomeProblemTextFile,
}

#[derive(Debug)]
struct RetrieveTestCasesOutcomeProblemTextFile {
    contents: String,
    location: AbsPathBuf,
    saved: bool,
}

impl Serialize for RetrieveTestCasesOutcomeProblemTextFile {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        let mut serializer = serializer.serialize_map(Some(3))?;
        serializer.serialize_entry("size", &self.contents.len())?;
        serializer.serialize_entry("location", &self.location)?;
        serializer.serialize_entry("saved", &self.saved)?;
        serializer.end()
    }
}

#[derive(Debug)]
pub(self) struct RetrieveTestCasesOutcomeBuilder {
    contest: OutcomeContest,
    submissions_urls: Vec<Url>,
    problems: Vec<RetrieveTestCasesOutcomeBuilderProblem>,
    save_files: bool,
}

impl RetrieveTestCasesOutcomeBuilder {
    pub(self) fn new(contest_slug: &str, contest_display: &str, save_files: bool) -> Self {
        Self {
            contest: OutcomeContest::new(contest_slug, contest_display),
            submissions_urls: vec![],
            problems: vec![],
            save_files,
        }
    }

    pub(self) fn push_submissions_url(&mut self, url: Url) {
        self.submissions_urls.push(url);
    }

    pub(self) fn problems(&self) -> &[RetrieveTestCasesOutcomeBuilderProblem] {
        &self.problems
    }

    pub(self) fn problems_mut(&mut self) -> &mut [RetrieveTestCasesOutcomeBuilderProblem] {
        &mut self.problems
    }

    pub(self) fn push_problem(&mut self, problem: RetrieveTestCasesOutcomeBuilderProblem) {
        self.problems.push(problem);
    }

    pub(self) fn problem_slugs(&self) -> IndexSet<String> {
        self.problems.iter().map(|p| p.slug.clone()).collect()
    }

    pub(self) fn finish(
        self,
        open_browser: bool,
        mut stderr: impl WriteColor,
    ) -> ServiceResult<RetrieveTestCasesOutcome> {
        for problem in &self.problems {
            let RetrieveTestCasesOutcomeBuilderProblem {
                test_suite_contents,
                test_suite_location,
                text_files,
                ..
            } = problem;
            if self.save_files {
                test_suite_contents.save(&test_suite_location)?;
            }
            for text_files in text_files.values() {
                let RetrieveTestCasesOutcomeBuilderProblemTextFiles {
                    in_contents,
                    in_location,
                    out_contents,
                    out_location,
                } = text_files;
                if self.save_files {
                    crate::fs::write(in_location, in_contents.as_ref())?;
                }
                if self.save_files {
                    crate::fs::write(out_location, out_contents.as_ref())?;
                }
            }
        }

        if open_browser {
            let mut urls = self.submissions_urls.iter().collect::<Vec<_>>();
            urls.extend(self.problems.iter().map(|p| &p.url));
            for url in urls {
                writeln!(stderr, "Opening {} in default browser...", url)?;
                let status = webbrowser::open(url.as_ref())?.status;
                if !status.success() {
                    return Err(ServiceErrorKind::Webbrowser(status).into());
                }
            }
            stderr.flush()?;
        }

        let saved = self.save_files;

        let problems = self
            .problems
            .into_iter()
            .map(|problem| RetrieveTestCasesOutcomeProblem {
                url: problem.url,
                slug_lower_case: problem.slug.to_lowercase(),
                slug_upper_case: problem.slug.to_uppercase(),
                slug_snake_case: problem.slug.to_snake_case(),
                slug_kebab_case: problem.slug.to_kebab_case(),
                slug_mixed_case: problem.slug.to_mixed_case(),
                slug_pascal_case: problem.slug.to_camel_case(),
                slug: problem.slug,
                display_name: problem.display_name,
                screen_name: problem.screen_name,
                test_suite: RetrieveTestCasesOutcomeProblemTestSuite {
                    contents: problem.test_suite_contents,
                    location: problem.test_suite_location,
                    saved,
                },
                text_files: problem
                    .text_files
                    .into_iter()
                    .map(|(name, text_files)| {
                        let text_files = RetrieveTestCasesOutcomeProblemTextFiles {
                            r#in: RetrieveTestCasesOutcomeProblemTextFile {
                                contents: text_files.in_contents,
                                location: text_files.in_location,
                                saved,
                            },
                            out: RetrieveTestCasesOutcomeProblemTextFile {
                                contents: text_files.out_contents,
                                location: text_files.out_location,
                                saved,
                            },
                        };
                        (name, text_files)
                    })
                    .collect(),
            })
            .collect();

        Ok(RetrieveTestCasesOutcome {
            contest: self.contest,
            submissions_urls: self.submissions_urls,
            problems,
        })
    }
}

#[derive(Debug)]
pub(self) struct RetrieveTestCasesOutcomeBuilderProblem {
    pub(self) url: Url,
    pub(self) slug: String,
    pub(self) display_name: String,
    pub(self) screen_name: Option<String>,
    pub(self) test_suite_contents: TestSuite,
    pub(self) test_suite_location: SuiteFilePath,
    pub(self) text_files: IndexMap<String, RetrieveTestCasesOutcomeBuilderProblemTextFiles>,
}

#[derive(Debug)]
pub(self) struct RetrieveTestCasesOutcomeBuilderProblemTextFiles {
    pub(self) in_contents: String,
    pub(self) in_location: AbsPathBuf,
    pub(self) out_contents: String,
    pub(self) out_location: AbsPathBuf,
}

#[derive(Debug, Serialize)]
pub(crate) struct RetrieveSubmissionsOutcome {
    #[serde(serialize_with = "util::serde::ser_indexmap_with_as_ref_str_keys")]
    submissions: IndexMap<Url, RetrieveSubmissionsOutcomeSubmission>,
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
        } else {
            let mut num_saved = 0;

            for (_, submission) in &self.submissions {
                stdout.set_color(color!(bold))?;
                stdout.write_str(&submission.problem.slug)?;
                if let Some(saved_as) = &submission.saved_as {
                    stdout.reset()?;
                    writeln!(
                        stdout,
                        " ({:?}): Saved to {}",
                        submission.language,
                        saved_as.display(),
                    )?;
                    num_saved += 1;
                } else {
                    stdout.set_color(color!(fg(Yellow), intense))?;
                    write!(stdout, " ({:?}): Not saved", submission.language)?;
                    stdout.reset()?;
                    writeln!(stdout)?;
                }
            }

            writeln!(stdout, "Saved {}.", plural!(num_saved, "file", "files"))?;
        }
        stdout.flush()
    }
}

#[derive(Debug, Serialize)]
struct RetrieveSubmissionsOutcomeSubmission {
    problem: RetrieveSubmissionsOutcomeSubmissionProblem,
    language: String,
    date_time: DateTime<FixedOffset>,
    verdict: RetrieveSubmissionsOutcomeSubmissionVerdict,
    saved_as: Option<AbsPathBuf>,
    detail: Option<RetrieveSubmissionsOutcomeSubmissionDetail>,
}

#[derive(Debug, Serialize)]
pub(self) struct RetrieveSubmissionsOutcomeSubmissionProblem {
    #[serde(serialize_with = "util::serde::ser_as_ref_str")]
    url: Url,
    slug: String,
    slug_lower_case: String,
    slug_upper_case: String,
    slug_snake_case: String,
    slug_kebab_case: String,
    slug_mixed_case: String,
    slug_pascal_case: String,
    display_name: String,
    screen_name: Option<String>,
}

#[derive(Debug, Serialize)]
struct RetrieveSubmissionsOutcomeSubmissionVerdict {
    is_ok: bool,
    string: String,
}

#[derive(Debug, Serialize)]
struct RetrieveSubmissionsOutcomeSubmissionDetail {
    code: String,
}

#[derive(Debug, Default)]
struct RetrieveSubmissionsOutcomeBuilder {
    pub(self) submissions: IndexMap<Url, RetrieveSubmissionsOutcomeBuilderSubmission>,
}

impl RetrieveSubmissionsOutcomeBuilder {
    pub(self) fn new() -> Self {
        Self::default()
    }

    pub(self) fn problem_slugs(&self) -> IndexSet<String> {
        self.submissions
            .values()
            .map(|s| s.problem_slug.clone())
            .collect()
    }

    pub(self) fn finish(self) -> ServiceResult<RetrieveSubmissionsOutcome> {
        for (_, submission) in &self.submissions {
            if let (Some(location), Some(code)) = (&submission.location, &submission.code) {
                crate::fs::write(location, code.as_ref())?;
            }
        }

        let submissions = self
            .submissions
            .into_iter()
            .map(|(url, submission)| {
                let submission = RetrieveSubmissionsOutcomeSubmission {
                    problem: RetrieveSubmissionsOutcomeSubmissionProblem {
                        url: submission.problem_url,
                        slug_lower_case: submission.problem_slug.to_lowercase(),
                        slug_upper_case: submission.problem_slug.to_uppercase(),
                        slug_snake_case: submission.problem_slug.to_snake_case(),
                        slug_kebab_case: submission.problem_slug.to_kebab_case(),
                        slug_mixed_case: submission.problem_slug.to_mixed_case(),
                        slug_pascal_case: submission.problem_slug.to_camel_case(),
                        slug: submission.problem_slug,
                        display_name: submission.problem_display_name,
                        screen_name: submission.problem_screen_name,
                    },
                    language: submission.language,
                    date_time: submission.date_time,
                    verdict: RetrieveSubmissionsOutcomeSubmissionVerdict {
                        is_ok: submission.verdict_is_ok,
                        string: submission.verdict_string,
                    },
                    saved_as: submission.location,
                    detail: submission
                        .code
                        .map(|code| RetrieveSubmissionsOutcomeSubmissionDetail { code }),
                };
                (url, submission)
            })
            .collect();
        Ok(RetrieveSubmissionsOutcome { submissions })
    }
}

#[derive(Debug)]
pub(self) struct RetrieveSubmissionsOutcomeBuilderSubmission {
    pub(self) problem_url: Url,
    pub(self) problem_slug: String,
    pub(self) problem_display_name: String,
    pub(self) problem_screen_name: Option<String>,
    pub(self) language: String,
    pub(self) date_time: DateTime<FixedOffset>,
    pub(self) verdict_is_ok: bool,
    pub(self) verdict_string: String,
    pub(self) location: Option<AbsPathBuf>,
    pub(self) code: Option<String>,
}

#[derive(Debug, Serialize)]
pub(crate) struct RetrieveLangsOutcome {
    #[serde(serialize_with = "util::serde::ser_as_ref_str")]
    url: Url,
    available_languages: NonEmptyIndexMap<String, String>,
}

impl RetrieveLangsOutcome {
    pub(self) fn new(url: Url, available_languages: NonEmptyIndexMap<String, String>) -> Self {
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

    fn print_pretty(&self, _: bool, mut stdout: impl WriteColor + HasTermProps) -> io::Result<()> {
        let mut table = Table::new();

        // `prettytable` assumes that East Asian Ambiguous Characters are halfwidth.
        let (column_separator, borders, top_sep, title_sep, intern_sep, bottom_sep);
        if ['─', '│', '┌', '┐', '└', '┘', '├', '┤', '┬', '┴', '┼']
            .iter()
            .all(|&c| stdout.char_width(c) == Some(1))
        {
            column_separator = '│';
            borders = '│';
            top_sep = LineSeparator::new('─', '┬', '┌', '┐');
            title_sep = LineSeparator::new('─', '┼', '├', '┤');
            intern_sep = LineSeparator::new('─', '┼', '├', '┤');
            bottom_sep = LineSeparator::new('─', '┴', '└', '┘');
        } else {
            column_separator = '|';
            borders = '|';
            top_sep = LineSeparator::new('-', '+', '+', '+');
            title_sep = LineSeparator::new('-', '+', '+', '+');
            intern_sep = LineSeparator::new('-', '+', '+', '+');
            bottom_sep = LineSeparator::new('-', '+', '+', '+');
        }

        *table.get_format() = FormatBuilder::new()
            .padding(1, 1)
            .column_separator(column_separator)
            .borders(borders)
            .separator(LinePosition::Top, top_sep)
            .separator(LinePosition::Title, title_sep)
            .separator(LinePosition::Intern, intern_sep)
            .separator(LinePosition::Bottom, bottom_sep)
            .build();

        table.set_titles(row!["Name", "ID"]);
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
    #[serde(serialize_with = "util::serde::ser_option_display")]
    location: Option<Uri>,
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
                self.response.location.fmt_debug_or("<none>"),
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

#[derive(Debug, Serialize)]
struct OutcomeContest {
    slug: String,
    slug_lower_case: String,
    slug_upper_case: String,
    slug_snake_case: String,
    slug_kebab_case: String,
    slug_mixed_case: String,
    slug_pascal_case: String,
    display_name: String,
}

impl OutcomeContest {
    fn new(slug: &str, display_name: &str) -> Self {
        Self {
            slug: slug.to_owned(),
            slug_lower_case: slug.to_lowercase(),
            slug_upper_case: slug.to_uppercase(),
            slug_snake_case: slug.to_snake_case(),
            slug_kebab_case: slug.to_kebab_case(),
            slug_mixed_case: slug.to_mixed_case(),
            slug_pascal_case: slug.to_camel_case(),
            display_name: display_name.to_owned(),
        }
    }
}

pub(crate) struct SessionProps {
    pub(crate) base_url: Option<&'static Url>,
    pub(crate) cookies_path: AbsPathBuf,
    pub(crate) api_token_path: AbsPathBuf,
    pub(crate) dropbox_path: Option<AbsPathBuf>,
    pub(crate) timeout: Option<Duration>,
    pub(crate) login_retries: Option<u32>,
    pub(crate) retries_on_get: u32,
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
        State::start(StateStartArgs {
            stdin,
            stderr,
            runtime,
            robots: self.robots,
            client,
            base_url: self.base_url,
            cookies_path: Some(self.cookies_path.as_path()),
            api_token_path: Some(self.api_token_path.as_path()),
            retries_on_get: self.retries_on_get,
            http_silent: self.http_silent,
            login_retries: self.login_retries,
        })
    }
}

pub(self) fn reqwest_async_client(
    timeout: impl Into<Option<Duration>>,
) -> reqwest::Result<reqwest::r#async::Client> {
    let mut builder = reqwest::r#async::Client::builder()
        .referer(false)
        .redirect(RedirectPolicy::none()) // Redirects manually
        .cookie_store(false) // Uses `CookieStore` directly
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
        .referer(false)
        .redirect(RedirectPolicy::none())
        .cookie_store(false)
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
    pub(crate) problems: Option<NonEmptyIndexSet<String>>,
    pub(crate) destinations: Destinations,
    pub(crate) open_in_browser: bool,
    pub(crate) attempt_full: bool,
    pub(crate) save_files: bool,
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
            save_files: self.save_files,
        })
    }
}

#[derive(Debug)]
pub(crate) struct RetrieveSubmissionsProps<'a, C: Contest> {
    pub(self) contest: C,
    pub(self) problems: Option<NonEmptyIndexSet<String>>,
    pub(self) src_paths: HashMap<&'a str, Template<AbsPathBuf>>,
    pub(self) fetch_all: bool,
    pub(self) save_files: bool,
}

impl<'a> RetrieveSubmissionsProps<'a, String> {
    pub(crate) fn new(
        config: &'a Config,
        mode: config::Mode,
        problems: Vec<String>,
        fetch_all: bool,
        save_files: bool,
    ) -> ConfigResult<Self> {
        let src_paths = config.src_paths(mode)?;
        Ok(RetrieveSubmissionsProps {
            contest: config.contest().to_owned(),
            problems: NonEmptyIndexSet::try_new(problems.into_iter().collect()),
            src_paths,
            fetch_all,
            save_files,
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
            save_files: self.save_files,
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

pub(crate) trait Contest: FromStr {
    fn slug(&self) -> Cow<str>;
}

impl Contest for String {
    fn slug(&self) -> Cow<str> {
        self.as_str().into()
    }
}

pub(self) trait DetermineSubmissionsUrls: Contest {
    fn submissions_urls(&self, problems: &[&str]) -> Vec<Url>;
}
