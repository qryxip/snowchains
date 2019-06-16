use crate::errors::{
    ParseContestNameError, ParseContestNameResult, ScrapeError, ScrapeResult, ServiceError,
    ServiceErrorKind, ServiceResult,
};
use crate::service::download::{self, DownloadProgress};
use crate::service::session::{FormBuilder, ParseWithBaseUrl as _, Password, Session, State};
use crate::service::{
    Contest, ExtractZip, LoginOutcome, RetrieveLangsOutcome, RetrieveLangsProps,
    RetrieveSubmissionsOutcome, RetrieveSubmissionsOutcomeBuilder,
    RetrieveSubmissionsOutcomeBuilderSubmission, RetrieveSubmissionsProps,
    RetrieveTestCasesOutcome, RetrieveTestCasesOutcomeBuilder,
    RetrieveTestCasesOutcomeBuilderProblem, RetrieveTestCasesOutcomeBuilderProblemTextFiles,
    RetrieveTestCasesProps, SessionProps, SubmitOutcome, SubmitOutcomeLanguage,
    SubmitOutcomeResponse, SubmitProps, ZipEntries, ZipEntriesSorting,
};
use crate::terminal::{HasTermProps, Input, WriteExt as _};
use crate::testsuite::{self, BatchSuite, InteractiveSuite, TestSuite};
use crate::util::collections::{NonEmptyIndexMap, NonEmptyVec};
use crate::util::str::CaseConversion;

use chrono::offset::TimeZone as _;
use chrono::{DateTime, FixedOffset, NaiveDateTime};
use cookie::Cookie;
use http::{header, StatusCode};
use indexmap::{indexmap, IndexMap, IndexSet};
use once_cell::sync::Lazy;
use regex::Regex;
use scraper::Html;
use serde::Deserialize;
use termcolor::WriteColor;
use url::Url;

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::str::FromStr;
use std::time::Duration;

pub(super) static BASE_URL: Lazy<Url> = Lazy::new(|| "https://yukicoder.me".parse().unwrap());

pub(super) fn login(
    props: SessionProps,
    stdin: impl Input,
    stderr: impl WriteColor + HasTermProps,
) -> ServiceResult<LoginOutcome> {
    Yukicoder::try_new(props, stdin, stderr)?.login(true)
}

pub(super) fn retrieve_testcases(
    props: (SessionProps, RetrieveTestCasesProps<String>),
    stdin: impl Input,
    stderr: impl WriteColor + HasTermProps,
) -> ServiceResult<RetrieveTestCasesOutcome> {
    let (sess_props, retrieve_props) = props;
    let retrieve_props = retrieve_props
        .convert_problems(CaseConversion::Upper)
        .parse_contest()?;
    Yukicoder::try_new(sess_props, stdin, stderr)?.retrieve_testcases(retrieve_props)
}

pub(super) fn retrieve_langs(
    props: (SessionProps, RetrieveLangsProps<String>),
    stdin: impl Input,
    stderr: impl WriteColor + HasTermProps,
) -> ServiceResult<RetrieveLangsOutcome> {
    let (sess_props, retrieve_props) = props;
    let retrieve_props = retrieve_props
        .convert_problem(CaseConversion::Upper)
        .parse_contest()?;
    Yukicoder::try_new(sess_props, stdin, stderr)?.retrieve_langs(retrieve_props)
}

pub(super) fn retrieve_submissions(
    props: (SessionProps, RetrieveSubmissionsProps<String>),
    stdin: impl Input,
    stderr: impl WriteColor + HasTermProps,
) -> ServiceResult<RetrieveSubmissionsOutcome> {
    let (sess_props, retrieve_props) = props;
    let retrieve_props = retrieve_props
        .convert_problems(CaseConversion::Upper)
        .parse_contest()?;
    Yukicoder::try_new(sess_props, stdin, stderr)?.retrieve_submissions(retrieve_props)
}

pub(super) fn submit(
    props: (SessionProps, SubmitProps<String>),
    stdin: impl Input,
    stderr: impl WriteColor + HasTermProps,
) -> ServiceResult<SubmitOutcome> {
    let (sess_props, submit_props) = props;
    let submit_props = submit_props
        .convert_problem(CaseConversion::Upper)
        .parse_contest()?;
    Yukicoder::try_new(sess_props, stdin, stderr)?.submit(submit_props)
}

#[derive(Debug)]
struct Yukicoder<I: Input, E: WriteColor + HasTermProps> {
    username: Username,
    state: State<I, E>,
}

impl<I: Input, E: WriteColor + HasTermProps> Session for Yukicoder<I, E> {
    type Stdin = I;
    type Stderr = E;

    fn state_ref(&self) -> &State<I, E> {
        &self.state
    }

    fn state_mut(&mut self) -> &mut State<I, E> {
        &mut self.state
    }
}

impl<I: Input, E: WriteColor + HasTermProps> DownloadProgress for Yukicoder<I, E> {}

impl<I: Input, E: WriteColor + HasTermProps> ExtractZip for Yukicoder<I, E> {
    type Write = E;

    fn out(&mut self) -> &mut E {
        self.stderr()
    }
}

impl<I: Input, E: WriteColor + HasTermProps> Yukicoder<I, E> {
    fn try_new(props: SessionProps, stdin: I, stderr: E) -> ServiceResult<Self> {
        let state = props.start_state(stdin, stderr)?;
        Ok(Self {
            username: Username::None,
            state,
        })
    }

    fn login(&mut self, assure: bool) -> ServiceResult<LoginOutcome> {
        self.fetch_username()?;
        if self.username.name().is_none() && (assure || self.ask_yn("Login? ", true)?) {
            self.stderr().write_str(
                r#"
Input "REVEL_SESSION".

Chrome: chrome://settings/cookies/detail?site=yukicoder.me&search=cookie
Firefox: sqlite3 "$YOUR_FIREFOX_PROFILE/cookies.sqlite" 'SELECT value FROM moz_cookies WHERE baseDomain="yukicoder.me" AND name="REVEL_SESSION"'

"#,
            )?;
            self.retry_login::<Password, _>(
                ("REVEL_SESSION: ",),
                "Confirmed.",
                "Wrong \"REVEL_SESSION\".",
                |this, (revel_session,)| {
                    this.clear_cookies()?;
                    let cookie = Cookie::new("REVEL_SESSION", revel_session);
                    this.insert_cookie(&cookie, &*BASE_URL)?;
                    this.fetch_username()?;
                    Ok(this.username.name().is_some())
                },
            )?;
        }
        let username = self.username.clone();
        writeln!(self.stderr(), "Username: {}", username)?;
        self.stderr().flush()?;
        Ok(LoginOutcome {})
    }

    fn fetch_username(&mut self) -> ServiceResult<()> {
        self.username = self.get("/").retry_recv_html()?.extract_username();
        Ok(())
    }

    fn retrieve_testcases(
        &mut self,
        props: RetrieveTestCasesProps<YukicoderContest>,
    ) -> ServiceResult<RetrieveTestCasesOutcome> {
        let RetrieveTestCasesProps {
            contest,
            problems,
            destinations,
            open_in_browser,
            attempt_full,
            save_files,
        } = props;

        if contest == YukicoderContest::No && problems.is_none() {
            return Err(ServiceErrorKind::PleaseSpecifyProblems.into());
        }

        self.login(false)?;

        let scrape = |html: &Html, problem: &str| -> ServiceResult<_> {
            let (display_name, suite) = html.extract_samples()?;
            let path = destinations.expand(problem)?;
            Ok((display_name, suite, path))
        };

        let contest_display = match contest {
            YukicoderContest::No => "問題一覧".to_owned(),
            YukicoderContest::Id(id) => {
                let contest = self
                    .api_v1_contest_future()?
                    .into_iter()
                    .find(|c| c.id == id);
                match contest {
                    Some(c) => c,
                    None => self
                        .api_v1_contest_past()?
                        .into_iter()
                        .find(|c| c.id == id)
                        .ok_or_else(|| ServiceErrorKind::ContestNotFound(id.to_string()))?,
                }
                .name
            }
        };
        let mut outcome =
            RetrieveTestCasesOutcomeBuilder::new(&contest.slug(), &contest_display, save_files);

        match (contest, problems.as_ref()) {
            (YukicoderContest::No, None) => unreachable!(),
            (YukicoderContest::No, Some(problems)) => {
                for problem in problems {
                    let mut url = BASE_URL.clone();
                    url.set_path(&format!("/problems/no/{}/submissions", problem));
                    outcome.push_submissions_url(url);
                }
                let (mut not_found, mut not_public) = (vec![], vec![]);
                for problem in problems {
                    let mut url = BASE_URL.clone();
                    url.set_path(&format!("/problems/no/{}", problem));
                    let res = self
                        .get(url.clone())
                        .acceptable(&[200, 404])
                        .warn(&[404])
                        .retry_send()?;
                    let status = res.status();
                    let html = self.retry_recv_html(res)?;
                    let public = html
                        .select(selector!("#content"))
                        .flat_map(|r| r.text())
                        .next()
                        .map_or(true, |t| !t.contains("非表示"));
                    if status == StatusCode::NOT_FOUND {
                        not_found.push(problem);
                    } else if !public {
                        not_public.push(problem);
                    } else {
                        let (display_name, suite, path) = scrape(&html, problem)?;
                        outcome.push_problem(RetrieveTestCasesOutcomeBuilderProblem {
                            url,
                            slug: problem.clone(),
                            display_name,
                            screen_name: Some(problem.clone()),
                            test_suite_contents: suite,
                            test_suite_location: path,
                            text_files: indexmap![],
                        });
                    }
                }
                let stderr = self.stderr();
                if !not_found.is_empty() {
                    stderr.set_color(color!(fg(Yellow), intense))?;
                    write!(stderr, "Not found: {:?}", not_found)?;;
                    stderr.reset()?;
                    writeln!(stderr)?;
                    stderr.flush()?;
                }
                if !not_public.is_empty() {
                    stderr.set_color(color!(fg(Yellow), intense))?;
                    write!(stderr, "Not public: {:?}", not_public)?;;
                    stderr.reset()?;
                    writeln!(stderr)?;
                    stderr.flush()?;
                }
            }
            (YukicoderContest::Id(id), problems) => {
                let mut url = BASE_URL.clone();
                url.set_path(&format!("/problems/contests/{}/submissions", id));
                outcome.push_submissions_url(url);

                let target_problems = self
                    .get(&format!("/contests/{}", id))
                    .retry_recv_html()?
                    .extract_problems()?;
                for (slug, no, url) in target_problems {
                    if problems.is_none() || problems.as_ref().unwrap().contains(&slug) {
                        let html = self.get(&url).retry_recv_html()?;
                        let (display_name, suite, path) = scrape(&html, &slug)?;
                        outcome.push_problem(RetrieveTestCasesOutcomeBuilderProblem {
                            url,
                            slug,
                            display_name,
                            screen_name: Some(no),
                            test_suite_contents: suite,
                            test_suite_location: path,
                            text_files: indexmap![],
                        });
                    }
                }
            }
        }

        if attempt_full {
            let nos = outcome
                .problems()
                .iter()
                .flat_map(|problem| match &problem.test_suite_contents {
                    TestSuite::Batch(_) => Some(problem.slug.clone()),
                    _ => None,
                })
                .collect::<Vec<_>>();

            if let Err(not_solved) = self.confirm_solved(&nos)? {
                return Err(ServiceError::from(
                    failure::err_msg(format!(
                        "Some problems are not yet solved: {:?}",
                        &*not_solved,
                    ))
                    .context(ServiceErrorKind::UnableToDownloadFull),
                ));
            }

            let client = self.client();
            let reqs = nos
                .iter()
                .map(|no| {
                    let mut url = BASE_URL.clone();
                    url.set_path(&format!("/problems/no/{}/testcase.zip", no));
                    let mut req = client.get(url.clone());
                    if let Some(value) = self.cookies_to_header_value(&url) {
                        req = req.header(header::COOKIE, value);
                    }
                    (download::Name::new(no.clone(), url.into_string()), req)
                })
                .collect::<Vec<_>>();

            for (no, zip) in nos.into_iter().zip(self.download_progress(reqs)?) {
                let problem = outcome
                    .problems_mut()
                    .iter_mut()
                    .find(|p| p.slug == no)
                    .unwrap(); // `no` comes from `outcome`

                if let TestSuite::Batch(suite) = &mut problem.test_suite_contents {
                    suite.clear_cases();
                }

                static ZIP_ENTRIES: Lazy<ZipEntries> = Lazy::new(|| ZipEntries {
                    in_entry: Regex::new(r"\Atest_in/([a-z0-9_]+)\.txt\z").unwrap(),
                    in_match_group: 1,
                    in_crlf_to_lf: true,
                    out_entry: Regex::new(r"\Atest_out/([a-z0-9_]+)\.txt\z").unwrap(),
                    out_match_group: 1,
                    out_crlf_to_lf: true,
                    sortings: vec![ZipEntriesSorting::Dictionary, ZipEntriesSorting::Number],
                });

                for (name, (in_location, in_contents, out_location, out_contents)) in
                    self.extract_zip(&no, &zip, &destinations.text_file_dir(&no)?, &ZIP_ENTRIES)?
                {
                    if let TestSuite::Batch(suite) = &mut problem.test_suite_contents {
                        suite.push_paths(&name, &in_location, &out_location);
                    }

                    problem.text_files.insert(
                        name,
                        RetrieveTestCasesOutcomeBuilderProblemTextFiles {
                            in_contents,
                            in_location,
                            out_contents,
                            out_location,
                        },
                    );
                }
            }
        }

        outcome.finish(open_in_browser, self.stderr())
    }

    fn retrieve_langs(
        &mut self,
        props: RetrieveLangsProps<YukicoderContest>,
    ) -> ServiceResult<RetrieveLangsOutcome> {
        let RetrieveLangsProps { contest, problem } = props;
        let problem = problem.ok_or(ServiceErrorKind::PleaseSpecifyProblem)?;
        self.login(true)?;
        let url = self.get_submit_url(&contest, &problem)?;
        let langs = self.get(url.as_str()).retry_recv_html()?.extract_langs()?;
        Ok(RetrieveLangsOutcome::new(url, langs))
    }

    fn retrieve_submissions(
        &mut self,
        props: RetrieveSubmissionsProps<YukicoderContest>,
    ) -> ServiceResult<RetrieveSubmissionsOutcome> {
        let RetrieveSubmissionsProps {
            contest,
            problems,
            src_paths,
            fetch_all,
            save_files,
        } = props;

        let problem_details = self
            .api_v1_problems()?
            .into_iter()
            .map(|p| (p.no.to_string(), p))
            .collect::<HashMap<_, _>>();

        let langs = self
            .api_v1_languages()?
            .into_iter()
            .map(|lang| {
                let joined = lang.join_name_and_ver();
                (lang.name, joined)
            })
            .collect::<HashMap<_, _>>();

        let names = match (&contest, &problems) {
            (YukicoderContest::No, None) => {
                return Err(ServiceErrorKind::PleaseSpecifyProblem.into());
            }
            (YukicoderContest::No, Some(nos)) => nos
                .iter()
                .map(|no| (no.clone(), no.clone()))
                .collect::<Vec<_>>(),
            (YukicoderContest::Id(id), problems) => {
                let iter = self
                    .get(format!("/contests/{}", id))
                    .retry_recv_html()?
                    .extract_problems()?
                    .into_iter()
                    .map(|(slug, no, _)| (slug, no));
                match problems {
                    None => iter.collect(),
                    Some(problems) => iter.filter(|(slug, _)| problems.contains(slug)).collect(),
                }
            }
        };

        #[derive(Default)]
        struct Found(IndexMap<String, IndexSet<String>>);

        impl Found {
            fn contains(&self, slug: &str, lang: &str) -> bool {
                self.0.get(slug).map_or(false, |ls| ls.contains(lang))
            }

            fn insert(&mut self, slug: String, lang: String) {
                self.0.entry(slug).or_default().insert(lang);
            }
        }

        let mut found = Found::default();

        let mut outcome = RetrieveSubmissionsOutcomeBuilder::new();

        for (slug, no) in names {
            let problem = problem_details
                .get(&no)
                .ok_or_else(|| ServiceErrorKind::NoSuchProblem(no.clone()))?;

            let (mut summaries, num_pages) = self
                .get("/problems/no")
                .extend_path_segments(&[&no, "submissions"])
                .push_url_query("my_submission", "enabled")
                .retry_recv_html()?
                .extract_submission_summaries()?;

            for i in 1..num_pages {
                let (rest_summaries, _) = self
                    .get("/problems/no")
                    .extend_path_segments(&[&no, "submissions"])
                    .push_url_query("my_submission", "enabled")
                    .push_url_query("page", &(i + 1).to_string())
                    .retry_recv_html()?
                    .extract_submission_summaries()?;
                summaries.extend(rest_summaries);
            }

            for summary in summaries {
                let language = langs.get(&summary.lang).ok_or_else(ScrapeError::new)?;

                let mut retrieve_code = || -> _ {
                    let res = self
                        .get(&summary.url)
                        .push_path_segment("source")
                        .acceptable(&[200, 403])
                        .warn(&[403])
                        .retry_send()?;
                    if res.status() == 200 {
                        self.retry_recv_text(res).map(Some)
                    } else {
                        Ok(None)
                    }
                };

                let (code, location) = match src_paths.get(language.as_str()) {
                    Some(location) if fetch_all => {
                        let code = retrieve_code()?;
                        let location = if found.contains(&slug, language) {
                            Some(location)
                        } else {
                            found.insert(slug.clone(), language.clone());
                            None
                        };
                        match (code, location) {
                            (None, _) => (None, None),
                            (Some(code), location) => (Some(code), location),
                        }
                    }
                    None if fetch_all => (retrieve_code()?, None),
                    Some(location) => {
                        if found.contains(&slug, language) {
                            (None, None)
                        } else {
                            found.insert(slug.clone(), language.clone());
                            match retrieve_code()? {
                                None => (None, None),
                                Some(code) => (Some(code), Some(location)),
                            }
                        }
                    }
                    None => (None, None),
                };
                let location = if save_files {
                    location.map(|l| l.expand(Some(&slug))).transpose()?
                } else {
                    None
                };

                outcome.submissions.insert(
                    summary.url,
                    RetrieveSubmissionsOutcomeBuilderSubmission {
                        problem_url: summary.no_url,
                        problem_slug: slug.clone(),
                        problem_display_name: problem.title.clone(),
                        problem_screen_name: Some(problem.problem_id.to_string()),
                        language: language.clone(),
                        date_time: summary.dt,
                        verdict_is_ok: summary.verdict_is_ac,
                        verdict_string: summary.verdict_string,
                        location,
                        code,
                    },
                );
            }
        }

        if let Some(problems) = problems {
            self.warn_not_found(&problems, &outcome.problem_slugs())?;
        }

        outcome.finish()
    }

    fn submit(&mut self, props: SubmitProps<YukicoderContest>) -> ServiceResult<SubmitOutcome> {
        let SubmitProps {
            contest,
            problem,
            lang_name,
            src_path,
            open_in_browser,
            skip_checking_if_accepted,
        } = props;

        let code = crate::fs::read_to_string(&src_path)?;

        self.login(true)?;
        let url = self.get_submit_url(&contest, &problem)?;
        let no = {
            lazy_regex!(r"\A(https://yukicoder\.me)?/problems/no/(\d+)/submit\z")
                .captures(url.as_ref())
                .map(|caps| caps[2].to_owned())
        };
        if let Some(no) = no {
            if !(self.confirm_solved(&[no])?.is_err() || skip_checking_if_accepted) {
                return Err(ServiceErrorKind::AlreadyAccepted.into());
            }
        }
        let html = self.get(url.as_ref()).retry_recv_html()?;
        let lang_id = html
            .extract_langs()?
            .get(&lang_name)
            .ok_or_else(|| ServiceErrorKind::NoSuchLang(lang_name.clone()))?
            .clone();
        let token = html.extract_csrf_token_from_submit_page()?;
        let form = FormBuilder::new()
            .text("csrf_token", token)
            .text("lang", lang_id.clone())
            .text("source", code.clone());
        let url = html.extract_url_from_submit_page()?;
        let res = self.post(&url).multipart(form).send()?;
        let location = res.location_uri()?;
        let rejected = location
            .as_ref()
            .map_or(true, |l| !l.path().contains("/submissions/"));
        if let Some(location) = &location {
            if !rejected && open_in_browser {
                self.open_in_browser(&location.to_string())?;
            }
        }
        Ok(SubmitOutcome {
            rejected,
            response: SubmitOutcomeResponse {
                status: res.status(),
                location,
            },
            language: SubmitOutcomeLanguage {
                name: lang_name,
                id: lang_id,
            },
            file: src_path,
            code,
        })
    }

    fn confirm_solved<'a>(
        &mut self,
        nos: &'a [impl 'a + AsRef<str>],
    ) -> ServiceResult<std::result::Result<(), NonEmptyVec<&'a str>>> {
        #[derive(Deserialize)]
        #[serde(rename_all = "PascalCase")]
        struct Problem {
            no: u64,
        }

        if let Some(username) = self.username.name() {
            let mut url = BASE_URL.clone();
            url.set_path("/api/v1/solved/name");
            url.path_segments_mut().unwrap().push(username);
            let solved = self
                .get(url)
                .retry_recv_json::<Vec<Problem>>()?
                .into_iter()
                .map(|Problem { no }| no.to_string())
                .collect::<HashSet<_>>();
            let not_solved = nos
                .iter()
                .map(AsRef::as_ref)
                .filter(|&no| !solved.contains(no))
                .collect::<Vec<_>>();
            Ok(match NonEmptyVec::try_new(not_solved) {
                None => Ok(()),
                Some(not_solved) => Err(not_solved),
            })
        } else {
            Ok(Ok(()))
        }
    }

    fn get_submit_url(&mut self, contest: &YukicoderContest, problem: &str) -> ServiceResult<Url> {
        let mut ret = match contest {
            YukicoderContest::No => {
                let mut url = BASE_URL.clone();
                url.set_path(&format!("/problems/no/{}", problem));
                url
            }
            YukicoderContest::Id(id) => self
                .get(&format!("/contests/{}", id))
                .retry_recv_html()?
                .extract_problems()?
                .into_iter()
                .filter(|(slug, _, _)| slug.eq_ignore_ascii_case(problem))
                .map(|(_, _, url)| url)
                .next()
                .ok_or_else(|| ServiceErrorKind::NoSuchProblem(problem.to_owned()))?,
        };
        if let Ok(mut path) = ret.path_segments_mut() {
            path.extend(&[".", "submit"]);
        }
        Ok(ret)
    }

    /// <https://petstore.swagger.io/?url=https://yukicoder.me/api/swagger.yaml#/problems/get_problems>
    fn api_v1_problems(&mut self) -> ServiceResult<Vec<api_v1::Problem>> {
        self.get("/api/v1/problems").retry_recv_json()
    }

    /// <https://petstore.swagger.io/?url=https://yukicoder.me/api/swagger.yaml#/language/get_languages>
    fn api_v1_languages(&mut self) -> ServiceResult<Vec<api_v1::Language>> {
        self.get("/api/v1/languages").retry_recv_json()
    }

    /// <https://petstore.swagger.io/?url=https://yukicoder.me/api/swagger.yaml#/contest/get_contest_future>
    fn api_v1_contest_future(&mut self) -> ServiceResult<Vec<api_v1::Contest>> {
        self.get("/api/v1/contest/future").retry_recv_json()
    }

    /// <https://petstore.swagger.io/?url=https://yukicoder.me/api/swagger.yaml#/contest/get_contest_past>
    fn api_v1_contest_past(&mut self) -> ServiceResult<Vec<api_v1::Contest>> {
        self.get("/api/v1/contest/past").retry_recv_json()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum YukicoderContest {
    /// "問題一覧".
    No,
    /// `Id`.
    Id(u64),
}

impl FromStr for YukicoderContest {
    type Err = ParseContestNameError;

    fn from_str(s: &str) -> ParseContestNameResult<Self> {
        if s.eq_ignore_ascii_case("no") {
            Ok(YukicoderContest::No)
        } else {
            s.parse()
                .map(YukicoderContest::Id)
                .map_err(|e| ParseContestNameError::new(s, e))
        }
    }
}

impl Contest for YukicoderContest {
    fn slug(&self) -> Cow<str> {
        match self {
            YukicoderContest::No => Cow::Borrowed("no"),
            YukicoderContest::Id(id) => Cow::Owned(id.to_string()),
        }
    }
}

#[derive(Clone, Debug)]
enum Username {
    None,
    // /public/img/anony.png (for now)
    Yukicoder(String),
    // https://avatars2.githubusercontent.com/...
    Github(String),
    // ?
    ProbablyTwitter(String),
}

impl Username {
    fn name(&self) -> Option<&str> {
        match self {
            Username::None => None,
            Username::Yukicoder(s) | Username::Github(s) | Username::ProbablyTwitter(s) => Some(&s),
        }
    }
}

impl fmt::Display for Username {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Username::None => write!(f, "<not logged in>"),
            Username::Yukicoder(s) => write!(f, "{} (yukicoder)", s.trim()),
            Username::Github(s) => write!(f, "{} (GitHub)", s.trim()),
            Username::ProbablyTwitter(s) => write!(f, "{} (probably Twitter)", s.trim()),
        }
    }
}

#[derive(Debug)]
struct SubmissionSummary {
    url: Url,
    no_url: Url,
    no_display_name: String,
    dt: DateTime<FixedOffset>,
    lang: String,
    verdict_is_ac: bool,
    verdict_string: String,
}

trait Extract {
    fn extract_username(&self) -> Username;
    fn extract_samples(&self) -> ScrapeResult<(String, TestSuite)>;
    fn extract_problems(&self) -> ScrapeResult<NonEmptyVec<(String, String, Url)>>;
    fn extract_submission_summaries(&self) -> ScrapeResult<(Vec<SubmissionSummary>, usize)>;
    fn extract_csrf_token_from_submit_page(&self) -> ScrapeResult<String>;
    fn extract_url_from_submit_page(&self) -> ScrapeResult<String>;
    fn extract_langs(&self) -> ScrapeResult<NonEmptyIndexMap<String, String>>;
}

impl Extract for Html {
    fn extract_username(&self) -> Username {
        let extract = || {
            let a = self.select(selector!("#usermenu > a")).next()?;
            let name = a.text().next()?.to_owned();
            let src = a.select(selector!("img")).next()?.value().attr("src")?;
            Some(if src == "/public/img/anony.png" {
                Username::Yukicoder(name)
            } else if src.starts_with("https://avatars2.githubusercontent.com") {
                Username::Github(name)
            } else {
                Username::ProbablyTwitter(name)
            })
        };
        extract().unwrap_or(Username::None)
    }

    fn extract_samples(&self) -> ScrapeResult<(String, TestSuite)> {
        #[derive(Clone, Copy, PartialEq)]
        enum ProblemKind {
            Regular,
            Special,
            Reactive,
        }

        let extract = || {
            let display_name = self
                .select(selector!("#content > h3"))
                .flat_map(|r| r.text())
                .nth(0)?
                .to_owned();
            let text = self
                .select(selector!("#content > div"))
                .flat_map(|r| r.text())
                .nth(1)?;
            let caps = lazy_regex!(
                "\\A / 実行時間制限 : 1ケース (\\d)\\.(\\d{3})秒 / メモリ制限 : \\d+ MB / \
                 (通常|スペシャルジャッジ|リアクティブ)問題.*\n?.*\\z",
            )
            .captures(text)?;
            let timelimit = {
                let s = caps[1].parse::<u64>().unwrap();
                let m = caps[2].parse::<u64>().unwrap();
                Duration::from_millis(1000 * s + m)
            };
            let kind = match &caps[3] {
                "通常" => ProblemKind::Regular,
                "スペシャルジャッジ" => ProblemKind::Special,
                "リアクティブ" => ProblemKind::Reactive,
                _ => return None,
            };
            let suite = match kind {
                ProblemKind::Regular | ProblemKind::Special => {
                    let mut samples = vec![];
                    for paragraph in self.select(selector!(
                        "#content > div.block > div.sample > div.paragraph",
                    )) {
                        let pres = paragraph
                            .select(selector!("pre"))
                            .flat_map(|r| r.text())
                            .collect::<Vec<_>>();
                        guard!(pres.len() == 2);
                        let input = pres[0].to_owned();
                        let output = match kind {
                            ProblemKind::Regular => Some(pres[1].to_owned()),
                            ProblemKind::Special => None,
                            ProblemKind::Reactive => unreachable!(),
                        };
                        samples.push((input, output));
                    }
                    let mut suite = BatchSuite::new(timelimit)
                        .sample_cases(samples.into_iter(), |i| format!("サンプル{}", i + 1));
                    if kind == ProblemKind::Special {
                        suite = suite.matching(testsuite::Match::Any);
                    }
                    suite.into()
                }
                ProblemKind::Reactive => InteractiveSuite::new(timelimit).into(),
            };
            Some((display_name, suite))
        };
        extract().ok_or_else(ScrapeError::new)
    }

    fn extract_problems(&self) -> ScrapeResult<NonEmptyVec<(String, String, Url)>> {
        self.select(selector!("#content > div.left > table.table > tbody > tr"))
            .map(|tr| {
                let slug = tr.select(selector!("td")).nth(0)?.text().next()?.to_owned();
                let no = tr.select(selector!("td")).nth(1)?.text().next()?.to_owned();
                let href = tr
                    .select(selector!("td"))
                    .nth(2)?
                    .select(selector!("a"))
                    .next()?
                    .value()
                    .attr("href")?;
                let url = BASE_URL.join(href).ok()?;
                Some((slug, no, url))
            })
            .collect::<Option<Vec<_>>>()
            .and_then(NonEmptyVec::try_new)
            .ok_or_else(ScrapeError::new)
    }

    fn extract_submission_summaries(&self) -> ScrapeResult<(Vec<SubmissionSummary>, usize)> {
        let table = self
            .select(selector!("table.table"))
            .next()
            .ok_or_else(ScrapeError::new)?;

        guard!(
            table
                .select(selector!("thead > tr > th"))
                .flat_map(|r| r.text())
                .collect::<Vec<_>>()
                == [
                    "#",
                    "提出日時",
                    "提出者",
                    "問題",
                    "言語",
                    "結果",
                    "実行時間",
                    "コード長",
                ]
        );

        let summaries = table
            .select(selector!("tbody > tr"))
            .map(|tr| {
                let url = tr
                    .select(selector!("td > a"))
                    .nth(0)?
                    .value()
                    .attr("href")?
                    .parse_with_base_url(Some(&BASE_URL))
                    .ok()?;

                let dt = tr.select(selector!("td.time")).next()?.text().next()?;
                let dt = NaiveDateTime::parse_from_str(dt, "%F %T").ok()?;
                let dt = FixedOffset::east(9 * 3600)
                    .from_local_datetime(&dt)
                    .single()?;

                let a = tr.select(selector!("td > a")).last()?;
                let no_url = a
                    .value()
                    .attr("href")?
                    .parse_with_base_url(Some(&BASE_URL))
                    .ok()?;
                let no_display_name = a.text().next()?.to_owned();

                let td = tr.select(selector!("td")).nth(5)?;
                let lang = td.text().next()?.to_owned();

                let verdict_string = tr
                    .select(selector!("td > span"))
                    .flat_map(|r| r.text())
                    .last()? // may contain multiple verdicts
                    .to_owned();

                Some(SubmissionSummary {
                    url,
                    no_url,
                    no_display_name,
                    dt,
                    lang,
                    verdict_is_ac: verdict_string == "AC",
                    verdict_string,
                })
            })
            .collect::<Option<Vec<_>>>()
            .ok_or_else(ScrapeError::new)?;

        let num_pages = if summaries.is_empty() {
            1
        } else {
            self.select(selector!("#content > div > nav > ul > li"))
                .flat_map(|r| r.text())
                .flat_map(|s| s.parse())
                .max()
                .ok_or_else(ScrapeError::new)?
        };

        Ok((summaries, num_pages))
    }

    fn extract_csrf_token_from_submit_page(&self) -> ScrapeResult<String> {
        self.select(selector!("#submit_form > input[name=\"csrf_token\"]"))
            .find_map(|r| r.value().attr("value").map(ToOwned::to_owned))
            .ok_or_else(ScrapeError::new)
    }

    fn extract_url_from_submit_page(&self) -> ScrapeResult<String> {
        self.select(selector!("#submit_form"))
            .find_map(|r| r.value().attr("action").map(ToOwned::to_owned))
            .ok_or_else(ScrapeError::new)
    }

    fn extract_langs(&self) -> ScrapeResult<NonEmptyIndexMap<String, String>> {
        let names = self
            .select(selector!("#lang > option"))
            .map(|option| {
                let name = option.text().next()?;
                let name = lazy_regex!(r"[\s\n]+")
                    .replace_all(name.trim(), " ")
                    .into_owned();
                let id = option.value().attr("value")?.to_owned();
                Some((name, id))
            })
            .map(|p| p.ok_or_else(ScrapeError::new))
            .collect::<ScrapeResult<IndexMap<_, _>>>()?;
        NonEmptyIndexMap::try_new(names).ok_or_else(ScrapeError::new)
    }
}

/// <https://petstore.swagger.io/?url=https://yukicoder.me/api/swagger.yaml>
mod api_v1 {
    use serde::Deserialize;

    /// <https://petstore.swagger.io/?url=https://yukicoder.me/api/swagger.yaml>
    #[derive(Debug, Deserialize)]
    #[serde(rename_all = "PascalCase")]
    pub(super) struct Problem {
        /// "問題No"
        pub(super) no: u64,
        /// "問題Id"
        pub(super) problem_id: u64,
        /// "問題名"
        pub(super) title: String,
        // __rest: (),
    }

    /// <https://petstore.swagger.io/?url=https://yukicoder.me/api/swagger.yaml>
    #[derive(Debug, Deserialize)]
    #[serde(rename_all = "PascalCase")]
    pub(super) struct Contest {
        /// "一意な値　コンテストID"
        pub(super) id: u64,
        /// "コンテスト名"
        pub(super) name: String,
        // __rest: (),
    }

    /// <https://petstore.swagger.io/?url=https://yukicoder.me/api/swagger.yaml>
    #[derive(Debug, Deserialize)]
    #[serde(rename_all = "PascalCase")]
    pub(super) struct Language {
        /// "言語ID　（一意な文字列）"
        pub(super) id: String,
        /// "言語名 "
        pub(super) name: String,
        /// "コンパイラバージョン"
        pub(super) ver: String,
    }

    impl Language {
        pub(super) fn join_name_and_ver(&self) -> String {
            format!("{} ({})", self.name, self.ver)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::service;
    use crate::service::session::ParseWithBaseUrl as _;
    use crate::service::yukicoder::{Extract as _, BASE_URL};

    use failure::Fallible;
    use pretty_assertions::assert_eq;
    use retry::OperationResult;
    use scraper::Html;

    use std::iter;
    use std::time::Duration;

    #[test]
    fn it_extracts_samples_from_no_1() -> Fallible<()> {
        test_extracting_samples(
            "https://yukicoder.me/problems/no/1",
            "No.1  道のショートカット",
            "cf65ae411bc8d32b75beb771905c9dc0",
        )
    }

    #[test]
    fn it_extracts_samples_from_no_188() -> Fallible<()> {
        test_extracting_samples(
            "https://yukicoder.me/problems/no/188",
            "No.188  HAPPY DAY",
            "671c7191064f7703abcb5e06fad3f32e",
        )
    }

    #[test]
    fn it_extracts_samples_from_no_192() -> Fallible<()> {
        test_extracting_samples(
            "https://yukicoder.me/problems/no/192",
            "No.192  合成数",
            "f8ce3328c431737dcb748770abd9a09b",
        )
    }

    #[test]
    fn it_extracts_samples_from_no_246() -> Fallible<()> {
        test_extracting_samples(
            "https://yukicoder.me/problems/no/246",
            "No.246  質問と回答",
            "9debfd89a82271d763b717313363acda",
        )
    }

    fn test_extracting_samples(
        url: &str,
        expected_display: &str,
        expected_md5: &str,
    ) -> Fallible<()> {
        let html =
            service::reqwest_sync_client(Duration::from_secs(60))?.retry_retrieve_html(url)?;
        let (actual_display, suite) = html.extract_samples()?;
        let actual_md5 = format!("{:?}", suite.md5()?);
        let actual = (actual_display.as_ref(), actual_md5.as_ref());
        let expected = (expected_display, expected_md5);

        suite.assert_serialize_correctly()?;
        assert_eq!(actual, expected);
        Ok(())
    }

    #[test]
    fn it_extracts_problems_names_and_hrefs_from_yukicoder_open_2015_small() -> Fallible<()> {
        static EXPECTED: &[(&str, &str, &str)] = &[
            ("A", "191", "https://yukicoder.me/problems/no/191"),
            ("B", "192", "https://yukicoder.me/problems/no/192"),
            ("C", "193", "https://yukicoder.me/problems/no/193"),
            ("D", "194", "https://yukicoder.me/problems/no/194"),
            ("E", "195", "https://yukicoder.me/problems/no/195"),
            ("F", "196", "https://yukicoder.me/problems/no/196"),
        ];

        let html = service::reqwest_sync_client(Duration::from_secs(60))?
            .retry_retrieve_html("https://yukicoder.me/contests/100")?;
        let actual = html.extract_problems()?;
        let actual = actual
            .iter()
            .map(|(s, d, u)| (s.as_ref(), d.as_ref(), u.as_ref()))
            .collect::<Vec<_>>();
        assert_eq!(actual, EXPECTED);
        Ok(())
    }

    #[test]
    fn it_extracts_submission_summaries() -> Fallible<()> {
        let client = service::reqwest_sync_client(Duration::from_secs(60))?;
        let (_, num_pages) = client
            .retry_retrieve_html("https://yukicoder.me/problems/no/1/submissions?date_asc=enabled")?
            .extract_submission_summaries()?;
        assert!(num_pages >= 21);
        Ok(())
    }

    trait RetryRetrieveHtml {
        fn retry_retrieve_html(&self, uri: &str) -> Fallible<Html>;
    }

    impl RetryRetrieveHtml for reqwest::Client {
        fn retry_retrieve_html(&self, uri: &str) -> Fallible<Html> {
            const RETRIES: usize = 2;
            const INTERVAL: Duration = Duration::from_secs(1);

            let url = uri.parse_with_base_url(Some(&BASE_URL))?;
            retry::retry(iter::repeat(INTERVAL).take(RETRIES), || {
                let text = self
                    .get(url.clone())
                    .send()
                    .and_then(|r| r.error_for_status()?.text());
                match text {
                    Ok(text) => OperationResult::Ok(Html::parse_document(&text)),
                    Err(err) => {
                        if err.is_http() || err.is_timeout() {
                            OperationResult::Retry(err)
                        } else {
                            OperationResult::Err(err)
                        }
                    }
                }
            })
            .map_err(Into::into)
        }
    }
}
