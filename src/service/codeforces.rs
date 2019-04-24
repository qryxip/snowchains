#![allow(non_snake_case)]

use crate::errors::{
    ParseContestNameError, ParseContestNameResult, ScrapeError, ScrapeResult, ServiceError,
    ServiceErrorKind, ServiceResult,
};
use crate::service::session::HttpSession;
use crate::service::{
    Contest, LoginOutcome, RetrieveLangsOutcome, RetrieveLangsProps, RetrieveSubmissionsOutcome,
    RetrieveSubmissionsProps, RetrieveTestCasesOutcome, RetrieveTestCasesOutcomeProblem,
    RetrieveTestCasesProps, Service, SessionProps, SubmitOutcome, SubmitProps,
};
use crate::terminal::{HasTermProps, Input, WriteColorExt as _};
use crate::testsuite::{self, BatchSuite, TestSuite};
use crate::util::collections::NonEmptyIndexMap;
use crate::util::scraper::ElementRefExt as _;
use crate::util::std_unstable::RemoveItem_ as _;
use crate::util::str::CaseConversion;

use if_chain::if_chain;
use indexmap::IndexMap;
use itertools::Itertools as _;
use maplit::hashmap;
use once_cell::sync::Lazy;
use rand::Rng;
use regex::Regex;
use reqwest::{header, StatusCode};
use scraper::{ElementRef, Html, Node, Selector};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Deserializer};
use serde_derive::{Deserialize, Serialize};
use sha2::{Digest as _, Sha512};
use termcolor::WriteColor;
use tokio::runtime::Runtime;
use url::Url;

use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap};
use std::io::{self, Write as _};
use std::str::FromStr;
use std::time::{Duration, SystemTime};

pub(super) fn login(
    props: SessionProps,
    stdin: impl Input,
    stderr: impl WriteColor + HasTermProps,
) -> ServiceResult<LoginOutcome> {
    Codeforces::try_new(props, stdin, stderr)?.login(LoginOption::Explicit)
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
    Codeforces::try_new(sess_props, stdin, stderr)?.retrieve_testcases(retrieve_props)
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
    Codeforces::try_new(sess_props, stdin, stderr)?.retrieve_langs(retrieve_props)
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
    Codeforces::try_new(sess_props, stdin, stderr)?.retrieve_submissions(retrieve_props)
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
    Codeforces::try_new(sess_props, stdin, stderr)?.submit(submit_props)
}

struct Codeforces<I: Input, E: WriteColor + HasTermProps> {
    login_retries: Option<u32>,
    stdin: I,
    stderr: E,
    session: HttpSession,
    runtime: Runtime,
    handle: Option<String>,
    api_key: Option<ApiKey>,
}

impl<I: Input, E: WriteColor + HasTermProps> Service for Codeforces<I, E> {
    type Stdin = I;
    type Stderr = E;

    fn requirements(&mut self) -> (&mut I, &mut E, &mut HttpSession, &mut Runtime) {
        (
            &mut self.stdin,
            &mut self.stderr,
            &mut self.session,
            &mut self.runtime,
        )
    }
}

impl<I: Input, E: WriteColor + HasTermProps> Codeforces<I, E> {
    fn try_new(props: SessionProps, stdin: I, mut stderr: E) -> ServiceResult<Self> {
        let mut runtime = Runtime::new()?;
        let session = props.start_session(&mut stderr, &mut runtime)?;
        Ok(Self {
            login_retries: props.login_retries,
            stdin,
            stderr,
            session,
            runtime,
            handle: None,
            api_key: None,
        })
    }

    fn login(&mut self, option: LoginOption) -> ServiceResult<LoginOutcome> {
        static HANDLE: &Lazy<Regex> = lazy_regex!(r#"\A/profile/([a-zA-Z0-9_\-]+)\z"#);

        let mut res = self.get("/enter").acceptable(&[200, 302]).send()?;
        let mut sent = false;

        if res.status() == 302 && option == LoginOption::Explicit {
            writeln!(self.stderr, "Already logged in.")?;
            self.stderr.flush()?;
        } else if res.status() == 200 {
            let mut retries = self.login_retries;
            loop {
                let mut payload = res
                    .html(&mut self.runtime)?
                    .extract_hidden_values(selector!("#enterForm"))?;
                let handle_or_email = self.prompt_reply_stderr("Handle/Email: ")?;
                let password = self.prompt_password_stderr("Password: ")?;
                payload.insert("handleOrEmail".to_owned(), handle_or_email);
                payload.insert("password".to_owned(), password);
                payload.insert("remember".to_owned(), "on".to_owned());
                res = self
                    .post("/enter")
                    .acceptable(&[200, 302])
                    .send_form(&payload)?;
                if res.status() == 302 {
                    sent = true;
                    break;
                }
                if retries == Some(0) {
                    return Err(ServiceErrorKind::LoginRetriesExceeded.into());
                }
                retries = retries.map(|n| n - 1);
                writeln!(self.stderr, "Failed to login. Try again.")?;
                self.stderr.flush()?;
            }
        }

        if sent {
            res = self.get("/enter").acceptable(&[302]).send()?;
        }
        let handle = if_chain! {
            if let Some(location) = res.headers().get(header::LOCATION);
            if let Ok(location) = location.to_str();
            if let Ok(location) = location.parse::<Url>();
            if let Some(caps) = HANDLE.captures(location.path());
            then {
                caps[1].to_owned()
            } else {
                unimplemented!("{:?}", res.headers().get(header::LOCATION))
            }
        };
        if option == LoginOption::Explicit {
            self.api::<Vec<User>>("user.info", &[("handles", &handle)])?;
        }
        self.handle = Some(handle);
        Ok(LoginOutcome {})
    }

    fn retrieve_testcases(
        &mut self,
        props: RetrieveTestCasesProps<CodeforcesContest>,
    ) -> ServiceResult<RetrieveTestCasesOutcome> {
        let RetrieveTestCasesProps {
            contest,
            problems,
            destinations,
            open_in_browser,
            ..
        } = props;

        let problems = problems.as_ref();
        let top_path = format!("/contest/{}", contest.id);

        let mut outcome = RetrieveTestCasesOutcome::new(&contest);

        let mut res = self.get(&top_path).acceptable(&[200, 302]).send()?;
        if res.status() == 302 {
            self.login(LoginOption::WithHandle)?;
            let contest_id = contest.id.to_string();
            let handle = self.handle.clone().unwrap();
            let standings = self.api::<Standings>(
                "contest.standings",
                &[("contestId", &contest_id), ("handles", &handle)],
            )?;
            // Codeforces returns "FAILED" if the contest has not started.
            if standings.contest.phase != ContestPhase::Finished {
                unimplemented!(
                    "phase = {:?}: Register in browser.",
                    standings.contest.phase
                );
            }
            res = self.get(&top_path).send()?;
        }
        for (slug, url) in res.html(&mut self.runtime)?.extract_problems()? {
            if problems.map_or(true, |ps| ps.contains(&slug)) {
                let suite = self.get(url.as_ref()).recv_html()?.extract_test_suite()?;
                let path = destinations.expand(&slug)?;
                outcome.push_problem(slug.clone(), url, suite, path);
            }
        }

        let mut not_found = problems
            .map(|ps| ps.iter().collect::<Vec<_>>())
            .unwrap_or_default();

        for RetrieveTestCasesOutcomeProblem {
            slug,
            test_suite,
            test_suite_path,
            ..
        } in &outcome.problems
        {
            test_suite.save(slug, test_suite_path, &mut self.stderr)?;
            not_found.remove_item_(&slug);
        }
        self.stderr.flush()?;

        if !not_found.is_empty() {
            self.stderr
                .with_reset(|w| writeln!(w.fg(11).set()?, "Not found: {:?}", not_found))?;
            self.stderr.flush()?;
        }

        if open_in_browser {
            self.open_in_browser(&format!("/contest/{}/my", contest.id))?;
            for RetrieveTestCasesOutcomeProblem { url, .. } in &outcome.problems {
                self.open_in_browser(url.as_str())?;
            }
        }

        Ok(outcome)
    }

    fn retrieve_langs(
        &mut self,
        props: RetrieveLangsProps<CodeforcesContest>,
    ) -> ServiceResult<RetrieveLangsOutcome> {
        let RetrieveLangsProps { contest, .. } = props;
        self.login(LoginOption::WithHandle)?;
        let url = self
            .session
            .resolve_url(&format!("/contest/{}/submit", contest.id))?;
        let langs = self.get(&url).recv_html()?.extract_langs()?;
        self.print_lang_list(&langs)?;
        Ok(RetrieveLangsOutcome::new(url, langs))
    }

    fn retrieve_submissions(
        &mut self,
        props: RetrieveSubmissionsProps<CodeforcesContest>,
    ) -> ServiceResult<RetrieveSubmissionsOutcome> {
        #[derive(Deserialize)]
        struct SubmitSource {
            source: String,
        }

        let RetrieveSubmissionsProps { contest, .. } = props;

        self.login(LoginOption::WithHandle)?;

        let contest_id = contest.id.to_string();
        let handle = self.handle.clone().unwrap();
        let submissions = self.api::<Vec<Submission>>(
            "contest.status",
            &[("contestId", &contest_id), ("handle", &handle)],
        )?;

        let csrf_token = self.get("/").recv_html()?.extract_meta_x_csrf_token()?;
        for submission in submissions {
            let submission_id = submission.id.to_string();
            let SubmitSource { source } = self
                .post("/data/submitSource")
                .acceptable(&[200])
                .form(&hashmap!("submissionId" => &submission_id, "csrf_token" => &csrf_token))
                .recv_json()?;
            dbg!((submission.id, &submission.programmingLanguage, source.len()));
        }

        unimplemented!()
    }

    fn submit(&mut self, props: SubmitProps<CodeforcesContest>) -> ServiceResult<SubmitOutcome> {
        let SubmitProps {
            contest,
            problem,
            lang_name,
            src_path,
            open_in_browser,
            skip_checking_if_accepted,
        } = props;

        let src = crate::fs::read_to_string(&src_path)?;
        let src_len = src.len();

        self.login(LoginOption::WithHandle)?;

        let contest_id = contest.id.to_string();
        let handle = self.handle.clone().unwrap();
        let standings = self.api::<Standings>(
            "contest.standings",
            &[("contestId", &contest_id), ("handles", &handle)],
        )?;

        if standings.problems.iter().all(|p| p.index != problem) {
            return Err(ServiceErrorKind::NoSuchProblem(problem).into());
        }
        if !skip_checking_if_accepted && standings.contest.phase != ContestPhase::Finished {
            let submissions = self.api::<Vec<Submission>>(
                "contest.status",
                &[("contestId", &contest_id), ("handle", &handle)],
            )?;
            if submissions
                .iter()
                .any(|s| s.problem.index == *problem && s.verdict == Some(SubmissionVerdict::Ok))
            {
                return Err(ServiceErrorKind::AlreadyAccepted.into());
            }
        }

        let submit_path = format!("/contest/{}/submit", contest.id);

        let html = self.get(&submit_path).recv_html()?;

        let lang_id = html
            .extract_langs()?
            .get(&lang_name)
            .ok_or_else(|| ServiceErrorKind::NoSuchLang(lang_name.clone()))?
            .clone();
        writeln!(
            self.stderr,
            "Submitting as {:?} (ID: {:?})",
            lang_name, lang_id,
        )?;

        let mut values = html.extract_hidden_values(selector!("form.submit-form"))?;
        values.insert("contestId".to_owned(), contest.id.to_string());
        values.insert("submittedProblemIndex".to_owned(), problem.clone());
        values.insert("tabSize".to_owned(), "4".to_owned());
        values.insert("programTypeId".to_owned(), lang_id.clone());
        values.insert("source".to_owned(), src);

        let status = self
            .post(&submit_path)
            .acceptable(&[200, 302])
            .send_form(&values)?
            .status();
        if status == 200 {
            return Err(ServiceError::from(ServiceErrorKind::SubmissionRejected {
                lang_name,
                lang_id,
                size: src_len,
                status: StatusCode::OK,
                location: None,
            }));
        }

        if open_in_browser {
            self.open_in_browser(&format!("/contest/{}/my", contest.id))?;
        }
        Ok(SubmitOutcome {})
    }

    fn api<T: DeserializeOwned + Send + Sync + 'static>(
        &mut self,
        method: &'static str,
        query_pairs: &[(&'static str, &str)],
    ) -> ServiceResult<T> {
        struct ApiOk<T: DeserializeOwned>(T);

        impl<'de, T: DeserializeOwned> Deserialize<'de> for ApiOk<T> {
            fn deserialize<D: Deserializer<'de>>(
                deserializer: D,
            ) -> std::result::Result<Self, D::Error> {
                #[derive(Deserialize)]
                struct Repr<E: DeserializeOwned> {
                    status: String,
                    #[serde(deserialize_with = "E::deserialize")]
                    result: E,
                }

                let repr = Repr::<T>::deserialize(deserializer)?;
                if repr.status == "OK" {
                    Ok(ApiOk(repr.result))
                } else {
                    Err(serde::de::Error::custom("`.status` must be \"OK\""))
                }
            }
        }

        struct ApiErr(String);

        impl<'de> Deserialize<'de> for ApiErr {
            fn deserialize<D: Deserializer<'de>>(
                deserializer: D,
            ) -> std::result::Result<Self, D::Error> {
                #[derive(Deserialize)]
                struct Repr {
                    status: String,
                    comment: String,
                }

                let repr = Repr::deserialize(deserializer)?;
                if repr.status == "FAILED" {
                    Ok(ApiErr(repr.comment))
                } else {
                    Err(serde::de::Error::custom("`.status` must be \"FAILED\""))
                }
            }
        }

        fn ask_api_key(
            this: &mut Codeforces<impl Input, impl WriteColor + HasTermProps>,
            p: &mut bool,
        ) -> io::Result<ApiKey> {
            *p = true;
            let key = this.prompt_password_stderr("API Key: ")?;
            let secret = this.prompt_password_stderr("API Secret: ")?;
            Ok(ApiKey { key, secret })
        }

        let mut asked_api_key = false;
        let mut api_key = if let Some(api_key) = &self.api_key {
            api_key.clone()
        } else if self.session.api_token().exists() {
            self.session.api_token().get_or_init()?.json::<ApiKey>()?
        } else {
            ask_api_key(self, &mut asked_api_key)?
        };

        loop {
            let mut url = base_url();

            url.set_path(&format!("/api/{}", method));

            let time = SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)?
                .as_secs()
                .to_string();
            let query_pairs = {
                let mut query_pairs = query_pairs.iter().cloned().collect::<BTreeMap<_, _>>();
                query_pairs.insert("apiKey", &api_key.key);
                query_pairs.insert("time", &time);
                query_pairs
            };
            url.query_pairs_mut().extend_pairs(query_pairs);

            let sig = {
                let rand = rand::thread_rng().gen_range(100_000u32, 1_000_000u32);
                let mut hasher = Sha512::new();
                let repr = format!("{}{}#{}", rand, &url.as_str()[26..], api_key.secret);
                hasher.input(&repr);
                format!("{}{}", rand, hex::encode(&hasher.result()))
            };
            url.query_pairs_mut().append_pair("apiSig", &sig);

            let res = self.get(url.as_str()).acceptable(&[200, 400]).send()?;
            if res.status() == 200 {
                if asked_api_key {
                    self.session
                        .api_token()
                        .get_or_init()?
                        .write_json(&api_key)?;
                }
                self.api_key = Some(api_key);
                let ApiOk(ret) = res.json(&mut self.runtime)?;
                break Ok(ret);
            }
            let ApiErr(comment) = res.json(&mut self.runtime)?;
            if !comment.starts_with("apiKey: ") {
                break Err(failure::err_msg(comment)
                    .context(ServiceErrorKind::Api)
                    .into());
            }
            api_key = ask_api_key(self, &mut asked_api_key)?;
        }
    }
}

#[derive(Clone, Copy, derive_more::Display)]
#[display(fmt = "{}", id)]
struct CodeforcesContest {
    id: u64,
}

impl FromStr for CodeforcesContest {
    type Err = ParseContestNameError;

    fn from_str(s: &str) -> ParseContestNameResult<Self> {
        s.parse()
            .map(|id| CodeforcesContest { id })
            .map_err(|e| ParseContestNameError::new(s, e))
    }
}

impl Contest for CodeforcesContest {
    fn slug(&self) -> Cow<str> {
        self.to_string().into()
    }
}

#[derive(PartialEq)]
enum LoginOption {
    WithHandle,
    Explicit,
}

#[derive(Clone, Serialize, Deserialize)]
struct ApiKey {
    key: String,
    secret: String,
}

#[derive(Debug, PartialEq, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
enum ContestPhase {
    Before,
    Coding,
    PendingSystemTest,
    SystemTest,
    Finished,
}

#[derive(Deserialize)]
struct User {
    // handle: String,
// email: Option<String>,
// vkld: Option<String>,
// openId: Option<String>,
// firstName: Option<String>,
// lastName: Option<String>,
// country: Option<String>,
// city: Option<String>,
// organization: Option<String>,
// contribution: f64,
// rank: Option<String>,
// rating: Option<f64>,
// maxRank: Option<String>,
// maxRanking: Option<f64>,
// lastOnlineTimeSeconds: f64,
// registrationTimeSeconds: f64,
// friendOfCount: f64,
// avatar: String,
// titlePhoto: String,
}

#[derive(Deserialize)]
struct ContestEntity {
    // id: u64,
    // name: String,
    // r#type: String,
    phase: ContestPhase,
    // frozen: bool,
    // durationSeconds: f64,
    // startTimeSeconds: Option<f64>,
    // relativeTimeSeconds: Option<f64>,
    // preparedBy: Option<String>,
    // websiteUrl: Option<String>,
    // description: Option<String>,
    // difficulty: Option<f64>,
    // kind: Option<String>,
    // icpcRegion: Option<String>,
    // country: Option<String>,
    // city: Option<String>,
    // season: Option<String>,
}

#[derive(Deserialize)]
struct Problem {
    // contestId: Option<u64>,
    // problemsetName: Option<String>,
    index: String,
    // name: String,
    // r#type: String,
    // points: Option<f64>,
    // tags: Vec<String>,
}

#[derive(Deserialize)]
struct Submission {
    id: u64,
    // contestId: Option<u64>,
    // creationTimeSeconds: f64,
    // relativeTimeSeconds: f64,
    problem: Problem,
    // author: (),
    programmingLanguage: String,
    verdict: Option<SubmissionVerdict>,
    // testset: String,
    // passedTestCount: f64,
    // timeConsumedMillis: f64,
    // memoryConsumedBytes: f64,
}

#[derive(PartialEq, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
enum SubmissionVerdict {
    Failed,
    Ok,
    Partial,
    CompilationError,
    RuntimeError,
    WrongAnswer,
    PresentationError,
    TimeLimitExceeded,
    MemoryLimitExceeded,
    IdlenessLimitExceeded,
    SecurityViolated,
    Crashed,
    InputPreparationCrashed,
    Challenged,
    Skipped,
    Testing,
    Rejected,
}

#[derive(Deserialize)]
struct Standings {
    contest: ContestEntity,
    problems: Vec<Problem>,
    // rows: Vec<()>,
}

trait Extract {
    fn extract_hidden_values(&self, form: &Selector) -> ScrapeResult<HashMap<String, String>>;
    fn extract_problems(&self) -> ScrapeResult<NonEmptyIndexMap<String, Url>>;
    fn extract_test_suite(&self) -> ScrapeResult<TestSuite>;
    fn extract_meta_x_csrf_token(&self) -> ScrapeResult<String>;
    fn extract_langs(&self) -> ScrapeResult<NonEmptyIndexMap<String, String>>;
}

impl Extract for Html {
    fn extract_hidden_values(&self, form: &Selector) -> ScrapeResult<HashMap<String, String>> {
        let mut values = self
            .select(form)
            .flat_map(|r| r.select(selector!("input[type=\"hidden\"]")))
            .flat_map(|input| {
                let input = input.value();
                let name = input.attr("name")?.to_owned();
                let value = input.attr("value")?.to_owned();
                Some((name, value))
            })
            .collect::<HashMap<String, String>>();
        if values.is_empty() {
            Err(ScrapeError::new())
        } else {
            if let Some(ftaa) = values.get_mut("ftaa") {
                *ftaa = "0".repeat(18);
            }
            if let Some(bfaa) = values.get_mut("bfaa") {
                *bfaa = "n/a".to_owned();
            }
            Ok(values)
        }
    }

    fn extract_problems(&self) -> ScrapeResult<NonEmptyIndexMap<String, Url>> {
        let problems = self
            .select(selector!("table.problems > tbody > tr > td.id > a"))
            .map(|a| {
                let slug = a.text().assert_one()?.trim().to_owned();
                let mut href = base_url();
                href.set_path(a.value().attr("href").ok_or_else(ScrapeError::new)?);
                Ok((slug, href))
            })
            .collect::<ScrapeResult<IndexMap<_, _>>>()?;
        NonEmptyIndexMap::try_new(problems).ok_or_else(ScrapeError::new)
    }

    fn extract_test_suite(&self) -> ScrapeResult<TestSuite> {
        let timelimit = self
            .select(selector!("#pageContent div.time-limit"))
            .flat_map(|r| r.text())
            .flat_map(|text| {
                let caps = lazy_regex!(r#"\A([0-9]{1,9})(\.[0-9])? seconds?\z"#).captures(text)?;
                let secs = caps[1].parse::<u64>().unwrap();
                let nanos = caps
                    .get(2)
                    .map(|s| 100_000_000 * u32::from(s.as_str().as_bytes()[1] - b'0'))
                    .unwrap_or(0);
                Some(Duration::new(secs, nanos))
            })
            .assert_one()?;
        dbg!(self
            .select(selector!("#pageContent div.input-file"))
            .flat_map(|r| r.text())
            .collect::<Vec<_>>());
        let input_file = self
            .select(selector!("#pageContent div.input-file"))
            .flat_map(|r| r.children_text())
            .assert_one()?;
        let output_file = self
            .select(selector!("#pageContent div.output-file"))
            .flat_map(|r| r.children_text())
            .assert_one()?;
        if input_file != "standard input" || output_file != "standard output" {
            unimplemented!();
        }

        let sample_test = self
            .select(selector!("#pageContent div.sample-test"))
            .assert_one()?;
        let ins = sample_test
            .select(selector!("div.input > pre"))
            .map(|p| p.fold_text_and_br())
            .collect::<Vec<_>>();
        let outs = sample_test
            .select(selector!("div.output > pre"))
            .map(|p| p.fold_text_and_br())
            .collect::<Vec<_>>();
        if ins.is_empty() || ins.len() != outs.len() {
            Err(ScrapeError::new())
        } else {
            Ok(TestSuite::Batch(
                BatchSuite::new(timelimit)
                    .matching(testsuite::Match::Exact)
                    .sample_cases(ins.into_iter().zip_eq(outs), |i| format!("sample {}", i)),
            ))
        }
    }

    fn extract_meta_x_csrf_token(&self) -> ScrapeResult<String> {
        self.select(selector!("meta[name=\"X-Csrf-Token\"]"))
            .next()
            .and_then(|r| r.value().attr("content").map(ToOwned::to_owned))
            .ok_or_else(ScrapeError::new)
    }

    fn extract_langs(&self) -> ScrapeResult<NonEmptyIndexMap<String, String>> {
        let td = self
            .select(selector!("form.submit-form > table > tbody > tr > td"))
            .find(|td| {
                td.select(selector!("select[name=\"programTypeId\"]"))
                    .next()
                    .is_some()
            })
            .ok_or_else(ScrapeError::new)?;
        let names = td
            .select(selector!("option"))
            .map(|option| {
                let name = option.text().next()?.to_owned();
                let id = option.value().attr("value")?.to_owned();
                Some((name, id))
            })
            .map(|o| o.ok_or_else(ScrapeError::new))
            .collect::<ScrapeResult<IndexMap<_, _>>>()?;
        NonEmptyIndexMap::try_new(names).ok_or_else(ScrapeError::new)
    }
}

trait AssertOne {
    type Item;
    fn assert_one(self) -> ScrapeResult<Self::Item>;
}

impl<T, I: Iterator<Item = T>> AssertOne for I {
    type Item = T;

    fn assert_one(self) -> ScrapeResult<Self::Item> {
        let mut vec = self.collect::<Vec<_>>();
        if_chain! {
            if let Some(ret) = vec.pop();
            if vec.is_empty();
            then {
                Ok(ret)
            } else {
                Err(ScrapeError::new())
            }
        }
    }
}

trait FoldTextAndBr {
    fn fold_text_and_br(&self) -> String;
}

impl<'a> FoldTextAndBr for ElementRef<'a> {
    fn fold_text_and_br(&self) -> String {
        self.children().fold("".to_owned(), |mut ret, node| {
            match node.value() {
                Node::Text(t) => ret += t,
                Node::Element(e) if e.name() == "br" => ret.push('\n'),
                _ => {}
            }
            ret
        })
    }
}

fn base_url() -> Url {
    "https://codeforces.com".parse().unwrap()
}

#[cfg(test)]
mod tests {
    use crate::errors::ServiceResult;
    use crate::service;
    use crate::service::codeforces::Extract as _;

    use failure::Fallible;
    use itertools::Itertools as _;
    use pretty_assertions::assert_eq;
    use scraper::Html;
    use url::Url;

    use std::time::Duration;

    #[test]
    fn test_extract_login_form() -> ServiceResult<()> {
        let form = service::reqwest_sync_client(Duration::from_secs(60))?
            .get_html("/enter")?
            .extract_hidden_values(selector!("#enterForm"))?;
        assert_eq!(form["action"], "enter");
        assert_eq!(form["csrf_token"].len(), 32);
        assert_eq!(form["ftaa"], "0".repeat(18));
        assert_eq!(form["bfaa"], "n/a");
        Ok(())
    }

    #[test]
    fn test_retrieve_from_educational_codeforces_round_46() -> Fallible<()> {
        static URLS: &[(&str, &str)] = &[
            ("A", "https://codeforces.com/contest/1000/problem/A"),
            ("B", "https://codeforces.com/contest/1000/problem/B"),
            ("C", "https://codeforces.com/contest/1000/problem/C"),
            ("D", "https://codeforces.com/contest/1000/problem/D"),
            ("E", "https://codeforces.com/contest/1000/problem/E"),
            ("F", "https://codeforces.com/contest/1000/problem/F"),
            ("G", "https://codeforces.com/contest/1000/problem/G"),
        ];

        static SUITES: &[&str] = &[
            "c277d91927de5f4ffde5c68888dd83b6",
            "796003638cf846e9ae6d2b7ac8b799c8",
            "272bd6f863f4cddf8310fad7f8e6e9dc",
            "6e17c97f18577da0d35f31f71bff0181",
            "bcf2810cc0e0abc70344b81fe54fbbd4",
            "20328ccf95014b444764147ee2b64912",
            "a8345ca8a05620ac0c359529ff32738c",
        ];

        let client = service::reqwest_sync_client(Duration::from_secs(60))?;

        let problems = client.get_html("/contest/1000")?.extract_problems()?;
        for ((problem_a, url_a), (problem_e, url_e)) in problems.iter().zip_eq(URLS) {
            assert_eq!(problem_a, problem_e);
            assert_eq!(url_a.as_ref(), *url_e);
        }
        for ((_, url), expected) in URLS.iter().zip_eq(SUITES) {
            let actual = client.get_html(url)?.extract_test_suite()?.md5()?;
            assert_eq!(format!("{:x}", actual), *expected);
        }
        Ok(())
    }

    trait GetHtml {
        fn get_html(&self, url_or_path: &str) -> reqwest::Result<Html>;
    }

    impl GetHtml for reqwest::Client {
        fn get_html(&self, url_or_path: &str) -> reqwest::Result<Html> {
            let url = url_or_path.parse::<Url>().unwrap_or_else(|_| {
                let mut url = "https://codeforces.com".parse::<Url>().unwrap();
                url.set_path(url_or_path);
                url
            });
            let text = self.get(url).send()?.error_for_status()?.text()?;
            Ok(Html::parse_document(&text))
        }
    }
}
