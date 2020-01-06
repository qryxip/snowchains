use crate::errors::{
    ParseContestNameError, ParseContestNameResult, ScrapeError, ScrapeResult, ServiceErrorKind,
    ServiceResult,
};
use crate::service::context::{ParseWithBaseUrl as _, UsernameAndPassword};
use crate::service::{
    self, Contest, HasContextMut as _, LoginOutcome, ParticipateOutcome, ParticipateOutcomeKind,
    RetrieveLangsOutcome, RetrieveLangsProps, RetrieveSubmissionsOutcome,
    RetrieveSubmissionsOutcomeBuilder, RetrieveSubmissionsOutcomeBuilderSubmission,
    RetrieveSubmissionsProps, RetrieveTestCasesOutcome, RetrieveTestCasesOutcomeBuilder,
    RetrieveTestCasesOutcomeBuilderProblem, RetrieveTestCasesProps, SubmitOutcome,
    SubmitOutcomeLanguage, SubmitOutcomeResponse, SubmitProps,
};
use crate::terminal::{HasTermProps, Input};
use crate::testsuite::{self, BatchSuite, TestSuite};
use crate::util::collections::NonEmptyIndexMap;
use crate::util::scraper::ElementRefExt as _;
use crate::util::str::CaseConversion;

use chrono::{FixedOffset, TimeZone as _};
use if_chain::if_chain;
use indexmap::{indexmap, IndexMap, IndexSet};
use itertools::Itertools as _;
use maplit::hashmap;
use once_cell::sync::Lazy;
use rand::Rng;
use regex::Regex;
use scraper::{Html, Selector};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Deserializer, Serialize};
use sha2::{Digest as _, Sha512};
use termcolor::WriteColor;
use url::Url;

use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap};
use std::str::FromStr;
use std::time::{Duration, SystemTime};

pub(super) static BASE_URL: Lazy<Url> = Lazy::new(|| "https://codeforces.com".parse().unwrap());

pub(super) fn login(
    ctx: service::Context<impl Input, impl WriteColor + HasTermProps>,
) -> ServiceResult<LoginOutcome> {
    Codeforces::start(ctx)?.login(LoginOption::Explicit)
}

pub(super) fn participate(
    contest: &str,
    ctx: service::Context<impl Input, impl WriteColor + HasTermProps>,
) -> ServiceResult<ParticipateOutcome> {
    Codeforces::start(ctx)?.participate(contest.parse()?)
}

pub(super) fn retrieve_testcases(
    props: RetrieveTestCasesProps<String>,
    ctx: service::Context<impl Input, impl WriteColor + HasTermProps>,
) -> ServiceResult<RetrieveTestCasesOutcome> {
    let props = props
        .convert_problems(CaseConversion::Upper)
        .parse_contest()?;
    Codeforces::start(ctx)?.retrieve_testcases(props)
}

pub(super) fn retrieve_langs(
    props: RetrieveLangsProps<String>,
    ctx: service::Context<impl Input, impl WriteColor + HasTermProps>,
) -> ServiceResult<RetrieveLangsOutcome> {
    let props = props
        .convert_problem(CaseConversion::Upper)
        .parse_contest()?;
    Codeforces::start(ctx)?.retrieve_langs(props)
}

pub(super) fn retrieve_submissions(
    props: RetrieveSubmissionsProps<String>,
    ctx: service::Context<impl Input, impl WriteColor + HasTermProps>,
) -> ServiceResult<RetrieveSubmissionsOutcome> {
    let props = props
        .convert_problems(CaseConversion::Upper)
        .parse_contest()?;
    Codeforces::start(ctx)?.retrieve_submissions(props)
}

pub(super) fn submit(
    props: SubmitProps<String>,
    ctx: service::Context<impl Input, impl WriteColor + HasTermProps>,
) -> ServiceResult<SubmitOutcome> {
    let props = props
        .convert_problem(CaseConversion::Upper)
        .parse_contest()?;
    Codeforces::start(ctx)?.submit(props)
}

#[derive(Debug)]
struct Codeforces<I: Input, E: WriteColor + HasTermProps> {
    handle: Option<String>,
    api_key: Option<ApiKey>,
    ctx: service::Context<I, E>,
}

impl<I: Input, E: WriteColor + HasTermProps> service::HasContextMut for Codeforces<I, E> {
    type Stdin = I;
    type Stderr = E;

    fn context(&self) -> &service::Context<I, E> {
        &self.ctx
    }

    fn context_mut(&mut self) -> &mut service::Context<I, E> {
        &mut self.ctx
    }
}

impl<I: Input, E: WriteColor + HasTermProps> Codeforces<I, E> {
    fn start(mut ctx: service::Context<I, E>) -> ServiceResult<Self> {
        ctx.get_robots_txt()?;
        Ok(Self {
            handle: None,
            api_key: None,
            ctx,
        })
    }

    fn login(&mut self, option: LoginOption) -> ServiceResult<LoginOutcome> {
        static HANDLE: &Lazy<Regex> = lazy_regex!(r#"\A/profile/([a-zA-Z0-9_\-]+)\z"#);

        let res = self.get("/enter").redirect_unlimited().retry_send()?;

        if res.url().path() == "/enter" {
            let mut html = self.retry_recv_html(res)?;
            self.retry_login::<UsernameAndPassword, _>(
                ("Handle/Email: ", "Password: "),
                "Successfully logged in.",
                "Failed to login. Try again.",
                |this, (handle_or_email, password)| {
                    let mut payload = html.extract_hidden_values(selector!("#enterForm"))?;
                    payload.insert("handleOrEmail".to_owned(), handle_or_email);
                    payload.insert("password".to_owned(), password);
                    payload.insert("remember".to_owned(), "on".to_owned());
                    let res = this
                        .post("/enter")
                        .acceptable(&[200])
                        .redirect_unlimited()
                        .form(&payload)
                        .send()?;
                    if res.url().path() == "/enter" {
                        html = this.recv_html(res)?;
                        Ok(false)
                    } else {
                        Ok(true)
                    }
                },
            )?;
        } else {
            writeln!(self.stderr(), "Already logged in.")?;
            self.stderr().flush()?;
        }

        let res = self.get("/enter").acceptable(&[302]).retry_send()?;

        let location = res.location_uri()?;
        let handle = if_chain! {
            if let Some(loc) = &location;
            if let Some(caps) = HANDLE.captures(loc.path());
            then {
                caps[1].to_owned()
            } else {
                return Err(ServiceErrorKind::UnexpectedRedirection(location).into());
            }
        };
        if option == LoginOption::Explicit {
            self.api::<Vec<api::User>>("user.info", &[("handles", &handle)])?;
        }
        self.handle = Some(handle);
        Ok(LoginOutcome {})
    }

    fn participate(&mut self, contest: CodeforcesContest) -> ServiceResult<ParticipateOutcome> {
        self.login(LoginOption::WithHandle)?;

        let phase = self
            .api::<Vec<api::Contest>>("contest.list", &[("gym", &contest.is_gym().to_string())])?
            .into_iter()
            .find(|c| c.id == contest.id)
            .ok_or_else(|| ServiceErrorKind::ContestNotFound(contest.slug().into()))?
            .phase;

        if phase == api::ContestPhase::Finished {
            Ok(ParticipateOutcomeKind::ContestIsFinished.into())
        } else {
            let status = self
                .get(contest.registration_url())
                .acceptable(&[200, 302])
                .warn(&[302])
                .retry_status()?;
            if status == 200 {
                self.open_in_browser(contest.registration_url())?;
                Ok(ParticipateOutcomeKind::Success.into())
            } else {
                Ok(ParticipateOutcomeKind::AlreadyParticipated.into())
            }
        }
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
            save_files,
            ..
        } = props;

        let problems = problems.as_ref();
        let top_path = format!("/contest/{}", contest.id);

        let contest_display = self
            .api::<Vec<api::Contest>>("contest.list", &[("gym", &contest.is_gym().to_string())])?
            .into_iter()
            .find(|c| c.id == contest.id)
            .ok_or_else(|| ServiceErrorKind::ContestNotFound(contest.slug().into()))?
            .name;

        let mut res = self.get(&top_path).acceptable(&[200, 302]).retry_send()?;
        if res.status() == 302 {
            if res.location_uri()?.map_or(false, |l| l.path() == "/enter") {
                self.login(LoginOption::WithHandle)?;
                res = self.get(&top_path).acceptable(&[200, 302]).retry_send()?;
            }
            if res.status() == 302 {
                self.get(contest.registration_url()).retry_status()?;
                self.open_in_browser(contest.registration_url())?;
                self.wait_enter("Press ENTER after registration: ")?;
                res = self.get(&top_path).retry_send()?;
            }
        }

        let mut outcome =
            RetrieveTestCasesOutcomeBuilder::new(&contest.slug(), &contest_display, save_files);
        outcome.push_submissions_url(contest.submissions_url());

        for ((slug, display_name), url) in self.retry_recv_html(res)?.extract_problems()? {
            if problems.map_or(true, |ps| ps.contains(&slug)) {
                let suite = self
                    .get(url.as_ref())
                    .retry_recv_html()?
                    .extract_test_suite()?;
                let path = destinations.expand(&slug)?;
                outcome.push_problem(RetrieveTestCasesOutcomeBuilderProblem {
                    url,
                    slug,
                    display_name,
                    screen_name: None,
                    test_suite_contents: suite,
                    test_suite_location: path,
                    text_files: indexmap!(),
                });
            }
        }

        if let Some(problems) = problems {
            self.warn_not_found(&problems, &outcome.problem_slugs())?;
        }

        outcome.finish(open_in_browser, self.stderr())
    }

    fn retrieve_langs(
        &mut self,
        props: RetrieveLangsProps<CodeforcesContest>,
    ) -> ServiceResult<RetrieveLangsOutcome> {
        let RetrieveLangsProps { contest, .. } = props;
        self.login(LoginOption::WithHandle)?;
        let url = format!("/contest/{}/submit", contest.id).parse_with_base_url(Some(&BASE_URL))?;
        let langs = self.get(&url).retry_recv_html()?.extract_langs()?;
        Ok(RetrieveLangsOutcome::new(url, langs))
    }

    fn retrieve_submissions(
        &mut self,
        props: RetrieveSubmissionsProps<CodeforcesContest>,
    ) -> ServiceResult<RetrieveSubmissionsOutcome> {
        let RetrieveSubmissionsProps {
            contest,
            problems,
            src_paths,
            fetch_all,
            save_files,
        } = props;

        self.login(LoginOption::WithHandle)?;
        let handle = self.handle.clone().unwrap();

        let submissions = self.api::<Vec<api::Submission>>(
            "contest.status",
            &[("contestId", &contest.id.to_string()), ("handle", &handle)],
        )?;

        let csrf_token = self
            .get("/")
            .retry_recv_html()?
            .extract_meta_x_csrf_token()?;

        let lang_full_names = self
            .get(format!("/contest/{}/submit", contest.id))
            .retry_recv_html()?
            .extract_langs()?
            .into_iter()
            .map(|(k, v)| (v, k))
            .collect::<HashMap<_, _>>();

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

        for submission in submissions {
            if problems
                .as_ref()
                .map_or(false, |ps| !ps.contains(&submission.problem.index))
            {
                continue;
            }

            let language = LANG_IDS
                .get(submission.programming_language.as_str())
                .and_then(|id| lang_full_names.get(*id))
                .ok_or_else(|| {
                    ServiceErrorKind::UnknownLanguage(submission.programming_language.clone())
                })?
                .to_owned();

            let submission_id = submission.id.to_string();
            let mut retrieve_code = || -> _ {
                #[derive(Deserialize)]
                struct SubmitSource {
                    source: String,
                }

                self.post("/data/submitSource")
                    .acceptable(&[200])
                    .form(&hashmap!("submissionId" => &submission_id, "csrf_token" => &csrf_token))
                    .recv_json()
                    .map(|SubmitSource { source }| source)
            };

            let problem_slug = submission.problem.index;

            let (code, location) = match src_paths.get(language.as_str()) {
                None if fetch_all => (Some(retrieve_code()?), None),
                Some(location) if fetch_all => {
                    if found.contains(&problem_slug, &language) {
                        (Some(retrieve_code()?), Some(location))
                    } else {
                        found.insert(problem_slug.clone(), language.clone());
                        (Some(retrieve_code()?), None)
                    }
                }
                None => (None, None),
                Some(location) => {
                    if found.contains(&problem_slug, &language) {
                        (None, None)
                    } else {
                        found.insert(problem_slug.clone(), language.clone());
                        (Some(retrieve_code()?), Some(location))
                    }
                }
            };
            let location = if save_files {
                location
                    .map(|l| l.expand(Some(&problem_slug)))
                    .transpose()?
            } else {
                None
            };

            outcome.submissions.insert(
                format!("/contest/{}/submission/{}", contest.id, submission.id)
                    .parse_with_base_url(Some(&BASE_URL))?,
                RetrieveSubmissionsOutcomeBuilderSubmission {
                    problem_url: format!("/contest/{}/problem/{}", contest.id, problem_slug)
                        .parse_with_base_url(Some(&BASE_URL))?,
                    problem_slug,
                    problem_display_name: submission.problem.name,
                    problem_screen_name: None,
                    language,
                    date_time: FixedOffset::east(0).timestamp(submission.creation_time_seconds, 0),
                    verdict_is_ok: submission.verdict == Some(api::SubmissionVerdict::Ok),
                    verdict_string: submission
                        .verdict
                        .map(|v| v.to_string())
                        .unwrap_or_default(),
                    location,
                    code,
                },
            );
        }

        if let Some(problems) = problems {
            self.warn_not_found(&problems, &outcome.problem_slugs())?;
        }

        outcome.finish()
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

        let code = crate::fs::read_to_string(&src_path)?;

        self.login(LoginOption::WithHandle)?;
        let handle = self.handle.clone().unwrap();

        let standings = self.api::<api::Standings>(
            "contest.standings",
            &[("contestId", &contest.id.to_string()), ("handles", &handle)],
        )?;

        if standings.problems.iter().all(|p| p.index != problem) {
            return Err(ServiceErrorKind::NoSuchProblem(problem).into());
        }
        if !skip_checking_if_accepted && standings.contest.phase != api::ContestPhase::Finished {
            let already_accepted = self
                .api::<Vec<api::Submission>>(
                    "contest.status",
                    &[("contestId", &contest.id.to_string()), ("handle", &handle)],
                )?
                .into_iter()
                .any(|s| {
                    s.problem.index == problem && s.verdict == Some(api::SubmissionVerdict::Ok)
                });
            if already_accepted {
                return Err(ServiceErrorKind::AlreadyAccepted.into());
            }
        }

        let submit_path = format!("/contest/{}/submit", contest.id);

        let html = self.get(&submit_path).retry_recv_html()?;

        let lang_id = html
            .extract_langs()?
            .get(&lang_name)
            .ok_or_else(|| ServiceErrorKind::NoSuchLang(lang_name.clone()))?
            .clone();

        let mut values = html.extract_hidden_values(selector!("form.submit-form"))?;
        values.insert("contestId".to_owned(), contest.id.to_string());
        values.insert("submittedProblemIndex".to_owned(), problem);
        values.insert("tabSize".to_owned(), "4".to_owned());
        values.insert("programTypeId".to_owned(), lang_id.clone());
        values.insert("source".to_owned(), code.clone());

        let res = self
            .post(&submit_path)
            .acceptable(&[200, 302])
            .warn(&[200])
            .form(&values)
            .send()?;
        let rejected = res.status() == 200;
        let location = res.location_uri()?;

        if !rejected && open_in_browser {
            self.open_in_browser(contest.submissions_url())?;
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
        ) -> ServiceResult<ApiKey> {
            let key = this.prompt_password_stderr("API Key: ")?;
            let secret = this.prompt_password_stderr("API Secret: ")?;
            let api_key = ApiKey { key, secret };
            this.api_token().get_or_init()?.write_json(&api_key)?;
            *p = true;
            Ok(api_key)
        }

        let mut asked_api_key = false;
        let mut api_key = if let Some(api_key) = &self.api_key {
            api_key.clone()
        } else if self.api_token().exists() {
            self.api_token().get_or_init()?.json::<ApiKey>()?
        } else {
            ask_api_key(self, &mut asked_api_key)?
        };

        loop {
            let mut url = BASE_URL.clone();

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

            let res = self
                .get(url)
                .acceptable(&[200, 400])
                .warn(&[400])
                .retry_send()?;

            if res.status() == 200 {
                if asked_api_key {
                    self.api_token().get_or_init()?.write_json(&api_key)?;
                }
                self.api_key = Some(api_key);
                let ApiOk(ret) = self.retry_recv_json(res)?;
                break Ok(ret);
            }

            let ApiErr(comment) = self.retry_recv_json(res)?;
            if !comment.starts_with("apiKey: ") {
                break Err(failure::err_msg(comment)
                    .context(ServiceErrorKind::Api)
                    .into());
            }

            api_key = ask_api_key(self, &mut asked_api_key)?;
        }
    }
}

static LANG_IDS: Lazy<IndexMap<&'static str, &'static str>> = Lazy::new(|| {
    indexmap!(
        "GNU C11" => "43",
        "Clang++17 Diagnostics" => "52",
        "GNU C++11" => "42",
        "GNU C++14" => "50",
        "GNU C++17" => "54",
        "MS C++ 2010" => "2",
        "MS C++ 2017" => "59",
        "Mono C#" => "9",
        "D" => "28",
        "Go" => "32",
        "Haskell" => "12",
        "Java 8" => "36",
        "Kotlin" => "48",
        "Ocaml" => "19",
        "Delphi" => "3",
        "FPC" => "4",
        "PascalABC.NET" => "51",
        "Perl" => "13",
        "PHP" => "6",
        "Python 2" => "7",
        "Python 3" => "31",
        "PyPy 2" => "40",
        "PyPy 3" => "41",
        "Ruby" => "8",
        "Rust" => "49",
        "Scala" => "20",
        "JavaScript" => "34",
        "Node.js" => "55",
    )
});

#[derive(Clone, Copy, Debug)]
struct CodeforcesContest {
    id: u64,
}

impl CodeforcesContest {
    fn is_gym(self) -> bool {
        self.id >= 100_000
    }

    fn registration_url(self) -> Url {
        let mut ret = BASE_URL.clone();
        ret.set_path(&format!("/contestRegistration/{}", self.id));
        ret
    }

    fn submissions_url(self) -> Url {
        let mut ret = BASE_URL.clone();
        ret.set_path(&format!("/contest/{}/my", self.id));
        ret
    }
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
        self.id.to_string().into()
    }
}

#[derive(Debug, PartialEq)]
enum LoginOption {
    WithHandle,
    Explicit,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ApiKey {
    key: String,
    secret: String,
}

trait Extract {
    fn extract_hidden_values(&self, form: &Selector) -> ScrapeResult<HashMap<String, String>>;
    fn extract_problems(&self) -> ScrapeResult<NonEmptyIndexMap<(String, String), Url>>;
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

    fn extract_problems(&self) -> ScrapeResult<NonEmptyIndexMap<(String, String), Url>> {
        self.select(selector!("table.problems > tbody > tr"))
            .skip(1)
            .map(|tr| {
                let a1 = tr.select(selector!("td.id > a")).next()?;
                let slug = a1.text().next()?.trim().to_owned();
                let href1 = a1.value().attr("href")?;

                let a2 = tr.select(selector!("td > div > div > a")).next()?;
                let display = a2.text().next()?.trim().to_owned();
                let href2 = a2.value().attr("href")?;

                guard!(href1 == href2);
                let url = BASE_URL.join(href1).ok()?;

                Some(((slug, display), url))
            })
            .collect::<Option<IndexMap<_, _>>>()
            .and_then(NonEmptyIndexMap::try_new)
            .ok_or_else(ScrapeError::new)
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

/// <https://codeforces.com/api/help>
mod api {
    use serde::Deserialize;

    /// "Represents a Codeforces user."
    ///
    /// <https://codeforces.com/api/help/objects#User>
    #[derive(Debug, Deserialize)]
    pub(super) struct User {
        // __rest: (),
    }

    /// "Represents a contest on Codeforces."
    ///
    /// <https://codeforces.com/api/help/objects#Contest>
    #[derive(Debug, Deserialize)]
    pub(super) struct Contest {
        /// "Integer."
        pub(super) id: u64,
        /// "String. Localized."
        pub(super) name: String,
        /// "Enum: BEFORE, CODING, PENDING_SYSTEM_TEST, SYSTEM_TEST, FINISHED."
        pub(super) phase: ContestPhase,
        // __rest: (),
    }

    /// "Enum: BEFORE, CODING, PENDING_SYSTEM_TEST, SYSTEM_TEST, FINISHED."
    ///
    /// <https://codeforces.com/api/help/objects#Contest>
    #[derive(Debug, PartialEq, Deserialize)]
    #[serde(rename_all = "SCREAMING_SNAKE_CASE")]
    pub(super) enum ContestPhase {
        Before,
        Coding,
        PendingSystemTest,
        SystemTest,
        Finished,
    }

    /// "Represents a problem."
    ///
    /// <https://codeforces.com/api/help/objects#Problem>
    #[derive(Debug, Deserialize)]
    pub(super) struct Problem {
        /// "String. Usually a letter of a letter, followed by a digit, that represent a problem index in a contest."
        pub(super) index: String,
        /// "String. Localized."
        pub(super) name: String,
        // __rest: (),
    }

    /// "Represents a submission."
    ///
    /// <https://codeforces.com/api/help/objects#Submission>
    #[derive(Debug, Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub(super) struct Submission {
        /// "Integer."
        pub(super) id: u64,
        /// "Integer. Time, when submission was created, in unix-format."
        pub(super) creation_time_seconds: i64,
        /// "Problem object."
        pub(super) problem: Problem,
        /// "String."
        pub(super) programming_language: String,
        /// "Enum: FAILED, OK, PARTIAL, COMPILATION_ERROR, RUNTIME_ERROR, WRONG_ANSWER, PRESENTATION_ERROR, TIME_LIMIT_EXCEEDED, MEMORY_LIMIT_EXCEEDED, IDLENESS_LIMIT_EXCEEDED, SECURITY_VIOLATED, CRASHED, INPUT_PREPARATION_CRASHED, CHALLENGED, SKIPPED, TESTING, REJECTED. Can be absent."
        pub(super) verdict: Option<SubmissionVerdict>,
        // __rest: (),
    }

    /// "Enum: FAILED, OK, PARTIAL, COMPILATION_ERROR, RUNTIME_ERROR, WRONG_ANSWER, PRESENTATION_ERROR, TIME_LIMIT_EXCEEDED, MEMORY_LIMIT_EXCEEDED, IDLENESS_LIMIT_EXCEEDED, SECURITY_VIOLATED, CRASHED, INPUT_PREPARATION_CRASHED, CHALLENGED, SKIPPED, TESTING, REJECTED. Can be absent."
    ///
    /// <https://codeforces.com/api/help/objects#Submission>
    #[derive(Debug, PartialEq, strum::Display, Deserialize)]
    #[strum(serialize_all = "shouty_snake_case")]
    #[serde(rename_all = "SCREAMING_SNAKE_CASE")]
    pub(super) enum SubmissionVerdict {
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

    /// "Returns object with three fields: "contest", "problems" and "rows". Field "contest" contains a Contest object. Field "problems" contains a list of Problem objects. Field "rows" contains a list of RanklistRow objects."
    ///
    /// <https://codeforces.com/api/help/methods#contest.standings>
    #[derive(Debug, Deserialize)]
    pub(super) struct Standings {
        pub(super) contest: Contest,
        pub(super) problems: Vec<Problem>,
        // __rest: (),
    }
}

#[cfg(test)]
mod tests {
    use crate::service;
    use crate::service::codeforces::Extract as _;

    use failure::Fallible;
    use pretty_assertions::assert_eq;
    use retry::OperationResult;
    use scraper::Html;
    use url::Url;

    use std::iter;
    use std::time::Duration;

    #[test]
    fn test_extract_login_form() -> Fallible<()> {
        let form = service::reqwest_sync_client(Duration::from_secs(60))?
            .retry_retrieve_html(&"https://codeforces.com/enter".parse().unwrap())?
            .extract_hidden_values(selector!("#enterForm"))?;
        assert_eq!(form["action"], "enter");
        assert_eq!(form["csrf_token"].len(), 32);
        assert_eq!(form["ftaa"], "0".repeat(18));
        assert_eq!(form["bfaa"], "n/a");
        Ok(())
    }

    #[test]
    fn test_retrieve_from_educational_codeforces_round_46() -> Fallible<()> {
        static EXPECTED: &[(&str, &str, &str, &str)] = &[
            (
                "A",
                "Codehorses T-shirts",
                "https://codeforces.com/contest/1000/problem/A",
                "c277d91927de5f4ffde5c68888dd83b6",
            ),
            (
                "B",
                "Light It Up",
                "https://codeforces.com/contest/1000/problem/B",
                "796003638cf846e9ae6d2b7ac8b799c8",
            ),
            (
                "C",
                "Covered Points Count",
                "https://codeforces.com/contest/1000/problem/C",
                "272bd6f863f4cddf8310fad7f8e6e9dc",
            ),
            (
                "D",
                "Yet Another Problem On a Subsequence",
                "https://codeforces.com/contest/1000/problem/D",
                "6e17c97f18577da0d35f31f71bff0181",
            ),
            (
                "E",
                "We Need More Bosses",
                "https://codeforces.com/contest/1000/problem/E",
                "bcf2810cc0e0abc70344b81fe54fbbd4",
            ),
            (
                "F",
                "One Occurrence",
                "https://codeforces.com/contest/1000/problem/F",
                "20328ccf95014b444764147ee2b64912",
            ),
            (
                "G",
                "Two-Paths",
                "https://codeforces.com/contest/1000/problem/G",
                "a8345ca8a05620ac0c359529ff32738c",
            ),
        ];

        let client = service::reqwest_sync_client(Duration::from_secs(60))?;

        let actual = client
            .retry_retrieve_html(&"https://codeforces.com/contest/1000".parse().unwrap())?
            .extract_problems()?
            .into_iter()
            .map(|((slug, display), url)| -> Fallible<_> {
                let md5 = client
                    .retry_retrieve_html(&url)?
                    .extract_test_suite()?
                    .md5()?;
                let md5 = format!("{:?}", md5);
                Ok((slug, display, url, md5))
            })
            .collect::<Fallible<Vec<_>>>()?;
        let actual = actual
            .iter()
            .map(|(s, d, u, m)| (s.as_ref(), d.as_ref(), u.as_ref(), m.as_ref()))
            .collect::<Vec<_>>();

        assert_eq!(actual, EXPECTED);
        Ok(())
    }

    trait RetryRetrieveHtml {
        fn retry_retrieve_html(
            &self,
            url: &Url,
        ) -> std::result::Result<Html, retry::Error<reqwest::Error>>;
    }

    impl RetryRetrieveHtml for reqwest::Client {
        fn retry_retrieve_html(
            &self,
            url: &Url,
        ) -> std::result::Result<Html, retry::Error<reqwest::Error>> {
            const RETRIES: usize = 2;
            const INTERVAL: Duration = Duration::from_secs(1);

            retry::retry(iter::repeat(INTERVAL).take(RETRIES), || {
                let text = self
                    .get(url.as_str())
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
        }
    }
}
