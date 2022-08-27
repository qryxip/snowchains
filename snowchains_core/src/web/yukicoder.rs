use crate::{
    testsuite::{
        BatchTestSuite, InteractiveTestSuite, Match, PartialBatchTestCase, PositiveFinite,
        TestSuite,
    },
    web::{
        yukicoder::api::SessionMutExt as _, Exec, Platform, ResponseExt as _,
        RetrieveFullTestCases, RetrieveLanguages, RetrieveLanguagesOutcome, RetrieveTestCases,
        RetrieveTestCasesOutcome, RetrieveTestCasesOutcomeProblem,
        RetrieveTestCasesOutcomeProblemContest, RetrieveTestCasesOutcomeProblemTextFiles, Session,
        SessionMut, Shell, Submit, SubmitOutcome,
    },
};
use easy_ext::ext;
use either::Either;
use eyre::{bail, Context as _, ContextCompat as _};
use indexmap::indexmap;
use itertools::Itertools as _;
use once_cell::sync::Lazy;
use scraper::{ElementRef, Html, Node};
use std::{collections::BTreeSet, convert::Infallible, hash::Hash, time::Duration};
use url::Url;

static BASE_URL: Lazy<Url> = lazy_url!("https://yukicoder.me");

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Yukicoder {}

impl Yukicoder {
    pub fn exec<A>(args: A) -> eyre::Result<<Self as Exec<A>>::Output>
    where
        Self: Exec<A>,
    {
        <Self as Exec<_>>::exec(args)
    }
}

impl Platform for Yukicoder {
    type CookieStorage = ();
    type LoginCredentials = Infallible;
    type ParticipateTarget = Infallible;
    type ParticipateCredentials = Infallible;
    type RetrieveLanguagesTarget = ();
    type RetrieveLanguagesCredentials = ();
    type RetrieveTestCasesTargets = YukicoderRetrieveTestCasesTargets;
    type RetrieveTestCasesCredentials = ();
    type RetrieveFullTestCasesCredentials = YukicoderRetrieveFullTestCasesCredentials;
    type RetrieveSubmissionSummariesTarget = Infallible;
    type RetrieveSubmissionSummariesCredentials = Infallible;
    type WatchSubmissionsTarget = Infallible;
    type WatchSubmissionsCredentials = Infallible;
    type SubmitTarget = YukicoderSubmitTarget;
    type SubmitCredentials = YukicoderSubmitCredentials;
}

impl<S: Shell> Exec<RetrieveLanguages<Self, S>> for Yukicoder {
    type Output = RetrieveLanguagesOutcome;

    fn exec(args: RetrieveLanguages<Self, S>) -> eyre::Result<RetrieveLanguagesOutcome> {
        let RetrieveLanguages {
            target: (),
            credentials: (),
            cookie_storage: (),
            timeout,
            shell,
        } = args;

        let names_by_id = Session::new(timeout, None, shell)?
            .get_available_language()?
            .into_iter()
            .map(|api::Language { id, name, ver }| (id, format!("{} ({})", name, ver)))
            .collect();

        Ok(RetrieveLanguagesOutcome { names_by_id })
    }
}

impl<S: Shell> Exec<RetrieveTestCases<Self, S>> for Yukicoder {
    type Output = RetrieveTestCasesOutcome;

    fn exec(args: RetrieveTestCases<Self, S>) -> eyre::Result<RetrieveTestCasesOutcome> {
        let RetrieveTestCases {
            targets,
            credentials: (),
            full,
            cookie_storage: (),
            timeout,
            shell,
        } = args;

        let mut sess = Session::new(timeout, None, shell)?;

        let mut outcome = retrieve_samples(&mut sess, targets)?;

        if let Some(RetrieveFullTestCases {
            credentials: YukicoderRetrieveFullTestCasesCredentials { api_key },
        }) = full
        {
            for outcome_problem in &mut outcome.problems {
                let problem_id = outcome_problem
                    .screen_name
                    .as_ref()
                    .expect("should be preset")
                    .parse()
                    .expect("should be integer");

                let in_file_names =
                    sess.get_test_case_files_by_problem_id(&api_key, problem_id, api::Which::In)?;

                let in_contents = super::download_with_progress(
                    sess.shell.progress_draw_target(),
                    in_file_names
                        .iter()
                        .map(|file_name| {
                            let req = sess.get_test_case_file_by_problem_id(
                                &api_key,
                                problem_id,
                                api::Which::In,
                                file_name,
                            )?;
                            Ok((format!("in/{}", file_name), req))
                        })
                        .collect::<Result<_, url::ParseError>>()?,
                )?;

                let out_file_names =
                    sess.get_test_case_files_by_problem_id(&api_key, problem_id, api::Which::Out)?;

                let out_contents = super::download_with_progress(
                    sess.shell.progress_draw_target(),
                    out_file_names
                        .iter()
                        .map(|file_name| {
                            let req = sess.get_test_case_file_by_problem_id(
                                &api_key,
                                problem_id,
                                api::Which::Out,
                                file_name,
                            )?;
                            Ok((format!("out/{}", file_name), req))
                        })
                        .collect::<Result<_, url::ParseError>>()?,
                )?;

                for (name, r#in) in in_file_names.into_iter().zip_eq(in_contents) {
                    outcome_problem.text_files.insert(
                        name,
                        RetrieveTestCasesOutcomeProblemTextFiles { r#in, out: None },
                    );
                }

                for (name, out) in out_file_names.into_iter().zip_eq(out_contents) {
                    if let Some(text_files) = outcome_problem.text_files.get_mut(&name) {
                        text_files.out = Some(out);
                    }
                }
            }
        }

        Ok(outcome)
    }
}

impl<S: Shell> Exec<Submit<Self, S>> for Yukicoder {
    type Output = SubmitOutcome;

    fn exec(args: Submit<Self, S>) -> eyre::Result<SubmitOutcome> {
        let Submit {
            target,
            credentials: YukicoderSubmitCredentials { api_key },
            language_id,
            code,
            watch_submission,
            cookie_storage: (),
            timeout,
            mut shell,
        } = args;

        if watch_submission {
            shell.warn("`watch_submissions` in yukicoder is not yet supported")?;
        }

        let mut sess = Session::new(timeout, None, shell)?;

        let problem_id = match target.parse()? {
            Either::Left(url) => match parse_problem_url(&url)? {
                Either::Left(problem_no) => sess.get_problem_by_problem_no(problem_no)?.problem_id,
                Either::Right(problem_id) => problem_id,
            },
            Either::Right((contest_id, problem_index)) => {
                let problem_index = match *problem_index.to_ascii_uppercase().into_bytes() {
                    [problem_index @ b'A'..=b'Z'] => problem_index,
                    _ => bail!("problem indexes for yukicoder must be `[a-zA-Z]`"),
                };

                let api::Contest {
                    problem_id_list, ..
                } = sess.get_contest_by_contest_id(contest_id)?;

                if problem_id_list.len() > 26 {
                    unimplemented!("{} problems", problem_id_list.len());
                }

                let (_, problem_id) = problem_id_list
                    .into_iter()
                    .enumerate()
                    .find(|&(i, _)| i == usize::from(problem_index - b'A'))
                    .with_context(|| {
                        format!("No such problem in `{}`: `{}`", contest_id, problem_index)
                    })?;
                problem_id
            }
        };

        match sess.submit_problem_by_problem_id(
            &api_key,
            problem_id,
            language_id.as_ref(),
            code.as_ref(),
        )? {
            Ok(submission_id) => Ok(SubmitOutcome {
                problem_screen_name: Some(problem_id.to_string()),
                submission_url: url!("/submissions/{}", submission_id),
                submissions_url: url!("/problems/{}/submissions?my_submission=enabled", problem_id),
            }),
            Err((status_code, message)) => {
                bail!("Submission rejected: ({}, {:?})", status_code, message);
            }
        }
    }
}

#[derive(Debug)]
pub enum YukicoderRetrieveTestCasesTargets {
    ProblemNos(BTreeSet<String>),
    Contest(String, Option<BTreeSet<String>>),
    Urls(BTreeSet<Url>),
}

#[derive(Debug)]
pub struct YukicoderRetrieveFullTestCasesCredentials {
    pub api_key: String,
}

#[derive(Debug)]
pub enum YukicoderSubmitTarget {
    Url(Url),
    Contest(String, String),
}

impl YukicoderSubmitTarget {
    pub fn from_problem_no(problem_no: &str) -> Self {
        Self::Url(
            format!(
                "https://yukicoder.me/problems/no/{}",
                form_urlencoded::byte_serialize(problem_no.as_ref()).format(""),
            )
            .parse()
            .expect("should be valid"),
        )
    }

    fn parse(&self) -> eyre::Result<Either<Url, (u64, String)>> {
        match self {
            Self::Url(url) => Ok(Either::Left(url.clone())),
            Self::Contest(contest_id, problem_index) => {
                let contest_id = parse_contest_id(contest_id)?;
                Ok(Either::Right((contest_id, problem_index.clone())))
            }
        }
    }
}

#[derive(Debug)]
pub struct YukicoderSubmitCredentials {
    pub api_key: String,
}

fn parse_problem_no(s: &str) -> eyre::Result<u64> {
    s.parse().with_context(|| {
        format!(
            "A problem number for yukicoder must be unsigned integer: {:?}",
            s,
        )
    })
}

fn parse_contest_id(s: &str) -> eyre::Result<u64> {
    s.parse().with_context(|| {
        format!(
            "A contest ID for yukicoder must be unsigned integer: {:?}",
            s,
        )
    })
}

fn parse_problem_url(url: &Url) -> eyre::Result<Either<u64, u64>> {
    if url.domain() != Some("yukicoder.me") {
        bail!("wrong domain. expected `yukicoder.me`: {}", url);
    }

    if let Some(caps) = static_regex!(r"\A/problems/no/([0-9]{1,5})\z").captures(url.path()) {
        Ok(Either::Left(
            caps[1].parse().expect("this is from `([0-9]{1,5})`"),
        ))
    } else if let Some(caps) = static_regex!(r"\A/problems/([0-9]{1,5})\z").captures(url.path()) {
        Ok(Either::Right(
            caps[1].parse().expect("this is from `([0-9]{1,5})`"),
        ))
    } else {
        bail!("not a URL for a problem in yukicoder: {}", url);
    }
}

fn retrieve_samples(
    mut sess: impl SessionMut,
    targets: YukicoderRetrieveTestCasesTargets,
) -> eyre::Result<RetrieveTestCasesOutcome> {
    let mut outcome = RetrieveTestCasesOutcome { problems: vec![] };

    match targets {
        YukicoderRetrieveTestCasesTargets::ProblemNos(problem_nos) => {
            for problem_no in &problem_nos {
                let problem_no = parse_problem_no(problem_no)?;

                let (url, test_suite) = retrieve_samples(&mut sess, problem_no)?;
                let api::Problem {
                    problem_id, title, ..
                } = sess.get_problem_by_problem_no(problem_no)?;

                outcome.problems.push(RetrieveTestCasesOutcomeProblem {
                    contest: None,
                    index: problem_no.to_string(),
                    url,
                    screen_name: Some(problem_id.to_string()),
                    display_name: title.clone(),
                    test_suite,
                    text_files: indexmap!(),
                });
            }
        }
        YukicoderRetrieveTestCasesTargets::Contest(contest_id, problem_indexes) => {
            let contest_id = parse_contest_id(&contest_id)?;

            let problem_indexes = problem_indexes
                .map(|problem_indexes| {
                    problem_indexes
                        .into_iter()
                        .map(|problem_index| {
                            match *problem_index
                                .to_ascii_uppercase()
                                .chars()
                                .collect::<Vec<_>>()
                            {
                                [problem_index @ 'A'..='Z'] => Ok(problem_index),
                                _ => bail!("problem indexes for yukicoder must be `[a-zA-Z]`"),
                            }
                        })
                        .collect::<eyre::Result<BTreeSet<_>>>()
                })
                .transpose()?;

            let api::Contest {
                name,
                problem_id_list,
                ..
            } = sess.get_contest_by_contest_id(contest_id)?;

            if problem_id_list.len() > 26 {
                unimplemented!("{} problems", problem_id_list.len());
            }

            let contest = &RetrieveTestCasesOutcomeProblemContest {
                id: contest_id.to_string(),
                display_name: name,
                url: url!("/contests/{}", contest_id),
                submissions_url: url!("/contests/{}/submissions?my_submission=enabled", contest_id),
            };

            let mut not_found = problem_indexes;

            for (index, problem_id) in problem_id_list.into_iter().enumerate() {
                let index = char::from(index as u8 + b'A');

                if let Some(not_found) = &mut not_found {
                    if !not_found.remove(&index) {
                        continue;
                    }
                }

                let api::Problem { no, title, .. } = sess.get_problem_by_problem_id(problem_id)?;
                let (url, test_suite) = retrieve_samples(&mut sess, no)?;

                outcome.problems.push(RetrieveTestCasesOutcomeProblem {
                    contest: Some(contest.clone()),
                    index: index.to_string(),
                    url,
                    screen_name: Some(problem_id.to_string()),
                    display_name: title,
                    test_suite,
                    text_files: indexmap!(),
                });
            }

            if let Some(not_found) = not_found {
                if !not_found.is_empty() {
                    bail!("No such problem: {:?}", not_found);
                }
            }
        }
        YukicoderRetrieveTestCasesTargets::Urls(urls) => {
            for url in urls {
                let api::Problem {
                    no,
                    problem_id,
                    title,
                } = match parse_problem_url(&url)? {
                    Either::Left(problem_no) => sess.get_problem_by_problem_no(problem_no)?,
                    Either::Right(problem_id) => sess.get_problem_by_problem_id(problem_id)?,
                };

                let (_, test_suite) = retrieve_samples(&mut sess, no)?;

                outcome.problems.push(RetrieveTestCasesOutcomeProblem {
                    contest: None,
                    index: no.to_string(),
                    url,
                    screen_name: Some(problem_id.to_string()),
                    display_name: title.clone(),
                    test_suite,
                    text_files: indexmap!(),
                });
            }
        }
    }

    return Ok(outcome);

    fn retrieve_samples(
        mut sess: impl SessionMut,
        problem_no: u64,
    ) -> eyre::Result<(Url, TestSuite)> {
        let url = url!("/problems/no/{}", problem_no);

        let test_suite = sess
            .get(url.clone())
            .colorize_status_code(&[200], (), ..)
            .send()?
            .ensure_status(&[200])?
            .html()?
            .extract_samples()?;

        Ok((url, test_suite))
    }
}

#[ext]
impl Html {
    fn extract_samples(&self) -> eyre::Result<TestSuite> {
        let (timelimit, kind) = self
            .select(static_selector!("#content > div"))
            .flat_map(|r| r.text())
            .find_map(|text| {
                let timelimit = {
                    let caps = static_regex!(r"([0-9]{1,3})\.([0-9]{3})秒").captures(text)?;

                    let secs = caps[1].parse::<u64>().unwrap();
                    let millis = caps[2].parse::<u64>().unwrap();

                    Duration::from_millis(1000 * secs + millis)
                };

                let kind = if text.contains("標準ジャッジ問題") {
                    Kind::Regular
                } else if text.contains("スペシャルジャッジ問題") {
                    Kind::Special
                } else if text.contains("リアクティブ問題") {
                    Kind::Reactive
                } else if text.contains("小数誤差許容問題") {
                    let (relative_error, absolute_error) = if let Some(caps) =
                        static_regex!(r"絶対誤差または相対誤差が\$10\^\{-([0-9]{1,10})\}\$\s*以下")
                            .captures(text)
                    {
                        let error = format!("1e-{}", &caps[1]).parse().ok();
                        (error, error)
                    } else {
                        (None, None)
                    };
                    Kind::Floating {
                        relative_error,
                        absolute_error,
                    }
                } else {
                    return None;
                };

                Some((timelimit, kind))
            })
            .with_context(|| "Could not parse the page")?;

        let test_suite = match kind {
            Kind::Regular | Kind::Special | Kind::Floating { .. } => {
                let r#match = if let Kind::Floating {
                    relative_error,
                    absolute_error,
                } = kind
                {
                    Match::Float {
                        relative_error,
                        absolute_error,
                    }
                } else {
                    Match::Lines
                };

                let mut test_suite = BatchTestSuite {
                    timelimit: Some(timelimit),
                    r#match,
                    cases: vec![],
                    extend: vec![],
                };

                for (i, paragraph) in self
                    .select(static_selector!(
                        "#content > div.block > div.sample > div.paragraph",
                    ))
                    .enumerate()
                {
                    if let [input, output] = *paragraph
                        .select(static_selector!("pre"))
                        .collect::<Vec<_>>()
                    {
                        test_suite.cases.push(PartialBatchTestCase {
                            name: Some(format!("sample{}", i + 1)),
                            r#in: input.fold_text_and_br().into(),
                            out: match kind {
                                Kind::Regular | Kind::Floating { .. } => {
                                    Some(output.fold_text_and_br().into())
                                }
                                _ => None,
                            },
                            timelimit: None,
                            r#match: None,
                        });
                    } else {
                        bail!("Could not extract sample cases");
                    }
                }

                TestSuite::Batch(test_suite)
            }
            Kind::Reactive => TestSuite::Interactive(InteractiveTestSuite {
                timelimit: Some(timelimit),
            }),
        };

        return Ok(test_suite);

        #[derive(Debug, Clone, Copy, PartialEq)]
        enum Kind {
            Regular,
            Special,
            Reactive,
            Floating {
                relative_error: Option<PositiveFinite<f64>>,
                absolute_error: Option<PositiveFinite<f64>>,
            },
        }

        #[ext]
        impl ElementRef<'_> {
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
    }
}

mod api {
    //! <https://petstore.swagger.io/?url=https://yukicoder.me/api/swagger.yaml>

    use crate::web::{ResponseExt as _, SessionMut};
    use eyre::bail;
    use maplit::hashmap;
    use once_cell::sync::Lazy;
    use reqwest::StatusCode;
    use serde::Deserialize;
    use url::Url;

    static BASE_URL: Lazy<Url> = lazy_url!("https://yukicoder.me/api/v1/");

    pub(super) trait SessionMutExt: SessionMut {
        /// > Get TestCaseFiles by ProblemId
        ///
        /// > 問題IDに対応するテストケースリストを取得します。
        fn get_test_case_files_by_problem_id(
            &mut self,
            token: &str,
            problem_id: u64,
            which: Which,
        ) -> eyre::Result<Vec<String>> {
            let url = BASE_URL.join(&format!("problems/{}/file/{}", problem_id, which))?;

            let res = self
                .get(url)
                .bearer_auth(token)
                .colorize_status_code(&[200], (), ..)
                .send()?
                .ensure_status(&[200, 404])?;

            if res.status() == 200 {
                res.json().map_err(Into::into)
            } else {
                let res = res.json::<serde_json::Value>()?;
                bail!("{}", serde_json::to_string_pretty(&res).unwrap());
            }
        }

        /// > Get TestCaseFile by ProblemId
        fn get_test_case_file_by_problem_id(
            &self,
            token: &str,
            problem_id: u64,
            which: Which,
            file_name: &str,
        ) -> Result<reqwest::RequestBuilder, url::ParseError> {
            let url = BASE_URL.join(&format!(
                "problems/{}/file/{}/{}",
                problem_id, which, file_name
            ))?;

            Ok(self.async_client().get(url).bearer_auth(token))
        }

        /// > Get problem by ProblemId
        ///
        /// > ProblemIdを指定して問題を取得します。APIキーを使うとログイン情報に依存する問題も取得できます
        fn get_problem_by_problem_id(&mut self, problem_id: u64) -> eyre::Result<Problem> {
            let url = BASE_URL.join(&format!("problems/{}", problem_id))?;

            let res = self
                .get(url)
                .colorize_status_code(&[200], (), ..)
                .send()?
                .ensure_status(&[200, 404])?;

            if res.status() == 200 {
                res.json().map_err(Into::into)
            } else {
                let res = res.json::<serde_json::Value>()?;
                bail!("{}", serde_json::to_string_pretty(&res).unwrap());
            }
        }

        /// <https://twitter.com/yukicoder/status/1281965396778606593>
        fn get_problem_by_problem_no(&mut self, problem_no: u64) -> eyre::Result<Problem> {
            let url = BASE_URL.join(&format!("problems/no/{}", problem_no))?;

            let res = self
                .get(url)
                .colorize_status_code(&[200], (), ..)
                .send()?
                .ensure_status(&[200, 404])?;

            if res.status() == 200 {
                res.json().map_err(Into::into)
            } else {
                let res = res.json::<serde_json::Value>()?;
                bail!("{}", serde_json::to_string_pretty(&res).unwrap());
            }
        }

        /// > Get all problems
        ///
        /// > 公開されているテスト以外のすべての問題を取得します
        fn get_all_problems(&mut self) -> eyre::Result<Vec<Problem>> {
            let url = BASE_URL.join("problems").unwrap();

            self.get(url)
                .colorize_status_code(&[200], (), ..)
                .send()?
                .ensure_status(&[200])?
                .json()
                .map_err(Into::into)
        }

        /// > Get available language
        ///
        /// > 利用できる言語を取得します。何度も呼び出すような想定ではありません
        fn get_available_language(&mut self) -> eyre::Result<Vec<Language>> {
            let url = BASE_URL.join("languages").unwrap();

            self.get(url)
                .colorize_status_code(&[200], (), ..)
                .send()?
                .ensure_status(&[200])?
                .json()
                .map_err(Into::into)
        }

        /// > コンテストIDからコンテスト情報を取得します。
        fn get_contest_by_contest_id(&mut self, contest_id: u64) -> eyre::Result<Contest> {
            let url = BASE_URL
                .join(&format!("contest/id/{}", contest_id))
                .unwrap();

            let res = self
                .get(url)
                .colorize_status_code(&[200], (), ..)
                .send()?
                .ensure_status(&[200, 404])?;

            if res.status() == 200 {
                res.json().map_err(Into::into)
            } else {
                let res = res.json::<serde_json::Value>()?;
                bail!("{}", serde_json::to_string_pretty(&res).unwrap());
            }
        }

        /// > Submit problem by ProblemId
        fn submit_problem_by_problem_id(
            &mut self,
            token: &str,
            problem_id: u64,
            lang: &str,
            source: &str,
        ) -> eyre::Result<std::result::Result<u64, (StatusCode, String)>> {
            let url = BASE_URL.join(&format!("problems/{}/submit", problem_id))?;

            let res = self
                .post(url)
                .form(&hashmap!("lang" => lang, "source" => source))
                .bearer_auth(token)
                .colorize_status_code(&[200], (), ..)
                .send()?
                .ensure_status(&[200, 403, 404])?;

            match res.status().as_u16() {
                200 => {
                    #[derive(Deserialize)]
                    #[serde(rename_all = "PascalCase")]
                    struct Ok {
                        submission_id: u64,
                    }

                    let Ok { submission_id } = res.json()?;
                    Ok(Ok(submission_id))
                }
                403 | 404 => {
                    let status = res.status();
                    let msg = res.text()?;
                    let msg = serde_json::from_str::<serde_json::Value>(&msg)
                        .map(|v| v.to_string())
                        .unwrap_or(msg);
                    Ok(Err((status, msg)))
                }
                _ => unreachable!(),
            }
        }
    }

    impl<S: SessionMut> SessionMutExt for S {}

    pub(super) trait ReqwestAsyncClientExt {
        /// > Get TestCaseFile by ProblemId
        fn get_test_case_file_by_problem_id(
            &self,
            token: &str,
            problem_id: u64,
            which: Which,
            file_name: &str,
        ) -> Result<reqwest::RequestBuilder, url::ParseError>;
    }

    impl ReqwestAsyncClientExt for reqwest::Client {
        fn get_test_case_file_by_problem_id(
            &self,
            token: &str,
            problem_id: u64,
            which: Which,
            file_name: &str,
        ) -> Result<reqwest::RequestBuilder, url::ParseError> {
            let url = BASE_URL.join(&format!("problems/{}/{}/{}", problem_id, which, file_name))?;
            Ok(self.get(url).bearer_auth(token))
        }
    }

    #[derive(Debug, Deserialize)]
    #[serde(rename_all = "PascalCase")]
    pub(super) struct Problem {
        pub(super) no: u64,
        pub(super) problem_id: u64,
        pub(super) title: String,
        //author_id: u64,
        //tester_id: u64,
        //level: f64,
        //problem_type: i32,
        //tags: String,
        //date: chrono::DateTime<chrono::FixedOffset>,
    }

    #[derive(Debug, strum::Display)]
    #[strum(serialize_all = "lowercase")]
    pub(super) enum Which {
        In,
        Out,
    }

    #[derive(Debug, Deserialize)]
    #[serde(rename_all = "PascalCase")]
    pub(super) struct Language {
        pub(super) id: String,
        pub(super) name: String,
        pub(super) ver: String,
    }

    #[derive(Debug, Deserialize)]
    #[serde(rename_all = "PascalCase")]
    pub(super) struct Contest {
        //pub(super) id: u64,
        pub(super) name: String,
        //pub(super) date: String,
        //pub(super) end_date: String,
        pub(super) problem_id_list: Vec<u64>,
    }
}
