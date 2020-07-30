use crate::{
    testsuite::{BatchTestSuite, InteractiveTestSuite, Match, PartialBatchTestCase, TestSuite},
    web::{
        yukicoder::api::SessionMutExt as _, CaseConverted, Exec, Platform, ResponseExt as _,
        RetrieveFullTestCases, RetrieveLanguages, RetrieveLanguagesOutcome, RetrieveTestCases,
        RetrieveTestCasesOutcome, RetrieveTestCasesOutcomeContest, RetrieveTestCasesOutcomeProblem,
        RetrieveTestCasesOutcomeProblemTextFiles, Session, SessionMut, Shell, Submit,
        SubmitOutcome, UpperCase,
    },
};
use anyhow::{bail, Context as _};
use easy_ext::ext;
use either::Either;
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
    pub fn exec<A>(args: A) -> anyhow::Result<<Self as Exec<A>>::Output>
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
    type WatchSubmissionsTarget = Infallible;
    type WatchSubmissionsCredentials = Infallible;
    type SubmitTarget = YukicoderSubmitTarget;
    type SubmitCredentials = YukicoderSubmitCredentials;
}

impl<S: Shell> Exec<RetrieveLanguages<Self, S>> for Yukicoder {
    type Output = RetrieveLanguagesOutcome;

    fn exec(args: RetrieveLanguages<Self, S>) -> anyhow::Result<RetrieveLanguagesOutcome> {
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

    fn exec(args: RetrieveTestCases<Self, S>) -> anyhow::Result<RetrieveTestCasesOutcome> {
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

    fn exec(args: Submit<Self, S>) -> anyhow::Result<SubmitOutcome> {
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
            Either::Left(problem_no) => sess.get_problem_by_problem_no(problem_no)?.problem_id,
            Either::Right((contest_id, problem_index)) => {
                let (_, problem_id) = sess
                    .get(url!("/contests/{}", contest_id))
                    .colorize_status_code(&[200], (), ..)
                    .send()?
                    .ensure_status(&[200])?
                    .html()?
                    .extract_problems()?
                    .into_iter()
                    .find(|(index, _)| index.eq_ignore_ascii_case(&problem_index))
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
                problem_screen_name: problem_id.to_string(),
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
}

impl YukicoderRetrieveTestCasesTargets {
    #[allow(clippy::type_complexity)]
    fn parse(&self) -> anyhow::Result<Either<BTreeSet<u64>, (u64, Option<BTreeSet<String>>)>> {
        match self {
            Self::ProblemNos(nos) => {
                let nos = nos
                    .iter()
                    .map(|no| parse_problem_no(no))
                    .collect::<Result<_, _>>()?;
                Ok(Either::Left(nos))
            }
            Self::Contest(contest_id, indexes) => {
                let contest_id = parse_contest_id(contest_id)?;
                Ok(Either::Right((contest_id, indexes.clone())))
            }
        }
    }
}

#[derive(Debug)]
pub struct YukicoderRetrieveFullTestCasesCredentials {
    pub api_key: String,
}

#[derive(Debug)]
pub enum YukicoderSubmitTarget {
    ProblemNo(String),
    Contest(String, String),
}

impl YukicoderSubmitTarget {
    fn parse(&self) -> anyhow::Result<Either<u64, (u64, String)>> {
        match self {
            Self::ProblemNo(no) => {
                let no = parse_problem_no(no)?;
                Ok(Either::Left(no))
            }
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

fn parse_problem_no(s: &str) -> anyhow::Result<u64> {
    s.parse().with_context(|| {
        format!(
            "A problem number for yukicoder must be unsigned integer: {:?}",
            s,
        )
    })
}

fn parse_contest_id(s: &str) -> anyhow::Result<u64> {
    s.parse().with_context(|| {
        format!(
            "A contest ID for yukicoder must be unsigned integer: {:?}",
            s,
        )
    })
}

fn retrieve_samples(
    mut sess: impl SessionMut,
    targets: YukicoderRetrieveTestCasesTargets,
) -> anyhow::Result<RetrieveTestCasesOutcome> {
    let mut outcome = RetrieveTestCasesOutcome {
        contest: None,
        problems: vec![],
    };

    match targets.parse()? {
        Either::Left(problem_nos) => {
            for &problem_no in &problem_nos {
                let (url, test_suite) = retrieve_samples(&mut sess, problem_no)?;
                let api::Problem {
                    problem_id, title, ..
                } = sess.get_problem_by_problem_no(problem_no)?;

                outcome.problems.push(RetrieveTestCasesOutcomeProblem {
                    index: problem_no.to_string(),
                    url,
                    screen_name: problem_id.to_string(),
                    display_name: title.clone(),
                    test_suite,
                    text_files: indexmap!(),
                });
            }
        }
        Either::Right((contest_id, problem_indexes)) => {
            outcome.contest = Some(RetrieveTestCasesOutcomeContest {
                id: contest_id.to_string(),
                submissions_url: url!("/contests/{}/submissions?my_submission=enabled", contest_id),
            });

            let mut not_found = problem_indexes.map(|problem_indexes| {
                problem_indexes
                    .iter()
                    .map(CaseConverted::<UpperCase>::new)
                    .collect::<BTreeSet<_>>()
            });

            for (index, problem_no) in sess
                .get(url!("/contests/{}", contest_id))
                .colorize_status_code(&[200], (), ..)
                .send()?
                .ensure_status(&[200])?
                .html()?
                .extract_problems()?
            {
                if let Some(not_found) = &mut not_found {
                    if !not_found.remove(&index) {
                        continue;
                    }
                }

                let (url, test_suite) = retrieve_samples(&mut sess, problem_no)?;
                let api::Problem {
                    problem_id, title, ..
                } = sess.get_problem_by_problem_no(problem_no)?;

                outcome.problems.push(RetrieveTestCasesOutcomeProblem {
                    index: problem_no.to_string(),
                    url,
                    screen_name: problem_id.to_string(),
                    display_name: title.clone(),
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
    }

    return Ok(outcome);

    fn retrieve_samples(
        mut sess: impl SessionMut,
        problem_no: u64,
    ) -> anyhow::Result<(Url, TestSuite)> {
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
    fn extract_problems(&self) -> anyhow::Result<Vec<(CaseConverted<UpperCase>, u64)>> {
        return (|| -> _ {
            self.select(static_selector!(
                "#content > div.left > table.table > tbody > tr",
            ))
            .map(|tr| {
                if let [td1, td2, ..] = *tr.select(static_selector!("td")).collect::<Vec<_>>() {
                    let index = CaseConverted::new(exactly_one_text(td1)?);
                    let no = exactly_one_text(td2)?.parse().ok()?;
                    Some((index, no))
                } else {
                    None
                }
            })
            .collect::<Option<_>>()
        })()
        .with_context(|| "Could not parse the contest page");

        fn exactly_one_text(element_ref: ElementRef<'_>) -> Option<&str> {
            element_ref.text().exactly_one().ok()
        }
    }

    fn extract_samples(&self) -> anyhow::Result<TestSuite> {
        let (timelimit, kind) = self
            .select(static_selector!("#content > div"))
            .flat_map(|r| r.text())
            .nth(1)
            .and_then(|text| {
                let timelimit = {
                    let caps = static_regex!(r"([0-9]{1,3})\.([0-9]{3})秒").captures(text)?;

                    let secs = caps[1].parse::<u64>().unwrap();
                    let millis = caps[2].parse::<u64>().unwrap();

                    Duration::from_millis(1000 * secs + millis)
                };

                let kind = {
                    let caps = static_regex!("(通常|スペシャルジャッジ|リアクティブ)問題")
                        .captures(text)?;

                    match &caps[1] {
                        "通常" => Kind::Regular,
                        "スペシャルジャッジ" => Kind::Special,
                        "リアクティブ" => Kind::Reactive,
                        _ => return None,
                    }
                };

                Some((timelimit, kind))
            })
            .with_context(|| "Could not parse the page")?;

        let test_suite = match kind {
            Kind::Regular | Kind::Special => {
                let mut test_suite = BatchTestSuite {
                    timelimit: Some(timelimit),
                    r#match: Match::Lines,
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
                            name: Some(format!("サンプル{}", i + 1)),
                            r#in: input.fold_text_and_br().into(),
                            out: if kind == Kind::Regular {
                                Some(output.fold_text_and_br().into())
                            } else {
                                None
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
    use anyhow::bail;
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
        ) -> anyhow::Result<Vec<String>> {
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
        fn get_problem_by_problem_id(&mut self, problem_id: u64) -> anyhow::Result<Problem> {
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
        fn get_problem_by_problem_no(&mut self, problem_no: u64) -> anyhow::Result<Problem> {
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
        fn get_all_problems(&mut self) -> anyhow::Result<Vec<Problem>> {
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
        fn get_available_language(&mut self) -> anyhow::Result<Vec<Language>> {
            let url = BASE_URL.join("languages").unwrap();

            self.get(url)
                .colorize_status_code(&[200], (), ..)
                .send()?
                .ensure_status(&[200])?
                .json()
                .map_err(Into::into)
        }

        /// > Submit problem by ProblemId
        fn submit_problem_by_problem_id(
            &mut self,
            token: &str,
            problem_id: u64,
            lang: &str,
            source: &str,
        ) -> anyhow::Result<std::result::Result<u64, (StatusCode, String)>> {
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
        //no: u64,
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
}
