use crate::{
    testsuite::{BatchTestSuite, InteractiveTestSuite, Match, PartialBatchTestCase, TestSuite},
    web::{
        yukicoder::api::SessionMutExt as _, CaseConverted, Exec, Platform, PlatformVariant,
        ResponseExt as _, RetrieveFullTestCases, RetrieveLanguages, RetrieveLanguagesOutcome,
        RetrieveSampleTestCases, RetrieveTestCasesOutcome, RetrieveTestCasesOutcomeProblem,
        RetrieveTestCasesOutcomeProblemTextFiles, SessionBuilder, SessionMut, Shell, Submit,
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
use std::{collections::BTreeSet, hash::Hash, time::Duration};
use url::Url;

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
    const VARIANT: PlatformVariant = PlatformVariant::Yukicoder;
}

impl<S: Shell> Exec<RetrieveLanguages<(), (), S, ()>> for Yukicoder {
    type Output = RetrieveLanguagesOutcome;

    fn exec(args: RetrieveLanguages<(), (), S, ()>) -> anyhow::Result<RetrieveLanguagesOutcome> {
        let RetrieveLanguages {
            target: (),
            timeout,
            cookie_store: (),
            shell,
            credentials: (),
        } = args;

        let names_by_id = SessionBuilder::new()
            .timeout(timeout)
            .shell(shell)
            .build()?
            .get_available_language()?
            .into_iter()
            .map(|api::Language { id, name, ver }| (id, format!("{} ({})", name, ver)))
            .collect();

        Ok(RetrieveLanguagesOutcome { names_by_id })
    }
}

#[allow(clippy::type_complexity)]
impl<'a, T1: AsRef<str>, T2: AsRef<str>, S: Shell>
    Exec<RetrieveSampleTestCases<Either<&'a [u64], (T1, Option<&'a [T2]>)>, (), S, ()>>
    for Yukicoder
{
    type Output = RetrieveTestCasesOutcome;

    fn exec(
        args: RetrieveSampleTestCases<Either<&'a [u64], (T1, Option<&'a [T2]>)>, (), S, ()>,
    ) -> anyhow::Result<RetrieveTestCasesOutcome> {
        let RetrieveSampleTestCases {
            targets,
            timeout,
            cookie_store: (),
            shell,
            credentials: (),
        } = args;

        let sess = SessionBuilder::new()
            .timeout(timeout)
            .shell(shell)
            .build()?;

        retrieve_samples(sess, targets)
    }
}

#[allow(clippy::type_complexity)]
impl<'a, T1: AsRef<str>, T2: AsRef<str>, S: Shell, F: FnOnce() -> anyhow::Result<String>>
    Exec<RetrieveFullTestCases<Either<&'a [u64], (T1, Option<&'a [T2]>)>, (), S, (F,)>>
    for Yukicoder
{
    type Output = RetrieveTestCasesOutcome;

    fn exec(
        args: RetrieveFullTestCases<Either<&'a [u64], (T1, Option<&'a [T2]>)>, (), S, (F,)>,
    ) -> anyhow::Result<RetrieveTestCasesOutcome> {
        let RetrieveFullTestCases {
            targets,
            timeout,
            cookie_store: (),
            shell,
            credentials: (api_token,),
        } = args;

        let api_token = api_token()?;

        let mut sess = SessionBuilder::new()
            .timeout(timeout)
            .shell(shell)
            .build()?;

        let mut outcome = retrieve_samples(&mut sess, targets)?;

        for outcome_problem in &mut outcome.problems {
            let problem_id = outcome_problem
                .screen_name
                .parse()
                .expect("should be integer");

            let in_file_names =
                sess.get_test_case_files_by_problem_id(&api_token, problem_id, api::Which::In)?;

            let in_contents = super::download_with_progress(
                sess.shell.progress_draw_target(),
                in_file_names
                    .iter()
                    .map(|file_name| {
                        let req = sess.get_test_case_file_by_problem_id(
                            &api_token,
                            problem_id,
                            api::Which::In,
                            file_name,
                        )?;
                        Ok((format!("in/{}", file_name), req))
                    })
                    .collect::<Result<_, url::ParseError>>()?,
            )?;

            let out_file_names =
                sess.get_test_case_files_by_problem_id(&api_token, problem_id, api::Which::Out)?;

            let out_contents = super::download_with_progress(
                sess.shell.progress_draw_target(),
                out_file_names
                    .iter()
                    .map(|file_name| {
                        let req = sess.get_test_case_file_by_problem_id(
                            &api_token,
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

        Ok(outcome)
    }
}

#[allow(clippy::type_complexity)]
impl<
        T1: AsRef<str>,
        T2: AsRef<str>,
        T3: AsRef<str>,
        T4: AsRef<str>,
        T5: AsRef<str>,
        S: Shell,
        F: FnOnce() -> anyhow::Result<String>,
    > Exec<Submit<Either<T1, (T2, T3)>, T4, T5, (), S, (F,)>> for Yukicoder
{
    type Output = SubmitOutcome;

    fn exec(
        args: Submit<Either<T1, (T2, T3)>, T4, T5, (), S, (F,)>,
    ) -> anyhow::Result<SubmitOutcome> {
        let Submit {
            target,
            language_id,
            code,
            watch_submission,
            timeout,
            cookie_store: (),
            shell,
            credentials: (api_key,),
        } = args;

        if watch_submission {
            todo!("`watch_submissions` in yukicoder is not yet supported");
        }

        let api_key = api_key()?;

        let mut sess = SessionBuilder::new()
            .timeout(timeout)
            .shell(shell)
            .build()?;

        let problem_id = match target {
            Either::Left(problem_no) => retrieve_problem_id(&mut sess, problem_no)?,
            Either::Right((contest_id, problem_slug)) => {
                let (contest_id, problem_slug) = (contest_id.as_ref(), problem_slug.as_ref());

                let (_, problem_id) = sess
                    .get(yukicoder_url(format!("/contests/{}", contest_id))?)
                    .colorize_status_code(&[200], (), ..)
                    .send()?
                    .ensure_status(&[200])?
                    .html()?
                    .extract_problems()?
                    .into_iter()
                    .find(|(slug, _)| slug.eq_ignore_ascii_case(problem_slug))
                    .with_context(|| {
                        format!("No such problem in `{}`: `{}`", contest_id, problem_slug)
                    })?;

                problem_id
            }
        };

        return match sess.submit_problem_by_problem_id(
            &api_key,
            problem_id,
            language_id.as_ref(),
            code.as_ref(),
        )? {
            Ok(submission_id) => Ok(SubmitOutcome {
                problem_screen_name: problem_id.to_string(),
                submission_url: format!("https://yukicoder.me/submissions/{}", submission_id)
                    .parse()?,
                submissions_url: yukicoder_url(format!(
                    "/problems/{}/submissions?my_submission=enabled",
                    problem_id,
                ))?,
            }),
            Err((status_code, message)) => {
                bail!("Submission rejected: ({}, {:?})", status_code, message);
            }
        };

        fn retrieve_problem_id(
            mut sess: impl SessionMut,
            problem_no: impl AsRef<str>,
        ) -> anyhow::Result<u64> {
            let url = yukicoder_url(format!("/problems/no/{}", problem_no.as_ref()))?;

            sess.get(url)
                .colorize_status_code(&[200], (), ..)
                .send()?
                .ensure_status(&[200])?
                .html()?
                .extract_problem_id()
        }
    }
}

fn retrieve_samples(
    mut sess: impl SessionMut,
    targets: Either<&[u64], (impl AsRef<str>, Option<&[impl AsRef<str>]>)>,
) -> anyhow::Result<RetrieveTestCasesOutcome> {
    let mut outcome = RetrieveTestCasesOutcome { problems: vec![] };

    match targets {
        Either::Left(problem_nos) => {
            for &problem_no in problem_nos {
                let (url, problem_id, test_suite) = retrieve_samples(&mut sess, problem_no)?;
                let api::Problem { title, .. } = sess.get_problem_by_problem_id(problem_id)?;

                outcome.problems.push(RetrieveTestCasesOutcomeProblem {
                    slug: problem_no.to_string(),
                    url,
                    screen_name: problem_id.to_string(),
                    display_name: title.clone(),
                    test_suite,
                    text_files: indexmap!(),
                });
            }
        }
        Either::Right((contest_id, problem_slugs)) => {
            let mut not_found = problem_slugs.map(|problem_slugs| {
                problem_slugs
                    .iter()
                    .map(CaseConverted::<UpperCase>::new)
                    .collect::<BTreeSet<_>>()
            });

            for (slug, problem_no) in sess
                .get(yukicoder_url(format!("/contests/{}", contest_id.as_ref()))?)
                .colorize_status_code(&[200], (), ..)
                .send()?
                .ensure_status(&[200])?
                .html()?
                .extract_problems()?
            {
                if let Some(not_found) = &mut not_found {
                    if !not_found.remove(&slug) {
                        continue;
                    }
                }

                let (url, problem_id, test_suite) = retrieve_samples(&mut sess, problem_no)?;
                let api::Problem { title, .. } = sess.get_problem_by_problem_id(problem_id)?;

                outcome.problems.push(RetrieveTestCasesOutcomeProblem {
                    slug: problem_no.to_string(),
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
    ) -> anyhow::Result<(Url, u64, TestSuite)> {
        let url = yukicoder_url(format!("/problems/no/{}", problem_no))?;

        let html = sess
            .get(url.clone())
            .colorize_status_code(&[200], (), ..)
            .send()?
            .ensure_status(&[200])?
            .html()?;

        let id = html.extract_problem_id()?;
        let test_suite = html.extract_samples()?;

        Ok((url, id, test_suite))
    }
}

fn yukicoder_url(rel_url: impl AsRef<str>) -> Result<Url, url::ParseError> {
    return BASE_URL.join(rel_url.as_ref());

    static BASE_URL: Lazy<Url> = lazy_url!("https://yukicoder.me");
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
                    let slug = CaseConverted::new(exactly_one_text(td1)?);
                    let no = exactly_one_text(td2)?.parse().ok()?;
                    Some((slug, no))
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

    fn extract_problem_id(&self) -> anyhow::Result<u64> {
        self.select(static_selector!("#content > div"))
            .nth(1)
            .into_iter()
            .flat_map(|r| r.text())
            .flat_map(|s| {
                static_regex!("ProblemId[ ]*:[ ]*(.*)")
                    .captures(s)
                    .and_then(|caps| caps[1].parse().ok())
            })
            .exactly_one()
            .ok()
            .with_context(|| "Could not extract the problem ID")
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
        //problem_id: u64,
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
