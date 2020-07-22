use crate::{
    testsuite::{BatchTestSuite, Match, PartialBatchTestCase, TestSuite},
    web::{
        codeforces::api::SessionMutExt as _, Cookies, Exec, Login, LoginOutcome, Participate,
        ParticipateOutcome, Platform, PlatformVariant, ResponseExt as _, RetrieveLanguages,
        RetrieveLanguagesOutcome, RetrieveTestCases, RetrieveTestCasesOutcome,
        RetrieveTestCasesOutcomeContest, RetrieveTestCasesOutcomeProblem, SessionBuilder,
        SessionMut, Shell, Submit, SubmitOutcome,
    },
};
use anyhow::{bail, Context as _};
use cookie_store::CookieStore;
use easy_ext::ext;
use indexmap::{indexmap, IndexMap};
use itertools::Itertools as _;
use once_cell::sync::Lazy;
use scraper::{ElementRef, Html, Node, Selector};
use std::{
    collections::{BTreeSet, HashMap},
    convert::Infallible,
    time::Duration,
};
use url::Url;

// Used by `url!` which is defined in `super`.
fn url_from_rel(rel_url: impl AsRef<str>) -> Result<Url, url::ParseError> {
    return BASE_URL.join(rel_url.as_ref());

    static BASE_URL: Lazy<Url> = lazy_url!("https://codeforces.com");
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Codeforces {}

impl Platform for Codeforces {
    const VARIANT: PlatformVariant = PlatformVariant::Codeforces;
}

impl Codeforces {
    pub fn exec<A>(args: A) -> anyhow::Result<<Self as Exec<A>>::Output>
    where
        Self: Exec<A>,
    {
        <Self as Exec<_>>::exec(args)
    }
}

impl<
        S: Shell,
        F1: FnMut(&CookieStore) -> anyhow::Result<()>,
        F2: FnMut() -> anyhow::Result<(String, String)>,
    > Exec<Login<Cookies<F1>, S, CodeforcesLoginCredentials<F2>>> for Codeforces
{
    type Output = LoginOutcome;

    fn exec(
        args: Login<Cookies<F1>, S, CodeforcesLoginCredentials<F2>>,
    ) -> anyhow::Result<LoginOutcome> {
        let Login {
            timeout,
            cookies:
                Cookies {
                    cookie_store,
                    on_update_cookie_store,
                },
            shell,
            credentials:
                CodeforcesLoginCredentials {
                    username_and_password,
                },
        } = args;

        let sess = SessionBuilder::new()
            .timeout(timeout)
            .cookie_store(Some(cookie_store))
            .on_update_cookie_store(on_update_cookie_store)
            .shell(shell)
            .build()?;

        let (outcome, _) = login(sess, username_and_password)?;
        Ok(outcome)
    }
}

impl<
        S: Shell,
        F1: FnMut(&CookieStore) -> anyhow::Result<()>,
        F2: FnMut() -> anyhow::Result<(String, String)>,
    >
    Exec<
        Participate<
            CodeforcesParticipateTarget,
            Cookies<F1>,
            S,
            CodeforcesParticipateCredentials<F2>,
        >,
    > for Codeforces
{
    type Output = ParticipateOutcome;

    fn exec(
        args: Participate<
            CodeforcesParticipateTarget,
            Cookies<F1>,
            S,
            CodeforcesParticipateCredentials<F2>,
        >,
    ) -> anyhow::Result<ParticipateOutcome> {
        let Participate {
            target: CodeforcesParticipateTarget { contest },
            timeout,
            cookies:
                Cookies {
                    cookie_store,
                    on_update_cookie_store,
                },
            shell,
            credentials:
                CodeforcesParticipateCredentials {
                    username_and_password,
                },
        } = args;

        let sess = SessionBuilder::new()
            .timeout(timeout)
            .cookie_store(Some(cookie_store))
            .on_update_cookie_store(on_update_cookie_store)
            .shell(shell)
            .build()?;

        let (outcome, _) = participate(sess, username_and_password, contest)?;
        Ok(outcome)
    }
}

impl<
        S: Shell,
        F1: FnMut(&CookieStore) -> anyhow::Result<()>,
        F2: FnMut() -> anyhow::Result<(String, String)>,
    >
    Exec<
        RetrieveLanguages<
            CodeforcesRetrieveLanguagesTarget,
            Cookies<F1>,
            S,
            CodeforcesRetrieveLanguagesCredentials<F2>,
        >,
    > for Codeforces
{
    type Output = RetrieveLanguagesOutcome;

    fn exec(
        args: RetrieveLanguages<
            CodeforcesRetrieveLanguagesTarget,
            Cookies<F1>,
            S,
            CodeforcesRetrieveLanguagesCredentials<F2>,
        >,
    ) -> anyhow::Result<RetrieveLanguagesOutcome> {
        let RetrieveLanguages {
            target: CodeforcesRetrieveLanguagesTarget { contest },
            timeout,
            cookies:
                Cookies {
                    cookie_store,
                    on_update_cookie_store,
                },
            shell,
            credentials:
                CodeforcesRetrieveLanguagesCredentials {
                    username_and_password,
                },
        } = args;

        let mut sess = SessionBuilder::new()
            .timeout(timeout)
            .cookie_store(Some(cookie_store))
            .on_update_cookie_store(on_update_cookie_store)
            .shell(shell)
            .build()?;

        participate(&mut sess, username_and_password, contest)?;

        let names_by_id = sess
            .get(url!("/contest/{}/submit", contest))
            .colorize_status_code(&[200], (), ..)
            .send()?
            .ensure_status(&[200])?
            .html()?
            .extract_available_langs()?;

        Ok(RetrieveLanguagesOutcome { names_by_id })
    }
}

impl<
        S: Shell,
        F1: FnMut(&CookieStore) -> anyhow::Result<()>,
        F2: FnMut() -> anyhow::Result<(String, String)>,
    >
    Exec<
        RetrieveTestCases<
            CodeforcesRetrieveTestCasesTargets,
            Cookies<F1>,
            S,
            CodeforcesRetrieveSampleTestCasesCredentials<F2>,
            Infallible,
        >,
    > for Codeforces
{
    type Output = RetrieveTestCasesOutcome;

    fn exec(
        args: RetrieveTestCases<
            CodeforcesRetrieveTestCasesTargets,
            Cookies<F1>,
            S,
            CodeforcesRetrieveSampleTestCasesCredentials<F2>,
            Infallible,
        >,
    ) -> anyhow::Result<RetrieveTestCasesOutcome> {
        let RetrieveTestCases {
            targets: CodeforcesRetrieveTestCasesTargets { contest, problems },
            timeout,
            cookies:
                Cookies {
                    cookie_store,
                    on_update_cookie_store,
                },
            shell,
            credentials:
                CodeforcesRetrieveSampleTestCasesCredentials {
                    username_and_password,
                },
            full: _,
        } = args;

        let mut sess = SessionBuilder::new()
            .timeout(timeout)
            .cookie_store(Some(cookie_store))
            .on_update_cookie_store(on_update_cookie_store)
            .shell(shell)
            .build()?;

        participate(&mut sess, username_and_password, contest)?;

        let mut problem_indices = problems.map(|ps| {
            ps.iter()
                .map(AsRef::as_ref)
                .map(str::to_uppercase)
                .collect::<BTreeSet<_>>()
        });

        let problems = sess
            .get(url!("/contest/{}", contest))
            .colorize_status_code(&[200], (), ..)
            .send()?
            .ensure_status(&[200])?
            .html()?
            .extract_problems()?
            .into_iter()
            .map(|(slug, display_name, url)| {
                if let Some(problem_indices) = &mut problem_indices {
                    if !problem_indices.remove(&slug) {
                        return Ok(None);
                    }
                }

                let test_suite = sess
                    .get(url.clone())
                    .colorize_status_code(&[200], (), ..)
                    .send()?
                    .html()?
                    .extract_test_cases()?;

                Ok(Some(RetrieveTestCasesOutcomeProblem {
                    slug,
                    url,
                    screen_name: "TODO: problems in Codeforces don't have global identifiers"
                        .to_owned(),
                    display_name,
                    test_suite,
                    text_files: indexmap!(),
                }))
            })
            .flat_map(Result::transpose)
            .collect::<anyhow::Result<_>>()?;

        if let Some(problem_indices) = problem_indices {
            if !problem_indices.is_empty() {
                bail!("No such problem indices: {:?}", problem_indices);
            }
        }

        Ok(RetrieveTestCasesOutcome {
            contest: Some(RetrieveTestCasesOutcomeContest {
                id: contest.to_string(),
                submissions_url: url!("/contest/{}/my", contest),
            }),
            problems,
        })
    }
}

impl<
        T1: AsRef<str>,
        T2: AsRef<str>,
        S: Shell,
        F1: FnMut(&CookieStore) -> anyhow::Result<()>,
        F2: FnMut() -> anyhow::Result<(String, String)>,
        F3: FnOnce() -> anyhow::Result<(String, String)>,
    >
    Exec<
        Submit<CodeforcesSubmitTarget, T1, T2, Cookies<F1>, S, CodeforcesSubmitCredentials<F2, F3>>,
    > for Codeforces
{
    type Output = SubmitOutcome;

    fn exec(
        args: Submit<
            CodeforcesSubmitTarget,
            T1,
            T2,
            Cookies<F1>,
            S,
            CodeforcesSubmitCredentials<F2, F3>,
        >,
    ) -> anyhow::Result<SubmitOutcome> {
        let Submit {
            target:
                CodeforcesSubmitTarget {
                    contest: contest_id,
                    problem: problem_index,
                },
            language_id,
            code,
            watch_submission,
            timeout,
            cookies:
                Cookies {
                    cookie_store,
                    on_update_cookie_store,
                },
            shell,
            credentials:
                CodeforcesSubmitCredentials {
                    username_and_password,
                    api_key_and_secret,
                },
        } = args;

        if watch_submission {
            todo!("`watch_submissions` in Codeforces is not yet supported");
        }

        let mut sess = SessionBuilder::new()
            .timeout(timeout)
            .cookie_store(Some(cookie_store))
            .on_update_cookie_store(on_update_cookie_store)
            .shell(shell)
            .build()?;

        let (_, handle) = participate(&mut sess, username_and_password, contest_id)?;

        let (_, problems, _) = sess.api_contest_standings(contest_id, None, None, "", "", false)?;

        let problem = problems
            .into_iter()
            .find(|api::Problem { index, .. }| index.eq_ignore_ascii_case(problem_index.as_ref()))
            .with_context(|| {
                format!("No such problem index: {:?}", problem_index.to_uppercase())
            })?;

        let url = url!("/contest/{}/submit", contest_id);

        let mut payload = sess
            .get(url.clone())
            .colorize_status_code(&[200], (), ..)
            .send()?
            .ensure_status(&[200])?
            .html()?
            .extract_hidden_values(static_selector!("form.submit-form"))?;

        payload.insert("contestId".to_owned(), contest_id.to_string());
        payload.insert("submittedProblemIndex".to_owned(), problem.index);
        payload.insert("tabSize".to_owned(), "4".to_owned());
        payload.insert("programTypeId".to_owned(), language_id.as_ref().to_owned());
        payload.insert("source".to_owned(), code.as_ref().to_owned());

        let res = sess
            .post(url)
            .form(&payload)
            .colorize_status_code(&[302], (), ..)
            .send()?
            .ensure_status(&[200, 302])?;

        if res.status() == 200 {
            bail!("Submission rejected");
        } else {
            let submissions_url = url_from_rel(res.location()?)?;

            let (api_key, api_secret) = api_key_and_secret()?;

            let submissions =
                sess.api_contest_status(&api_key, &api_secret, contest_id, &handle, 1, Some(1))?;

            let submission = submissions
                .get(0)
                .with_context(|| "Recieved no submission")?;

            let submission_url = url!("/contest/{}/submission/{}", contest_id, submission.id);

            Ok(SubmitOutcome {
                problem_screen_name: problem.name,
                submission_url,
                submissions_url,
            })
        }
    }
}

#[derive(Debug)]
pub struct CodeforcesLoginCredentials<F: FnMut() -> anyhow::Result<(String, String)>> {
    pub username_and_password: F,
}

#[derive(Debug)]
pub struct CodeforcesParticipateTarget {
    pub contest: u64,
}

#[derive(Debug)]
pub struct CodeforcesParticipateCredentials<F: FnMut() -> anyhow::Result<(String, String)>> {
    pub username_and_password: F,
}

#[derive(Debug)]
pub struct CodeforcesRetrieveLanguagesTarget {
    pub contest: u64,
}

#[derive(Debug)]
pub struct CodeforcesRetrieveLanguagesCredentials<F: FnMut() -> anyhow::Result<(String, String)>> {
    pub username_and_password: F,
}

#[derive(Debug)]
pub struct CodeforcesRetrieveTestCasesTargets {
    pub contest: u64,
    pub problems: Option<BTreeSet<String>>,
}

#[derive(Debug)]
pub struct CodeforcesRetrieveSampleTestCasesCredentials<
    F: FnMut() -> anyhow::Result<(String, String)>,
> {
    pub username_and_password: F,
}

#[derive(Debug)]
pub struct CodeforcesSubmitTarget {
    pub contest: u64,
    pub problem: String,
}

#[derive(Debug)]
pub struct CodeforcesSubmitCredentials<
    F1: FnMut() -> anyhow::Result<(String, String)>,
    F2: FnOnce() -> anyhow::Result<(String, String)>,
> {
    pub username_and_password: F1,
    pub api_key_and_secret: F2,
}

fn login(
    mut sess: impl SessionMut,
    mut username_and_password: impl FnMut() -> anyhow::Result<(String, String)>,
) -> anyhow::Result<(LoginOutcome, String)> {
    let url = url!("/enter");

    let mut res = sess
        .get(url.clone())
        .colorize_status_code(&[200, 302], (), ..)
        .send()?
        .ensure_status(&[200, 302])?;

    if res.status() == 302 {
        let handle = handle(&url_from_rel(res.location()?)?).to_owned();
        return Ok((LoginOutcome::AlreadyLoggedIn, handle));
    }

    return loop {
        let (handle_or_email, password) = username_and_password()?;

        let mut payload = res
            .html()?
            .extract_hidden_values(static_selector!("#enterForm"))?;

        payload.insert("handleOrEmail".to_owned(), handle_or_email);
        payload.insert("password".to_owned(), password);
        payload.insert("remember".to_owned(), "on".to_owned());

        res = sess
            .post(url.clone())
            .form(&payload)
            .colorize_status_code(&[200, 302], (), ..)
            .send()?
            .ensure_status(&[200, 302])?;

        if res.status() == 302 {
            let handle = handle(&url_from_rel(res.location()?)?).to_owned();
            break Ok((LoginOutcome::Success, handle));
        }

        sess.shell().warn("Failed to login. Try again")?;
    };

    fn handle(url: &Url) -> &str {
        url.path_segments().and_then(Iterator::last).unwrap_or("")
    }
}

fn participate(
    mut sess: impl SessionMut,
    username_and_password: impl FnMut() -> anyhow::Result<(String, String)>,
    contest_id: u64,
) -> anyhow::Result<(ParticipateOutcome, String)> {
    let (_, handle) = login(&mut sess, username_and_password)?;

    let api::Contest { phase, .. } = sess
        .api_contest_list(is_gym(contest_id))?
        .into_iter()
        .find(|&api::Contest { id, .. }| id == contest_id)
        .with_context(|| format!("No such contest: `{}`", contest_id))?;

    if phase == api::ContestPhase::Finished {
        Ok((ParticipateOutcome::ContestIsFinished, handle))
    } else {
        let url = url!("/contestRegistration/{}", contest_id);

        let status = sess
            .get(url.clone())
            .colorize_status_code(&[200, 302], (), ..)
            .send()?
            .ensure_status(&[200, 302])?
            .status();

        let outcome = if status == 200 {
            todo!("Contest registration for Codeforces is not yet implemented. Please open {} in browser", url);
        } else {
            ParticipateOutcome::AlreadyParticipated
        };

        Ok((outcome, handle))
    }
}

fn is_gym(contest_id: u64) -> bool {
    contest_id >= 100_000
}

#[ext]
impl Html {
    fn extract_hidden_values(&self, form: &Selector) -> anyhow::Result<HashMap<String, String>> {
        let mut values = self
            .select(form)
            .flat_map(|r| r.select(static_selector!("input[type=\"hidden\"]")))
            .flat_map(|input| {
                let input = input.value();
                let name = input.attr("name")?.to_owned();
                let value = input.attr("value")?.to_owned();
                Some((name, value))
            })
            .collect::<HashMap<String, String>>();

        if values.is_empty() {
            bail!("Could not extract the `name` and `value`");
        }

        if let Some(ftaa) = values.get_mut("ftaa") {
            *ftaa = "0".repeat(18);
        }
        if let Some(bfaa) = values.get_mut("bfaa") {
            *bfaa = "n/a".to_owned();
        }
        Ok(values)
    }

    fn extract_available_langs(&self) -> anyhow::Result<IndexMap<String, String>> {
        self.select(static_selector!(
            "form.submit-form > table > tbody > tr > td"
        ))
        .find(|td| {
            td.select(static_selector!("select[name=\"programTypeId\"]"))
                .next()
                .is_some()
        })
        .with_context(|| "Could not find `select[name=\"programTypeId\"]`")?
        .select(static_selector!("option"))
        .map(|option| {
            let id = option.value().attr("value")?.to_owned();
            let name = option.text().next()?.to_owned();
            Some((id, name))
        })
        .collect::<Option<IndexMap<_, _>>>()
        .filter(|ls| !ls.is_empty())
        .with_context(|| "Could not extract the available languages")
    }

    fn extract_problems(&self) -> anyhow::Result<Vec<(String, String, Url)>> {
        self.select(static_selector!("table.problems > tbody > tr"))
            .skip(1)
            .map(|tr| {
                let a1 = tr.select(static_selector!("td.id > a")).next()?;
                let slug = a1.text().next()?.trim().to_owned();
                let href1 = a1.value().attr("href")?;

                let a2 = tr.select(static_selector!("td > div > div > a")).next()?;
                let display = a2.text().next()?.trim().to_owned();
                let href2 = a2.value().attr("href")?;

                if href1 != href2 {
                    return None;
                }

                let url = "https://codeforces.com"
                    .parse::<Url>()
                    .unwrap()
                    .join(href1)
                    .ok()?;

                Some((slug, display, url))
            })
            .collect::<Option<Vec<_>>>()
            .filter(|ss| !ss.is_empty())
            .with_context(|| "Could not extract problem names")
    }

    fn extract_test_cases(&self) -> anyhow::Result<TestSuite> {
        let timelimit = self
            .select(static_selector!("#pageContent div.time-limit"))
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
            .exactly_one()
            .ok()
            .with_context(|| "Could not extract the timelimit")?;

        let input_file_text = self
            .select(static_selector!("#pageContent div.input-file"))
            .flat_map(|r| r.children())
            .flat_map(|r| match r.value() {
                Node::Text(t) => Some(&**t),
                _ => None,
            })
            .collect::<Vec<_>>();

        let output_file_text = self
            .select(static_selector!("#pageContent div.output-file"))
            .flat_map(|r| r.children())
            .flat_map(|r| match r.value() {
                Node::Text(t) => Some(&**t),
                _ => None,
            })
            .collect::<Vec<_>>();

        if !(input_file_text.contains(&"standard input")
            && output_file_text.contains(&"standard output"))
        {
            todo!();
        }
        let r#match = Match::Lines;

        let sample_test = self
            .select(static_selector!("#pageContent div.sample-test"))
            .exactly_one()
            .ok()
            .with_context(|| "Could not find `.sample-test`")?;

        let ins = sample_test
            .select(static_selector!("div.input > pre"))
            .map(|p| p.fold_text_and_br())
            .collect::<Vec<_>>();

        let outs = sample_test
            .select(static_selector!("div.output > pre"))
            .map(|p| p.fold_text_and_br())
            .collect::<Vec<_>>();

        if ins.is_empty() || ins.len() != outs.len() {
            bail!("in: {}, out: {}", ins.len(), outs.len());
        }

        let cases = ins
            .into_iter()
            .zip_eq(outs)
            .enumerate()
            .map(|(i, (r#in, out))| PartialBatchTestCase {
                name: Some(format!("Example {}", i + 1)),
                r#in: r#in.into(),
                out: Some(out.into()),
                r#match: None,
                timelimit: None,
            })
            .collect();

        return Ok(TestSuite::Batch(BatchTestSuite {
            timelimit: Some(timelimit),
            r#match,
            cases,
            extend: vec![],
        }));

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

    fn extract_meta_x_csrf_token(&self) -> anyhow::Result<String> {
        self.select(static_selector!("meta[name=\"X-Csrf-Token\"]"))
            .next()
            .and_then(|r| r.value().attr("content").map(ToOwned::to_owned))
            .with_context(|| "Could not extract the `X-Csrf-Token`")
    }
}

/// <https://codeforces.com/apiHelp>
mod api {
    use crate::web::SessionMut;
    use anyhow::anyhow;
    use rand::Rng as _;
    use serde::{
        de::{DeserializeOwned, Deserializer, Error as _},
        Deserialize,
    };
    use sha2::{Digest as _, Sha512};
    use std::time::SystemTime;
    use url::Url;

    /// "Represents a Codeforces user."
    ///
    /// <https://codeforces.com/apiHelp/objects#User>
    #[derive(Debug, Deserialize)]
    pub(super) struct User {
        // __rest: (),
    }

    /// > Represents a contest on Codeforces.
    ///
    /// <https://codeforces.com/apiHelp/objects#Contest>
    #[derive(Debug, Deserialize)]
    pub(super) struct Contest {
        /// > Integer.
        pub(super) id: u64,
        /// > String. Localized.
        pub(super) name: String,
        /// > Enum: BEFORE, CODING, PENDING_SYSTEM_TEST, SYSTEM_TEST, FINISHED.
        pub(super) phase: ContestPhase,
        // __rest: (),
    }

    /// > Enum: BEFORE, CODING, PENDING_SYSTEM_TEST, SYSTEM_TEST, FINISHED.
    ///
    /// <https://codeforces.com/apiHelp/objects#Contest>
    #[derive(Debug, PartialEq, Deserialize)]
    #[serde(rename_all = "SCREAMING_SNAKE_CASE")]
    pub(super) enum ContestPhase {
        Before,
        Coding,
        PendingSystemTest,
        SystemTest,
        Finished,
    }

    /// > Represents a problem.
    ///
    /// <https://codeforces.com/apiHelp/objects#Problem>
    #[derive(Debug, Deserialize)]
    pub(super) struct Problem {
        /// > String. Usually a letter of a letter, followed by a digit, that represent a problem index in a contest.
        pub(super) index: String,
        /// > String. Localized.
        pub(super) name: String,
        // __rest: (),
    }

    /// "Represents a submission."
    ///
    /// <https://codeforces.com/apiHelp/objects#Submission>
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
    /// <https://codeforces.com/apiHelp/objects#Submission>
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

    #[derive(Debug, Deserialize)]
    pub(super) struct RanklistRow {
        //__rest: ()
    }

    pub(super) trait SessionMutExt: SessionMut {
        fn api_contest_list(&mut self, gym: bool) -> anyhow::Result<Vec<Contest>> {
            let mut url = "https://codeforces.com/api/contest.list"
                .parse::<Url>()
                .unwrap();
            url.query_pairs_mut().append_pair("gym", &gym.to_string());

            api(self, url)
        }

        fn api_contest_standings(
            &mut self,
            contest_id: u64,
            from: Option<usize>,
            count: Option<usize>,
            handles: &str,
            room: &str,
            show_unofficial: bool,
        ) -> anyhow::Result<(Contest, Vec<Problem>, Vec<RanklistRow>)> {
            let mut url = "https://codeforces.com/api/contest.standings"
                .parse::<Url>()
                .unwrap();

            url.query_pairs_mut()
                .append_pair("contestId", &contest_id.to_string())
                .append_pair("from", &from.map(|n| n.to_string()).unwrap_or_default())
                .append_pair("count", &count.map(|n| n.to_string()).unwrap_or_default())
                .append_pair("handles", handles)
                .append_pair("room", room)
                .append_pair("showUnofficial", &show_unofficial.to_string());

            let ContestStandings {
                contest,
                problems,
                rows,
            } = api(self, url)?;

            return Ok((contest, problems, rows));

            #[derive(Debug, Deserialize)]
            struct ContestStandings {
                contest: Contest,
                problems: Vec<Problem>,
                rows: Vec<RanklistRow>,
            }
        }

        fn api_contest_status(
            &mut self,
            api_key: &str,
            api_secret: &str,
            contest_id: u64,
            handle: &str,
            from: usize,
            count: Option<usize>,
        ) -> anyhow::Result<Vec<Submission>> {
            let time = SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)?
                .as_secs()
                .to_string();

            let mut url = "https://codeforces.com/api/contest.status"
                .parse::<Url>()
                .unwrap();

            url.query_pairs_mut()
                .append_pair("apiKey", api_key)
                .append_pair("contestId", &contest_id.to_string())
                .append_pair("count", &count.map(|n| n.to_string()).unwrap_or_default())
                .append_pair("from", &from.to_string())
                .append_pair("handle", handle)
                .append_pair("time", &time);

            let api_sig = {
                let rand = rand::thread_rng().gen_range(100_000u32, 1_000_000u32);
                let repr = format!("{}{}#{}", rand, &url.as_str()[26..], api_secret);
                let digest = Sha512::digest(repr.as_ref());
                format!("{}{}", rand, hex::encode(digest))
            };

            url.query_pairs_mut().append_pair("apiSig", &api_sig);

            api(self, url)
        }
    }

    impl<S: SessionMut> SessionMutExt for S {}

    fn api<S: SessionMut, T: DeserializeOwned>(mut sess: S, url: Url) -> anyhow::Result<T> {
        let res = sess.get(url).colorize_status_code(&[200], (), ..).send()?;

        return if res.status() == 200 {
            let ApiOk(ok) = res.json()?;
            Ok(ok)
        } else {
            let ApiErr(msg) = res.json()?;
            Err(anyhow!("API error: {:?}", msg))
        };

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
                    Ok(Self(repr.result))
                } else {
                    Err(D::Error::custom("`.status` must be \"OK\""))
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
                    Ok(Self(repr.comment))
                } else {
                    Err(D::Error::custom("`.status` must be \"FAILED\""))
                }
            }
        }
    }
}
