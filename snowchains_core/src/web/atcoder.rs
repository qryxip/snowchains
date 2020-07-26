use crate::{
    testsuite::{
        BatchTestSuite, InteractiveTestSuite, Match, PartialBatchTestCase, PositiveFinite,
        TestSuite,
    },
    web::{
        AnsiColored, CaseConverted, CookieStorage, Exec, Login, LoginOutcome, LowerCase,
        Participate, ParticipateOutcome, Platform, ResponseExt as _, RetrieveFullTestCases,
        RetrieveLanguages, RetrieveLanguagesOutcome, RetrieveTestCases, RetrieveTestCasesOutcome,
        RetrieveTestCasesOutcomeContest, RetrieveTestCasesOutcomeProblem,
        RetrieveTestCasesOutcomeProblemTextFiles, Session, SessionMut, Shell, Submit,
        SubmitOutcome, UpperCase, WatchSubmissions,
    },
};
use anyhow::{anyhow, bail, Context as _};
use chrono::{DateTime, FixedOffset, Local, Utc};
use easy_ext::ext;
use indexmap::{indexmap, IndexMap};
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use itertools::Itertools as _;
use maplit::hashmap;
use once_cell::sync::Lazy;
use regex::Regex;
use reqwest::header;
use scraper::{ElementRef, Html, Selector};
use serde::Deserialize;
use serde_json::json;
use std::{
    collections::{BTreeMap, BTreeSet},
    convert::Infallible,
    hash::Hash,
    io,
    marker::PhantomData,
    str::FromStr,
    time::Duration,
};
use termcolor::{Color, ColorSpec, WriteColor};
use tokio::runtime::Runtime;
use unicode_width::UnicodeWidthStr as _;
use url::Url;

static BASE_URL: Lazy<Url> = lazy_url!("https://atcoder.jp");

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Atcoder<'closures> {
    Infallible(Infallible, PhantomData<fn() -> &'closures ()>),
}

impl Atcoder<'_> {
    pub fn exec<A>(args: A) -> anyhow::Result<<Self as Exec<A>>::Output>
    where
        Self: Exec<A>,
    {
        <Self as Exec<_>>::exec(args)
    }
}

impl<'closures> Platform for Atcoder<'closures> {
    type CookieStorage = CookieStorage;
    type LoginCredentials = AtcoderLoginCredentials<'closures>;
    type ParticipateTarget = AtcoderParticipateTarget;
    type ParticipateCredentials = AtcoderParticipateCredentials<'closures>;
    type RetrieveLanguagesTarget = AtcoderRetrieveLanguagesTarget;
    type RetrieveLanguagesCredentials = AtcoderRetrieveLanguagesCredentials<'closures>;
    type RetrieveTestCasesTargets = AtcoderRetrieveTestCasesTargets;
    type RetrieveTestCasesCredentials = AtcoderRetrieveSampleTestCasesCredentials<'closures>;
    type RetrieveFullTestCasesCredentials = AtcoderRetrieveFullTestCasesCredentials;
    type WatchSubmissionsTarget = AtcoderWatchSubmissionsTarget;
    type WatchSubmissionsCredentials = AtcoderWatchSubmissionsCredentials<'closures>;
    type SubmitTarget = AtcoderSubmitTarget;
    type SubmitCredentials = AtcoderSubmitCredentials<'closures>;
}

impl<S: Shell> Exec<Login<Self, S>> for Atcoder<'_> {
    type Output = LoginOutcome;

    fn exec(args: Login<Self, S>) -> anyhow::Result<LoginOutcome> {
        let Login {
            credentials:
                AtcoderLoginCredentials {
                    username_and_password,
                },
            cookie_storage,
            timeout,
            shell,
        } = args;

        let mut sess = Session::new(timeout, Some(cookie_storage), shell)?;

        if check_logged_in(&mut sess)? {
            Ok(LoginOutcome::AlreadyLoggedIn)
        } else {
            login(sess, username_and_password)?;
            Ok(LoginOutcome::Success)
        }
    }
}

impl<S: Shell> Exec<Participate<Self, S>> for Atcoder<'_> {
    type Output = ParticipateOutcome;

    fn exec(args: Participate<Self, S>) -> anyhow::Result<ParticipateOutcome> {
        let Participate {
            target: AtcoderParticipateTarget { contest },
            credentials:
                AtcoderParticipateCredentials {
                    username_and_password,
                },
            cookie_storage,
            timeout,
            shell,
        } = args;

        let contest = CaseConverted::new(contest);
        let sess = Session::new(timeout, Some(cookie_storage), shell)?;
        participate(sess, username_and_password, &contest, true)
    }
}

impl<S: Shell> Exec<RetrieveLanguages<Self, S>> for Atcoder<'_> {
    type Output = RetrieveLanguagesOutcome;

    fn exec(args: RetrieveLanguages<Self, S>) -> anyhow::Result<RetrieveLanguagesOutcome> {
        let RetrieveLanguages {
            target,
            credentials:
                AtcoderRetrieveLanguagesCredentials {
                    username_and_password,
                },
            cookie_storage,
            timeout,
            shell,
        } = args;

        let AtcoderRetrieveLanguagesTarget {
            contest_and_problem,
        } = target;
        let (contest, problem) = if let Some((contest, problem)) = contest_and_problem {
            (
                CaseConverted::<LowerCase>::new(contest),
                Some(CaseConverted::<UpperCase>::new(problem)),
            )
        } else {
            (CaseConverted::<LowerCase>::new("practice"), None)
        };

        let mut sess = Session::new(timeout, Some(cookie_storage), shell)?;

        login(&mut sess, username_and_password)?;

        let url = if let Some(problem) = problem {
            retrieve_tasks_page(&mut sess, || unreachable!(), &contest)?
                .extract_task_indexes_and_urls()?
                .remove(&problem)
                .with_context(|| "")?
        } else {
            url!("/contests/{}/submit", contest)
        };

        let names_by_id = sess
            .get(url)
            .colorize_status_code(&[200], (), ..)
            .send()?
            .ensure_status(&[200])?
            .html()?
            .extract_langs()?;

        Ok(RetrieveLanguagesOutcome { names_by_id })
    }
}

impl<S: Shell> Exec<RetrieveTestCases<Self, S>> for Atcoder<'_> {
    type Output = RetrieveTestCasesOutcome;

    fn exec(args: RetrieveTestCases<Self, S>) -> anyhow::Result<RetrieveTestCasesOutcome> {
        let RetrieveTestCases {
            targets,
            credentials:
                AtcoderRetrieveSampleTestCasesCredentials {
                    username_and_password,
                },
            full,
            cookie_storage,
            timeout,
            shell,
        } = args;

        let AtcoderRetrieveTestCasesTargets { contest, problems } = targets;

        let mut sess = Session::new(timeout, Some(cookie_storage), shell)?;

        let mut outcome = retrieve_sample_test_cases(
            &mut sess,
            username_and_password,
            &contest,
            problems.as_ref(),
        )?;

        if let Some(RetrieveFullTestCases {
            credentials:
                AtcoderRetrieveFullTestCasesCredentials {
                    dropbox_access_token,
                },
        }) = full
        {
            for problem in &mut outcome.problems {
                let mut retrieve = |dir_file_name: &'static str| -> anyhow::Result<_> {
                    let path = format!("/{}/{}/{}", contest, problem.index, dir_file_name);
                    let ListFolder { entries } =
                        list_folder(&mut sess, &dropbox_access_token, &path)?;
                    retrieve_files(&mut sess, &dropbox_access_token, &path, &entries)
                };

                let (in_contents, mut out_contents) = (retrieve("in")?, retrieve("out")?);

                problem.text_files = in_contents
                    .into_iter()
                    .map(|(name, r#in)| {
                        let out = out_contents.shift_remove(&name);
                        (name, RetrieveTestCasesOutcomeProblemTextFiles { r#in, out })
                    })
                    .collect();
            }
        }

        return Ok(outcome);

        static URL: &str =
            "https://www.dropbox.com/sh/arnpe0ef5wds8cv/AAAk_SECQ2Nc6SVGii3rHX6Fa?dl=0";

        #[derive(Deserialize)]
        #[serde(untagged)]
        enum ListFolderResult {
            Ok(ListFolder),
            Err(serde_json::Value),
        }

        #[derive(Deserialize)]
        struct ListFolder {
            entries: Vec<ListFolderEntry>,
        }

        #[derive(Deserialize)]
        struct ListFolderEntry {
            name: String,
        }

        fn list_folder(
            mut sess: impl SessionMut,
            access_token: &str,
            path: &str,
        ) -> anyhow::Result<ListFolder> {
            let result = sess
                .post(static_url!("https://api.dropboxapi.com/2/files/list_folder").clone())
                .bearer_auth(access_token)
                .json(&json!({ "shared_link": { "url": URL }, "path": &path }))
                .colorize_status_code(&[200], (), ..)
                .send()?
                .ensure_status(&[200, 409])?
                .json::<ListFolderResult>()?;

            match result {
                ListFolderResult::Ok(list_folder) => Ok(list_folder),
                ListFolderResult::Err(err) => {
                    Err(anyhow::Error::msg(serde_json::to_string_pretty(&err)?)
                        .context(format!("Failed to read `{}`", path)))
                }
            }
        }

        fn retrieve_files(
            mut sess: impl SessionMut,
            access_token: &str,
            path: &str,
            entries: &[ListFolderEntry],
        ) -> anyhow::Result<IndexMap<String, String>> {
            let contents = super::download_with_progress(
                sess.shell().progress_draw_target(),
                entries
                    .iter()
                    .map(|ListFolderEntry { name }| {
                        let path = format!("{}/{}", path, name);
                        let req = sess
                            .async_client()
                            .post("https://content.dropboxapi.com/2/sharing/get_shared_link_file")
                            .bearer_auth(access_token)
                            .header(
                                "Dropbox-API-Arg",
                                json!({ "url": URL, "path": &path }).to_string(),
                            );
                        (path, req)
                    })
                    .collect(),
            )?;

            Ok(entries
                .iter()
                .map(|ListFolderEntry { name }| name.clone())
                .zip_eq(contents)
                .collect())
        }
    }
}

impl<S: Shell> Exec<Submit<Self, S>> for Atcoder<'_> {
    type Output = SubmitOutcome;

    fn exec(args: Submit<Self, S>) -> anyhow::Result<SubmitOutcome> {
        let Submit {
            target: AtcoderSubmitTarget { contest, problem },
            credentials:
                AtcoderSubmitCredentials {
                    username_and_password,
                },
            language_id,
            code,
            watch_submission,
            cookie_storage,
            timeout,
            shell,
        } = args;

        let contest = CaseConverted::<LowerCase>::new(contest);
        let problem = CaseConverted::<UpperCase>::new(problem);

        let mut sess = Session::new(timeout, Some(cookie_storage), shell)?;

        let tasks_page = retrieve_tasks_page(&mut sess, username_and_password, &contest)?;

        let url = tasks_page
            .extract_task_indexes_and_urls()?
            .remove(&problem)
            .with_context(|| format!("No such problem: `{}`", problem))?;

        let problem_screen_name =
            static_regex!(r"\A/contests/[a-z0-9_\-]+/tasks/([a-z0-9_]+)/?\z$")
                .captures(url.path())
                .map(|cs| cs[1].to_owned())
                .with_context(|| "Could not extract screen name of the problem")?;

        let csrf_token = sess
            .get(url)
            .colorize_status_code(&[200], (), ..)
            .send()?
            .ensure_status(&[200])?
            .html()?
            .extract_csrf_token()?;

        let res = sess
            .post(url!("/contests/{}/submit", contest))
            .form(&hashmap! {
                "data.TaskScreenName" => &*problem_screen_name,
                "data.LanguageId" => language_id.as_ref(),
                "sourceCode" => code.as_ref(),
                "csrf_token" => &csrf_token,
            })
            .colorize_status_code(&[302], (), ..)
            .send()?
            .ensure_status(&[200, 302])?;

        if res.status() == 302 {
            let loc = res.location_url()?;

            if loc.path().starts_with("/contests/") && loc.path().ends_with("/submissions/me") {
                let (submission_summaries, _) =
                    retrieve_submission_summaries_from_page_1(&mut sess, &contest, || {
                        bail!("Should be logged in")
                    })?;

                let outcome = SubmitOutcome {
                    problem_screen_name,
                    submission_url: submission_summaries[0].url.clone(),
                    submissions_url: url!("/contests/{}/submissions/me", contest),
                };

                if watch_submission {
                    watch_submissions(sess, &contest, &submission_summaries)?;
                }

                Ok(outcome)
            } else {
                sess.get(loc).colorize_status_code((), (), ..).send()?;
                bail!("Submission rejected");
            }
        } else {
            bail!("Submission rejected");
        }
    }
}

impl<S: Shell> Exec<WatchSubmissions<Self, S>> for Atcoder<'_> {
    type Output = Option<AnsiColored>;

    fn exec(args: WatchSubmissions<Self, S>) -> anyhow::Result<Option<AnsiColored>> {
        let WatchSubmissions {
            target: AtcoderWatchSubmissionsTarget { contest },
            credentials:
                AtcoderWatchSubmissionsCredentials {
                    username_and_password,
                },
            cookie_storage,
            timeout,
            shell,
        } = args;

        let contest = CaseConverted::<LowerCase>::new(contest);

        let mut sess = Session::new(timeout, Some(cookie_storage), shell)?;

        let (summaries, _) =
            retrieve_submission_summaries_from_page_1(&mut sess, &contest, username_and_password)?;

        if summaries
            .iter()
            .all(|SubmissionSummary { verdict, .. }| verdict != "WJ" && verdict.len() <= 3)
        {
            let content = AnsiColored::new(|w| print_submissions(w, &summaries))?;
            Ok(Some(content))
        } else {
            watch_submissions(sess, &contest, &summaries)?;
            Ok(None)
        }
    }
}

pub struct AtcoderLoginCredentials<'closures> {
    pub username_and_password: &'closures mut dyn FnMut() -> anyhow::Result<(String, String)>,
}

#[derive(Debug)]
pub struct AtcoderParticipateTarget {
    pub contest: String,
}

pub struct AtcoderParticipateCredentials<'closures> {
    pub username_and_password: &'closures mut dyn FnMut() -> anyhow::Result<(String, String)>,
}

#[derive(Debug)]
pub struct AtcoderRetrieveLanguagesTarget {
    pub contest_and_problem: Option<(String, String)>,
}

pub struct AtcoderRetrieveLanguagesCredentials<'closures> {
    pub username_and_password: &'closures mut dyn FnMut() -> anyhow::Result<(String, String)>,
}

#[derive(Debug)]
pub struct AtcoderRetrieveTestCasesTargets {
    pub contest: String,
    pub problems: Option<BTreeSet<String>>,
}

pub struct AtcoderRetrieveSampleTestCasesCredentials<'closures> {
    pub username_and_password: &'closures mut dyn FnMut() -> anyhow::Result<(String, String)>,
}

#[derive(Debug)]
pub struct AtcoderRetrieveFullTestCasesCredentials {
    pub dropbox_access_token: String,
}

#[derive(Debug)]
pub struct AtcoderWatchSubmissionsTarget {
    pub contest: String,
}

pub struct AtcoderWatchSubmissionsCredentials<'closures> {
    pub username_and_password: &'closures mut dyn FnMut() -> anyhow::Result<(String, String)>,
}

#[derive(Debug)]
pub struct AtcoderSubmitTarget {
    pub contest: String,
    pub problem: String,
}

pub struct AtcoderSubmitCredentials<'closures> {
    pub username_and_password: &'closures mut dyn FnMut() -> anyhow::Result<(String, String)>,
}

fn retrieve_sample_test_cases(
    mut sess: impl SessionMut,
    username_and_password: impl FnMut() -> anyhow::Result<(String, String)>,
    contest: &str,
    problems: Option<&BTreeSet<String>>,
) -> anyhow::Result<RetrieveTestCasesOutcome> {
    let contest = CaseConverted::<LowerCase>::new(contest);

    let html = retrieve_tasks_page(&mut sess, username_and_password, &contest)?;
    let indexes_and_urls = html.extract_task_indexes_and_urls()?;

    let test_suites = sess
        .get(url!("/contests/{}/tasks_print", contest))
        .send()?
        .html()?
        .extract_samples()?;

    if indexes_and_urls.len() != test_suites.len() {
        bail!(
            "Found {} task(s) in `tasks`, {} task(s) in `tasks_print`",
            indexes_and_urls.len(),
            test_suites.len(),
        );
    }

    let mut problems =
        problems.map(|ps| ps.iter().map(|p| p.to_uppercase()).collect::<BTreeSet<_>>());

    let mut outcome = RetrieveTestCasesOutcome {
        contest: Some(RetrieveTestCasesOutcomeContest {
            id: (*contest).to_owned(),
            submissions_url: url!("/contests/{}/submissions/me", contest),
        }),
        problems: vec![],
    };

    for ((index, url), (display_name, test_suite)) in
        indexes_and_urls.into_iter().zip_eq(test_suites)
    {
        if problems.as_mut().map_or(true, |ps| ps.remove(&*index)) {
            let screen_name = url
                .path_segments()
                .and_then(Iterator::last)
                .with_context(|| "Empty URL")?
                .to_owned();

            outcome.problems.push(RetrieveTestCasesOutcomeProblem {
                url,
                index: index.into(),
                screen_name,
                display_name,
                test_suite,
                text_files: indexmap![],
            });
        }
    }

    if let Some(problems) = problems {
        if !problems.is_empty() {
            bail!("No such problems: {:?}", problems);
        }
    }

    Ok(outcome)
}

fn login(
    mut sess: impl SessionMut,
    mut username_and_password: impl FnMut() -> anyhow::Result<(String, String)>,
) -> anyhow::Result<()> {
    while {
        let (username, password) = username_and_password()?;

        let csrf_token = sess
            .get(url!("/login"))
            .colorize_status_code(&[200], (), ..)
            .send()?
            .ensure_status(&[200])?
            .html()?
            .extract_csrf_token()?;

        let payload = hashmap!(
            "csrf_token" => csrf_token,
            "username" => username,
            "password" => password,
        );

        sess.post(url!("/login"))
            .form(&payload)
            .colorize_status_code(&[302], (), ..)
            .send()?
            .ensure_status(&[302])?;

        !check_logged_in(&mut sess)?
    } {}

    Ok(())
}

fn check_logged_in(mut sess: impl SessionMut) -> anyhow::Result<bool> {
    let status = sess
        .get(url!("/settings"))
        .colorize_status_code(&[200], &[302], ())
        .send()?
        .ensure_status(&[200, 302])?
        .status();

    Ok(status == 200)
}

fn participate(
    mut sess: impl SessionMut,
    credentials: impl FnMut() -> anyhow::Result<(String, String)>,
    contest: &CaseConverted<LowerCase>,
    explicit: bool,
) -> anyhow::Result<ParticipateOutcome> {
    let res = sess
        .get(url!("/contests/{}", contest))
        .colorize_status_code(&[200], (), ..)
        .send()?
        .ensure_status(&[200, 404])?;

    if res.status() == 404 {
        bail!(
            "The contest `{}` does not exist, or your are not authorized",
            contest,
        );
    }

    let html = res.html()?;

    let status = ContestStatus::now(html.extract_contest_duration()?, contest);

    if !explicit {
        status.raise_if_not_begun()?;
    }

    login(&mut sess, credentials)?;

    if status.is_finished() {
        Ok(ParticipateOutcome::ContestIsFinished)
    } else {
        let html = sess
            .get(url!("/contests/{}", contest))
            .colorize_status_code(&[200], (), ..)
            .send()?
            .ensure_status(&[200])?
            .html()?;

        if html.contains_registration_button()? {
            let csrf_token = html.extract_csrf_token()?;

            sess.post(url!("/contests/{}/register", contest))
                .form(&hashmap!("csrf_token" => csrf_token))
                .colorize_status_code(&[302], (), ..)
                .send()?
                .ensure_status(&[302])?;

            Ok(ParticipateOutcome::Success)
        } else {
            Ok(ParticipateOutcome::AlreadyParticipated)
        }
    }
}

fn retrieve_tasks_page(
    mut sess: impl SessionMut,
    username_and_password: impl FnMut() -> anyhow::Result<(String, String)>,
    contest: &CaseConverted<LowerCase>,
) -> anyhow::Result<Html> {
    let res = sess
        .get(url!("/contests/{}/tasks", contest))
        .colorize_status_code(&[200], &[404], ..)
        .send()?
        .ensure_status(&[200, 404])?;

    if res.status() == 200 {
        res.html().map_err(Into::into)
    } else {
        participate(&mut sess, username_and_password, contest, false)?;

        sess.get(url!("/contests/{}/tasks", contest))
            .colorize_status_code(&[200], (), ..)
            .send()?
            .ensure_status(&[200])?
            .html()
            .map_err(Into::into)
    }
}

fn retrieve_submission_summaries_from_page_1(
    mut sess: impl SessionMut,
    contest: &CaseConverted<LowerCase>,
    username_and_password: impl FnMut() -> anyhow::Result<(String, String)>,
) -> anyhow::Result<(Vec<SubmissionSummary>, u32)> {
    let res = sess
        .get(submissions_me(contest, 1)?)
        .colorize_status_code(&[200], &[302], ..)
        .send()?
        .ensure_status(&[200, 302])?;

    return if res.status() == 200 {
        res
    } else {
        participate(&mut sess, username_and_password, contest, false)?;
        sess.get(submissions_me(contest, 1)?)
            .colorize_status_code(&[200], (), ..)
            .send()?
            .ensure_status(&[200])?
    }
    .html()?
    .extract_submissions();

    fn submissions_me(contest: &CaseConverted<LowerCase>, page: u32) -> anyhow::Result<Url> {
        let mut url = url!("/contests/{}/submissions/me", contest);
        url.query_pairs_mut().append_pair("page", &page.to_string());
        Ok(url)
    }
}

fn print_submissions(mut wtr: impl WriteColor, summaries: &[SubmissionSummary]) -> io::Result<()> {
    let task_display_max_width = summaries
        .iter()
        .map(|SubmissionSummary { task_display, .. }| task_display.width())
        .max()
        .unwrap_or(0);

    let lang_max_width = summaries
        .iter()
        .map(|SubmissionSummary { lang, .. }| lang.width())
        .max()
        .unwrap_or(0);

    for summary in summaries {
        write!(
            wtr,
            "│ {} │ {} │ {} │ ",
            summary.datetime,
            align_left(&summary.task_display, task_display_max_width),
            align_left(&summary.lang, lang_max_width),
        )?;

        if summary.verdict == "WJ" {
            wtr.set_color(&ColorSpec::new().set_bold(true))?;
            write!(wtr, "WJ")?;
            wtr.reset()?;
        } else if JUDGING.is_match(&summary.verdict) {
            wtr.set_color(&color_spec(&summary.verdict))?;
            write!(wtr, "{}", summary.verdict)?;
            wtr.reset()?;
        } else {
            wtr.set_color(&color_spec(&summary.verdict))?;
            write!(wtr, "{:<3}", summary.verdict)?;
            wtr.reset()?;
            write!(
                wtr,
                " │ {:>8} │ {:>9} │",
                summary.exec_time.as_deref().unwrap_or(""),
                summary.memory.as_deref().unwrap_or(""),
            )?;
        }

        writeln!(wtr)?;
    }

    wtr.flush()?;
    return Ok(());

    static JUDGING: Lazy<Regex> = lazy_regex!(r"\A\s*([0-9]{1,3})/([0-9]{1,3})\s*(\S*)\s*\z");

    fn color_spec(verdict: &str) -> ColorSpec {
        // https://atcoder.jp/contests/arc001/glossary

        return match verdict.trim() {
            "AC" => from_fg_and_bold(Some(Color::Green), true),
            "CE" | "RE" | "WA" => from_fg_and_bold(Some(Color::Yellow), true),
            "MLE" | "TLE" | "OLE" => from_fg_and_bold(Some(Color::Red), true),
            "IE" | "WJ" | "WR" => from_fg_and_bold(None, true),
            s => {
                if let Some(caps) = JUDGING.captures(s) {
                    color_spec(&caps[3])
                } else {
                    ColorSpec::new()
                }
            }
        };

        fn from_fg_and_bold(fg: Option<Color>, bold: bool) -> ColorSpec {
            let mut spec = ColorSpec::new();
            spec.set_fg(fg).set_bold(bold);
            spec
        }
    }
}

fn watch_submissions(
    mut sess: impl SessionMut,
    contest: &CaseConverted<LowerCase>,
    summaries: &[SubmissionSummary],
) -> anyhow::Result<()> {
    let mut rt = Runtime::new()?;
    let mut handles = vec![];

    let mp = MultiProgress::with_draw_target(sess.shell().progress_draw_target());

    let task_display_max_width = summaries
        .iter()
        .map(|SubmissionSummary { task_display, .. }| task_display.width())
        .max()
        .unwrap_or(0);

    let lang_max_width = summaries
        .iter()
        .map(|SubmissionSummary { lang, .. }| lang.width())
        .max()
        .unwrap_or(0);

    for summary in summaries {
        let pb = mp.add(ProgressBar::new(0));

        pb.set_style(
            ProgressStyle::default_bar().template("{prefix}{msg:3.bold}                        │"),
        );

        pb.set_prefix(&format!(
            "│ {} │ {} │ {} │ ",
            summary.datetime,
            align_left(&summary.task_display, task_display_max_width),
            align_left(&summary.lang, lang_max_width),
        ));

        if is_wj_or_judging(&summary.verdict) {
            let id = summary.id().to_owned();

            let mut url = url!("/contests/{}/submissions/me/status/json", contest);

            url.query_pairs_mut()
                .append_pair("reload", "true")
                .append_pair("sids[]", &id);

            let client = sess.async_client().clone();

            let cookie_header = sess.cookie_header(&"https://atcoder.jp".parse().unwrap());

            handles.push(rt.spawn(async move {
                let finish_pb = || tokio::task::block_in_place(|| pb.finish_at_current_pos());

                macro_rules! trap(($result:expr $(,)?) => {
                    match $result {
                        Ok(ok) => ok,
                        Err(err) => {
                            finish_pb();
                            return Err(err.into());
                        }
                    }
                });

                loop {
                    #[derive(Deserialize)]
                    #[serde(rename_all = "PascalCase")]
                    struct VerdictProgress {
                        interval: Option<u64>,
                        result: IndexMap<String, VerdictProgressResult>,
                    }

                    #[derive(Deserialize)]
                    #[serde(rename_all = "PascalCase")]
                    struct VerdictProgressResult {
                        html: String,
                        //score: String,
                    }

                    let res = trap!(
                        client
                            .get(url.clone())
                            .header(header::COOKIE, &cookie_header)
                            .send()
                            .await,
                    );

                    let VerdictProgress { interval, result } = trap!(res.json().await);

                    let VerdictProgressResult { html } = trap!(result
                        .get(&id)
                        .with_context(|| format!("Not found: `{}`", id)));

                    let text = tokio::task::block_in_place(|| {
                        Html::parse_fragment(html)
                            .root_element()
                            .text()
                            .map(ToOwned::to_owned)
                            .collect::<Vec<_>>()
                    });

                    if let Some(interval) = interval {
                        let verdict = match &text[..] {
                            [verdict] => verdict,
                            _ => trap!(Err(anyhow!("Could not extract information"))),
                        };

                        if let Some(caps) = JUDGING.captures(verdict) {
                            let position = caps[1].parse().unwrap();
                            let length = caps[2].parse().unwrap();
                            let verdict = &caps[3];

                            pb.set_style(ProgressStyle::default_bar().template(&format!(
                                "{{prefix}}{{msg:3{style}}} {{pos:>3{style}}}/\
                                         {{len:>3{style}}} {{bar:14{style}}} │",
                                style = style(verdict),
                            )));

                            pb.set_message(verdict);
                            pb.set_length(length);
                            pb.set_position(position);
                        } else {
                            pb.set_message(verdict);
                        }

                        tokio::task::block_in_place(|| pb.set_message(&verdict));
                        tokio::time::delay_for(Duration::from_millis(interval)).await;
                    } else {
                        let (verdict, time_and_memory) = match &text[..] {
                            [verdict, time_and_memory] => (verdict, time_and_memory),
                            _ => trap!(Err(anyhow!("Could not extract information"))),
                        };

                        let (exec_time, memory) =
                            match *time_and_memory.split(" ms").collect::<Vec<_>>() {
                                [time, memory] => (format!("{} ms", time), memory),
                                _ => trap!(Err(anyhow!("Could not extract information"))),
                            };

                        tokio::task::block_in_place(|| {
                            finish(&pb, verdict, &exec_time, memory);
                        });
                        break Result::<(), anyhow::Error>::Ok(());
                    }
                }
            }));
        } else {
            finish(
                &pb,
                &summary.verdict,
                summary.exec_time.as_deref().unwrap_or(""),
                summary.memory.as_deref().unwrap_or(""),
            );
        }
    }

    mp.join()?;

    for handle in handles {
        rt.block_on(handle)??;
    }

    return Ok(());

    static JUDGING: Lazy<Regex> = lazy_regex!(r"\A\s*([0-9]{1,3})/([0-9]{1,3})\s*(\S*)\s*\z");

    fn is_wj_or_judging(verdict: &str) -> bool {
        verdict == "WJ" || verdict.starts_with(|c| matches!(c, '0'..='9'))
    }

    fn finish(pb: &ProgressBar, verdict: &str, exec_time: &str, memory: &str) {
        pb.set_style(ProgressStyle::default_bar().template(&format!(
            "{{prefix}}{{msg:3{}}} │ {} │ {} │",
            style(verdict),
            align_right(exec_time, 8),
            align_right(memory, 9),
        )));
        pb.finish_with_message(verdict);
    };

    fn style(verdict: &str) -> &'static str {
        // https://atcoder.jp/contests/arc001/glossary

        match verdict.trim() {
            "AC" => ".green.bold",
            "CE" | "RE" | "WA" => ".yellow.bold",
            "MLE" | "TLE" | "OLE" => ".red.bold",
            "IE" | "WJ" | "WR" => ".bold",
            s => {
                if let Some(caps) = JUDGING.captures(s) {
                    style(&caps[3])
                } else {
                    ""
                }
            }
        }
    }
}

fn align_left(s: &str, n: usize) -> String {
    let spaces = n.saturating_sub(s.width());
    s.chars().chain(itertools::repeat_n(' ', spaces)).collect()
}

fn align_right(s: &str, n: usize) -> String {
    let spaces = n.saturating_sub(s.width());
    itertools::repeat_n(' ', spaces).chain(s.chars()).collect()
}

#[derive(Debug)]
enum ContestStatus {
    Finished,
    Active,
    NotBegun(CaseConverted<LowerCase>, DateTime<Local>),
}

impl ContestStatus {
    fn now(dur: (DateTime<Utc>, DateTime<Utc>), contest_id: &CaseConverted<LowerCase>) -> Self {
        let (start, end) = dur;
        let now = Utc::now();
        if now < start {
            ContestStatus::NotBegun(contest_id.to_owned(), start.with_timezone(&Local))
        } else if now > end {
            ContestStatus::Finished
        } else {
            ContestStatus::Active
        }
    }

    fn is_finished(&self) -> bool {
        match self {
            ContestStatus::Finished => true,
            _ => false,
        }
    }

    fn raise_if_not_begun(&self) -> anyhow::Result<()> {
        if let ContestStatus::NotBegun(contest, time) = self {
            bail!("`{}` will begin at {}", contest, time)
        }
        Ok(())
    }
}

#[derive(Debug)]
struct SubmissionSummary {
    url: Url,
    task_url: Url,
    task_index: String,
    task_display: String,
    task_screen: String,
    datetime: DateTime<FixedOffset>,
    lang: String,
    verdict: String,
    exec_time: Option<String>,
    memory: Option<String>,
}

impl SubmissionSummary {
    fn id(&self) -> &str {
        self.url
            .path_segments()
            .and_then(Iterator::last)
            .unwrap_or("")
    }
}

#[ext]
impl Html {
    fn extract_csrf_token(&self) -> anyhow::Result<String> {
        (|| -> _ {
            let token = self
                .select(static_selector!("[name=\"csrf_token\"]"))
                .next()?
                .value()
                .attr("value")?
                .to_owned();

            if token.is_empty() {
                None
            } else {
                Some(token)
            }
        })()
        .with_context(|| "Could not find the CSRF token")
    }

    fn extract_contest_duration(&self) -> anyhow::Result<(DateTime<Utc>, DateTime<Utc>)> {
        (|| -> _ {
            static FORMAT: &str = "%F %T%z";
            let mut it = self.select(static_selector!("time"));
            let t1 = it.next()?.text().next()?;
            let t2 = it.next()?.text().next()?;
            let t1 = DateTime::parse_from_str(t1, FORMAT).ok()?;
            let t2 = DateTime::parse_from_str(t2, FORMAT).ok()?;
            Some((t1.with_timezone(&Utc), t2.with_timezone(&Utc)))
        })()
        .with_context(|| "Could not find the contest duration")
    }

    fn contains_registration_button(&self) -> anyhow::Result<bool> {
        let insert_participant_box = self
            .select(static_selector!("#main-container .insert-participant-box"))
            .next()
            .with_context(|| "Could not find the registration button")?;

        Ok(insert_participant_box
            .select(static_selector!("form"))
            .find(|r| r.value().attr("method") == Some("POST"))
            .into_iter()
            .flat_map(|r| r.text())
            .any(|s| ["参加登録", "Register"].contains(&s)))
    }

    fn extract_task_indexes_and_urls(
        &self,
    ) -> anyhow::Result<IndexMap<CaseConverted<UpperCase>, Url>> {
        self.select(static_selector!(
            "#main-container > div.row > div.col-sm-12 > div.panel > table.table > tbody > tr",
        ))
        .map(|tr| {
            let a = tr.select(static_selector!("td.text-center > a")).next()?;
            let index = CaseConverted::new(a.text().next()?);
            let url = BASE_URL.join(a.value().attr("href")?).ok()?;
            Some((index, url))
        })
        .collect::<Option<IndexMap<_, _>>>()
        .filter(|m| !m.is_empty())
        .with_context(|| "Could not extract task indexes and URLs")
    }

    fn extract_samples(&self) -> anyhow::Result<IndexMap<String, TestSuite>> {
        return self
            .select(static_selector!(
                "#main-container > div.row > div[class=\"col-sm-12\"]",
            ))
            .map(|div| {
                let display_name = div
                    .select(static_selector!("span"))
                    .flat_map(|r| r.text())
                    .next()
                    .and_then(|s| static_regex!(r"[A-Z0-9]+ - (.+)").captures(s))
                    .map(|caps| caps[1].to_owned())
                    .with_context(|| "Could not extract the display name")?;

                let timelimit = div
                    .select(static_selector!("p"))
                    .flat_map(|r| r.text())
                    .flat_map(parse_timelimit)
                    .exactly_one()
                    .ok()
                    .with_context(|| "Could not extract the timelimit")?;

                // In `tasks_print`, there are multiple `#task-statement`s.
                let samples = div
                    .select(static_selector!("div[id=\"task-statement\"]"))
                    .exactly_one()
                    .ok()
                    .and_then(extract_samples)
                    .with_context(|| "Could not extract the sample cases")?;

                let test_suite = if timelimit == Duration::new(0, 0) {
                    TestSuite::Unsubmittable
                } else if let Samples::Batch(r#match, samples) = samples {
                    TestSuite::Batch(BatchTestSuite {
                        timelimit: Some(timelimit),
                        r#match,
                        cases: samples
                            .into_iter()
                            .enumerate()
                            .map(|(i, (input, output))| PartialBatchTestCase {
                                name: Some(format!("Sample {}", i + 1)),
                                r#in: input.into(),
                                out: Some(output.into()),
                                timelimit: None,
                                r#match: None,
                            })
                            .collect(),
                        extend: vec![],
                    })
                } else {
                    TestSuite::Interactive(InteractiveTestSuite {
                        timelimit: Some(timelimit),
                    })
                };

                Ok((display_name, test_suite))
            })
            .collect();

        fn parse_timelimit(text: &str) -> Option<Duration> {
            let caps =
                static_regex!(r"\A\D*([0-9]{1,9})(\.[0-9]{1,3})?\s*(m)?sec.*\z").captures(text)?;
            let (mut b, mut e) = (caps[1].parse::<u64>().unwrap(), 0);
            if let Some(cap) = caps.get(2) {
                let n = cap.as_str().len() as u32 - 1;
                b *= 10u64.pow(n);
                b += cap.as_str()[1..].parse::<u64>().ok()?;
                e -= n as i32;
            }
            if caps.get(3).is_none() {
                e += 3;
            }
            let timelimit = if e < 0 {
                b / 10u64.pow(-e as u32)
            } else {
                b * 10u64.pow(e as u32)
            };
            Some(Duration::from_millis(timelimit))
        }

        fn extract_samples(task_statement: ElementRef<'_>) -> Option<Samples> {
            // TODO:
            // - https://atcoder.jp/contests/arc019/tasks/arc019_4 (interactive)
            // - https://atcoder.jp/contests/arc021/tasks/arc021_4 (interactive)
            // - https://atcoder.jp/contests/cf17-final-open/tasks/cf17_final_f
            // - https://atcoder.jp/contests/jag2016-domestic/tasks
            // - https://atcoder.jp/contests/chokudai001/tasks/chokudai_001_a

            static IN_JA: Lazy<Regex> = lazy_regex!(r"\A[\s\n]*入力例\s*(\d{1,2})[.\n]*\z");
            static OUT_JA: Lazy<Regex> = lazy_regex!(r"\A[\s\n]*出力例\s*(\d{1,2})[.\n]*\z");
            static IN_EN: Lazy<Regex> = lazy_regex!(r"\ASample Input\s?([0-9]{1,2}).*\z");
            static OUT_EN: Lazy<Regex> = lazy_regex!(r"\ASample Output\s?([0-9]{1,2}).*\z");

            // Current style (Japanese)
            static P1_HEAD: Lazy<Selector> =
                lazy_selector!("span.lang > span.lang-ja > div.part > section > h3");
            static P1_CONTENT: Lazy<Selector> =
                lazy_selector!("span.lang > span.lang-ja > div.part > section > pre");
            // Current style (English)
            static P2_HEAD: Lazy<Selector> =
                lazy_selector!("span.lang > span.lang-en > div.part > section > h3");
            static P2_CONTENT: Lazy<Selector> =
                lazy_selector!("span.lang>span.lang-en>div.part>section>pre");
            // ARC019..ARC057 \ {ARC019/C, ARC046/D, ARC050, ARC052/{A, C}, ARC053, ARC055},
            // ABC007..ABC040 \ {ABC036}, ATC001, ATC002
            static P3_HEAD: Lazy<Selector> = lazy_selector!("div.part > section > h3");
            static P3_CONTENT: Lazy<Selector> = lazy_selector!("div.part > section > pre");
            // ARC002..ARC018, ARC019/C, ABC001..ABC006
            static P4_HEAD: Lazy<Selector> = lazy_selector!("div.part > h3,pre");
            static P4_CONTENT: Lazy<Selector> = lazy_selector!("div.part > section > pre");
            // ARC001, dwacon2018-final/{A, B}
            static P5_HEAD: Lazy<Selector> = lazy_selector!("h3,pre");
            static P5_CONTENT: Lazy<Selector> = lazy_selector!("section > pre");
            // ARC046/D, ARC050, ARC052/{A, C}, ARC053, ARC055, ABC036, ABC041
            static P6_HEAD: Lazy<Selector> = lazy_selector!("section > h3");
            static P6_CONTENT: Lazy<Selector> = lazy_selector!("section > pre");
            // ABC034
            static P7_HEAD: Lazy<Selector> =
                lazy_selector!("span.lang > span.lang-ja > section > h3");
            static P7_CONTENT: Lazy<Selector> =
                lazy_selector!("span.lang > span.lang-ja > section > pre");
            // practice contest (Japanese)
            static P8_HEAD: Lazy<Selector> =
                lazy_selector!("span.lang > span.lang-ja > div.part > h3");
            static P8_CONTENT: Lazy<Selector> =
                lazy_selector!("span.lang > span.lang-ja > div.part > section > pre");

            let stmt = task_statement;
            try_extract_samples(stmt, &P1_HEAD, &P1_CONTENT, &IN_JA, &OUT_JA)
                .or_else(|| try_extract_samples(stmt, &P2_HEAD, &P2_CONTENT, &IN_EN, &OUT_EN))
                .or_else(|| try_extract_samples(stmt, &P3_HEAD, &P3_CONTENT, &IN_JA, &OUT_JA))
                .or_else(|| try_extract_samples(stmt, &P4_HEAD, &P4_CONTENT, &IN_JA, &OUT_JA))
                .or_else(|| try_extract_samples(stmt, &P5_HEAD, &P5_CONTENT, &IN_JA, &OUT_JA))
                .or_else(|| try_extract_samples(stmt, &P6_HEAD, &P6_CONTENT, &IN_JA, &OUT_JA))
                .or_else(|| try_extract_samples(stmt, &P7_HEAD, &P7_CONTENT, &IN_JA, &OUT_JA))
                .or_else(|| try_extract_samples(stmt, &P8_HEAD, &P8_CONTENT, &IN_JA, &OUT_JA))
        }

        fn try_extract_samples(
            task_statement: ElementRef<'_>,
            selector_for_header: &'static Selector,
            selector_for_content: &'static Selector,
            re_input: &'static Regex,
            re_output: &'static Regex,
        ) -> Option<Samples> {
            if task_statement
                .select(static_selector!("strong"))
                .flat_map(|r| r.text())
                .any(|t| t.contains("インタラクティブ") || t.contains("Interactive"))
            {
                return Some(Samples::Interactive);
            }

            let matching = {
                let error = task_statement
                    .select(static_selector!("var"))
                    .flat_map(|r| r.text())
                    .flat_map(|t| parse_floating_error(t))
                    .next();

                let relative = task_statement
                    .text()
                    .any(|s| s.contains("相対誤差") || s.contains("relative error"));

                let absolute = task_statement
                    .text()
                    .any(|s| s.contains("絶対誤差") || s.contains("absolute error"));

                match (error, relative, absolute) {
                    (Some(error), true, true) => Match::Float {
                        relative_error: Some(error),
                        absolute_error: Some(error),
                    },
                    (Some(error), true, false) => Match::Float {
                        relative_error: Some(error),
                        absolute_error: None,
                    },
                    (Some(error), false, true) => Match::Float {
                        relative_error: None,
                        absolute_error: Some(error),
                    },
                    _ => Match::Lines,
                }
            };

            let mut inputs = BTreeMap::<usize, _>::new();
            let mut outputs = BTreeMap::<usize, _>::new();
            let mut next = None;
            let selector = or(selector_for_header, selector_for_content);
            for elem_ref in task_statement.select(&selector) {
                if elem_ref.value().name() == "h3" {
                    let text = elem_ref.collect_text();
                    if let Some(caps) = re_input.captures(&text) {
                        next = Some((true, parse_zenkaku(&caps[1]).ok()?));
                    } else if let Some(caps) = re_output.captures(&text) {
                        next = Some((false, parse_zenkaku(&caps[1]).ok()?));
                    }
                } else if ["pre", "section"].contains(&elem_ref.value().name()) {
                    if let Some((is_input, n)) = next {
                        let text = elem_ref.collect_text();
                        if is_input {
                            inputs.insert(n, text);
                        } else {
                            outputs.insert(n, text);
                        }
                    }
                    next = None;
                }
            }
            let mut samples = vec![];
            for (i, input) in inputs {
                if let Some(output) = outputs.remove(&i) {
                    samples.push((input, output));
                }
            }

            for (input, output) in &mut samples {
                for s in &mut [input, output] {
                    if !(s.is_empty() || s.ends_with('\n')) {
                        s.push('\n');
                    }

                    if !is_valid_text(s) {
                        return None;
                    }
                }
            }

            if samples.is_empty() {
                return None;
            }

            Some(Samples::Batch(matching, samples))
        }

        fn or(selector1: &Selector, selector2: &Selector) -> Selector {
            let mut ret = selector1.clone();
            ret.selectors.extend(selector2.selectors.clone());
            ret
        }

        fn parse_floating_error(s: &str) -> Option<PositiveFinite<f64>> {
            let caps = static_regex!(r"\A10\^\{(-?[0-9]{1,2})\}\z").captures(s)?;
            format!("1e{}", &caps[1]).parse().ok()
        }

        fn parse_zenkaku<T: FromStr>(s: &str) -> Result<T, T::Err> {
            match s.parse() {
                Ok(v) => Ok(v),
                Err(e) => {
                    if s.chars().all(|c| '０' <= c && c <= '９') {
                        s.chars()
                            .map(|c| {
                                char::from((u32::from(c) - u32::from('０') + u32::from('0')) as u8)
                            })
                            .collect::<String>()
                            .parse()
                    } else {
                        Err(e)
                    }
                }
            }
        }

        fn is_valid_text(s: &str) -> bool {
            s == "\n"
                || ![' ', '\n'].iter().any(|&c| s.starts_with(c))
                    && s.chars().all(|c| {
                        c.is_ascii() && (c.is_ascii_whitespace() == [' ', '\n'].contains(&c))
                    })
        }

        #[ext]
        impl ElementRef<'_> {
            fn collect_text(&self) -> String {
                self.text().fold("".to_owned(), |mut r, s| {
                    r.push_str(s);
                    r
                })
            }
        }

        enum Samples {
            Batch(Match, Vec<(String, String)>),
            Interactive,
        }
    }

    fn extract_langs(&self) -> anyhow::Result<IndexMap<String, String>> {
        self.select(static_selector!("#select-lang option"))
            .filter(|r| r.text().next().is_some()) // Ignores `<option></option>`
            .map(|option| {
                let id = option.value().attr("value")?.to_owned();
                let name = option.text().next()?.to_owned();
                Some((id, name))
            })
            .collect::<Option<IndexMap<_, _>>>()
            .filter(|m| !m.is_empty())
            .with_context(|| "Could not extract the available languages")
    }

    fn extract_submissions(&self) -> anyhow::Result<(Vec<SubmissionSummary>, u32)> {
        (|| {
            let num_pages = self
                .select(static_selector!(
                    "#main-container > div.row > div.col-sm-12 > div.text-center > ul > li > a",
                ))
                .flat_map(|r| r.text())
                .flat_map(|t| t.parse::<u32>().ok())
                .max()
                .unwrap_or(1);

            let mut submissions = vec![];

            static SELECTOR: Lazy<Selector> = lazy_selector!(
                "#main-container > div.row > div.col-sm-12 > div.panel-submission
                 > div.table-responsive > table.table > tbody > tr",
            );

            for tr in self.select(&SELECTOR) {
                let (task_url, task_index, task_display, task_screen, datetime) = {
                    static SLUG: Lazy<Regex> = lazy_regex!(r"\A(\w+).*\z");
                    static SCREEN: Lazy<Regex> =
                        lazy_regex!(r"\A/contests/[\w-]+/tasks/([\w-]+)\z");
                    static DATETIME: &str = "%F %T%z";

                    let a = tr.select(static_selector!("td > a")).next()?;
                    let task_display = a.text().next()?.to_owned();
                    let task_index = SLUG.captures(&task_display)?[1].to_owned();
                    let task_path = a.value().attr("href")?;
                    let task_screen = SCREEN.captures(task_path)?[1].to_owned();
                    let mut task_url = "https://atcoder.jp".parse::<Url>().unwrap();
                    task_url.set_path(task_path);
                    let datetime = tr
                        .select(static_selector!("td > time"))
                        .next()?
                        .text()
                        .next()?;
                    let datetime = DateTime::parse_from_str(datetime, DATETIME).ok()?;
                    (task_url, task_index, task_display, task_screen, datetime)
                };

                let lang = tr
                    .select(static_selector!("td"))
                    .nth(3)?
                    .text()
                    .next()?
                    .to_owned();

                let verdict = tr
                    .select(static_selector!("td > span"))
                    .next()?
                    .text()
                    .next()?
                    .to_owned();

                let exec_time = tr
                    .select(static_selector!("td"))
                    .nth(7)
                    .and_then(|r| r.text().exactly_one().ok())
                    .map(ToOwned::to_owned);

                let memory = tr
                    .select(static_selector!("td"))
                    .nth(8)
                    .and_then(|r| r.text().exactly_one().ok())
                    .map(ToOwned::to_owned);

                let url = tr
                    .select(static_selector!("td.text-center > a"))
                    .flat_map(|a| {
                        let text = a.text().next()?;
                        if !["詳細", "Detail"].contains(&text) {
                            return None;
                        }
                        let mut url = "https://atcoder.jp".parse::<Url>().unwrap();
                        url.set_path(a.value().attr("href")?);
                        Some(url)
                    })
                    .next()?;

                submissions.push(SubmissionSummary {
                    url,
                    task_url,
                    task_index,
                    task_display,
                    task_screen,
                    lang,
                    datetime,
                    verdict,
                    exec_time,
                    memory,
                })
            }
            Some((submissions, num_pages))
        })()
        .with_context(|| "Could not parse the submissions page")
    }
}
