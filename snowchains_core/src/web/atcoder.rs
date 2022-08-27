use crate::{
    testsuite::{
        BatchTestSuite, InteractiveTestSuite, Match, PartialBatchTestCase, PositiveFinite,
        TestSuite,
    },
    web::{
        AnsiColored, CaseConverted, CookieStorage, Exec, Login, LoginOutcome, LowerCase,
        Participate, ParticipateOutcome, Platform, ProblemInContest, ProblemsInContest,
        ResponseExt as _, RetrieveFullTestCases, RetrieveLanguages, RetrieveLanguagesOutcome,
        RetrieveSubmissionSummaries, RetrieveTestCases, RetrieveTestCasesOutcome,
        RetrieveTestCasesOutcomeProblem, RetrieveTestCasesOutcomeProblemContest,
        RetrieveTestCasesOutcomeProblemTextFiles, Session, SessionMut, Shell, Submit,
        SubmitOutcome, WatchSubmissions,
    },
};
use chrono::{DateTime, FixedOffset, Local, Utc};
use easy_ext::ext;
use either::Either;
use eyre::{bail, eyre, Context as _, ContextCompat as _};
use indexmap::{indexmap, IndexMap};
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use itertools::Itertools as _;
use maplit::{btreemap, hashmap, hashset};
use once_cell::sync::Lazy;
use regex::Regex;
use reqwest::header;
use scraper::{ElementRef, Html, Selector};
use serde::{Deserialize, Serialize, Serializer};
use serde_json::json;
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    convert::Infallible,
    fmt,
    hash::Hash,
    io,
    marker::PhantomData,
    mem,
    ops::Deref,
    path::Path,
    str::FromStr,
    time::Duration,
};
use termcolor::{Color, ColorSpec, WriteColor};
use tokio::runtime::Runtime;
use unicode_width::UnicodeWidthStr as _;
use url::Url;

static BASE_URL: Lazy<Url> = lazy_url!("https://atcoder.jp");

pub fn contest_id_from_url(url: &Url) -> eyre::Result<String> {
    if url.domain() != Some("atcoder.jp") {
        bail!("wrong domain. expected `atcoder.jp`: {}", url);
    }

    static_regex!(r"\A/contests/([a-z0-9_\-]+)/.*\z$")
        .captures(url.path())
        .map(|caps| caps[1].to_owned())
        .with_context(|| "Could not extract contest ID of the problem")
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Atcoder<'closures> {
    Infallible(Infallible, PhantomData<fn() -> &'closures ()>),
}

impl Atcoder<'_> {
    pub fn exec<A>(args: A) -> eyre::Result<<Self as Exec<A>>::Output>
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
    type RetrieveTestCasesTargets = ProblemsInContest;
    type RetrieveTestCasesCredentials = AtcoderRetrieveSampleTestCasesCredentials<'closures>;
    type RetrieveFullTestCasesCredentials = AtcoderRetrieveFullTestCasesCredentials;
    type RetrieveSubmissionSummariesTarget = AtcoderRetrieveSubmissionSummariesTarget;
    type RetrieveSubmissionSummariesCredentials =
        AtcoderRetrieveSubmissionSummariesCredentials<'closures>;
    type WatchSubmissionsTarget = AtcoderWatchSubmissionsTarget;
    type WatchSubmissionsCredentials = AtcoderWatchSubmissionsCredentials<'closures>;
    type SubmitTarget = ProblemInContest;
    type SubmitCredentials = AtcoderSubmitCredentials<'closures>;
}

impl<S: Shell> Exec<Login<Self, S>> for Atcoder<'_> {
    type Output = LoginOutcome;

    fn exec(args: Login<Self, S>) -> eyre::Result<LoginOutcome> {
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

    fn exec(args: Participate<Self, S>) -> eyre::Result<ParticipateOutcome> {
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

    fn exec(args: RetrieveLanguages<Self, S>) -> eyre::Result<RetrieveLanguagesOutcome> {
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
            (CaseConverted::<LowerCase>::new(contest), Some(problem))
        } else {
            (CaseConverted::<LowerCase>::new("practice"), None)
        };

        let mut sess = Session::new(timeout, Some(cookie_storage), shell)?;

        if !check_logged_in(&mut sess)? {
            login(&mut sess, username_and_password)?;
        }

        let url = if let Some(problem) = problem {
            let (_, url) = retrieve_tasks_page(&mut sess, || unreachable!(), &contest)?
                .extract_task_indexes_and_urls()?
                .into_iter()
                .find(|(name, _)| name.eq_ignore_ascii_case(&problem))
                .with_context(|| "")?;
            url
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

    fn exec(args: RetrieveTestCases<Self, S>) -> eyre::Result<RetrieveTestCasesOutcome> {
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

        let mut sess = Session::new(timeout, Some(cookie_storage), shell)?;

        let mut outcome = retrieve_sample_test_cases(&mut sess, username_and_password, &targets)?;

        if let Some(RetrieveFullTestCases {
            credentials:
                AtcoderRetrieveFullTestCasesCredentials {
                    dropbox_access_token,
                },
        }) = full
        {
            static DROPBOX_PATH_PREFIXES: Lazy<HashMap<String, String>> = Lazy::new(|| {
                serde_json::from_str(include_str!("../../resources/dropbox-path-prefixes.json"))
                    .unwrap()
            });

            for problem in &mut outcome.problems {
                let path_prefix = {
                    let contest = &problem.contest.as_ref().expect("should be `Some`").id;
                    DROPBOX_PATH_PREFIXES
                        .get(contest)
                        .cloned()
                        .unwrap_or_else(|| format!("/{}/", contest))
                };

                let problem_dir = format!("{}{}", path_prefix, problem.index);

                let problem_dir_entries =
                    list_paths_with_name_filter(&mut sess, &dropbox_access_token, &problem_dir)?;

                let mut list_file_paths = |in_out_dir_file_name: &'static str| -> _ {
                    if problem_dir_entries.has_folder(in_out_dir_file_name) {
                        list_paths_with_name_filter(
                            &mut sess,
                            &dropbox_access_token,
                            &format!("{}{}/{}", path_prefix, problem.index, in_out_dir_file_name),
                        )
                        .map(|es| es.files())
                    } else {
                        Ok(vec![])
                    }
                };

                let (in_file_paths, out_file_paths) = match *problem_dir_entries.folder_names() {
                    ["in", "out"] => (list_file_paths("in")?, list_file_paths("out")?),
                    ["in"] => (list_file_paths("in")?, vec![]),
                    ["out"] => (problem_dir_entries.files(), list_file_paths("out")?),
                    [] => (problem_dir_entries.files(), vec![]),
                    _ => bail!(
                        "unexpected format (path-prefix: {:?}, files: {:?}, folders: {:?})",
                        path_prefix,
                        problem_dir_entries.files(),
                        problem_dir_entries.folders(),
                    ),
                };

                let mut retrieve_files = |file_paths| -> eyre::Result<_> {
                    retrieve_files(&mut sess, &dropbox_access_token, file_paths)
                };
                let in_contents = retrieve_files(&in_file_paths)?;
                let mut out_contents = retrieve_files(&out_file_paths)?;

                problem.text_files = in_contents
                    .into_iter()
                    .map(|(name, r#in)| {
                        let out = out_contents.remove(&name);
                        (name, RetrieveTestCasesOutcomeProblemTextFiles { r#in, out })
                    })
                    .collect();
            }
        }

        return Ok(outcome);

        static URL: &str =
            "https://www.dropbox.com/sh/arnpe0ef5wds8cv/AAAk_SECQ2Nc6SVGii3rHX6Fa?dl=0";

        struct Entries(Vec<Either<String, String>>);

        impl Entries {
            fn has_folder(&self, name: &str) -> bool {
                self.0
                    .iter()
                    .any(|e| matches!(e, Either::Right(s) if s.split('/').last().unwrap() == name))
            }

            fn files(&self) -> Vec<String> {
                self.0
                    .iter()
                    .flat_map(|e| e.as_ref().left().cloned())
                    .collect()
            }

            fn folders(&self) -> Vec<&str> {
                self.0
                    .iter()
                    .flat_map(|e| e.as_ref().right().map(Deref::deref))
                    .collect()
            }

            fn folder_names(&self) -> Vec<&str> {
                self.0
                    .iter()
                    .flat_map(|e| e.as_ref().right().map(Deref::deref))
                    .map(|p| p.split('/').last().unwrap())
                    .collect()
            }
        }

        fn list_paths_with_name_filter(
            mut sess: impl SessionMut,
            access_token: &str,
            path: &str,
        ) -> eyre::Result<Entries> {
            #[derive(Deserialize)]
            struct ListFolderResult {
                entries: Vec<Metadata>,
                cursor: String,
                has_more: bool,
            }

            #[derive(Deserialize)]
            #[serde(tag = ".tag", rename_all = "snake_case")]
            enum Metadata {
                File { name: String },
                Folder { name: String },
                Deleted { name: String },
            }

            impl Metadata {
                fn is_valid(&self) -> bool {
                    !(matches!(self, Self::Folder { name } if name == "etc")
                        || matches!(
                            self, Self::File { name }
                            if !(name.is_ascii()
                                && [
                                    None,
                                    Some("txt".as_ref()),
                                    Some("in".as_ref()),
                                    Some("out".as_ref()),
                                ]
                                .contains(&Path::new(name).extension()))
                        ))
                }
            }

            fn ensure_status_ok(
                res: reqwest::blocking::Response,
                err_context: impl FnOnce() -> String,
            ) -> eyre::Result<reqwest::blocking::Response> {
                if res.status() != 200 {
                    let msg = { res }.text()?;
                    let msg = if let Ok(msg) = serde_json::from_str::<serde_json::Value>(&msg) {
                        serde_json::to_string_pretty(&msg).unwrap()
                    } else {
                        msg
                    };
                    return Err(eyre!("{}", msg)).context(err_context());
                }
                Ok(res)
            }

            let res = sess
                .post(static_url!("https://api.dropboxapi.com/2/files/list_folder").clone())
                .bearer_auth(access_token)
                .json(&json!({ "shared_link": { "url": URL }, "path": path }))
                .colorize_status_code(&[200], (), ..)
                .send()?
                .ensure_status(&[200, 400, 409])?;

            let res = ensure_status_ok(res, || {
                format!("could not retrieve file names in `{}`", path)
            })?;

            let mut output = vec![];

            let mut list_folder_result = res.json::<ListFolderResult>()?;
            output.extend(mem::take(&mut list_folder_result.entries));
            while list_folder_result.has_more {
                debug_assert!(list_folder_result.entries.is_empty());

                let res = sess
                    .post(
                        static_url!("https://api.dropboxapi.com/2/files/list_folder/continue")
                            .clone(),
                    )
                    .bearer_auth(access_token)
                    .json(&json!({ "cursor": &list_folder_result.cursor }))
                    .colorize_status_code(&[200], (), ..)
                    .send()?
                    .ensure_status(&[200, 400, 409])?;

                let res = ensure_status_ok(res, || {
                    format!(
                        "could not retrieve file names at cursor `{}`",
                        list_folder_result.cursor,
                    )
                })?;

                list_folder_result = res.json()?;
                output.extend(mem::take(&mut list_folder_result.entries));
            }

            output
                .into_iter()
                .filter(Metadata::is_valid)
                .map(|metadata| {
                    let join = |name: &str| format!("{}/{}", path.trim_end_matches('/'), name);
                    match metadata {
                        Metadata::File { name } => Ok(Either::Left(join(&name))),
                        Metadata::Folder { name } => Ok(Either::Right(join(&name))),
                        Metadata::Deleted { name } => bail!("deleted: {:?}", name),
                    }
                })
                .collect::<eyre::Result<_>>()
                .map(Entries)
        }

        fn retrieve_files(
            mut sess: impl SessionMut,
            access_token: &str,
            file_paths: &[String],
        ) -> eyre::Result<IndexMap<String, String>> {
            let contents = super::download_with_progress(
                sess.shell().progress_draw_target(),
                file_paths
                    .iter()
                    .map(|path| {
                        let req = sess
                            .async_client()
                            .post("https://content.dropboxapi.com/2/sharing/get_shared_link_file")
                            .bearer_auth(access_token)
                            .header(
                                "Dropbox-API-Arg",
                                json!({ "url": URL, "path": path }).to_string(),
                            );
                        (path.clone(), req)
                    })
                    .collect(),
            )?;

            return Ok(file_paths.iter().map(file_stem).zip_eq(contents).collect());

            fn file_stem(path: impl AsRef<str>) -> String {
                path.as_ref()
                    .split('/')
                    .last()
                    .unwrap()
                    .split('.')
                    .next()
                    .unwrap()
                    .to_owned()
            }
        }
    }
}

impl<S: Shell> Exec<RetrieveSubmissionSummaries<Self, S>> for Atcoder<'_> {
    type Output = AtcoderRetrieveSubmissionSummariesOutcome;

    fn exec(
        args: RetrieveSubmissionSummaries<Self, S>,
    ) -> eyre::Result<AtcoderRetrieveSubmissionSummariesOutcome> {
        let RetrieveSubmissionSummaries {
            target: AtcoderRetrieveSubmissionSummariesTarget { contest },
            credentials:
                AtcoderRetrieveSubmissionSummariesCredentials {
                    username_and_password,
                },
            cookie_storage,
            timeout,
            shell,
        } = args;

        let contest = CaseConverted::<LowerCase>::new(contest);

        let mut sess = Session::new(timeout, Some(cookie_storage), shell)?;

        let (mut summaries, num_pages) =
            retrieve_submission_summaries(&mut sess, &contest, 1, username_and_password)?;

        for page in 2..=num_pages {
            let (extend, _) = retrieve_submission_summaries(&mut sess, &contest, page, || {
                bail!("should be logged in");
            })?;
            summaries.extend(extend);
        }

        Ok(AtcoderRetrieveSubmissionSummariesOutcome { summaries })
    }
}

impl<S: Shell> Exec<Submit<Self, S>> for Atcoder<'_> {
    type Output = SubmitOutcome;

    fn exec(args: Submit<Self, S>) -> eyre::Result<SubmitOutcome> {
        let Submit {
            target,
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

        let mut sess = Session::new(timeout, Some(cookie_storage), shell)?;

        let (contest, url) = match target {
            ProblemInContest::Index { contest, problem } => {
                let contest = CaseConverted::<LowerCase>::new(contest);

                let tasks_page = retrieve_tasks_page(&mut sess, username_and_password, &contest)?;

                let (_, url) = tasks_page
                    .extract_task_indexes_and_urls()?
                    .into_iter()
                    .find(|(name, _)| name.eq_ignore_ascii_case(&problem))
                    .with_context(|| format!("No such problem: `{}`", problem))?;

                (contest, url)
            }
            ProblemInContest::Url { url } => {
                let contest = CaseConverted::new(contest_id_from_url(&url)?);
                (contest, url)
            }
        };

        let problem_screen_name =
            static_regex!(r"\A/contests/[a-zA-Z0-9_\-]+/tasks/([a-zA-Z0-9_\-]+)/?\z$")
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
                    retrieve_submission_summaries(&mut sess, &contest, 1, || {
                        bail!("Should be logged in")
                    })?;

                let outcome = SubmitOutcome {
                    problem_screen_name: Some(problem_screen_name),
                    submission_url: submission_summaries[0].detail.clone(),
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
    type Output = ();

    fn exec(args: WatchSubmissions<Self, S>) -> eyre::Result<()> {
        let WatchSubmissions {
            target: AtcoderWatchSubmissionsTarget { contest },
            credentials:
                AtcoderWatchSubmissionsCredentials {
                    username_and_password,
                },
            cookie_storage,
            timeout,
            mut shell,
        } = args;

        let contest = CaseConverted::<LowerCase>::new(contest);

        let mut sess = Session::new(timeout, Some(cookie_storage), &mut shell)?;

        let (summaries, _) =
            retrieve_submission_summaries(&mut sess, &contest, 1, username_and_password)?;

        let any_incomplete = summaries.iter().any(|SubmissionSummary { status, .. }| {
            matches!(status, Verdict::Wj | Verdict::Judging(..))
        });

        if any_incomplete {
            watch_submissions(sess, &contest, &summaries)?;
        } else {
            let content = AnsiColored::new(|w| print_submissions(w, &summaries))?;
            shell.print_ansi(content.get())?;
        }
        Ok(())
    }
}

pub struct AtcoderLoginCredentials<'closures> {
    pub username_and_password: &'closures mut dyn FnMut() -> eyre::Result<(String, String)>,
}

#[derive(Debug)]
pub struct AtcoderParticipateTarget {
    pub contest: String,
}

pub struct AtcoderParticipateCredentials<'closures> {
    pub username_and_password: &'closures mut dyn FnMut() -> eyre::Result<(String, String)>,
}

#[derive(Debug)]
pub struct AtcoderRetrieveLanguagesTarget {
    pub contest_and_problem: Option<(String, String)>,
}

pub struct AtcoderRetrieveLanguagesCredentials<'closures> {
    pub username_and_password: &'closures mut dyn FnMut() -> eyre::Result<(String, String)>,
}

pub struct AtcoderRetrieveSampleTestCasesCredentials<'closures> {
    pub username_and_password: &'closures mut dyn FnMut() -> eyre::Result<(String, String)>,
}

#[derive(Debug)]
pub struct AtcoderRetrieveFullTestCasesCredentials {
    pub dropbox_access_token: String,
}

#[derive(Debug)]
pub struct AtcoderRetrieveSubmissionSummariesTarget {
    pub contest: String,
}

pub struct AtcoderRetrieveSubmissionSummariesCredentials<'closures> {
    pub username_and_password: &'closures mut dyn FnMut() -> eyre::Result<(String, String)>,
}

#[non_exhaustive]
#[derive(Debug, Serialize)]
pub struct AtcoderRetrieveSubmissionSummariesOutcome {
    summaries: Vec<SubmissionSummary>,
}

impl AtcoderRetrieveSubmissionSummariesOutcome {
    pub fn to_json(&self) -> String {
        serde_json::to_string(self).expect("should not fail")
    }
}

#[derive(Debug)]
pub struct AtcoderWatchSubmissionsTarget {
    pub contest: String,
}

pub struct AtcoderWatchSubmissionsCredentials<'closures> {
    pub username_and_password: &'closures mut dyn FnMut() -> eyre::Result<(String, String)>,
}

pub struct AtcoderSubmitCredentials<'closures> {
    pub username_and_password: &'closures mut dyn FnMut() -> eyre::Result<(String, String)>,
}

fn retrieve_sample_test_cases(
    mut sess: impl SessionMut,
    mut username_and_password: impl FnMut() -> eyre::Result<(String, String)>,
    targets: &ProblemsInContest,
) -> eyre::Result<RetrieveTestCasesOutcome> {
    let problems = match targets.clone() {
        ProblemsInContest::Indexes { contest, problems } => {
            let contest = CaseConverted::<LowerCase>::new(contest);
            let html = retrieve_tasks_page(&mut sess, username_and_password, &contest)?;

            let contest_display_name = html
                .extract_title()?
                .trim_start_matches("Tasks - ")
                .to_owned();

            let only = &mut problems
                .as_ref()
                .map(|ps| ps.iter().map(|p| p.to_lowercase()).collect::<BTreeSet<_>>());

            let indexes_and_urls = html
                .extract_task_indexes_and_urls()?
                .into_iter()
                .filter(|(index, _)| {
                    if let Some(only) = only {
                        only.remove(&index.to_lowercase())
                    } else {
                        true
                    }
                })
                .collect::<IndexMap<_, _>>();

            if let Some(only) = only {
                if !only.is_empty() {
                    bail!("No such problems: {:?}", only);
                }
            }

            btreemap!(contest => (contest_display_name, indexes_and_urls))
        }
        ProblemsInContest::Urls { urls } => {
            let mut problems: BTreeMap<_, (_, _, HashSet<_>)> = btreemap!();

            for url in urls {
                let contest = CaseConverted::new(contest_id_from_url(&url)?);

                if let Some((_, _, only)) = problems.get_mut(&contest) {
                    only.insert(url);
                } else {
                    let html =
                        retrieve_tasks_page(&mut sess, &mut username_and_password, &contest)?;
                    let contest_display_name = html
                        .extract_title()?
                        .trim_start_matches("Tasks - ")
                        .to_owned();
                    let indexes_and_urls = html.extract_task_indexes_and_urls()?;
                    problems.insert(
                        contest,
                        (contest_display_name, indexes_and_urls, hashset!(url)),
                    );
                }
            }

            problems
                .into_iter()
                .map(
                    |(contest, (contest_display_name, indexes_and_urls, only))| {
                        let indexes_and_urls = indexes_and_urls
                            .into_iter()
                            .filter(|(_, url)| only.contains(url))
                            .collect();
                        (contest, (contest_display_name, indexes_and_urls))
                    },
                )
                .collect()
        }
    };

    let mut outcome = RetrieveTestCasesOutcome { problems: vec![] };

    for (contest, (contest_display_name, mut indexes_and_urls)) in problems {
        let test_suites = sess
            .get(url!("/contests/{}/tasks_print", contest))
            .colorize_status_code(&[200], (), ..)
            .send()?
            .ensure_status(&[200])?
            .html()?
            .extract_samples();

        if indexes_and_urls.len() > test_suites.len() {
            sess.shell().warn(format!(
                "Found {} task(s) in `tasks`, {} task(s) in `tasks_print`",
                indexes_and_urls.len(),
                test_suites.len(),
            ))?;
        }

        let contest = &RetrieveTestCasesOutcomeProblemContest {
            id: (*contest).to_owned(),
            display_name: contest_display_name,
            url: url!("/contests/{}", contest),
            submissions_url: url!("/contests/{}/submissions/me", contest),
        };

        for result in test_suites {
            match result {
                Ok((index, display_name, test_suite)) => {
                    if let Some(url) = indexes_and_urls.shift_remove(&*index) {
                        let screen_name = url
                            .path_segments()
                            .and_then(Iterator::last)
                            .with_context(|| "Empty URL")?
                            .to_owned();

                        let test_suite = match test_suite {
                            Ok(test_suite) => test_suite,
                            Err(err) => {
                                sess.shell().warn(err)?;

                                TestSuite::Batch(BatchTestSuite {
                                    timelimit: None,
                                    r#match: Match::Lines,
                                    cases: vec![],
                                    extend: vec![],
                                })
                            }
                        };

                        outcome.problems.push(RetrieveTestCasesOutcomeProblem {
                            contest: Some(contest.clone()),
                            url,
                            index,
                            screen_name: Some(screen_name),
                            display_name,
                            test_suite,
                            text_files: indexmap![],
                        });
                    }
                }
                Err(err) => sess.shell().warn(err)?,
            }
        }

        for (index, _) in indexes_and_urls {
            sess.shell()
                .warn(format!("Could not find `{}` in `tasks_print`", index))?;
        }
    }

    Ok(outcome)
}

fn login(
    mut sess: impl SessionMut,
    mut username_and_password: impl FnMut() -> eyre::Result<(String, String)>,
) -> eyre::Result<()> {
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

fn check_logged_in(mut sess: impl SessionMut) -> eyre::Result<bool> {
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
    credentials: impl FnMut() -> eyre::Result<(String, String)>,
    contest: &CaseConverted<LowerCase>,
    explicit: bool,
) -> eyre::Result<ParticipateOutcome> {
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

    if !check_logged_in(&mut sess)? {
        login(&mut sess, credentials)?;
    }

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
    username_and_password: impl FnMut() -> eyre::Result<(String, String)>,
    contest: &CaseConverted<LowerCase>,
) -> eyre::Result<Html> {
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

fn retrieve_submission_summaries(
    mut sess: impl SessionMut,
    contest: &CaseConverted<LowerCase>,
    page: u32,
    username_and_password: impl FnMut() -> eyre::Result<(String, String)>,
) -> eyre::Result<(Vec<SubmissionSummary>, u32)> {
    let res = sess
        .get(submissions_me(contest, page))
        .colorize_status_code(&[200], &[302], ..)
        .send()?
        .ensure_status(&[200, 302])?;

    return if res.status() == 200 {
        res
    } else {
        participate(&mut sess, username_and_password, contest, false)?;
        sess.get(submissions_me(contest, page))
            .colorize_status_code(&[200], (), ..)
            .send()?
            .ensure_status(&[200])?
    }
    .html()?
    .extract_submissions();

    fn submissions_me(contest: &CaseConverted<LowerCase>, page: u32) -> Url {
        let mut url = url!("/contests/{}/submissions/me", contest);
        url.query_pairs_mut().append_pair("page", &page.to_string());
        url
    }
}

fn print_submissions(mut wtr: impl WriteColor, summaries: &[SubmissionSummary]) -> io::Result<()> {
    let task_display_max_width = summaries
        .iter()
        .map(
            |SubmissionSummary {
                 task: SubmissionSummaryTask { display_name, .. },
                 ..
             }| display_name.width(),
        )
        .max()
        .unwrap_or(0);

    let lang_max_width = summaries
        .iter()
        .map(|SubmissionSummary { language, .. }| language.width())
        .max()
        .unwrap_or(0);

    for summary in summaries {
        write!(
            wtr,
            "│ {} │ {} │ {} │ ",
            summary.submission_time,
            align_left(&summary.task.display_name, task_display_max_width),
            align_left(&summary.language, lang_max_width),
        )?;

        match &summary.status {
            Verdict::Wj => {
                wtr.set_color(color_spec!(Bold))?;
                write!(wtr, "WJ")?;
                wtr.reset()?;
            }
            verdict @ Verdict::Judging(..) => {
                wtr.set_color(&verdict.color_spec())?;
                write!(wtr, "{}", verdict)?;
                wtr.reset()?;
            }
            verdict => {
                wtr.set_color(&verdict.color_spec())?;
                write!(wtr, "{}", align_left(&verdict.to_string(), 3))?;
                wtr.reset()?;
                write!(
                    wtr,
                    " │ {:>8} │ {:>10} │",
                    summary.exec_time.as_deref().unwrap_or(""),
                    summary.memory.as_deref().unwrap_or(""),
                )?;
            }
        }

        writeln!(wtr)?;
    }

    wtr.flush()?;
    Ok(())
}

fn watch_submissions(
    mut sess: impl SessionMut,
    contest: &CaseConverted<LowerCase>,
    summaries: &[SubmissionSummary],
) -> eyre::Result<()> {
    let rt = Runtime::new()?;
    let mut handles = vec![];

    let mp = MultiProgress::with_draw_target(sess.shell().progress_draw_target());

    let task_display_max_width = summaries
        .iter()
        .map(
            |SubmissionSummary {
                 task: SubmissionSummaryTask { display_name, .. },
                 ..
             }| display_name.width(),
        )
        .max()
        .unwrap_or(0);

    let lang_max_width = summaries
        .iter()
        .map(|SubmissionSummary { language, .. }| language.width())
        .max()
        .unwrap_or(0);

    for summary in summaries {
        let pb = mp.add(ProgressBar::new(0));

        pb.set_style(
            ProgressStyle::default_bar().template("{prefix}{msg:3.bold}                         │"),
        );

        pb.set_prefix(&format!(
            "│ {} │ {} │ {} │ ",
            summary.submission_time,
            align_left(&summary.task.display_name, task_display_max_width),
            align_left(&summary.language, lang_max_width),
        ));

        if matches!(
            summary.status,
            Verdict::Wj | Verdict::Wr | Verdict::Judging(..)
        ) {
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
                            _ => trap!(Err(eyre!("Could not extract information"))),
                        };

                        tokio::task::block_in_place(|| {
                            if let Some(caps) = JUDGING.captures(verdict) {
                                let position = caps[1].parse().unwrap();
                                let length = caps[2].parse().unwrap();
                                let verdict = &caps[3];

                                pb.set_style(ProgressStyle::default_bar().template(&format!(
                                    "{{prefix}}{{msg:3{style}}} {{pos:>3{style}}}/\
                                     {{len:>3{style}}} {{bar:15{style}}} │",
                                    style = style(verdict),
                                )));

                                pb.set_message(verdict);
                                pb.set_length(length);
                                pb.set_position(position);
                            } else {
                                pb.set_message(verdict);
                            }
                        });

                        tokio::time::sleep(Duration::from_millis(interval)).await;
                    } else {
                        let (verdict, time_and_memory) = match &text[..] {
                            [verdict, time_and_memory] => (verdict, time_and_memory),
                            _ => trap!(Err(eyre!("Could not extract information"))),
                        };

                        let verdict = Verdict::new(verdict);

                        let (exec_time, memory) =
                            match *time_and_memory.split(" ms").collect::<Vec<_>>() {
                                [time, memory] => (format!("{} ms", time), memory),
                                _ => trap!(Err(eyre!("Could not extract information"))),
                            };

                        tokio::task::block_in_place(|| {
                            finish(&pb, &verdict, &exec_time, memory);
                        });
                        break Result::<(), eyre::Error>::Ok(());
                    }
                }
            }));
        } else {
            finish(
                &pb,
                &summary.status,
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

    fn finish(pb: &ProgressBar, verdict: &Verdict, exec_time: &str, memory: &str) {
        pb.set_style(ProgressStyle::default_bar().template(&format!(
            "{{prefix}}{{msg:3{}}} │ {} │ {} │",
            verdict.progress_style(),
            align_right(exec_time, 8),
            align_right(memory, 10),
        )));
        pb.finish_with_message(&verdict.to_string());
    }

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
        matches!(self, ContestStatus::Finished)
    }

    fn raise_if_not_begun(&self) -> eyre::Result<()> {
        if let ContestStatus::NotBegun(contest, time) = self {
            bail!("`{}` will begin at {}", contest, time)
        }
        Ok(())
    }
}

#[derive(Debug, Serialize)]
struct SubmissionSummary {
    submission_time: DateTime<FixedOffset>,
    task: SubmissionSummaryTask,
    user: SubmissionSummaryUser,
    language: String,
    score: String,
    code_size: String,
    status: Verdict,
    exec_time: Option<String>,
    memory: Option<String>,
    detail: Url,
}

impl SubmissionSummary {
    fn id(&self) -> &str {
        self.detail
            .path_segments()
            .and_then(Iterator::last)
            .unwrap_or("")
    }
}

#[derive(Debug, Serialize)]
struct SubmissionSummaryTask {
    display_name: String,
    url: Url,
}

#[derive(Debug, Serialize)]
struct SubmissionSummaryUser {
    name: String,
    url: Url,
}

#[derive(Debug, PartialEq)]
enum Verdict {
    Ac,
    Ce,
    Re,
    Wa,
    Mle,
    Tle,
    Ole,
    Ie,
    Wj,
    Wr,
    Unknown(String),
    Judging(u64, u64, Option<Box<Self>>),
}

impl Verdict {
    fn new(s: &str) -> Self {
        return match s {
            "AC" => Self::Ac,
            "CE" => Self::Ce,
            "RE" => Self::Re,
            "WA" => Self::Wa,
            "MLE" => Self::Mle,
            "TLE" => Self::Tle,
            "OLE" => Self::Ole,
            "IE" => Self::Ie,
            "WJ" => Self::Wj,
            "WR" => Self::Wr,
            s => {
                if let Some(caps) = JUDGING.captures(s) {
                    let numer = caps[1].parse().unwrap();
                    let denom = caps[2].parse().unwrap();
                    let verdict = if caps[3].is_empty() {
                        None
                    } else {
                        Some(Box::new(Self::new(&caps[3])))
                    };
                    Self::Judging(numer, denom, verdict)
                } else {
                    Self::Unknown(s.to_owned())
                }
            }
        };

        static JUDGING: Lazy<Regex> = lazy_regex!(r"\A\s*([0-9]{1,3})/([0-9]{1,3})\s*(\S*)\s*\z");
    }

    fn color_spec(&self) -> ColorSpec {
        return match self {
            Self::Ac => from_fg_and_bold(Some(Color::Green), true),
            Self::Ce | Self::Re | Self::Wa => from_fg_and_bold(Some(Color::Yellow), true),
            Self::Mle | Self::Tle | Self::Ole => from_fg_and_bold(Some(Color::Red), true),
            Self::Ie | Self::Wj | Self::Wr => from_fg_and_bold(None, true),
            Self::Judging(_, _, Some(v)) => v.color_spec(),
            Self::Unknown(_) | Self::Judging(_, _, None) => ColorSpec::new(),
        };

        fn from_fg_and_bold(fg: Option<Color>, bold: bool) -> ColorSpec {
            let mut spec = ColorSpec::new();
            spec.set_fg(fg).set_bold(bold);
            spec
        }
    }

    fn progress_style(&self) -> &'static str {
        match self {
            Self::Ac => ".green.bold",
            Self::Ce | Self::Re | Self::Wa => ".yellow.bold",
            Self::Mle | Self::Tle | Self::Ole => ".red.bold",
            Self::Ie | Self::Wj | Self::Wr => ".bold",
            Self::Judging(_, _, Some(v)) => v.progress_style(),
            Self::Unknown(_) | Self::Judging(_, _, None) => "",
        }
    }
}

impl fmt::Display for Verdict {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Ac => fmt::Display::fmt("AC", fmt),
            Self::Ce => fmt::Display::fmt("CE", fmt),
            Self::Re => fmt::Display::fmt("RE", fmt),
            Self::Wa => fmt::Display::fmt("WA", fmt),
            Self::Mle => fmt::Display::fmt("MLE", fmt),
            Self::Tle => fmt::Display::fmt("TLE", fmt),
            Self::Ole => fmt::Display::fmt("OLE", fmt),
            Self::Ie => fmt::Display::fmt("IE", fmt),
            Self::Wj => fmt::Display::fmt("WJ", fmt),
            Self::Wr => fmt::Display::fmt("WR", fmt),
            Self::Unknown(s) => fmt::Display::fmt(s, fmt),
            Self::Judging(n, d, Some(v)) => write!(fmt, "{}/{} {}", n, d, v),
            Self::Judging(n, d, None) => write!(fmt, "{}/{}", n, d),
        }
    }
}

impl Serialize for Verdict {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.to_string().serialize(serializer)
    }
}

#[ext]
impl Html {
    fn extract_title(&self) -> eyre::Result<&str> {
        self.select(static_selector!(":root > head > title"))
            .flat_map(|r| r.text())
            .exactly_one()
            .ok()
            .with_context(|| "Could not find `<title>`")
    }

    fn extract_csrf_token(&self) -> eyre::Result<String> {
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

    fn extract_contest_duration(&self) -> eyre::Result<(DateTime<Utc>, DateTime<Utc>)> {
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

    fn contains_registration_button(&self) -> eyre::Result<bool> {
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

    fn extract_task_indexes_and_urls(&self) -> eyre::Result<IndexMap<String, Url>> {
        self.select(static_selector!(
            "#main-container > div.row > div.col-sm-12 > div.panel > table.table > tbody > tr",
        ))
        .map(|tr| {
            let a = tr.select(static_selector!("td.text-center > a")).next()?;
            let index = a.text().next()?.to_owned();
            let url = BASE_URL.join(a.value().attr("href")?).ok()?;
            Some((index, url))
        })
        .collect::<Option<IndexMap<_, _>>>()
        .filter(|m| !m.is_empty())
        .with_context(|| "Could not extract task indexes and URLs")
    }

    fn extract_samples(&self) -> Vec<eyre::Result<(String, String, eyre::Result<TestSuite>)>> {
        return self
            .select(static_selector!(
                "#main-container > div.row div[class=\"col-sm-12\"]",
            ))
            .map(|div| {
                let (index, display_name) = {
                    let title_with_index = div
                        .select(static_selector!(":scope > span"))
                        .flat_map(|r| r.text())
                        .next()
                        .with_context(|| "Could not find the title")?;

                    let caps = static_regex!(r"([a-zA-Z0-9]+) - (.+)")
                        .captures(title_with_index)
                        .with_context(|| {
                            format!("Could not parse the title: {:?}", title_with_index)
                        })?;

                    (caps[1].to_owned(), caps[2].to_owned())
                };

                let test_suite = (|| {
                    let timelimit = div
                        .select(static_selector!(":scope > p"))
                        .flat_map(|r| r.text())
                        .flat_map(parse_timelimit)
                        .exactly_one()
                        .map_err(|_| "Could not extract the timelimit")?;

                    // In `tasks_print`, there are multiple `#task-statement`s.
                    let samples = div
                        .select(static_selector!(":scope > div[id=\"task-statement\"]"))
                        .exactly_one()
                        .ok()
                        .and_then(extract_samples)
                        .ok_or("Could not extract the sample cases")?;

                    Ok::<_, &str>(if timelimit == Duration::new(0, 0) {
                        TestSuite::Unsubmittable
                    } else if let Samples::Batch(r#match, samples) = samples {
                        TestSuite::Batch(BatchTestSuite {
                            timelimit: Some(timelimit),
                            r#match,
                            cases: samples
                                .into_iter()
                                .enumerate()
                                .map(|(i, (input, output))| PartialBatchTestCase {
                                    name: Some(format!("sample{}", i + 1)),
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
                    })
                })()
                .map_err(|e| eyre!("{}: {}", index, e));

                Ok((index, display_name, test_suite))
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
            #[allow(clippy::blocks_in_if_conditions)]
            if task_statement
                .select(static_selector!("strong"))
                .flat_map(|r| r.text())
                .any(|s| {
                    s.contains("インタラクティブ")
                        || s.contains("対話式の問題")
                        || s.contains("Interactive")
                })
            {
                return Some(Samples::Interactive);
            }

            let matching = {
                let error = task_statement
                    .select(static_selector!("var"))
                    .flat_map(|r| r.text())
                    .flat_map(parse_floating_error)
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
                for s in [input, output] {
                    if s == " \n" {
                        *s = "".to_string();
                    }

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
                    if s.chars().all(|c| ('０'..='９').contains(&c)) {
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

    fn extract_langs(&self) -> eyre::Result<IndexMap<String, String>> {
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

    fn extract_submissions(&self) -> eyre::Result<(Vec<SubmissionSummary>, u32)> {
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
                let submission_time = {
                    static DATETIME: &str = "%F %T%z";

                    let submission_time = tr
                        .select(static_selector!("td > time"))
                        .next()?
                        .text()
                        .next()?;

                    DateTime::parse_from_str(submission_time, DATETIME).ok()?
                };

                let task = {
                    let a = tr.select(static_selector!("td > a")).next()?;

                    SubmissionSummaryTask {
                        display_name: a.text().next()?.to_owned(),
                        url: BASE_URL.join(a.value().attr("href")?).ok()?,
                    }
                };

                let user = {
                    let td = tr.select(static_selector!("td")).nth(2)?;

                    SubmissionSummaryUser {
                        name: td
                            .text()
                            .filter(|s| !s.chars().all(|c| c.is_whitespace()))
                            .exactly_one()
                            .ok()?
                            .to_owned(),
                        url: BASE_URL
                            .join(
                                td.select(static_selector!("a"))
                                    .next()?
                                    .value()
                                    .attr("href")?,
                            )
                            .ok()?,
                    }
                };

                let language = tr
                    .select(static_selector!("td"))
                    .nth(3)?
                    .text()
                    .next()?
                    .to_owned();

                let score = tr
                    .select(static_selector!("td"))
                    .nth(4)?
                    .text()
                    .next()?
                    .to_owned();

                let code_size = tr
                    .select(static_selector!("td"))
                    .nth(5)?
                    .text()
                    .next()?
                    .to_owned();

                let status = tr
                    .select(static_selector!("td > span"))
                    .next()?
                    .text()
                    .next()?;
                let status = Verdict::new(status);

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

                let detail = tr
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
                    submission_time,
                    task,
                    user,
                    language,
                    score,
                    code_size,
                    status,
                    exec_time,
                    memory,
                    detail,
                })
            }
            Some((submissions, num_pages))
        })()
        .with_context(|| "Could not parse the submissions page")
    }
}
