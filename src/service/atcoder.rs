use crate::errors::{ScrapeError, ScrapeResult, ServiceError, ServiceErrorKind, ServiceResult};
use crate::path::AbsPath;
use crate::service::context::{ParseWithBaseUrl as _, UsernameAndPassword};
use crate::service::download::{self, DownloadProgress};
use crate::service::{
    self, Contest, HasContextMut as _, LoginOutcome, ParticipateOutcome, ParticipateOutcomeKind,
    RetrieveLangsOutcome, RetrieveLangsProps, RetrieveSubmissionsOutcome,
    RetrieveSubmissionsOutcomeBuilder, RetrieveSubmissionsOutcomeBuilderSubmission,
    RetrieveSubmissionsProps, RetrieveTestCasesOutcome, RetrieveTestCasesOutcomeBuilder,
    RetrieveTestCasesOutcomeBuilderProblem, RetrieveTestCasesOutcomeBuilderProblemTextFiles,
    RetrieveTestCasesProps, SubmitOutcome, SubmitOutcomeLanguage, SubmitOutcomeResponse,
    SubmitProps,
};
use crate::terminal::{HasTermProps, Input, WriteExt as _};
use crate::testsuite::{self, BatchSuite, Destinations, InteractiveSuite, TestSuite};
use crate::util;
use crate::util::collections::{NonEmptyIndexMap, NonEmptyIndexSet};
use crate::util::indexmap::IndexSetAsRefStrExt as _;
use crate::util::num::PositiveFinite;
use crate::util::scraper::ElementRefExt as _;
use crate::util::str::CaseConversion;

use chrono::{DateTime, FixedOffset, Local, Utc};
use failure::{Fail as _, ResultExt as _};
use http01::{header, StatusCode, Uri};
use if_chain::if_chain;
use indexmap::{indexmap, indexset, IndexMap, IndexSet};
use itertools::Itertools as _;
use maplit::hashmap;
use once_cell::sync::Lazy;
use regex::Regex;
use scraper::{ElementRef, Html, Selector};
use serde::{Deserialize, Serialize};
use serde_json::json;
use termcolor::WriteColor;
use url::Url;

use std::borrow::Cow;
use std::collections::{BTreeMap, HashSet};
use std::convert::Infallible;
use std::path::Path;
use std::str::FromStr;
use std::time::Duration;
use std::{f64, vec};

pub(super) static BASE_URL: Lazy<Url> = Lazy::new(|| "https://atcoder.jp".parse().unwrap());

pub(super) fn login(
    dropbox_path: Option<&AbsPath>,
    ctx: service::Context<impl Input, impl WriteColor + HasTermProps>,
) -> ServiceResult<LoginOutcome> {
    let mut atcoder = Atcoder::start(ctx)?;
    atcoder.login_if_not(true)?;
    if let Some(dropbox_path) = dropbox_path {
        atcoder.auth_dropbox(dropbox_path, true)?;
    }
    Ok(LoginOutcome {})
}

pub(super) fn participate(
    contest: &str,
    ctx: service::Context<impl Input, impl WriteColor + HasTermProps>,
) -> ServiceResult<ParticipateOutcome> {
    Atcoder::start(ctx)?.register_explicitly(&AtcoderContest::new(contest))
}

pub(super) fn retrieve_testcases(
    props: RetrieveTestCasesProps<String>,
    ctx: service::Context<impl Input, impl WriteColor + HasTermProps>,
) -> ServiceResult<RetrieveTestCasesOutcome> {
    let props = props
        .convert_problems(CaseConversion::Upper)
        .parse_contest()
        .unwrap();
    Atcoder::start(ctx)?.retrieve_testcases(&props)
}

pub(super) fn retrieve_langs(
    props: RetrieveLangsProps<String>,
    ctx: service::Context<impl Input, impl WriteColor + HasTermProps>,
) -> ServiceResult<RetrieveLangsOutcome> {
    let props = props
        .convert_problem(CaseConversion::Upper)
        .parse_contest()
        .unwrap();
    Atcoder::start(ctx)?.retrieve_langs(props)
}

pub(super) fn retrieve_submissions(
    props: RetrieveSubmissionsProps<String>,
    ctx: service::Context<impl Input, impl WriteColor + HasTermProps>,
) -> ServiceResult<RetrieveSubmissionsOutcome> {
    let props = props
        .convert_problems(CaseConversion::Upper)
        .parse_contest()
        .unwrap();
    Atcoder::start(ctx)?.retrieve_submissions(&props)
}

pub(super) fn submit(
    props: SubmitProps<String>,
    ctx: service::Context<impl Input, impl WriteColor + HasTermProps>,
) -> ServiceResult<SubmitOutcome> {
    let props = props
        .convert_problem(CaseConversion::Upper)
        .parse_contest()
        .unwrap();
    Atcoder::start(ctx)?.submit(props)
}

#[derive(Debug)]
struct Atcoder<I: Input, E: WriteColor + HasTermProps>(service::Context<I, E>);

impl<I: Input, E: WriteColor + HasTermProps> service::HasContextMut for Atcoder<I, E> {
    type Stdin = I;
    type Stderr = E;

    fn context(&self) -> &service::Context<I, E> {
        &self.0
    }

    fn context_mut(&mut self) -> &mut service::Context<I, E> {
        &mut self.0
    }
}

impl<I: Input, E: WriteColor + HasTermProps> DownloadProgress for Atcoder<I, E> {}

impl<I: Input, E: WriteColor + HasTermProps> Atcoder<I, E> {
    fn start(mut ctx: service::Context<I, E>) -> ServiceResult<Self> {
        ctx.get_robots_txt()?;
        Ok(Self(ctx))
    }

    fn login_if_not(&mut self, explicit: bool) -> ServiceResult<()> {
        if self.has_cookie() {
            let status = self
                .get("/settings")
                .acceptable(&[200, 302])
                .warn(&[200])
                .retry_status()?;
            if status == StatusCode::OK {
                if explicit {
                    writeln!(self.stderr(), "Already logged in.")?;
                    self.stderr().flush()?;
                }
                return Ok(());
            }
        }

        self.retry_login::<UsernameAndPassword, _>(
            ("Username: ", "Password: "),
            "Successfully logged in.",
            "Failed to login. Try again.",
            |this, (username, password)| {
                let payload = hashmap!(
                    "csrf_token" => this.get("/login").retry_recv_html()?.extract_csrf_token()?,
                    "username" => username,
                    "password" => password,
                );
                this.post("/login").form(&payload).send()?;

                if this
                    .get("/settings")
                    .acceptable(&[200, 302])
                    .warn(&[302])
                    .retry_status()?
                    == 200
                {
                    Ok(true)
                } else {
                    this.clear_cookies()?;
                    Ok(false)
                }
            },
        )
    }

    fn auth_dropbox(&mut self, auth_path: &AbsPath, explicit: bool) -> ServiceResult<String> {
        static CLIENT_ID: &str = "6h5mn3yn8o3qbk3";
        static CLIENT_SECRET: &str = "g2p59fafm3d0lnz";

        #[derive(Serialize, Deserialize)]
        struct AuthToken {
            access_token: String,

            // not used for now:
            #[serde(skip_serializing_if = "Option::is_none")]
            token_type: Option<TokenType>,
            #[serde(skip_serializing_if = "Option::is_none")]
            account_id: Option<String>,
        }

        #[derive(Serialize, Deserialize)]
        #[serde(rename_all = "snake_case")]
        enum TokenType {
            Bearer,
        }

        if auth_path.exists() {
            let auth = crate::fs::read_json::<AuthToken>(auth_path)?;
            let status = self
                .post("https://api.dropboxapi.com/2/users/get_current_account")
                .acceptable(&[200, 401])
                .warn(&[401])
                .bearer_auth(&auth.access_token)
                .status()?;
            if status == 200 {
                if explicit {
                    self.stderr().write_str("Already authorized.\n")?;
                    self.stderr().flush()?;
                }
                return Ok(auth.access_token);
            }
            self.stderr().write_str("Invalid `accesss_token`.\n")?;
            self.stderr().flush()?;
        }

        self.open_in_browser(&format!(
            "https://dropbox.com/oauth2/authorize?response_type=code&client_id={}",
            CLIENT_ID,
        ))?;
        let code = self.prompt_password_stderr("Authorization code: ")?;
        let auth = self
            .post("https://api.dropboxapi.com/oauth2/token")
            .acceptable(&[200])
            .basic_auth(CLIENT_ID, Some(CLIENT_SECRET))
            .form(&hashmap!("code" => code.as_str(), "grant_type" => "authorization_code"))
            .recv_json::<AuthToken>()?;

        crate::fs::write_json_pretty(auth_path, &auth)?;
        writeln!(self.stderr(), "Wrote {}", auth_path.display())?;
        self.stderr().flush()?;

        Ok(auth.access_token)
    }

    fn register_explicitly(
        &mut self,
        contest: &AtcoderContest,
    ) -> ServiceResult<ParticipateOutcome> {
        self.register_if_active_or_explicit(contest, true)
    }

    fn fetch_tasks_page(&mut self, contest: &AtcoderContest) -> ServiceResult<Html> {
        let res = self
            .get(&contest.url_tasks())
            .acceptable(&[200, 302, 404])
            .warn(&[302, 404])
            .retry_send()?;

        if res.status() == 200 {
            self.retry_recv_html(res)
        } else {
            self.register_if_active_or_explicit(contest, false)?;
            self.get(&contest.url_tasks()).retry_recv_html()
        }
    }

    fn register_if_active_or_explicit(
        &mut self,
        contest: &AtcoderContest,
        explicit: bool,
    ) -> ServiceResult<ParticipateOutcome> {
        let res = self
            .get(&contest.url_top())
            .acceptable(&[200, 302])
            .warn(&[302])
            .retry_send()?;

        let html = if res.status() == 302 {
            Err(ServiceErrorKind::ContestNotFound(contest.slug().into()).into())
        } else {
            self.retry_recv_html(res)
        }?;

        let status = ContestStatus::now(html.extract_contest_duration()?, &contest.slug());

        if !explicit {
            status.raise_if_not_begun()?;
        }

        if status.is_finished() {
            Ok(ParticipateOutcomeKind::ContestIsFinished.into())
        } else {
            self.login_if_not(false)?;
            let html = self.get(&contest.url_top()).retry_recv_html()?;
            if html.contains_registration_button()? {
                let csrf_token = html.extract_csrf_token()?;
                let url = contest.url_register();
                let payload = hashmap!("csrf_token" => csrf_token);
                self.post(&url).form(&payload).send()?;
                Ok(ParticipateOutcomeKind::Success.into())
            } else {
                Ok(ParticipateOutcomeKind::AlreadyParticipated.into())
            }
        }
    }

    fn retrieve_testcases(
        &mut self,
        props: &RetrieveTestCasesProps<AtcoderContest>,
    ) -> ServiceResult<RetrieveTestCasesOutcome> {
        let RetrieveTestCasesProps {
            contest,
            problems,
            destinations,
            open_in_browser,
            attempt_full,
            save_files,
            dropbox_path,
        } = props;
        let problems = problems.as_ref();

        let html = self.fetch_tasks_page(contest)?;
        let contest_display = html.extract_contest_display_name()?;
        let slugs_and_uris = html.extract_task_slugs_and_uris()?;
        let suites = self
            .get(&contest.url_tasks_print())
            .retry_recv_html()?
            .extract_as_suites(&contest.slug())?;

        if slugs_and_uris.len() != suites.len() {
            return Err(ScrapeError::new().into());
        }

        let mut outcome =
            RetrieveTestCasesOutcomeBuilder::new(&contest.slug(), &contest_display, *save_files);
        outcome.push_submissions_url(contest.url_submissions_me(1));

        for ((slug, uri), (display_name, suite)) in slugs_and_uris.into_iter().zip_eq(suites) {
            if problems.map_or(true, |ps| ps.iter().any(|p| *p == slug)) {
                let path = destinations.expand(&slug)?;
                let url = uri.parse_with_base_url(Some(&BASE_URL))?;
                let screen_name = url
                    .path_segments()
                    .and_then(Iterator::last)
                    .unwrap_or_default()
                    .to_owned();

                outcome.push_problem(RetrieveTestCasesOutcomeBuilderProblem {
                    url,
                    slug,
                    display_name,
                    screen_name: Some(screen_name),
                    test_suite_contents: suite,
                    test_suite_location: path,
                    text_files: indexmap!(),
                });
            }
        }

        if *attempt_full {
            let dropbox_path = dropbox_path.as_ref().ok_or_else(|| {
                failure::err_msg("`session.dropbox` is `false` in the config file")
                    .context(ServiceErrorKind::UnableToDownloadFull)
            })?;
            self.retrieve_from_dropbox(
                dropbox_path,
                &contest.slug(),
                destinations,
                outcome.problems_mut(),
            )?;
        }

        if let Some(problems) = problems {
            self.warn_not_found(problems, &outcome.problem_slugs())?;
        }

        outcome.finish(*open_in_browser, self.stderr())
    }

    fn retrieve_from_dropbox(
        &mut self,
        auth_path: &AbsPath,
        contest_slug: &str,
        destinations: &Destinations,
        problems: &mut [RetrieveTestCasesOutcomeBuilderProblem],
    ) -> ServiceResult<()> {
        static URL: &str =
            "https://www.dropbox.com/sh/arnpe0ef5wds8cv/AAAk_SECQ2Nc6SVGii3rHX6Fa?dl=0";

        #[derive(Deserialize)]
        struct ListFolder {
            entries: Vec<ListFolderEntry>,
        }

        #[derive(Deserialize)]
        struct ListFolderEntry {
            name: String,
        }

        fn confirm_folders_exist<'a>(
            this: &mut Atcoder<impl Input, impl WriteColor + HasTermProps>,
            token: &str,
            contest_slug: &str,
            problems: impl Iterator<Item = &'a str>,
        ) -> ServiceResult<()> {
            #[derive(Deserialize)]
            #[serde(untagged)]
            enum ListFolderResult {
                Ok(ListFolder),
                Err(serde_json::Value),
            }

            let path = format!("/{}", contest_slug);
            let result = this
                .post("https://api.dropboxapi.com/2/files/list_folder")
                .bearer_auth(token)
                .acceptable(&[200, 409])
                .warn(&[409])
                .json(&json!({
                    "shared_link": { "url": URL },
                    "path": &path,
                }))
                .recv_json::<ListFolderResult>()?;
            match result {
                ListFolderResult::Ok(ok) => {
                    let names = ok
                        .entries
                        .iter()
                        .map(|e| e.name.as_str())
                        .collect::<HashSet<_>>();
                    let not_found = problems.filter(|p| !names.contains(p)).collect::<Vec<_>>();
                    if not_found.is_empty() {
                        Ok(())
                    } else {
                        Err(ServiceError::from(
                            failure::err_msg(format!("Some problems not found: {:?}", not_found))
                                .context(ServiceErrorKind::UnableToDownloadFull),
                        ))
                    }
                }
                ListFolderResult::Err(err) => Err(ServiceError::from(
                    failure::err_msg(serde_json::to_string_pretty(&err).unwrap())
                        .context(failure::err_msg(format!("Failed to read {:?}", path)))
                        .context(ServiceErrorKind::UnableToDownloadFull),
                )),
            }
        }

        fn list_folder(
            this: &mut Atcoder<impl Input, impl WriteColor + HasTermProps>,
            token: &str,
            path: &str,
        ) -> ServiceResult<ListFolder> {
            this.post("https://api.dropboxapi.com/2/files/list_folder")
                .bearer_auth(token)
                .acceptable(&[200])
                .json(&json!({ "shared_link": { "url": URL }, "path": path }))
                .recv_json()
        }

        fn retrieve_files(
            this: &mut Atcoder<impl Input, impl WriteColor + HasTermProps>,
            token: &str,
            prefix: &str,
            entries: &[ListFolderEntry],
        ) -> ServiceResult<IndexMap<String, String>> {
            static ENDPOINT: &str = "https://content.dropboxapi.com/2/sharing/get_shared_link_file";
            let filenames = entries.iter().map(|e| &e.name).collect::<Vec<_>>();
            let client = this.client();
            let reqs = filenames
                .iter()
                .map(|filename| {
                    let req = client
                        .post(ENDPOINT)
                        .header(header::AUTHORIZATION, format!("Bearer {}", token))
                        .header(
                            "Dropbox-API-Arg",
                            json!({ "url": URL, "path": format!("{}/{}", prefix, filename) })
                                .to_string(),
                        );
                    let long_name = format!("{} ({})", ENDPOINT, filename);
                    (download::Name::new(filename.as_str(), long_name), req)
                })
                .collect();
            let files = this.download_progress(reqs)?;
            files
                .into_iter()
                .zip_eq(filenames)
                .map(|(contents, filename)| {
                    let stem = Path::new(&filename)
                        .file_stem()
                        .unwrap_or_default()
                        .to_str()
                        .unwrap()
                        .to_owned();
                    let contents = String::from_utf8(contents)
                        .with_context(|_| ServiceErrorKind::UnableToDownloadFull)?;
                    Ok((stem, contents))
                })
                .collect()
        }

        let token = self.auth_dropbox(auth_path, false)?;

        confirm_folders_exist(
            self,
            &token,
            &contest_slug,
            problems.iter().map(|p| p.slug.as_ref()),
        )?;

        for problem in problems {
            if let TestSuite::Batch(suite) = &mut problem.test_suite_contents {
                suite.clear_cases();
            }

            let (in_contents, mut out_contents) = {
                let in_dir = format!("/{}/{}/in", contest_slug, problem.slug);
                let entries = list_folder(self, &token, &in_dir)?.entries;
                let in_contents = retrieve_files(self, &token, &in_dir, &entries)?;
                let out_dir = format!("/{}/{}/out", contest_slug, problem.slug);
                let entries = list_folder(self, &token, &out_dir)?.entries;
                let out_contents = retrieve_files(self, &token, &out_dir, &entries)?;
                (in_contents, out_contents)
            };

            for (name, in_contents) in in_contents {
                if let Some(out_contents) = out_contents.shift_remove(&name) {
                    let dir = destinations.text_file_dir(&problem.slug)?;
                    let in_location = dir.join("in").join(&name).with_extension("txt");
                    let out_location = dir.join("out").join(&name).with_extension("txt");

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
        Ok(())
    }

    fn retrieve_langs(
        &mut self,
        props: RetrieveLangsProps<AtcoderContest>,
    ) -> ServiceResult<RetrieveLangsOutcome> {
        let RetrieveLangsProps { contest, problem } = props;
        self.login_if_not(false)?;
        let url = match problem {
            None => contest.url_submit(),
            Some(problem) => self
                .fetch_tasks_page(&contest)?
                .extract_task_slugs_and_uris()?
                .into_element(&problem)
                .ok_or_else(|| ServiceErrorKind::NoSuchProblem(problem.clone()))?
                .parse_with_base_url(Some(&BASE_URL))?,
        };
        let langs = self.get(&url).retry_recv_html()?.extract_langs()?;
        Ok(RetrieveLangsOutcome::new(url, langs))
    }

    fn retrieve_submissions(
        &mut self,
        props: &RetrieveSubmissionsProps<AtcoderContest>,
    ) -> ServiceResult<RetrieveSubmissionsOutcome> {
        let RetrieveSubmissionsProps {
            contest,
            problems,
            src_paths,
            fetch_all,
            save_files,
        } = props;

        let res = self
            .get(&contest.url_submissions_me(1))
            .acceptable(&[200, 302])
            .warn(&[302])
            .retry_send()?;

        let first_page = if res.status() == 200 {
            self.retry_recv_html(res)?
        } else {
            self.register_if_active_or_explicit(contest, false)?;
            self.get(&contest.url_submissions_me(1)).retry_recv_html()?
        };

        let submissions = {
            let (mut submissions, num_pages) = first_page.extract_submissions()?;
            for i in 2..=num_pages {
                let page = self.get(&contest.url_submissions_me(i)).retry_recv_html()?;
                submissions.extend(page.extract_submissions()?.0);
            }
            submissions
        };

        let mut outcome = RetrieveSubmissionsOutcomeBuilder::new();

        #[derive(Default)]
        struct Found {
            inner: IndexMap<String, IndexSet<String>>,
        }

        impl Found {
            fn contains(&self, submission: &Submission) -> bool {
                self.inner
                    .get(submission.task_slug.as_str())
                    .map_or(false, |m| m.contains(submission.lang.as_str()))
            }

            fn insert(&mut self, submission: &Submission) {
                self.inner
                    .entry(submission.task_slug.clone())
                    .or_insert_with(IndexSet::new)
                    .insert(submission.lang.clone());
            }
        }

        let (mut found, mut ignored) = (Found::default(), indexset!());
        for submission in submissions {
            let mut retrieve_code = || -> ServiceResult<_> {
                self.get(&submission.url)
                    .retry_recv_html()?
                    .extract_submitted_code()
                    .map_err(Into::into)
            };
            if problems
                .as_ref()
                .map_or(true, |ps| ps.contains(&submission.task_slug))
            {
                let (code, path_to_save) = match src_paths.get(submission.lang.as_str()) {
                    Some(path) if *fetch_all => {
                        let code = retrieve_code()?;
                        if found.contains(&submission) {
                            (Some(code), None)
                        } else {
                            found.insert(&submission);
                            (Some(code), Some(path))
                        }
                    }
                    None if *fetch_all => (Some(retrieve_code()?), None),
                    Some(path) => {
                        if found.contains(&submission) {
                            (None, None)
                        } else {
                            found.insert(&submission);
                            let code = retrieve_code()?;
                            (Some(code), Some(path))
                        }
                    }
                    None => {
                        ignored.insert(submission.lang.clone());
                        (None, None)
                    }
                };

                let path_to_save = if *save_files {
                    path_to_save
                        .map(|p| p.expand(Some(&submission.task_slug)))
                        .transpose()?
                } else {
                    None
                };

                outcome.submissions.insert(
                    submission.url,
                    RetrieveSubmissionsOutcomeBuilderSubmission {
                        problem_url: submission.task_url,
                        problem_slug: submission.task_slug,
                        problem_display_name: submission.task_display,
                        problem_screen_name: Some(submission.task_screen),
                        language: submission.lang,
                        date_time: submission.datetime,
                        verdict_is_ok: submission.verdict_is_ac,
                        verdict_string: submission.verdict,
                        location: path_to_save,
                        code,
                    },
                );
            }
        }

        if let Some(ignored) = NonEmptyIndexSet::try_new(ignored) {
            self.stderr().set_color(color!(fg(Yellow), intense))?;
            writeln!(self.stderr(), "Ignored: {}", ignored.format_as_str_list())?;
            self.stderr().reset()?;
        }

        if let Some(problems) = problems {
            self.warn_not_found(problems, &outcome.problem_slugs())?;
        }

        outcome.finish()
    }

    fn submit(&mut self, props: SubmitProps<AtcoderContest>) -> ServiceResult<SubmitOutcome> {
        let SubmitProps {
            contest,
            problem,
            lang_name,
            src_path,
            open_in_browser,
            skip_checking_if_accepted,
        } = props;
        let tasks_page = self.fetch_tasks_page(&contest)?;
        let checks_if_accepted =
            !skip_checking_if_accepted && contest != AtcoderContest::Practice && {
                let status =
                    ContestStatus::now(tasks_page.extract_contest_duration()?, &contest.slug());
                status.raise_if_not_begun()?;
                status.is_active()
            };
        let uri = tasks_page
            .extract_task_slugs_and_uris()?
            .into_element(&problem)
            .ok_or_else(|| ServiceErrorKind::NoSuchProblem(problem.clone()))?;
        let task_screen = {
            lazy_regex!(r"\A/contests/[a-z0-9_\-]+/tasks/([a-z0-9_]+)/?\z$")
                .captures(&uri.to_string())
                .map(|cs| cs[1].to_owned())
                .ok_or_else(ScrapeError::new)?
        };
        if checks_if_accepted {
            let (submissions, num_pages) = self
                .get(&contest.url_submissions_me(1))
                .retry_recv_html()?
                .extract_submissions()?;
            if submissions
                .iter()
                .any(|s| s.task_screen == task_screen && s.verdict_is_ac)
            {
                return Err(ServiceErrorKind::AlreadyAccepted.into());
            }
            for i in 2..=num_pages {
                if self
                    .get(&contest.url_submissions_me(i))
                    .retry_recv_html()?
                    .extract_submissions()?
                    .0
                    .iter()
                    .any(|s| s.task_screen == task_screen && s.verdict_is_ac)
                {
                    return Err(ServiceErrorKind::AlreadyAccepted.into());
                }
            }
        }

        let code = crate::fs::read_to_string(&src_path)?;
        let html = self.get(uri).retry_recv_html()?;
        let lang_id = html
            .extract_langs()?
            .get(&lang_name)
            .ok_or_else(|| ServiceErrorKind::NoSuchLang(lang_name.clone()))?
            .clone();
        let csrf_token = html.extract_csrf_token()?;
        let url = contest.url_submit();
        let payload = hashmap!(
            "data.TaskScreenName" => task_screen.as_str(),
            "data.LanguageId" => &lang_id,
            "sourceCode" => &code,
            "csrf_token" => &csrf_token,
        );

        let (rejected, status, location);
        match self.post(&url).form(&payload).send() {
            Ok(res) => {
                status = res.status();
                location = res.location_uri()?;
                rejected = location.as_ref().map_or(true, |loc| {
                    let path = loc.path();
                    !(path.starts_with("/contests/") && path.ends_with("/submissions/me"))
                });
            }
            Err(err) => if_chain! {
                if let ServiceError::Context(ctx) = &err;
                if let ServiceErrorKind::UnexpectedStatusCode(_, st, _) = ctx.get_context();
                then {
                    rejected = true;
                    status = *st;
                    location = None;
                } else {
                    return Err(err);
                }
            },
        }

        if !rejected && open_in_browser {
            self.open_in_browser(&contest.url_submissions_me(1))?;
        }

        Ok(SubmitOutcome {
            rejected,
            response: SubmitOutcomeResponse { status, location },
            language: SubmitOutcomeLanguage {
                name: lang_name,
                id: lang_id,
            },
            file: src_path,
            code,
        })
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
enum AtcoderContest {
    Practice,
    Screen(String),
}

impl AtcoderContest {
    fn new(s: &str) -> Self {
        if s.eq_ignore_ascii_case("practice") {
            AtcoderContest::Practice
        } else {
            AtcoderContest::Screen(s.to_owned())
        }
    }

    fn url_top(&self) -> Url {
        let mut ret = BASE_URL.clone();
        ret.set_path(&format!("/contests/{}", self.slug()));
        ret
    }

    fn url_tasks(&self) -> Url {
        let mut ret = BASE_URL.clone();
        ret.set_path(&format!("/contests/{}/tasks", self.slug()));
        ret
    }

    fn url_tasks_print(&self) -> Url {
        let mut ret = BASE_URL.clone();
        ret.set_path(&format!("/contests/{}/tasks_print", self.slug()));
        ret
    }

    fn url_register(&self) -> Url {
        let mut ret = BASE_URL.clone();
        ret.set_path(&format!("/contests/{}/register", self.slug()));
        ret
    }

    fn url_submit(&self) -> Url {
        let mut ret = BASE_URL.clone();
        ret.set_path(&format!("/contests/{}/submit", self.slug()));
        ret
    }

    fn url_submissions_me(&self, page: u32) -> Url {
        let mut ret = BASE_URL.clone();
        ret.set_path(&format!("/contests/{}/submissions/me", self.slug()));
        ret.query_pairs_mut().append_pair("page", &page.to_string());
        ret
    }
}

impl FromStr for AtcoderContest {
    type Err = Infallible;

    fn from_str(s: &str) -> std::result::Result<Self, Infallible> {
        Ok(Self::new(s))
    }
}

impl Contest for AtcoderContest {
    fn slug(&self) -> Cow<str> {
        match self {
            AtcoderContest::Practice => "practice".into(),
            AtcoderContest::Screen(s) => s.into(),
        }
    }
}

#[derive(Debug)]
enum ContestStatus {
    Finished,
    Active,
    NotBegun(String, DateTime<Local>),
}

impl ContestStatus {
    fn now(dur: (DateTime<Utc>, DateTime<Utc>), contest_slug: &str) -> Self {
        let (start, end) = dur;
        let now = Utc::now();
        if now < start {
            ContestStatus::NotBegun(contest_slug.to_owned(), start.with_timezone(&Local))
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

    fn is_active(&self) -> bool {
        match self {
            ContestStatus::Active => true,
            _ => false,
        }
    }

    fn raise_if_not_begun(&self) -> ServiceResult<()> {
        if let ContestStatus::NotBegun(s, t) = self {
            Err(ServiceErrorKind::ContestNotBegun(s.clone(), *t).into())
        } else {
            Ok(())
        }
    }
}

#[derive(Debug)]
struct Submission {
    url: Url,
    task_url: Url,
    task_slug: String,
    task_display: String,
    task_screen: String,
    datetime: DateTime<FixedOffset>,
    lang: String,
    verdict_is_ac: bool,
    verdict: String,
}

trait Extract {
    fn extract_csrf_token(&self) -> ScrapeResult<String>;
    fn extract_contest_display_name(&self) -> ScrapeResult<String>;
    fn contains_registration_button(&self) -> ScrapeResult<bool>;
    fn extract_task_slugs_and_uris(&self) -> ScrapeResult<NonEmptyIndexMap<String, Uri>>;
    fn extract_as_suites(
        &self,
        contest_slug: &str,
    ) -> ScrapeResult<NonEmptyIndexMap<String, TestSuite>>;
    fn extract_contest_duration(&self) -> ScrapeResult<(DateTime<Utc>, DateTime<Utc>)>;
    fn extract_submissions(&self) -> ScrapeResult<(Vec<Submission>, u32)>;
    fn extract_submitted_code(&self) -> ScrapeResult<String>;
    fn extract_lang_id_by_name(&self, name: &str) -> ScrapeResult<String>;
    fn extract_langs(&self) -> ScrapeResult<NonEmptyIndexMap<String, String>>;
}

impl Extract for Html {
    fn extract_csrf_token(&self) -> ScrapeResult<String> {
        let extract_csrf_token = || {
            let token = self
                .select(selector!("[name=\"csrf_token\"]"))
                .next()?
                .value()
                .attr("value")?
                .to_owned();
            guard!(!token.is_empty());
            Some(token)
        };
        extract_csrf_token().ok_or_else(ScrapeError::new)
    }

    fn extract_contest_display_name(&self) -> ScrapeResult<String> {
        self.select(selector!("#navbar-collapse"))
            .flat_map(|r| r.select(selector!("a.contest-title")))
            .flat_map(|r| r.text().map(ToOwned::to_owned))
            .next()
            .ok_or_else(ScrapeError::new)
    }

    fn contains_registration_button(&self) -> ScrapeResult<bool> {
        let insert_participant_box = self
            .select(selector!("#main-container .insert-participant-box"))
            .next()
            .ok_or_else(ScrapeError::new)?;
        Ok(insert_participant_box
            .select(selector!("form"))
            .any(|r| r.value().attr("method") == Some("POST")))
    }

    fn extract_task_slugs_and_uris(&self) -> ScrapeResult<NonEmptyIndexMap<String, Uri>> {
        self.select(selector!(
            "#main-container > div.row > div.col-sm-12 > div.panel > table.table > tbody > tr",
        ))
        .map(|tr| {
            let a = tr.select(selector!("td.text-center > a")).next()?;
            let slug = a.text().next()?.to_owned();
            let uri = a.value().attr("href")?.parse().ok()?;
            Some((slug, uri))
        })
        .collect::<Option<IndexMap<_, _>>>()
        .and_then(NonEmptyIndexMap::try_new)
        .ok_or_else(ScrapeError::new)
    }

    fn extract_as_suites(
        &self,
        contest_slug: &str,
    ) -> ScrapeResult<NonEmptyIndexMap<String, TestSuite>> {
        enum Samples {
            Batch(Vec<(String, String)>, testsuite::Match),
            Interactive,
        }

        fn parse_timelimit(text: &str) -> Option<Duration> {
            let caps =
                lazy_regex!(r"\A\D*([0-9]{1,9})(\.[0-9]{1,3})?\s*(m)?sec.*\z").captures(text)?;
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

        fn extract_samples(task_statement: ElementRef) -> Option<Samples> {
            // TODO:
            // - https://atcoder.jp/contests/arc019/tasks/arc019_4 (interactive)
            // - https://atcoder.jp/contests/arc021/tasks/arc021_4 (interactive)
            // - https://atcoder.jp/contests/cf17-final-open/tasks/cf17_final_f
            // - https://atcoder.jp/contests/jag2016-domestic/tasks
            // - https://atcoder.jp/contests/chokudai001/tasks/chokudai_001_a

            static IN_JA: &Lazy<Regex> = lazy_regex!(r"\A[\s\n]*入力例\s*(\d{1,2})[.\n]*\z");
            static OUT_JA: &Lazy<Regex> = lazy_regex!(r"\A[\s\n]*出力例\s*(\d{1,2})[.\n]*\z");
            static IN_EN: &Lazy<Regex> = lazy_regex!(r"\ASample Input\s?([0-9]{1,2}).*\z");
            static OUT_EN: &Lazy<Regex> = lazy_regex!(r"\ASample Output\s?([0-9]{1,2}).*\z");

            // Current style (Japanese)
            static P1_HEAD: &Lazy<Selector> =
                selector!("span.lang > span.lang-ja > div.part > section > h3");
            static P1_CONTENT: &Lazy<Selector> =
                selector!("span.lang > span.lang-ja > div.part > section > pre");
            // Current style (English)
            static P2_HEAD: &Lazy<Selector> =
                selector!("span.lang > span.lang-en > div.part > section > h3");
            static P2_CONTENT: &Lazy<Selector> =
                selector!("span.lang>span.lang-en>div.part>section>pre");
            // ARC019..ARC057 \ {ARC019/C, ARC046/D, ARC050, ARC052/{A, C}, ARC053, ARC055},
            // ABC007..ABC040 \ {ABC036}, ATC001, ATC002
            static P3_HEAD: &Lazy<Selector> = selector!("div.part > section > h3");
            static P3_CONTENT: &Lazy<Selector> = selector!("div.part > section > pre");
            // ARC002..ARC018, ARC019/C, ABC001..ABC006
            static P4_HEAD: &Lazy<Selector> = selector!("div.part > h3,pre");
            static P4_CONTENT: &Lazy<Selector> = selector!("div.part > section > pre");
            // ARC001, dwacon2018-final/{A, B}
            static P5_HEAD: &Lazy<Selector> = selector!("h3,pre");
            static P5_CONTENT: &Lazy<Selector> = selector!("section > pre");
            // ARC046/D, ARC050, ARC052/{A, C}, ARC053, ARC055, ABC036, ABC041
            static P6_HEAD: &Lazy<Selector> = selector!("section > h3");
            static P6_CONTENT: &Lazy<Selector> = selector!("section > pre");
            // ABC034
            static P7_HEAD: &Lazy<Selector> = selector!("span.lang > span.lang-ja > section > h3");
            static P7_CONTENT: &Lazy<Selector> =
                selector!("span.lang > span.lang-ja > section > pre");
            // practice contest (Japanese)
            static P8_HEAD: &Lazy<Selector> = selector!("span.lang > span.lang-ja > div.part > h3");
            static P8_CONTENT: &Lazy<Selector> =
                selector!("span.lang > span.lang-ja > div.part > section > pre");

            let stmt = task_statement;
            try_extract_samples(stmt, P1_HEAD, P1_CONTENT, IN_JA, OUT_JA)
                .or_else(|| try_extract_samples(stmt, P2_HEAD, P2_CONTENT, IN_EN, OUT_EN))
                .or_else(|| try_extract_samples(stmt, P3_HEAD, P3_CONTENT, IN_JA, OUT_JA))
                .or_else(|| try_extract_samples(stmt, P4_HEAD, P4_CONTENT, IN_JA, OUT_JA))
                .or_else(|| try_extract_samples(stmt, P5_HEAD, P5_CONTENT, IN_JA, OUT_JA))
                .or_else(|| try_extract_samples(stmt, P6_HEAD, P6_CONTENT, IN_JA, OUT_JA))
                .or_else(|| try_extract_samples(stmt, P7_HEAD, P7_CONTENT, IN_JA, OUT_JA))
                .or_else(|| try_extract_samples(stmt, P8_HEAD, P8_CONTENT, IN_JA, OUT_JA))
        }

        fn try_extract_samples(
            task_statement: ElementRef,
            selector_for_header: &'static Selector,
            selector_for_content: &'static Selector,
            re_input: &'static Regex,
            re_output: &'static Regex,
        ) -> Option<Samples> {
            if task_statement
                .select(selector!("strong"))
                .flat_map(|r| r.text())
                .any(|t| t.contains("インタラクティブ") || t.contains("Interactive"))
            {
                return Some(Samples::Interactive);
            }

            let matching = {
                let error = task_statement
                    .select(selector!("var"))
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
                    (Some(error), true, true) => testsuite::Match::Float {
                        relative_error: Some(error),
                        absolute_error: Some(error),
                    },
                    (Some(error), true, false) => testsuite::Match::Float {
                        relative_error: Some(error),
                        absolute_error: None,
                    },
                    (Some(error), false, true) => testsuite::Match::Float {
                        relative_error: None,
                        absolute_error: Some(error),
                    },
                    _ => testsuite::Match::Exact,
                }
            };
            let mut inputs = BTreeMap::<usize, _>::new();
            let mut outputs = BTreeMap::<usize, _>::new();
            let mut next = None;
            let selector = util::scraper::or(selector_for_header, selector_for_content);
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
                    guard!(is_valid_text(s));
                }
            }

            guard!(!samples.is_empty());
            Some(Samples::Batch(samples, matching))
        }

        fn parse_floating_error(s: &str) -> Option<PositiveFinite<f64>> {
            let caps = lazy_regex!(r"\A10\^\{(-?[0-9]{1,2})\}\z").captures(s)?;
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

        let outcome = self
            .select(selector!(
                "#main-container > div.row > div[class=\"col-sm-12\"]",
            ))
            .map(|div| {
                let display_name = div
                    .select(selector!("span"))
                    .flat_map(|r| r.text())
                    .next()?
                    .to_owned();
                let timelimit = div
                    .select(selector!("p"))
                    .flat_map(|r| r.text())
                    .flat_map(parse_timelimit)
                    .next()?;
                // In `tasks_print`, there are multiple `#task-statement`s.
                let samples = div
                    .select(selector!("div[id=\"task-statement\"]"))
                    .next()
                    .and_then(extract_samples);
                Some((display_name, timelimit, samples))
            })
            .collect::<Option<Vec<_>>>()
            .ok_or_else(ScrapeError::new)?;

        if outcome.is_empty() {
            Err(ScrapeError::new())
        } else {
            let ret = outcome
                .into_iter()
                .map(|(display_name, timelimit, samples)| {
                    let suite = if timelimit == Duration::new(0, 0) {
                        TestSuite::Unsubmittable
                    } else if [
                        // https://atcoder.jp/contests/arc019/tasks/arc019_4
                        ("arc019", "D - ほんとうのたたかい"),
                        // https://atcoder.jp/contests/arc021/tasks/arc021_4
                        ("arc021", "D - だいたい最小全域木"),
                    ]
                    .contains(&(contest_slug, &display_name))
                    {
                        InteractiveSuite::new(timelimit).into()
                    } else {
                        match samples {
                            None => BatchSuite::new(timelimit).into(),
                            Some(Samples::Batch(cases, matching)) => BatchSuite::new(timelimit)
                                .matching(matching)
                                .sample_cases(cases.into_iter(), |i| format!("Sample {}", i + 1))
                                .into(),
                            Some(Samples::Interactive) => InteractiveSuite::new(timelimit).into(),
                        }
                    };
                    (display_name, suite)
                })
                .collect::<IndexMap<_, _>>();
            NonEmptyIndexMap::try_new(ret).ok_or_else(ScrapeError::new)
        }
    }

    fn extract_contest_duration(&self) -> ScrapeResult<(DateTime<Utc>, DateTime<Utc>)> {
        fn extract(this: &Html) -> Option<(DateTime<Utc>, DateTime<Utc>)> {
            static FORMAT: &str = "%F %T%z";
            let mut it = this.select(selector!("time"));
            let t1 = it.next()?.text().next()?;
            let t2 = it.next()?.text().next()?;
            let t1 = DateTime::parse_from_str(t1, FORMAT).ok()?;
            let t2 = DateTime::parse_from_str(t2, FORMAT).ok()?;
            Some((t1.with_timezone(&Utc), t2.with_timezone(&Utc)))
        }

        extract(self).ok_or_else(ScrapeError::new)
    }

    fn extract_submissions(&self) -> ScrapeResult<(Vec<Submission>, u32)> {
        let extract = || {
            let num_pages = self
                .select(selector!(
                    "#main-container > div.row > div.col-sm-12 > div.text-center > ul > li > a",
                ))
                .flat_map(|r| r.text())
                .flat_map(|t| t.parse::<u32>().ok())
                .max()
                .unwrap_or(1);
            let mut submissions = vec![];
            static SELECTOR: &Lazy<Selector> = selector!(
                "#main-container > div.row > div.col-sm-12 > div.panel-submission
                 > div.table-responsive > table.table > tbody > tr",
            );
            for tr in self.select(SELECTOR) {
                let (task_url, task_slug, task_display, task_screen, datetime) = {
                    static SLUG: &Lazy<Regex> = lazy_regex!(r"\A(\w+).*\z");
                    static SCREEN: &Lazy<Regex> =
                        lazy_regex!(r"\A/contests/[\w-]+/tasks/([\w-]+)\z");
                    static DATETIME: &str = "%F %T%z";
                    let a = tr.select(selector!("td > a")).next()?;
                    let task_display = a.text().next()?.to_owned();
                    let task_slug = SLUG.captures(&task_display)?[1].to_owned();
                    let task_path = a.value().attr("href")?;
                    let task_screen = SCREEN.captures(task_path)?[1].to_owned();
                    let mut task_url = BASE_URL.clone();
                    task_url.set_path(task_path);
                    let datetime = tr.select(selector!("td > time")).next()?.text().next()?;
                    let datetime = DateTime::parse_from_str(datetime, DATETIME).ok()?;
                    (task_url, task_slug, task_display, task_screen, datetime)
                };
                let lang = tr.select(selector!("td")).nth(3)?.text().next()?.to_owned();
                let verdict = tr
                    .select(selector!("td > span"))
                    .next()?
                    .text()
                    .next()?
                    .to_owned();
                let url = tr
                    .select(selector!("td.text-center > a"))
                    .flat_map(|a| {
                        let text = a.text().next()?;
                        guard!(["詳細", "Detail"].contains(&text));
                        let mut url = BASE_URL.clone();
                        url.set_path(a.value().attr("href")?);
                        Some(url)
                    })
                    .next()?;
                submissions.push(Submission {
                    url,
                    task_url,
                    task_slug,
                    task_display,
                    task_screen,
                    lang,
                    datetime,
                    verdict_is_ac: verdict == "AC",
                    verdict,
                })
            }
            Some((submissions, num_pages))
        };

        extract().ok_or_else(ScrapeError::new)
    }

    fn extract_submitted_code(&self) -> ScrapeResult<String> {
        let submission_code = self
            .select(selector!("#submission-code"))
            .next()
            .ok_or_else(ScrapeError::new)?;
        Ok(submission_code.text().next().unwrap_or_default().to_owned())
    }

    fn extract_lang_id_by_name(&self, name: &str) -> ScrapeResult<String> {
        self.select(selector!("#select-language > option"))
            .find(|r| r.text().next().map_or(false, |t| t == name))
            .ok_or_else(ScrapeError::new)?
            .value()
            .attr("value")
            .map(ToOwned::to_owned)
            .ok_or_else(ScrapeError::new)
    }

    fn extract_langs(&self) -> ScrapeResult<NonEmptyIndexMap<String, String>> {
        let names = self
            .select(selector!("#select-lang option"))
            .map(|option| {
                let name = option.text().next()?.to_owned();
                let id = option.value().attr("value")?.to_owned();
                Some((name, id))
            })
            .map(|p| p.ok_or_else(ScrapeError::new))
            .collect::<ScrapeResult<IndexMap<_, _>>>()?;
        NonEmptyIndexMap::try_new(names).ok_or_else(ScrapeError::new)
    }
}

#[cfg(test)]
mod tests {
    use crate::service;
    use crate::service::atcoder::Extract as _;
    use crate::testsuite::TestSuite;

    use failure::Fallible;
    use itertools::Itertools as _;
    use pretty_assertions::assert_eq;
    use retry::OperationResult;
    use scraper::Html;
    use url::Url;

    use std::iter;
    use std::time::Duration;

    #[test]
    fn it_extracts_a_contest_display_name() -> Fallible<()> {
        let display_name = service::reqwest_sync_client(Duration::from_secs(60))?
            .retry_retrieve_html("/contests/arc001/tasks")?
            .extract_contest_display_name()?;
        assert_eq!(display_name, "AtCoder Regular Contest 001");
        Ok(())
    }

    #[test]
    fn it_extracts_a_timelimit_from_apg4b_b() -> Fallible<()> {
        let suites = service::reqwest_sync_client(Duration::from_secs(60))?
            .retry_retrieve_html("/contests/apg4b/tasks/APG4b_b")?
            .extract_as_suites("apg4b")?;
        match suites.get("B - 1.01.出力とコメント") {
            Some(TestSuite::Unsubmittable) => Ok(()),
            suite => panic!("Got {:?}", suite),
        }
    }

    #[test]
    fn it_extracts_timelimits_and_sample_cases_from_arc001() -> Fallible<()> {
        static EXPECTED: &[(&str, &str, &str)] = &[
            ("A", "arc001_1", "d0a203b1a8e80ea6b5d77ba33dd31812"),
            ("B", "arc001_2", "8bf21c1a2e7e6b386d83ffef47b6b302"),
            ("C", "arc001_3", "fadc6a33d9b009b679d35c837d509ee7"),
            ("D", "arc001_4", "a7b8f7528a89fe733a18b829cefdadd5"),
        ];
        test_sample_extraction("arc001", EXPECTED)
    }

    #[test]
    fn it_extracts_timelimits_and_sample_cases_from_arc002() -> Fallible<()> {
        static EXPECTED: &[(&str, &str, &str)] = &[
            ("A", "arc002_1", "af6ae0d1fe88e2bf2eb8a9f97f4a3bd3"),
            ("B", "arc002_2", "251d9d839971aeadbca20fdd5bebe1e1"),
            ("C", "arc002_3", "ed797649ddb36669b5c83c0bb520fa4d"),
            ("D", "arc002_4", "91aaf382f4f2071185b5646ca48b26ef"),
        ];
        test_sample_extraction("arc002", EXPECTED)
    }

    #[test]
    fn it_extracts_timelimits_and_sample_cases_from_arc019() -> Fallible<()> {
        static EXPECTED: &[(&str, &str, &str)] = &[
            ("A", "arc019_1", "f4f9813e9b83d964ddec2c86a814ed88"),
            ("B", "arc019_2", "c3213ee3db31947143bb6e33e06c5a35"),
            ("C", "arc019_3", "05a26fe2160257e1a6208b9389d4ae64"),
            ("D", "arc019_4", "9debfd89a82271d763b717313363acda"),
        ];
        test_sample_extraction("arc019", EXPECTED)
    }

    #[test]
    fn it_extracts_timelimits_and_sample_cases_from_arc021() -> Fallible<()> {
        static EXPECTED: &[(&str, &str, &str)] = &[
            ("A", "arc021_1", "25ddfb733bce9101f8f53cbd694ae19f"),
            ("B", "arc021_2", "6dc19eaba78778063b8ccf0a1bc4e575"),
            ("C", "arc021_3", "b455b810fbc5cded70623ab538a3ef7b"),
            ("D", "arc021_4", "a818e644c08e2a2daf2ad316e4b3d1dd"),
        ];
        test_sample_extraction("arc021", EXPECTED)
    }

    #[test]
    fn it_extracts_timelimits_and_sample_cases_from_arc058() -> Fallible<()> {
        static EXPECTED: &[(&str, &str, &str)] = &[
            ("C", "arc058_a", "f134a1c2f5c9c9613a0a40c43906fd78"),
            ("D", "arc058_b", "8b9eb58dfa9c95b4766bab15c8439258"),
            ("E", "arc058_c", "2f04c46c9245f7a5378dd72e074a0983"),
            ("F", "arc058_d", "8c456a84332f2921703eeefca0493245"),
        ];
        test_sample_extraction("arc058", EXPECTED)
    }

    #[test]
    fn it_extracts_timelimits_and_sample_cases_from_abc011() -> Fallible<()> {
        static EXPECTED: &[(&str, &str, &str)] = &[
            ("A", "abc011_1", "9d0cab85f775693e032c9d7ecc59e5cd"),
            ("B", "abc011_2", "aec6741969522b7b6cc1dc47b9374aa2"),
            ("C", "abc011_3", "74cfb5ae94304b069245ba49a71b136f"),
            ("D", "abc011_4", "0cb6050b366d4f51e23d12a811a3a93d"),
        ];
        test_sample_extraction("abc011", EXPECTED)
    }

    #[test]
    fn it_extracts_timelimits_and_sample_cases_from_abc041() -> Fallible<()> {
        static EXPECTED: &[(&str, &str, &str)] = &[
            ("A", "abc041_a", "d7797b2e885f35f588895af8f199cfe1"),
            ("B", "abc041_b", "37f3335e442bdbaa7c4d6a144080627f"),
            ("C", "abc041_c", "2fae4d4d77d851bbdba6f9cfec6c6bde"),
            ("D", "abc041_d", "bb30dd61021373384657c6fe52e81a27"),
        ];
        test_sample_extraction("abc041", EXPECTED)
    }

    #[test]
    fn it_extracts_timelimits_and_sample_cases_from_dp() -> Fallible<()> {
        static EXPECTED: &[(&str, &str, &str)] = &[
            ("A", "dp_a", "57824a35ccbfca022c43f6713aa1bd5b"),
            ("B", "dp_b", "6a69157b35671faf222fe080e3cc71c8"),
            ("C", "dp_c", "548ed4cfa13f83bb5420b596de282eb9"),
            ("D", "dp_d", "6f52641c6508bfecb4249d37d1448b26"),
            ("E", "dp_e", "2d6f370dcf5b035b8ee6919a04ae757b"),
            ("F", "dp_f", "8ef36c34cbf19287835d43da6f821a6d"),
            ("G", "dp_g", "caf6f77d3ad22d379ad4c5f31433bc97"),
            ("H", "dp_h", "da82bdf7e006bc9b8feac5a80ae1ffe2"),
            ("I", "dp_i", "0082fa046f474d23e12d293e4888b5dd"),
            ("J", "dp_j", "b0a84653c7b7e5ddbcec43bab122ce84"),
            ("K", "dp_k", "92761b907e6a2cc9d1d9beb88d9d4ae5"),
            ("L", "dp_l", "73f37f0dcd679d69456932c169b62ccb"),
            ("M", "dp_m", "230b2471a92d9ce917b19216e12969a2"),
            ("N", "dp_n", "003343a26117a1cb59cc199e3aa84d3c"),
            ("O", "dp_o", "195dcd10c15b05c2f23744142d8b9b16"),
            ("P", "dp_p", "535a0d3c5146233734026683cbc1768f"),
            ("Q", "dp_q", "7201fc360ecc3e95e9648c1877de9f46"),
            ("R", "dp_r", "68350e3245f5f3ceedace82f704b19f0"),
            ("S", "dp_s", "2cab298936cd86b02f60d0a55e3f2a82"),
            ("T", "dp_t", "c208bbd0ffa39ed40a7b71f8bee5b1ef"),
            ("U", "dp_u", "874ae39e4c1e09c38cbae68207ea10f9"),
            ("V", "dp_v", "1ecc63556a26e8aa4b7f72e926f7b633"),
            ("W", "dp_w", "5f0e2145ef41079c4ad0f0b5bc22b502"),
            ("X", "dp_x", "ea27e9783f14a158932330d9ea5991c5"),
            ("Y", "dp_y", "7f88d4dfe1220f2a1e2a6ffc54432a39"),
            ("Z", "dp_z", "909d93bcbb5d7efea059c02a5932c77f"),
        ];
        test_sample_extraction("dp", EXPECTED)
    }

    fn test_sample_extraction(
        contest: &str,
        expected: &'static [(&'static str, &'static str, &'static str)],
    ) -> Fallible<()> {
        let client = service::reqwest_sync_client(Duration::from_secs(60))?;
        let slugs_and_uris = client
            .retry_retrieve_html(&format!("/contests/{}/tasks", contest))?
            .extract_task_slugs_and_uris()?;
        let suites = client
            .retry_retrieve_html(&format!("/contests/{}/tasks_print", contest))?
            .extract_as_suites(contest)?;
        for (
            ((expected_slug, expected_screen, expected_md5), (actual_slug, actual_uri)),
            (_, actual_suite),
        ) in expected.iter().zip_eq(&slugs_and_uris).zip_eq(&suites)
        {
            let expected_uri = format!("/contests/{}/tasks/{}", contest, expected_screen);
            assert_eq!(actual_slug, *expected_slug);
            assert_eq!(actual_uri, &*expected_uri);
            let actual_md5 = actual_suite.md5()?;
            actual_suite.assert_serialize_correctly()?;
            assert_eq!(format!("{:x}", actual_md5), *expected_md5);
        }
        Ok(())
    }

    #[test]
    fn it_extracts_a_submitted_source_code() -> Fallible<()> {
        let code = service::reqwest_sync_client(Duration::from_secs(60))?
            .retry_retrieve_html("/contests/utpc2011/submissions/2067")?
            .extract_submitted_code()?;
        assert_eq!(
            format!("{:x}", md5::compute(&code)),
            "1d805f5f226cd9d6dd90081a47505b7b",
        );
        Ok(())
    }

    trait RetryRetrieveHtml {
        fn retry_retrieve_html(
            &self,
            path: &str,
        ) -> std::result::Result<Html, retry::Error<reqwest::Error>>;
    }

    impl RetryRetrieveHtml for reqwest::Client {
        fn retry_retrieve_html(
            &self,
            path: &str,
        ) -> std::result::Result<Html, retry::Error<reqwest::Error>> {
            const RETRIES: usize = 2;
            const INTERVAL: Duration = Duration::from_secs(1);

            let mut url = "https://atcoder.jp".parse::<Url>().unwrap();
            url.set_path(path);
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
