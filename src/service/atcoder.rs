use crate::errors::{
    FileResult, ScrapeError, ScrapeResult, ServiceError, ServiceErrorKind, ServiceResult,
};
use crate::path::AbsPath;
use crate::service::download::DownloadProgress;
use crate::service::session::HttpSession;
use crate::service::{
    Contest, LoginOutcome, ParticipateOutcome, PrintTargets as _, RetrieveLangsOutcome,
    RetrieveLangsProps, RetrieveSubmissionsOutcome, RetrieveSubmissionsProps,
    RetrieveTestCasesOutcome, RetrieveTestCasesOutcomeProblem, RetrieveTestCasesProps, Service,
    SessionProps, SubmitOutcome, SubmitProps,
};
use crate::terminal::{HasTerm, Term, WriteAnsi as _};
use crate::testsuite::{self, BatchSuite, Destinations, InteractiveSuite, TestSuite};
use crate::util::collections::NonEmptyIndexMap;
use crate::util::lang_unstable::Never;
use crate::util::num::PositiveFinite;
use crate::util::std_unstable::RemoveItem_ as _;
use crate::util::str::CaseConversion;

use chrono::{DateTime, Local, Utc};
use failure::ResultExt as _;
use indexmap::IndexMap;
use itertools::Itertools as _;
use maplit::hashmap;
use once_cell::sync::Lazy;
use regex::Regex;
use reqwest::{header, StatusCode};
use select::document::Document;
use select::predicate::{Predicate, Text};
use serde_derive::{Deserialize, Serialize};
use serde_json::json;
use tokio::runtime::Runtime;
use url::Url;

use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::io::Write as _;
use std::ops::Deref;
use std::path::Path;
use std::str::FromStr;
use std::time::Duration;
use std::{f64, vec};

/// Logins to "atcoder.jp".
pub(crate) fn login(mut props: SessionProps, term: impl Term) -> ServiceResult<LoginOutcome> {
    let dropbox_path = props.dropbox_path.take();
    let mut atcoder = Atcoder::try_new(props, term)?;
    atcoder.login_if_not(true)?;
    if let Some(dropbox_path) = dropbox_path {
        atcoder.auth_dropbox(&dropbox_path, true)?;
    }
    Ok(LoginOutcome {})
}

/// Participates in a `contest_name`.
pub(crate) fn participate(
    contest: &str,
    props: SessionProps,
    term: impl Term,
) -> ServiceResult<ParticipateOutcome> {
    Atcoder::try_new(props, term)?.register_explicitly(&AtcoderContest::new(contest))
}

/// Accesses to pages of the problems and extracts pairs of sample input/output
/// from them.
pub(crate) fn retrieve_testcases(
    props: (SessionProps, RetrieveTestCasesProps<String>),
    mut term: impl Term,
) -> ServiceResult<RetrieveTestCasesOutcome> {
    let (mut sess_props, retrieve_props) = props;
    let dropbox_path = sess_props.dropbox_path.take();
    let dropbox_path = dropbox_path.as_ref().map(Deref::deref);
    let retrieve_props = retrieve_props
        .convert_problems(CaseConversion::Upper)
        .parse_contest()
        .unwrap();
    retrieve_props.print_targets(term.stderr())?;
    Atcoder::try_new(sess_props, term)?.retrieve_testcases(&retrieve_props, dropbox_path)
}

pub(crate) fn retrieve_langs(
    props: (SessionProps, RetrieveLangsProps<String>),
    mut term: impl Term,
) -> ServiceResult<RetrieveLangsOutcome> {
    let (sess_props, retrieve_props) = props;
    let retrieve_props = retrieve_props
        .convert_problem(CaseConversion::Upper)
        .parse_contest()
        .unwrap();
    retrieve_props.print_targets(term.stderr())?;
    Atcoder::try_new(sess_props, term)?.retrieve_langs(retrieve_props)
}

/// Downloads submitted source codes.
pub(crate) fn retrieve_submissions(
    props: (SessionProps, RetrieveSubmissionsProps<String>),
    mut term: impl Term,
) -> ServiceResult<RetrieveSubmissionsOutcome> {
    let (sess_props, retrieve_props) = props;
    let retrieve_props = retrieve_props
        .convert_problems(CaseConversion::Upper)
        .parse_contest()
        .unwrap();
    retrieve_props.print_targets(term.stderr())?;
    Atcoder::try_new(sess_props, term)?.retrieve_submissions(&retrieve_props)
}

/// Submits a source code.
pub(crate) fn submit(
    props: (SessionProps, SubmitProps<String>),
    mut term: impl Term,
) -> ServiceResult<SubmitOutcome> {
    let (sess_props, submit_props) = props;
    let submit_props = submit_props
        .convert_problem(CaseConversion::Upper)
        .parse_contest()
        .unwrap();
    submit_props.print_targets(term.stderr())?;
    Atcoder::try_new(sess_props, term)?.submit(&submit_props)
}

pub(self) struct Atcoder<T: Term> {
    login_retries: Option<u32>,
    term: T,
    session: HttpSession,
    runtime: Runtime,
}

impl<T: Term> HasTerm for Atcoder<T> {
    type Term = T;

    fn term(&mut self) -> &mut T {
        &mut self.term
    }
}

impl<T: Term> Service for Atcoder<T> {
    type Stderr = T::Stderr;

    fn requirements(&mut self) -> (&mut T::Stderr, &mut HttpSession, &mut Runtime) {
        (self.term.stderr(), &mut self.session, &mut self.runtime)
    }
}

impl<T: Term> DownloadProgress for Atcoder<T> {
    type Write = T::Stderr;

    fn requirements(&mut self) -> (&mut T::Stderr, &HttpSession, &mut Runtime) {
        (self.term.stderr(), &self.session, &mut self.runtime)
    }
}

impl<T: Term> Atcoder<T> {
    fn try_new(props: SessionProps, mut term: T) -> ServiceResult<Self> {
        let mut runtime = Runtime::new()?;
        let session = props.start_session(term.stderr(), &mut runtime)?;
        Ok(Self {
            login_retries: props.login_retries,
            term,
            session,
            runtime,
        })
    }

    fn login_if_not(&mut self, explicit: bool) -> ServiceResult<()> {
        if self.session.has_cookie() {
            let status = self.get("/settings").acceptable(&[200, 302]).status()?;
            if status == StatusCode::OK {
                if explicit {
                    writeln!(self.stderr(), "Already logged in.")?;
                    self.stderr().flush()?;
                }
                return Ok(());
            }
        }

        let mut retries = self.login_retries;
        loop {
            let payload = hashmap!(
                "csrf_token" => self.get("/login").recv_html()?.extract_csrf_token()?,
                "username" => self.prompt_reply_stderr("Username: ")?,
                "password" => self.prompt_password_stderr("Password: ")?,
            );
            self.post("/login").send_form(&payload)?;
            if self.get("/settings").acceptable(&[200, 302]).status()? == 200 {
                writeln!(self.stderr(), "Successfully logged in.")?;
                break self.stderr().flush().map_err(Into::into);
            }
            if retries == Some(0) {
                return Err(ServiceErrorKind::LoginRetriesExceeded.into());
            }
            retries = retries.map(|n| n - 1);
            writeln!(self.stderr(), "Failed to login. Try again.")?;
            self.stderr().flush()?;
            self.session.clear_cookies()?;
        }
    }

    fn auth_dropbox(&mut self, auth_path: &AbsPath, explicit: bool) -> ServiceResult<String> {
        static CLIENT_ID: &str = "6h5mn3yn8o3qbk3";
        static CLIENT_SECRET: &str = "g2p59fafm3d0lnz";

        #[derive(Serialize, Deserialize)]
        struct AuthToken {
            access_token: String,
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
                .no_cookie()
                .bearer_auth(&auth.access_token)
                .send()?
                .status();
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
            .no_cookie()
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

    fn fetch_tasks_page(&mut self, contest: &AtcoderContest) -> ServiceResult<Document> {
        let res = self
            .get(&contest.url_tasks())
            .acceptable(&[200, 302, 404])
            .send()?;
        if res.status() == 200 {
            res.document(&mut self.runtime)
        } else {
            self.register_if_active_or_explicit(contest, false)?;
            self.get(&contest.url_tasks()).recv_html()
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
            .send()?;
        if res.status() == StatusCode::FOUND {
            return Err(ServiceErrorKind::ContestNotFound(contest.to_string()).into());
        }
        let page = res.document(&mut self.runtime)?;
        let duration = page.extract_contest_duration()?;
        let status = duration.check_current_status(contest.to_string());
        if !explicit {
            status.raise_if_not_begun()?;
        }
        if explicit || *contest == AtcoderContest::Practice || status.is_active() {
            self.login_if_not(false)?;
            let csrf_token = self
                .get(&contest.url_top())
                .recv_html()?
                .extract_csrf_token()?;
            let url = contest.url_register();
            let payload = hashmap!("csrf_token" => csrf_token);
            self.post(&url).send_form(&payload)?;
        }
        Ok(ParticipateOutcome {})
    }

    fn retrieve_testcases(
        &mut self,
        props: &RetrieveTestCasesProps<AtcoderContest>,
        dropbox_path: Option<&AbsPath>,
    ) -> ServiceResult<RetrieveTestCasesOutcome> {
        let RetrieveTestCasesProps {
            contest,
            problems,
            destinations,
            open_in_browser,
            only_scraped,
        } = props;
        let names_and_urls = self
            .fetch_tasks_page(contest)?
            .extract_task_urls_with_names()?
            .into_iter()
            .filter(|(name, _)| match problems.as_ref() {
                None => true,
                Some(problems) => problems.iter().any(|p| p == name),
            })
            .collect::<Vec<_>>();
        let mut outcome = RetrieveTestCasesOutcome::new(contest);
        for (name, url) in names_and_urls {
            let suite = match contest.preset_suite(&name) {
                Some(suite) => suite,
                None => self.get(&url).recv_html()?.extract_as_suite()?,
            };
            let path = destinations.expand(&name)?;
            let url = self.session.resolve_url(&url)?;
            outcome.push_problem(name, url, suite, path);
        }
        let mut not_found = match problems.as_ref() {
            None => vec![],
            Some(problems) => problems.iter().collect(),
        };
        if !*only_scraped {
            if let Some(dropbox_path) = dropbox_path {
                let suites = outcome
                    .problems
                    .iter_mut()
                    .map(|p| (p.name.as_str(), &mut p.test_suite))
                    .collect::<BTreeMap<_, _>>();
                self.retrieve_from_dropbox(dropbox_path, &contest.slug(), suites, destinations)?;
            }
        }
        for RetrieveTestCasesOutcomeProblem {
            name,
            test_suite,
            test_suite_path,
            ..
        } in &outcome.problems
        {
            test_suite.save(name, test_suite_path, self.stderr())?;
            not_found.remove_item_(&name);
        }
        self.stderr().flush()?;
        if !not_found.is_empty() {
            self.stderr()
                .with_reset(|o| writeln!(o.fg(11)?, "Not found: {:?}", not_found))?;
            self.stderr().flush()?;
        }
        if *open_in_browser {
            self.open_in_browser(&contest.url_submissions_me(1))?;
            for RetrieveTestCasesOutcomeProblem { url, .. } in &outcome.problems {
                self.open_in_browser(url.as_str())?;
            }
        }
        Ok(outcome)
    }

    fn retrieve_from_dropbox(
        &mut self,
        auth_path: &AbsPath,
        contest_slug: &str,
        mut suites: BTreeMap<&str, &mut TestSuite>,
        destinations: &Destinations,
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
            this: &mut Atcoder<impl Term>,
            token: &str,
            contest_slug: &str,
            mut problems: impl Iterator<Item = &'a str>,
        ) -> ServiceResult<bool> {
            #[derive(Deserialize)]
            #[serde(untagged)]
            enum ListFolderResult {
                Ok(ListFolder),
                Err(ErrorWithSummary),
            }

            #[derive(Deserialize)]
            struct ErrorWithSummary {}

            let result = this
                .post("https://api.dropboxapi.com/2/files/list_folder")
                .bearer_auth(token)
                .acceptable(&[200, 409])
                .no_cookie()
                .json(&json!({
                    "shared_link": { "url": URL },
                    "path": format!("/{}", contest_slug)
                }))
                .recv_json::<ListFolderResult>()?;
            Ok(match result {
                ListFolderResult::Ok(ok) => {
                    let names = ok
                        .entries
                        .iter()
                        .map(|e| e.name.as_str())
                        .collect::<HashSet<_>>();
                    problems.all(|p| names.contains(p))
                }
                ListFolderResult::Err(_) => false,
            })
        }

        fn list_folder(
            this: &mut Atcoder<impl Term>,
            token: &str,
            path: &str,
        ) -> ServiceResult<ListFolder> {
            this.post("https://api.dropboxapi.com/2/files/list_folder")
                .bearer_auth(token)
                .acceptable(&[200])
                .no_cookie()
                .json(&json!({ "shared_link": { "url": URL }, "path": path }))
                .recv_json()
        }

        fn retrieve_files(
            this: &mut Atcoder<impl Term>,
            token: &str,
            prefix: &str,
            entries: &[ListFolderEntry],
        ) -> ServiceResult<HashMap<String, Vec<u8>>> {
            static ENDPOINT: &str = "https://content.dropboxapi.com/2/sharing/get_shared_link_file";
            let filenames = entries.iter().map(|e| &e.name).collect::<Vec<_>>();
            let urls = vec![ENDPOINT; filenames.len()];
            let client = this.session.client();
            let alt_reqs = filenames
                .iter()
                .map(|filename| {
                    client
                        .post(ENDPOINT)
                        .header(header::AUTHORIZATION, format!("Bearer {}", token))
                        .header(
                            "Dropbox-API-Arg",
                            json!({ "url": URL, "path": format!("{}/{}", prefix, filename) })
                                .to_string(),
                        )
                })
                .collect();
            let files = this.download_progress(&urls, &filenames, Some(alt_reqs))?;
            Ok(files
                .into_iter()
                .zip_eq(filenames)
                .map(|(content, filename)| {
                    let stem = Path::new(&filename)
                        .file_stem()
                        .unwrap_or_default()
                        .to_str()
                        .unwrap()
                        .to_owned();
                    (stem, content)
                })
                .collect())
        }

        let token = self.auth_dropbox(auth_path, false)?;
        if !confirm_folders_exist(self, &token, &contest_slug, suites.keys().cloned())? {
            return Ok(());
        }
        let contents = suites
            .iter()
            .filter_map(|(problem, suite)| match suite {
                TestSuite::Unsubmittable => None,
                _ => Some(problem),
            })
            .map(|problem| {
                let in_dir = format!("/{}/{}/in", contest_slug, problem);
                let entries = list_folder(self, &token, &in_dir)?.entries;
                let in_contents = retrieve_files(self, &token, &in_dir, &entries)?;
                let out_dir = format!("/{}/{}/out", contest_slug, problem);
                let entries = list_folder(self, &token, &out_dir)?.entries;
                let mut out_contents = retrieve_files(self, &token, &out_dir, &entries)?;
                let contents = in_contents
                    .into_iter()
                    .filter_map(|(name, i)| out_contents.remove(&name).map(|o| (name, (i, o))))
                    .collect::<BTreeMap<_, _>>();
                Ok((problem.to_owned(), contents))
            })
            .collect::<ServiceResult<BTreeMap<_, _>>>()?;
        for (problem, contents) in contents {
            let dir = destinations.text_file_dir(problem)?;
            let (in_dir, out_dir) = (dir.join("in"), dir.join("out"));
            let contents_len = contents.len();
            let paths = contents
                .into_iter()
                .map(|(case_name, (input, output))| {
                    let in_path = in_dir.join(&case_name).with_extension("txt");
                    let out_path = out_dir.join(&case_name).with_extension("txt");
                    crate::fs::write(&in_path, &input)?;
                    crate::fs::write(&out_path, &output)?;
                    Ok((case_name, (in_path, out_path)))
                })
                .collect::<FileResult<BTreeMap<_, _>>>()?;
            self.stderr()
                .with_reset(|o| write!(o.bold()?, "{}", problem))?;
            writeln!(
                self.stderr(),
                ": Saved {} to {}",
                plural!(2 * contents_len, "file", "files"),
                dir.display(),
            )?;
            if let Some(TestSuite::Batch(suite)) = suites.get_mut(problem) {
                suite.clear_cases();
                for (case_name, (in_path, out_path)) in paths {
                    suite.push_path(case_name, in_path, out_path);
                }
            }
        }
        self.stderr().flush().map_err(Into::into)
    }

    fn retrieve_langs(
        &mut self,
        props: RetrieveLangsProps<AtcoderContest>,
    ) -> ServiceResult<RetrieveLangsOutcome> {
        let RetrieveLangsProps { contest, problem } = props;
        self.login_if_not(false)?;
        let url = match problem {
            None => contest.url_submit(),
            Some(problem) => {
                let url = self
                    .fetch_tasks_page(&contest)?
                    .extract_task_urls_with_names()?
                    .remove(&problem)
                    .ok_or_else(|| ServiceErrorKind::NoSuchProblem(problem.clone()))?;
                self.session.resolve_url(&url)?
            }
        };
        let langs = self.get(&url).recv_html()?.extract_langs()?;
        self.print_lang_list(&langs)?;
        Ok(RetrieveLangsOutcome::new(url, langs))
    }

    fn retrieve_submissions(
        &mut self,
        props: &RetrieveSubmissionsProps<AtcoderContest>,
    ) -> ServiceResult<RetrieveSubmissionsOutcome> {
        fn collect_urls(
            detail_urls: &mut HashMap<(String, String), String>,
            submissions: vec::IntoIter<Submission>,
        ) {
            for submission in submissions {
                let key = (submission.task_name, submission.lang_name);
                if detail_urls.get(&key).is_none() {
                    detail_urls.insert(key, submission.detail_url);
                }
            }
        }

        let RetrieveSubmissionsProps {
            contest,
            problems,
            src_paths,
        } = props;

        let first_page = {
            let res = self
                .get(&contest.url_submissions_me(1))
                .acceptable(&[200, 302])
                .send()?;
            if res.status() == 200 {
                res.document(&mut self.runtime)?
            } else {
                self.register_if_active_or_explicit(contest, false)?;
                self.get(&contest.url_submissions_me(1))
                    .acceptable(&[200, 302])
                    .recv_html()?
            }
        };
        let (submissions, num_pages) = first_page.extract_submissions()?;
        let mut detail_urls = HashMap::new();
        collect_urls(&mut detail_urls, submissions);
        for i in 2..=num_pages {
            let page = self.get(&contest.url_submissions_me(i)).recv_html()?;
            let (submission, _) = page.extract_submissions()?;
            collect_urls(&mut detail_urls, submission);
        }
        let mut results = vec![];
        for ((task_name, lang_name), detail_url) in detail_urls {
            if problems.is_some() && !problems.as_ref().unwrap().iter().any(|p| p == &task_name) {
                continue;
            }
            let code = self
                .get(&detail_url)
                .recv_html()?
                .extract_submitted_code()?;
            if let Some(path_template) = src_paths.get(lang_name.as_str()) {
                let path = path_template.expand(Some(&task_name.to_lowercase()))?;
                crate::fs::write(&path, code.as_bytes())?;
                results.push((task_name, lang_name, path));
            } else {
                self.stderr()
                    .with_reset(|o| writeln!(o.fg(11)?, "Ignoring {:?}", lang_name))?;
                self.stderr().flush()?;
            }
        }
        let mut not_found = match problems.as_ref() {
            None => vec![],
            Some(problems) => problems.iter().collect(),
        };
        for (task_name, lang_name, path) in &results {
            writeln!(
                self.stderr(),
                "{} - {:?}: Saved to {}",
                task_name,
                lang_name,
                path.display()
            )?;
            not_found.remove_item_(&task_name);
        }
        if !not_found.is_empty() {
            self.stderr()
                .with_reset(|o| writeln!(o.fg(11)?, "Not found: {:?}", not_found))?;
            self.stderr().flush()?;
        }
        let stderr = self.stderr();
        writeln!(stderr, "Saved {}.", plural!(results.len(), "file", "files"))?;
        stderr.flush()?;
        Ok(RetrieveSubmissionsOutcome {})
    }

    fn submit(&mut self, props: &SubmitProps<AtcoderContest>) -> ServiceResult<SubmitOutcome> {
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
            !skip_checking_if_accepted && *contest != AtcoderContest::Practice && {
                let duration = tasks_page.extract_contest_duration()?;
                let status = duration.check_current_status(contest.to_string());
                status.raise_if_not_begun()?;
                status.is_active()
            };
        let url = tasks_page
            .extract_task_urls_with_names()?
            .remove(problem)
            .ok_or_else(|| ServiceErrorKind::NoSuchProblem(problem.clone()))?;
        let task_screen_name = {
            static SCREEN_NAME: Lazy<Regex> =
                lazy_regex!(r"\A/contests/[a-z0-9_\-]+/tasks/([a-z0-9_]+)/?\z$");
            SCREEN_NAME
                .captures(&url)
                .map(|cs| cs[1].to_owned())
                .ok_or_else(ScrapeError::new)?
        };
        if checks_if_accepted {
            let (mut submissions, num_pages) = self
                .get(&contest.url_submissions_me(1))
                .recv_html()?
                .extract_submissions()?;
            if submissions.any(|s| s.task_screen_name == task_screen_name && s.is_ac) {
                return Err(ServiceErrorKind::AlreadyAccepted.into());
            }
            for i in 2..=num_pages {
                if self
                    .get(&contest.url_submissions_me(i))
                    .recv_html()?
                    .extract_submissions()?
                    .0
                    .any(|s| s.task_screen_name == task_screen_name && s.is_ac)
                {
                    return Err(ServiceErrorKind::AlreadyAccepted.into());
                }
            }
        }

        let source_code = crate::fs::read_to_string(src_path)?;
        let document = self.get(&url).recv_html()?;
        let lang_id = document
            .extract_langs()?
            .get(lang_name)
            .ok_or_else(|| ServiceErrorKind::NoSuchLang(lang_name.clone()))?
            .clone();
        writeln!(
            self.stderr(),
            "Submitting as {:?} (ID: {:?})",
            lang_name,
            lang_id,
        )?;
        let csrf_token = document.extract_csrf_token()?;
        let url = contest.url_submit();
        let payload = hashmap!(
            "data.TaskScreenName" => task_screen_name.as_str(),
            "data.LanguageId" => &lang_id,
            "sourceCode" => &source_code,
            "csrf_token" => &csrf_token,
        );

        let error = |status: StatusCode, location: Option<String>| -> _ {
            ServiceError::from(ServiceErrorKind::SubmissionRejected {
                lang_name: lang_name.clone(),
                lang_id: lang_id.clone(),
                size: source_code.len(),
                status,
                location,
            })
        };

        match self.post(&url).send_form(&payload) {
            Ok(res) => {
                let location = res
                    .headers()
                    .get(header::LOCATION)
                    .ok_or_else(|| error(res.status(), None))?;
                let location = location
                    .to_str()
                    .with_context(|_| ServiceErrorKind::ReadHeader(header::LOCATION))?;
                if !(location.starts_with("/contests/") && location.ends_with("/submissions/me")) {
                    return Err(error(res.status(), Some(location.to_owned())));
                }
            }
            Err(err) => {
                if let ServiceError::Context(ctx) = &err {
                    if let ServiceErrorKind::UnexpectedStatusCode(_, status, _) = ctx.get_context()
                    {
                        return Err(error(*status, None));
                    }
                }
                return Err(err);
            }
        }

        if *open_in_browser {
            self.open_in_browser(&contest.url_submissions_me(1))?;
        }
        Ok(SubmitOutcome {})
    }
}

#[derive(Clone, PartialEq, Eq, derive_more::Display)]
enum AtcoderContest {
    #[display(fmt = "practice contest")]
    Practice,
    #[display(fmt = "AtCoder Programming Guide for beginners")]
    Apg4b,
    #[display(fmt = "ARC{:>03}", _0)]
    Arc(u32),
    #[display(fmt = "ABC{:>03}", _0)]
    Abc(u32),
    #[display(fmt = "AGC{:>03}", _0)]
    Agc(u32),
    #[display(fmt = "ATC{:>03}", _0)]
    Atc(u32),
    #[display(fmt = "APC{:>03}", _0)]
    Apc(u32),
    #[display(fmt = "Chokudai SpeedRun {:>03}", _0)]
    ChokudaiS(u32),
    #[display(fmt = "{}", _0)]
    Other(String),
}

impl AtcoderContest {
    fn new(s: &str) -> Self {
        static NAME: Lazy<Regex> = lazy_regex!(r"\A\s*([a-zA-Z_]+)(\d{3})\s*\z");
        if let Some(caps) = NAME.captures(s) {
            let name = caps[1].to_lowercase();
            let number = caps[2].parse::<u32>().unwrap_or(0);
            if name == "abc" {
                return AtcoderContest::Abc(number);
            } else if name == "arc" {
                return AtcoderContest::Arc(number);
            } else if name == "agc" {
                return AtcoderContest::Agc(number);
            } else if name == "atc" {
                return AtcoderContest::Atc(number);
            } else if name == "apc" {
                return AtcoderContest::Apc(number);
            } else if name == "chokudai_s" || name == "chokudais" {
                return AtcoderContest::ChokudaiS(number);
            }
        }
        if s == "practice" {
            AtcoderContest::Practice
        } else if s == "apg4b" {
            AtcoderContest::Apg4b
        } else {
            AtcoderContest::Other(s.to_owned())
        }
    }

    fn url_top(&self) -> Url {
        let mut ret = "https://atcoder.jp".parse::<Url>().unwrap();
        ret.set_path(&format!("/contests/{}", self.slug()));
        ret
    }

    fn url_tasks(&self) -> Url {
        let mut ret = "https://atcoder.jp".parse::<Url>().unwrap();
        ret.set_path(&format!("/contests/{}/tasks", self.slug()));
        ret
    }

    fn url_register(&self) -> Url {
        let mut ret = "https://atcoder.jp".parse::<Url>().unwrap();
        ret.set_path(&format!("/contests/{}/register", self.slug()));
        ret
    }

    fn url_submit(&self) -> Url {
        let mut ret = "https://atcoder.jp".parse::<Url>().unwrap();
        ret.set_path(&format!("/contests/{}/submit", self.slug()));
        ret
    }

    fn url_submissions_me(&self, page: u32) -> Url {
        let mut ret = "https://atcoder.jp".parse::<Url>().unwrap();
        ret.set_path(&format!("/contests/{}/submissions/me", self.slug()));
        ret.query_pairs_mut().append_pair("page", &page.to_string());
        ret
    }

    fn preset_suite(&self, problem: &str) -> Option<TestSuite> {
        match (self, problem) {
            (AtcoderContest::Arc(19), "D") => Some(InteractiveSuite::new(Duration::from_secs(2))),
            (AtcoderContest::Arc(21), "D") => Some(InteractiveSuite::new(Duration::from_secs(4))),
            _ => None,
        }
        .map(Into::into)
    }
}

impl Contest for AtcoderContest {
    fn slug(&self) -> Cow<str> {
        match self {
            AtcoderContest::Practice => "practice".into(),
            AtcoderContest::Apg4b => "apg4b".into(),
            AtcoderContest::Abc(n) => format!("abc{:>03}", n).into(),
            AtcoderContest::Arc(n) => format!("arc{:>03}", n).into(),
            AtcoderContest::Agc(n) => format!("agc{:>03}", n).into(),
            AtcoderContest::Atc(n) => format!("atc{:>03}", n).into(),
            AtcoderContest::Apc(n) => format!("apc{:>03}", n).into(),
            AtcoderContest::ChokudaiS(n) => format!("chokudai_s{:>03}", n).into(),
            AtcoderContest::Other(s) => s.into(),
        }
    }
}

impl FromStr for AtcoderContest {
    type Err = Never;

    fn from_str(s: &str) -> std::result::Result<Self, Never> {
        Ok(Self::new(s))
    }
}

#[derive(Debug)]
enum ContestStatus {
    Finished,
    Active,
    NotBegun(String, DateTime<Local>),
}

impl ContestStatus {
    fn is_active(&self) -> bool {
        match *self {
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

struct ContestDuration(DateTime<Utc>, DateTime<Utc>);

impl ContestDuration {
    fn check_current_status(&self, contest_name: String) -> ContestStatus {
        let now = Utc::now();
        if now < self.0 {
            ContestStatus::NotBegun(contest_name, self.0.with_timezone(&Local))
        } else if now > self.1 {
            ContestStatus::Finished
        } else {
            ContestStatus::Active
        }
    }
}

struct Submission {
    task_name: String,
    task_screen_name: String,
    lang_name: String,
    detail_url: String,
    is_ac: bool,
}

trait Extract {
    fn extract_csrf_token(&self) -> ScrapeResult<String>;
    fn extract_task_urls_with_names(&self) -> ScrapeResult<NonEmptyIndexMap<String, String>>;
    fn extract_as_suite(&self) -> ScrapeResult<TestSuite>;
    fn extract_contest_duration(&self) -> ScrapeResult<ContestDuration>;
    fn extract_submissions(&self) -> ScrapeResult<(vec::IntoIter<Submission>, u32)>;
    fn extract_submitted_code(&self) -> ScrapeResult<String>;
    fn extract_lang_id_by_name(&self, name: &str) -> ScrapeResult<String>;
    fn extract_langs(&self) -> ScrapeResult<NonEmptyIndexMap<String, String>>;
}

impl Extract for Document {
    fn extract_csrf_token(&self) -> ScrapeResult<String> {
        let extract_csrf_token = || {
            let token = self
                .find(selector!("[name=\"csrf_token\"]"))
                .next()?
                .attr("value")?
                .to_owned();
            guard!(!token.is_empty());
            Some(token)
        };
        extract_csrf_token().ok_or_else(ScrapeError::new)
    }

    fn extract_task_urls_with_names(&self) -> ScrapeResult<NonEmptyIndexMap<String, String>> {
        self.find(selector!(
            "#main-container > div.row > div.col-sm-12 > div.panel > table.table > tbody > tr",
        ))
        .map(|node| {
            let node = node.find(selector!("td.text-center > a")).next()?;
            let name = node.find(Text).next()?.text();
            let url = node.attr("href")?.to_owned();
            Some((name, url))
        })
        .collect::<Option<IndexMap<String, String>>>()
        .and_then(NonEmptyIndexMap::try_new)
        .ok_or_else(ScrapeError::new)
    }

    fn extract_as_suite(&self) -> ScrapeResult<TestSuite> {
        enum Samples {
            Batch(Vec<(String, String)>, testsuite::Match),
            Interactive,
        }

        fn extract_samples(this: &Document) -> Option<Samples> {
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
            let p1_head =
                selector!("#task-statement > span.lang > span.lang-ja > div.part > section > h3");
            let p1_content =
                selector!("#task-statement > span.lang > span.lang-ja > div.part > section > pre");
            // Current style (English)
            let p2_head =
                selector!("#task-statement > span.lang > span.lang-en > div.part > section > h3");
            let p2_content =
                selector!("#task-statement>span.lang>span.lang-en>div.part>section>pre");
            // ARC019..ARC057 \ {ARC019/C, ARC046/D, ARC050, ARC052/{A, C}, ARC053, ARC055},
            // ABC007..ABC040 \ {ABC036}, ATC001, ATC002
            let p3_head = selector!("#task-statement > div.part > section > h3");
            let p3_content = selector!("#task-statement > div.part > section > pre");
            // ARC002..ARC018, ARC019/C, ABC001..ABC006
            let p4_head = selector!("#task-statement > div.part > h3,pre");
            let p4_content = selector!("#task-statement > div.part > section > pre");
            // ARC001, dwacon2018-final/{A, B}
            let p5_head = selector!("#task-statement > h3,pre");
            let p5_content = selector!("#task-statement > section > pre");
            // ARC046/D, ARC050, ARC052/{A, C}, ARC053, ARC055, ABC036, ABC041
            let p6_head = selector!("#task-statement > section > h3");
            let p6_content = selector!("#task-statement > section > pre");
            // ABC034
            let p7_head = selector!("#task-statement > span.lang > span.lang-ja > section > h3");
            let p7_content =
                selector!("#task-statement > span.lang > span.lang-ja > section > pre");
            // practice contest (Japanese)
            let p8_head = selector!("#task-statement > span.lang > span.lang-ja > div.part > h3");
            let p8_content =
                selector!("#task-statement > span.lang > span.lang-ja > div.part > section > pre");

            try_extract_samples(this, p1_head, p1_content, &IN_JA, &OUT_JA)
                .or_else(|| try_extract_samples(this, p2_head, p2_content, &IN_EN, &OUT_EN))
                .or_else(|| try_extract_samples(this, p3_head, p3_content, &IN_JA, &OUT_JA))
                .or_else(|| try_extract_samples(this, p4_head, p4_content, &IN_JA, &OUT_JA))
                .or_else(|| try_extract_samples(this, p5_head, p5_content, &IN_JA, &OUT_JA))
                .or_else(|| try_extract_samples(this, p6_head, p6_content, &IN_JA, &OUT_JA))
                .or_else(|| try_extract_samples(this, p7_head, p7_content, &IN_JA, &OUT_JA))
                .or_else(|| try_extract_samples(this, p8_head, p8_content, &IN_JA, &OUT_JA))
        }

        fn try_extract_samples(
            this: &Document,
            predicate_for_header: impl Predicate,
            predicate_for_content: impl Predicate,
            re_input: &'static Regex,
            re_output: &'static Regex,
        ) -> Option<Samples> {
            for strong in this.find(selector!("#task-statement strong")) {
                let text = strong.text();
                for word in &["インタラクティブ", "Interactive"] {
                    if text.find(word).is_some() {
                        return Some(Samples::Interactive);
                    }
                }
            }
            let matching = {
                let error = this
                    .find(selector!("#task-statement var").child(Text))
                    .flat_map(|var| parse_floating_error(&var.text()))
                    .next();
                let all_text = this
                    .find(selector!("#task-statement").descendant(Text))
                    .map(|text| text.text())
                    .collect::<Vec<_>>();
                let relative = all_text
                    .iter()
                    .any(|s| s.contains("相対誤差") || s.contains("relative error"));
                let absolute = all_text
                    .iter()
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
            for node in this.find(predicate_for_header.or(predicate_for_content)) {
                if node.name() == Some("h3") {
                    let text = node.text();
                    if let Some(caps) = re_input.captures(&text) {
                        next = Some((true, parse_zenkaku(&caps[1]).ok()?));
                    } else if let Some(caps) = re_output.captures(&text) {
                        next = Some((false, parse_zenkaku(&caps[1]).ok()?));
                    }
                } else if [Some("pre"), Some("section")].contains(&node.name()) {
                    if let Some((is_input, n)) = next {
                        if is_input {
                            inputs.insert(n, node.text());
                        } else {
                            outputs.insert(n, node.text());
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
                    if !s.ends_with('\n') {
                        s.push('\n');
                    }
                    guard!(is_valid_text(s));
                }
            }

            guard!(!samples.is_empty());
            Some(Samples::Batch(samples, matching))
        }

        fn parse_floating_error(s: &str) -> Option<PositiveFinite<f64>> {
            static R: Lazy<Regex> = lazy_regex!(r"\A([0-9]{1,2})\^\{(-?[0-9]{1,2})\}\z");
            let caps = R.captures(s)?;
            let base = caps[1].parse::<f64>().ok()?;
            let exp = caps[2].parse::<f64>().ok()?;
            PositiveFinite::try_new(base.powf(exp)).ok()
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

        fn extract_timelimit(this: &Document) -> Option<Duration> {
            static TIMELIMIT: Lazy<Regex> =
                lazy_regex!(r"\A\D*([0-9]{1,9})(\.[0-9]{1,3})?\s*(m)?sec.*\z");
            let text = this
                .find(selector!("#main-container > div.row > div.col-sm-12 > p").child(Text))
                .next()?
                .text();
            let caps = TIMELIMIT.captures(&text)?;
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

        let timelimit = extract_timelimit(self).ok_or_else(ScrapeError::new)?;
        if timelimit == Duration::from_millis(0) {
            return Ok(TestSuite::Unsubmittable);
        }
        match extract_samples(self) {
            None => Ok(BatchSuite::new(timelimit).into()),
            Some(Samples::Batch(cases, matching)) => Ok(BatchSuite::new(timelimit)
                .matching(matching)
                .sample_cases(cases.into_iter(), |i| format!("Sample {}", i + 1))
                .into()),
            Some(Samples::Interactive) => Ok(InteractiveSuite::new(timelimit).into()),
        }
    }

    fn extract_contest_duration(&self) -> ScrapeResult<ContestDuration> {
        fn extract(this: &Document) -> Option<(DateTime<Utc>, DateTime<Utc>)> {
            static FORMAT: &'static str = "%F %T%z";
            let t1 = this.find(selector!("time").child(Text)).nth(0)?.text();
            let t2 = this.find(selector!("time").child(Text)).nth(1)?.text();
            let t1 = DateTime::parse_from_str(&t1, FORMAT).ok()?;
            let t2 = DateTime::parse_from_str(&t2, FORMAT).ok()?;
            Some((t1.with_timezone(&Utc), t2.with_timezone(&Utc)))
        }

        match extract(self) {
            Some((t1, t2)) => Ok(ContestDuration(t1, t2)),
            None => Err(ScrapeError::new()),
        }
    }

    fn extract_submissions(&self) -> ScrapeResult<(vec::IntoIter<Submission>, u32)> {
        let extract = || {
            let num_pages = self
                .find(
                    selector!(
                        "#main-container > div.row > div.col-sm-12 > div.text-center > ul > li > a",
                    )
                    .child(Text),
                )
                .filter_map(|text| text.text().parse::<u32>().ok())
                .max()
                .unwrap_or(1);
            let mut submissions = vec![];
            let pred = selector!(
                "#main-container > div.row > div.col-sm-12 > div.panel-submission
                 > div.table-responsive > table.table > tbody > tr",
            );
            for tr in self.find(pred) {
                let (task_name, task_screen_name) = {
                    static SCREEN_NAME: Lazy<Regex> = lazy_regex!(r"\A(\w+).*\z");
                    static TASK_SCREEN_NAME: Lazy<Regex> =
                        lazy_regex!(r"\A/contests/[\w-]+/tasks/([\w-]+)\z");
                    let a = tr.find(selector!("td > a")).nth(0)?;
                    let task_full_name = a.find(Text).next()?.text();
                    let task_name = SCREEN_NAME.captures(&task_full_name)?[1].to_owned();
                    let task_url = a.attr("href")?;
                    let task_screen_name = TASK_SCREEN_NAME.captures(task_url)?[1].to_owned();
                    (task_name, task_screen_name)
                };
                let lang_name = tr.find(selector!("td")).nth(3)?.find(Text).next()?.text();
                let is_ac = {
                    let status = tr.find(selector!("td > span").child(Text)).nth(0)?.text();
                    status == "AC"
                };
                let detail_url = tr
                    .find(selector!("td.text-center > a"))
                    .flat_map(|a| -> Option<String> {
                        let text = a.find(Text).next()?.text();
                        guard!(["詳細", "Detail"].contains(&text.as_str()));
                        a.attr("href").map(ToOwned::to_owned)
                    })
                    .next()?;
                submissions.push(Submission {
                    task_name,
                    task_screen_name,
                    lang_name,
                    detail_url,
                    is_ac,
                })
            }
            Some((submissions.into_iter(), num_pages))
        };
        extract().ok_or_else(ScrapeError::new)
    }

    fn extract_submitted_code(&self) -> ScrapeResult<String> {
        let submission_code = self
            .find(selector!("#submission-code"))
            .next()
            .ok_or_else(ScrapeError::new)?;
        Ok(submission_code
            .find(Text)
            .next()
            .map(|t| t.text())
            .unwrap_or_else(|| "".to_owned()))
    }

    fn extract_lang_id_by_name(&self, name: &str) -> ScrapeResult<String> {
        for option in self.find(selector!("#select-language > option")) {
            if let Some(text) = option.find(Text).next().map(|n| n.text()) {
                if text == name {
                    return option
                        .attr("value")
                        .map(ToOwned::to_owned)
                        .ok_or_else(ScrapeError::new);
                }
            }
        }
        Err(ScrapeError::new())
    }

    fn extract_langs(&self) -> ScrapeResult<NonEmptyIndexMap<String, String>> {
        let names = self
            .find(selector!("#select-lang option"))
            .map(|option| {
                let name = option.find(Text).next()?.text();
                let id = option.attr("value")?.to_owned();
                Some((name, id))
            })
            .map(|p| p.ok_or_else(ScrapeError::new))
            .collect::<ScrapeResult<IndexMap<_, _>>>()?;
        NonEmptyIndexMap::try_new(names).ok_or_else(ScrapeError::new)
    }
}

#[cfg(test)]
mod tests {
    use crate::errors::ServiceResult;
    use crate::service::atcoder::{Atcoder, AtcoderContest, Extract as _};
    use crate::service::session::{HttpSession, HttpSessionInitParams, UrlBase};
    use crate::service::{self, Contest, Service as _};
    use crate::terminal::{Term, TermImpl};
    use crate::testsuite::TestSuite;

    use failure::Fallible;
    use itertools::Itertools as _;
    use pretty_assertions::assert_eq;
    use tokio::runtime::Runtime;
    use url::Host;

    use std::time::Duration;

    #[test]
    fn it_extracts_task_urls_from_arc001() -> ServiceResult<()> {
        let mut atcoder = start()?;
        let page = atcoder.fetch_tasks_page(&AtcoderContest::new("arc001"))?;
        let urls_and_names = page.extract_task_urls_with_names()?;
        static EXPECTED: &[(&str, &str)] = &[
            ("A", "/contests/arc001/tasks/arc001_1"),
            ("B", "/contests/arc001/tasks/arc001_2"),
            ("C", "/contests/arc001/tasks/arc001_3"),
            ("D", "/contests/arc001/tasks/arc001_4"),
        ];
        assert_eq!(EXPECTED.len(), urls_and_names.len());
        for ((actual_name, actual_url), &(expected_name, expected_url)) in
            urls_and_names.into_iter().zip_eq(EXPECTED)
        {
            assert_eq!(expected_name, actual_name);
            assert_eq!(expected_url, actual_url);
        }
        Ok(())
    }

    #[test]
    fn it_extracts_a_timelimit_from_apg4b_b() -> ServiceResult<()> {
        let mut atcoder = start()?;
        let page = atcoder.get("/contests/apg4b/tasks/APG4b_b").recv_html()?;
        match page.extract_as_suite()? {
            TestSuite::Unsubmittable => Ok(()),
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
            ("F", "dp_f", "ce5fbf4be0003ba508acd37131b4b726"),
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
        let mut atcoder = start()?;
        let contest = AtcoderContest::new(contest);
        let page = atcoder.fetch_tasks_page(&contest)?;
        let urls_and_names = page.extract_task_urls_with_names()?;
        for ((actual_name, actual_url), (expected_name, expected_slug, expected_md5)) in
            urls_and_names.iter().zip_eq(expected.iter())
        {
            let expected_url = format!("/contests/{}/tasks/{}", contest.slug(), expected_slug);
            assert_eq!(actual_name, expected_name);
            assert_eq!(*actual_url, expected_url);
            let problem_page = atcoder.get(actual_url).recv_html()?;
            let actual_suite = problem_page.extract_as_suite()?;
            let actual_md5 = actual_suite.md5()?;
            actual_suite.assert_serialize_correctly()?;
            assert_eq!(format!("{:x}", actual_md5), *expected_md5);
        }
        Ok(())
    }

    #[test]
    fn it_extracts_a_submitted_source_code() -> ServiceResult<()> {
        static URL: &str = "/contests/utpc2011/submissions/2067";
        let mut atcoder = start()?;
        let page = atcoder.get(URL).recv_html()?;
        let code = page.extract_submitted_code()?;
        assert_eq!(
            format!("{:x}", md5::compute(&code)),
            "1d805f5f226cd9d6dd90081a47505b7b",
        );
        Ok(())
    }

    fn start() -> ServiceResult<Atcoder<impl Term>> {
        let client = service::reqwest_async_client(Duration::from_secs(60))?;
        let base = UrlBase::new(Host::Domain("atcoder.jp"), true, None);
        let mut term = TermImpl::null();
        let mut runtime = Runtime::new()?;
        let session = HttpSession::try_new(HttpSessionInitParams {
            out: term.stderr(),
            runtime: &mut runtime,
            robots: true,
            client,
            base: Some(base),
            cookies_path: None,
            api_token_path: None,
            silent: true,
        })?;
        Ok(Atcoder {
            login_retries: Some(0),
            term,
            session,
            runtime,
        })
    }
}
