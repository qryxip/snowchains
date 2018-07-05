use errors::{ServiceError, ServiceResult, SubmitError};
use palette::Palette;
use service::session::{GetPost, HttpSession};
use service::{Contest, Credentials, DownloadProp, RestoreProp, SessionProp, SubmitProp};
use testsuite::{SuiteFilePath, TestSuite};
use util::std_unstable::RemoveItem_ as _RemoveItem_;

use chrono::{DateTime, Local, Utc};
use regex::Regex;
use reqwest::StatusCode;
use select::document::Document;
use select::predicate::{And, Attr, Class, Name, Predicate, Text};

use std::collections::{BTreeMap, HashMap};
use std::time::Duration;
use std::{fmt, vec};

/// Logins to "beta.atcoder.jp".
pub(crate) fn login(sess_prop: &SessionProp) -> ServiceResult<()> {
    AtCoder::start(sess_prop)?.login_if_not(true)
}

/// Participates in a `contest_name`.
pub(crate) fn participate(contest_name: &str, sess_prop: &SessionProp) -> ServiceResult<()> {
    AtCoder::start(sess_prop)?.register_explicitly(&AtcoderContest::new(contest_name))
}

/// Accesses to pages of the problems and extracts pairs of sample input/output
/// from them.
pub(crate) fn download(
    sess_prop: &SessionProp,
    download_prop: DownloadProp<String>,
) -> ServiceResult<()> {
    let download_prop = download_prop.parse_contest().lowerize_problems();
    AtCoder::start(sess_prop)?.download(&download_prop)
}

/// Downloads submitted source codes.
pub(crate) fn restore(
    sess_prop: &SessionProp,
    restore_prop: RestoreProp<String>,
) -> ServiceResult<()> {
    let restore_prop = restore_prop.parse_contest().upperize_problems();
    AtCoder::start(sess_prop)?.restore(&restore_prop)
}

/// Submits a source code.
pub(crate) fn submit(
    sess_prop: &SessionProp,
    submit_prop: SubmitProp<String>,
) -> ServiceResult<()> {
    AtCoder::start(sess_prop)?.submit(&submit_prop.parse_contest())
}

pub(self) struct AtCoder {
    session: HttpSession,
    credentials: Credentials,
}

impl GetPost for AtCoder {
    fn session(&mut self) -> &mut HttpSession {
        &mut self.session
    }
}

impl AtCoder {
    fn start(sess_prop: &SessionProp) -> ServiceResult<Self> {
        let session = sess_prop.start_session()?;
        Ok(Self {
            session,
            credentials: sess_prop.credentials.clone(),
        })
    }

    fn login_if_not(&mut self, eprints_message_if_already_logged_in: bool) -> ServiceResult<()> {
        if self.session.has_cookie() {
            let res = self.get("/settings").acceptable(&[200, 302]).send()?;
            if res.status() == StatusCode::Ok {
                if eprints_message_if_already_logged_in {
                    eprintln!("Already logged in.");
                }
                return Ok(());
            }
        }

        while !self.try_logging_in()? {
            eprintln!("Failed to login. Try again.");
            self.session.clear_cookies()?;
        }
        Ok(())
    }

    fn try_logging_in(&mut self) -> ServiceResult<bool> {
        #[derive(Debug, Serialize)]
        struct Payload<'a> {
            username: &'a str,
            password: &'a str,
            csrf_token: String,
        }

        let csrf_token = self.get("/login").recv_html()?.extract_csrf_token()?;
        let (username, password) = self.credentials.or_ask("Username: ")?;
        let payload = Payload {
            username: &username,
            password: &password,
            csrf_token,
        };
        self.post("/login").send_form(&payload)?;
        let res = self.get("/settings").acceptable(&[200, 302]).send()?;
        let success = res.status() == StatusCode::Ok;
        if success {
            println!("Successfully logged in.");
        } else if self.credentials.not_none() {
            return Err(ServiceError::WrongCredentialsOnTest);
        }
        Ok(success)
    }

    fn register_explicitly(&mut self, contest: &AtcoderContest) -> ServiceResult<()> {
        self.register_if_active_or_explicit(contest, true)
    }

    fn fetch_tasks_page(&mut self, contest: &AtcoderContest) -> ServiceResult<Document> {
        let res = self
            .get(&contest.url_tasks())
            .acceptable(&[200, 302, 404])
            .send()?;
        if res.status() == StatusCode::Ok {
            Ok(Document::from_read(res)?)
        } else {
            self.register_if_active_or_explicit(contest, false)?;
            Ok(self.get(&contest.url_tasks()).recv_html()?)
        }
    }

    fn register_if_active_or_explicit(
        &mut self,
        contest: &AtcoderContest,
        explicit: bool,
    ) -> ServiceResult<()> {
        #[derive(Debug, Serialize)]
        struct Payload {
            csrf_token: String,
        }

        let mut res = self.get(&contest.url_top()).acceptable(&[200, 302]).send()?;
        if res.status() == StatusCode::Found {
            return Err(ServiceError::ContestNotFound(contest.to_string()));
        }
        let page = Document::from(res.text()?.as_str());
        let duration = page.extract_contest_duration()?;
        let status = duration.check_current_status(contest.to_string());
        if !explicit {
            status.raise_if_not_begun()?;
        }
        if explicit || *contest == AtcoderContest::Practice || status.is_active() {
            self.login_if_not(false)?;
            let payload = Payload {
                csrf_token: self
                    .get(&contest.url_top())
                    .recv_html()?
                    .extract_csrf_token()?,
            };
            let url = contest.url_register();
            self.post(&url).send_form(&payload)?;
        }
        Ok(())
    }

    fn download(&mut self, prop: &DownloadProp<AtcoderContest>) -> ServiceResult<()> {
        let DownloadProp {
            contest,
            problems,
            download_dir,
            extension,
            open_browser,
        } = prop;
        let outputs = self
            .fetch_tasks_page(contest)?
            .extract_task_urls_with_names()?
            .into_iter()
            .map(|(name, url)| (name.to_lowercase(), url))
            .filter(|(name, _)| {
                problems.is_none() || problems.as_ref().unwrap().iter().any(|s| s == name)
            })
            .map(|(name, url)| -> ServiceResult<_> {
                let suite = self.get(&url).recv_html()?.extract_as_suite(contest)?;
                let path = SuiteFilePath::new(download_dir, &name, *extension);
                Ok((url, suite, path, name))
            })
            .collect::<ServiceResult<Vec<_>>>()?;
        let mut not_found = match problems.as_ref() {
            None => vec![],
            Some(problems) => problems.iter().collect(),
        };
        for (_, suite, path, name) in &outputs {
            suite.save(path, true)?;
            not_found.remove_item_(&name);
        }
        if !not_found.is_empty() {
            let msg = format!("Not found: {:?}", not_found);
            eprintln!("{}", Palette::Warning.paint(msg));
        }
        if *open_browser {
            self.session
                .open_in_browser(&contest.url_submissions_me(1))?;
            for (url, _, _, _) in &outputs {
                self.session.open_in_browser(url)?;
            }
        }
        Ok(())
    }

    fn restore(&mut self, prop: &RestoreProp<AtcoderContest>) -> ServiceResult<()> {
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

        let RestoreProp {
            contest,
            problems,
            src_paths,
            replacers,
        } = prop;
        let first_page = self.get(&contest.url_submissions_me(1)).recv_html()?;
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
            let code = self.get(&detail_url).recv_html()?.extract_submitted_code()?;
            let lang_id = first_page.extract_lang_id(&lang_name)?;
            if let Some(path_template) = src_paths.get(lang_id.as_str()) {
                let path = path_template.expand(&task_name.to_lowercase())?;
                let code = match replacers.get(lang_id.as_str()) {
                    Some(replacer) => replacer.replace_from_submission(&task_name, &code)?,
                    None => code,
                };
                ::fs::write(&path, code.as_bytes())?;
                results.push((task_name, lang_name, lang_id, path));
            } else {
                let msg = format!("Ignoring {:?} (id: {})", lang_name, lang_id);
                eprintln!("{}", Palette::Warning.paint(msg));
            }
        }
        let mut not_found = match problems.as_ref() {
            None => vec![],
            Some(problems) => problems.iter().collect(),
        };
        for (task_name, lang_name, lang_id, path) in &results {
            println!(
                "{} - {:?} (id: {}): Saved to {}",
                task_name,
                lang_name,
                lang_id,
                path.display()
            );
            not_found.remove_item_(&task_name);
        }
        if !not_found.is_empty() {
            let msg = format!("Not found: {:?}", not_found);
            eprintln!("{}", Palette::Warning.paint(msg));
        }
        println!(
            "Saved {} file{}.",
            results.len(),
            if results.len() > 1 { "s" } else { "" }
        );
        Ok(())
    }

    #[allow(non_snake_case)]
    fn submit(&mut self, prop: &SubmitProp<AtcoderContest>) -> ServiceResult<()> {
        #[derive(Debug, Serialize)]
        struct Payload {
            #[serde(rename = "data.TaskScreenName")]
            dataTaskScreenName: String,
            #[serde(rename = "data.LanguageId")]
            dataLanguageId: String,
            sourceCode: String,
            csrf_token: String,
        }

        let SubmitProp {
            contest,
            problem,
            lang_id,
            src_path,
            replacer,
            open_browser,
            skip_checking_if_accepted,
        } = prop;
        let tasks_page = self.fetch_tasks_page(&contest)?;
        let checks_if_accepted = !skip_checking_if_accepted && *contest != AtcoderContest::Practice
            && {
                let duration = tasks_page.extract_contest_duration()?;
                let status = duration.check_current_status(contest.to_string());
                status.raise_if_not_begun()?;
                status.is_active()
            };
        for (name, url) in tasks_page.extract_task_urls_with_names()? {
            if name.to_uppercase() == problem.to_uppercase() {
                #[cfg_attr(rustfmt, rustfmt_skip)]
                let task_screen_name = {
                    lazy_static! {
                        static ref SCREEN_NAME: Regex = Regex::new(
                            r"\A/contests/[a-z0-9_\-]+/tasks/([a-z0-9_]+)/?\z$").unwrap();
                    }
                    if let Some(caps) = SCREEN_NAME.captures(&url) {
                        caps[1].to_owned()
                    } else {
                        break;
                    }
                };
                if checks_if_accepted {
                    let (submissions, num_pages) = self
                        .get(&contest.url_submissions_me(1))
                        .recv_html()?
                        .extract_submissions()?;
                    if submissions
                        .into_iter()
                        .any(|s| s.task_screen_name == task_screen_name && s.is_ac)
                    {
                        return Err(ServiceError::AlreadyAccepted);
                    }
                    for i in 2..=num_pages {
                        if self
                            .get(&contest.url_submissions_me(i))
                            .recv_html()?
                            .extract_submissions()?
                            .0
                            .any(|s| s.task_screen_name == task_screen_name && s.is_ac)
                        {
                            return Err(ServiceError::AlreadyAccepted);
                        }
                    }
                }
                let source_code = ::fs::read_to_string(src_path)?;
                let source_code = match replacer {
                    Some(replacer) => replacer.replace_as_submission(&problem, &source_code)?,
                    None => source_code,
                };
                let csrf_token = self.get(&url).recv_html()?.extract_csrf_token()?;
                let url = contest.url_submit();
                let payload = Payload {
                    dataTaskScreenName: task_screen_name,
                    dataLanguageId: lang_id.clone(),
                    sourceCode: source_code,
                    csrf_token,
                };
                self.post(&url).send_form(&payload)?;
                if *open_browser {
                    self.session
                        .open_in_browser(&contest.url_submissions_me(1))?;
                }
                return Ok(());
            }
        }
        Err(SubmitError::NoSuchProblem(problem.clone()).into())
    }
}

#[derive(Clone, PartialEq, Eq)]
enum AtcoderContest {
    Practice,
    Apg4b,
    Arc(u32),
    Abc(u32),
    Agc(u32),
    ChokudaiS(u32),
    Other(String),
}

impl AtcoderContest {
    fn new(s: &str) -> Self {
        lazy_static! {
            static ref NAME: Regex = Regex::new(r"\A\s*([a-zA-Z_]+)(\d{3})\s*\z").unwrap();
        }
        if let Some(caps) = NAME.captures(s) {
            let name = caps[1].to_lowercase();
            let number = caps[2].parse::<u32>().unwrap_or(0);
            if name == "abc" {
                return AtcoderContest::Abc(number);
            } else if name == "arc" {
                return AtcoderContest::Arc(number);
            } else if name == "agc" {
                return AtcoderContest::Agc(number);
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

    fn url_top(&self) -> String {
        static BASE: &'static str = "/contests/";
        match self {
            AtcoderContest::Practice => format!("{}practice", BASE),
            AtcoderContest::Apg4b => format!("{}apg4b", BASE),
            AtcoderContest::Abc(n) => format!("{}abc{:>03}", BASE, n),
            AtcoderContest::Arc(n) => format!("{}arc{:>03}", BASE, n),
            AtcoderContest::Agc(n) => format!("{}agc{:>03}", BASE, n),
            AtcoderContest::ChokudaiS(n) => format!("{}chokudai_s{:>03}", BASE, n),
            AtcoderContest::Other(s) => format!("{}{}", BASE, s),
        }
    }

    fn url_tasks(&self) -> String {
        format!("{}/tasks", self.url_top())
    }

    fn url_register(&self) -> String {
        format!("{}/register", self.url_top())
    }

    fn url_submit(&self) -> String {
        format!("{}/submit", self.url_top())
    }

    fn url_submissions_me(&self, page: u32) -> String {
        format!("{}/submissions/me?page={}", self.url_top(), page)
    }
}

impl fmt::Display for AtcoderContest {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AtcoderContest::Practice => write!(f, "practice contest"),
            AtcoderContest::Apg4b => write!(f, "AtCoder Programming Guide for beginners"),
            AtcoderContest::Abc(n) => write!(f, "ABC{:>03}", n),
            AtcoderContest::Arc(n) => write!(f, "ARC{:>03}", n),
            AtcoderContest::Agc(n) => write!(f, "AGC{:>03}", n),
            AtcoderContest::ChokudaiS(n) => write!(f, "Chokudai SpeedRun {:>03}", n),
            AtcoderContest::Other(s) => write!(f, "{}", s),
        }
    }
}

impl Contest for AtcoderContest {
    fn from_string(s: String) -> Self {
        Self::new(&s)
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
        match self {
            ContestStatus::NotBegun(s, t) => Err(ServiceError::ContestNotBegun(s.clone(), *t)),
            _ => Ok(()),
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
    fn extract_csrf_token(&self) -> ServiceResult<String>;
    fn extract_task_urls_with_names(&self) -> ServiceResult<Vec<(String, String)>>;
    fn extract_as_suite(&self, contest: &AtcoderContest) -> ServiceResult<TestSuite>;
    fn extract_contest_duration(&self) -> ServiceResult<ContestDuration>;
    fn extract_submissions(&self) -> ServiceResult<(vec::IntoIter<Submission>, u32)>;
    fn extract_submitted_code(&self) -> ServiceResult<String>;
    fn extract_lang_id(&self, lang_name: &str) -> ServiceResult<String>;
}

impl Extract for Document {
    fn extract_csrf_token(&self) -> ServiceResult<String> {
        self.find(Attr("name", "csrf_token"))
            .next()
            .and_then(|node| node.attr("value").map(ToOwned::to_owned))
            .filter(|token| !token.is_empty())
            .ok_or(ServiceError::Scrape)
    }

    fn extract_task_urls_with_names(&self) -> ServiceResult<Vec<(String, String)>> {
        let extract = || {
            let mut names_and_pathes = vec![];
            let predicate = Attr("id", "main-container")
                .child(And(Name("div"), Class("row")))
                .child(And(Name("div"), Class("col-sm-12")))
                .child(And(Name("div"), Class("panel")))
                .child(And(Name("table"), Class("table")))
                .child(Name("tbody"))
                .child(Name("tr"));
            for node in self.find(predicate) {
                let node = node.find(And(Name("td"), Class("text-center"))).next()?;
                let node = node.find(Name("a")).next()?;
                let url = node.attr("href")?.to_owned();
                let name = node.find(Text).next()?.text();
                info!(
                    "Extracting problem links: Found #main-container>[[omitted]]>a[href={:?}]{{{}}}",
                    url, name
                );
                names_and_pathes.push((name, url));
            }
            if names_and_pathes.is_empty() {
                None
            } else {
                Some(names_and_pathes)
            }
        };
        extract().ok_or(ServiceError::Scrape)
    }

    fn extract_as_suite(&self, contest: &AtcoderContest) -> ServiceResult<TestSuite> {
        enum Samples {
            Simple(Vec<(String, String)>),
            Interactive,
        }

        fn extract_samples(this: &Document, contest: &AtcoderContest) -> Option<Samples> {
            lazy_static! {
                static ref IN_JA: Regex =
                    Regex::new(r"\A[\s\n]*入力例\s*(\d{1,3})+[.\n]*\z").unwrap();
                static ref OUT_JA: Regex =
                    Regex::new(r"\A[\s\n]*出力例\s*(\d{1,3})+[.\n]*\z").unwrap();
                static ref IN_EN: Regex = Regex::new(r"\ASample Input\s?(\d{1,3}).*\z").unwrap();
                static ref OUT_EN: Regex = Regex::new(r"\ASample Output\s?(\d{1,3}).*\z").unwrap();
            }
            // Current style (Japanese)
            let predicate1 = Attr("id", "task-statement")
                .child(And(Name("span"), Class("lang")))
                .child(And(Name("span"), Class("lang-ja")))
                .child(And(Name("div"), Class("part")))
                .child(Name("section"))
                .child(Name("h3").or(Name("pre")));
            // Current style (English)
            let predicate2 = Attr("id", "task-statement")
                .child(And(Name("span"), Class("lang")))
                .child(And(Name("span"), Class("lang-en")))
                .child(And(Name("div"), Class("part")))
                .child(Name("section"))
                .child(Name("h3").or(Name("pre")));
            // ARC019 to ARC057, ABC007 to ABC040
            let predicate3 = Attr("id", "task-statement")
                .child(And(Name("div"), Class("part")))
                .child(Name("section"))
                .child(Name("h3").or(Name("pre")));
            // ARC002 to ARC018, ABC001 to ABC006
            let predicate4 = Attr("id", "task-statement")
                .child(And(Name("div"), Class("part")))
                .child(Name("h3").or(Name("pre")))
                .or(Attr("id", "task-statement")
                    .child(And(Name("div"), Class("part")))
                    .child(Name("section"))
                    .child(Name("pre")));
            // ARC001
            let predicate5 = Attr("id", "task-statement")
                .child(Name("h3").or(Name("pre")))
                .or(Attr("id", "task-statement")
                    .child(Name("section"))
                    .child(Name("pre")));
            // ABC041
            let predicate6 = Attr("id", "task-statement")
                .child(Name("section"))
                .child(Name("h3").or(Name("pre")));
            // practice contest (Japanese)
            let predicate7 = Attr("id", "task-statement")
                .child(And(Name("span"), Class("lang")))
                .child(And(Name("span"), Class("lang-ja")))
                .child(And(Name("div"), Class("part")))
                .child(Name("h3"))
                .or(Attr("id", "task-statement")
                    .child(And(Name("span"), Class("lang")))
                    .child(And(Name("span"), Class("lang-ja")))
                    .child(And(Name("div"), Class("part")))
                    .child(Name("section"))
                    .child(Name("pre")));
            static INFO1: &str =
                "#task-statement>span.lang>span.lang-ja>div.part>section>h3{{...}}+pre{{...}}";
            static INFO2: &str =
                "#task-statement>span.lang>span.lang-en>div.part>section>h3{{...}}+pre{{...}}";
            static INFO3: &str = "#task-statement>div.part>section>h3{{...}}+pre{{...}}";
            static INFO4: &str = "#task-statement>div.part>h3{{...}}+section>pre{{...}}";
            static INFO5: &str = "#task-statement>h3{{...}}+section>pre{{...}}";
            static INFO6: &str = "#task-statement>section>h3{{...}}+pre{{...}}";
            static INFO7: &str =
            "#task-statement>span.lang>span.lang-ja.div.part>section>h3{{...}}+section>pre{{...}}";
            let on_current = || {
                try_extract_samples(this, predicate1, &IN_JA, &OUT_JA, INFO1)
                    .or_else(|| try_extract_samples(this, predicate2, &IN_EN, &OUT_EN, INFO2))
            };
            let on_arc019_to_arc057 = || {
                try_extract_samples(this, predicate3, &IN_JA, &OUT_JA, INFO3)
                    .or_else(|| try_extract_samples(this, predicate4, &IN_JA, &OUT_JA, INFO4))
                    .or_else(|| try_extract_samples(this, predicate5, &IN_JA, &OUT_JA, INFO5))
                    .or_else(|| try_extract_samples(this, predicate6, &IN_JA, &OUT_JA, INFO6))
            };
            let on_arc002_to_arc018 = || {
                try_extract_samples(this, predicate4, &IN_JA, &OUT_JA, INFO4)
                    .or_else(|| try_extract_samples(this, predicate3, &IN_JA, &OUT_JA, INFO3))
                    .or_else(|| try_extract_samples(this, predicate5, &IN_JA, &OUT_JA, INFO5))
                    .or_else(|| try_extract_samples(this, predicate6, &IN_JA, &OUT_JA, INFO6))
            };
            let on_arc001 = || try_extract_samples(this, predicate5, &IN_JA, &OUT_JA, INFO5);
            let on_abc041 = || try_extract_samples(this, predicate6, &IN_JA, &OUT_JA, INFO6);
            let on_practice = || try_extract_samples(this, predicate7, &IN_JA, &OUT_JA, INFO7);
            match *contest {
                AtcoderContest::Arc(n) if 19 <= n && n <= 57 => on_arc019_to_arc057(),
                AtcoderContest::Abc(n) if 7 <= n && n <= 40 => on_arc019_to_arc057(),
                AtcoderContest::Arc(n) if 2 <= n && n <= 18 => on_arc002_to_arc018(),
                AtcoderContest::Abc(n) if 1 <= n && n <= 6 => on_arc002_to_arc018(),
                AtcoderContest::Arc(1) => on_arc001(),
                AtcoderContest::Abc(41) => on_abc041(),
                AtcoderContest::Practice => on_practice(),
                _ => on_current(),
            }
        }

        fn try_extract_samples<P: Predicate>(
            this: &Document,
            predicate_for_h3_or_pre_or_section: P,
            re_input: &Regex,
            re_output: &Regex,
            info: &'static str,
        ) -> Option<Samples> {
            for strong in this.find(Attr("id", "task-statement").descendant(Name("strong"))) {
                let text = strong.text();
                for word in &["インタラクティブ", "Interactive"] {
                    if text.find(word).is_some() {
                        info!("Extracting sample cases: Found word {:?}", word);
                        return Some(Samples::Interactive);
                    }
                }
            }
            info!("Extracting sample cases: Searching {}...", info);
            let mut inputs = BTreeMap::<u8, _>::new();
            let mut outputs = BTreeMap::<u8, _>::new();
            let mut next = None;
            for node in this.find(predicate_for_h3_or_pre_or_section) {
                if node.name() == Some("h3") {
                    if let Some(caps) = re_input.captures(&node.text()) {
                        next = Some((true, caps[1].parse().unwrap()));
                        info!("Extracting sample cases: Found h3{{{:?}}}", node.text());
                    } else if let Some(caps) = re_output.captures(&node.text()) {
                        next = Some((false, caps[1].parse().unwrap()));
                        info!("Extracting sample cases: Found h3{{{:?}}}", node.text());
                    } else {
                        info!("Extracting sample cases: Skipping h3{{{:?}}}", node.text());
                    }
                } else if [Some("pre"), Some("section")].contains(&node.name()) {
                    if let Some((is_input, n)) = next {
                        if is_input {
                            info!(
                            "Extracting sample cases: Extracted Input {}: {:?}, from pre{{”}}",
                            n,
                            node.text()
                        );
                            inputs.insert(n, node.text());
                        } else {
                            info!(
                            "Extracting sample cases: Extracted Output {}: {:?} from pre{{”}}",
                            n,
                            node.text()
                        );
                            outputs.insert(n, node.text());
                        }
                    } else {
                        info!("Extracting sample cases: Skipping pre{{{:?}}}", node.text());
                    }
                    next = None;
                } else {
                    unreachable!(
                        r#"Node name should be "h3" "pre", or "section", got {:?}"#,
                        node.name()
                    );
                }
            }
            let mut samples = vec![];
            for (i, input) in inputs {
                if let Some(output) = outputs.remove(&i) {
                    samples.push((input, output));
                }
            }
            if samples.is_empty() {
                None
            } else {
                Some(Samples::Simple(samples))
            }
        }

        fn extract_timelimit(this: &Document) -> Option<Duration> {
            lazy_static! {
                static ref TIMELIMIT: Regex = Regex::new(r"\A\D*(\d+)\s*(m)?sec.*\z").unwrap();
            }
            let predicate = Attr("id", "main-container")
                .child(And(Name("div"), Class("row")))
                .child(And(Name("div"), Class("col-sm-12")))
                .child(Name("p"))
                .child(Text);
            let text = this.find(predicate).next()?.text();
            info!(
                "Extracting timelimit: Found #main-container>div.row>div.col-sm-12>p{{{:?}}}",
                text
            );
            let caps = TIMELIMIT.captures(&text)?;
            let timelimit =
                if caps.get(2).is_some() { 1 } else { 1000 } * caps[1].parse::<u64>().ok()?;
            info!(
                "Extracting timelimit: Successfully extracted: {}ms",
                timelimit
            );
            Some(Duration::from_millis(timelimit))
        }

        let timelimit = extract_timelimit(self).ok_or_else(|| ServiceError::Scrape)?;
        if timelimit == Duration::from_millis(0) {
            return Ok(TestSuite::Unsubmittable);
        }
        match extract_samples(self, contest) {
            Some(Samples::Simple(samples)) => Ok(TestSuite::simple(timelimit, None, None, samples)),
            Some(Samples::Interactive) => Ok(TestSuite::interactive(timelimit)),
            None => {
                warn!("Extracting sample cases: Could not extract sample cases");
                Ok(TestSuite::simple(timelimit, None, None, vec![]))
            }
        }
    }

    fn extract_contest_duration(&self) -> ServiceResult<ContestDuration> {
        fn extract(this: &Document) -> Option<(String, String)> {
            let predicate = Name("time").child(Text);
            let t1 = this.find(predicate).nth(0)?.text();
            info!("Extracting contest duration: Found time{{{}}}", t1);
            let t2 = this.find(predicate).nth(1)?.text();
            info!("Extracting contest duration: Found time{{{}}}", t2);
            Some((t1, t2))
        }

        match extract(self) {
            Some((t1, t2)) => {
                static FORMAT: &'static str = "%F %T%z";
                let t1 = DateTime::parse_from_str(&t1, FORMAT)?.with_timezone(&Utc);
                let t2 = DateTime::parse_from_str(&t2, FORMAT)?.with_timezone(&Utc);
                Ok(ContestDuration(t1, t2))
            }
            None => Err(ServiceError::Scrape),
        }
    }

    fn extract_submissions(&self) -> ServiceResult<(vec::IntoIter<Submission>, u32)> {
        let extract = || {
            let num_pages = {
                let predicate = Attr("id", "main-container")
                    .child(Name("div").and(Class("row")))
                    .child(Name("div").and(Class("text-center")))
                    .child(Name("ul").and(Class("pagination")))
                    .child(Name("li"));
                let num_pages = self.find(predicate).count() as u32;
                let suf = if num_pages > 1 { "s" } else { "" };
                info!("Extracting submissions: Found {} page{}", num_pages, suf);
                num_pages
            };
            let mut submissions = vec![];
            let predicate = Attr("id", "main-container")
                .child(And(Name("div"), Class("row")))
                .child(And(Name("div"), Class("col-sm-12")))
                .child(And(Name("div"), Class("panel-submission")))
                .child(And(Name("div"), Class("table-responsive")))
                .child(And(Name("table"), Class("table")))
                .child(Name("tbody"))
                .child(Name("tr"));
            for tr in self.find(predicate) {
                info!("Extracting submissions: Found #main-container>[[omitted]]>tr>");
                let (task_name, task_screen_name) = {
                    lazy_static! {
                        static ref SCREEN_NAME: Regex = Regex::new(r"\A(\w+).*\z").unwrap();
                        static ref TASK_SCREEN_NAME: Regex =
                            Regex::new(r"\A/contests/[\w-]+/tasks/([\w-]+)\z").unwrap();
                    }
                    let a = tr.find(Name("td").child(Name("a"))).nth(0)?;
                    let task_full_name = a.find(Text).next()?.text();
                    let task_name = SCREEN_NAME.captures(&task_full_name)?[1].to_owned();
                    let task_url = a.attr("href")?;
                    let task_screen_name = TASK_SCREEN_NAME.captures(task_url)?[1].to_owned();
                    info!(
                        "Extracting submissions: Found {:?}, {:?} from tr>td>a[href={:?}]{{{:?}}}",
                        task_name, task_screen_name, task_url, task_full_name,
                    );
                    (task_name, task_screen_name)
                };
                let lang_name = tr.find(Name("td")).nth(3)?.find(Text).next()?.text();
                let is_ac = {
                    let pred = Name("td").child(Name("span")).child(Text);
                    let status = tr.find(pred).nth(0)?.text();
                    info!("Extracting submissions: Found tr>td>span>{:?}", status);
                    status == "AC"
                };
                let detail_url = tr
                    .find(Name("td").and(Class("text-center")).child(Name("a")))
                    .flat_map(|a| -> Option<String> {
                        let text = a.find(Text).next()?.text();
                        if text != "詳細" && text != "Detail" {
                            return None;
                        }
                        let href = a.attr("href")?.to_owned();
                        info!("Extracting submissions: Found tr>td>a[href={:?}]", href);
                        Some(href)
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
        extract().ok_or_else(|| ServiceError::Scrape)
    }

    fn extract_submitted_code(&self) -> ServiceResult<String> {
        let extract = || {
            let predicate = Attr("id", "submission-code").child(Text);
            let code = self.find(predicate).next()?.text();
            info!(
                "Extracting submitted code: Found {} byte{} of code from #submission-code",
                code.len(),
                if code.len() > 1 { "s" } else { "" },
            );
            Some(code)
        };
        extract().ok_or_else(|| ServiceError::Scrape)
    }

    fn extract_lang_id(&self, lang_name: &str) -> ServiceResult<String> {
        let predicate = Attr("id", "select-language").child(Name("option"));
        for option in self.find(predicate) {
            if let Some(text) = option.find(Text).next().map(|n| n.text()) {
                if text == lang_name {
                    return option
                        .attr("value")
                        .map(ToOwned::to_owned)
                        .ok_or_else(|| ServiceError::Scrape);
                }
            }
        }
        Err(ServiceError::Scrape)
    }
}

#[cfg(test)]
mod tests {
    use errors::SessionResult;
    use service::atcoder::{AtCoder, AtcoderContest, Extract as _Extract};
    use service::session::{GetPost as _GetPost, HttpSession, UrlBase};
    use service::{self, Credentials};
    use testsuite::TestSuite;

    use env_logger;
    use url::Host;

    use std::borrow::Borrow;
    use std::time::Duration;

    #[test]
    #[ignore]
    fn it_extracts_task_urls_from_arc001() {
        let _ = env_logger::try_init();
        let mut atcoder = start().unwrap();
        let page = atcoder
            .fetch_tasks_page(&AtcoderContest::new("arc001"))
            .unwrap();
        let urls_and_names = page.extract_task_urls_with_names().unwrap();
        static EXPECTED: &[(&str, &str)] = &[
            ("A", "/contests/arc001/tasks/arc001_1"),
            ("B", "/contests/arc001/tasks/arc001_2"),
            ("C", "/contests/arc001/tasks/arc001_3"),
            ("D", "/contests/arc001/tasks/arc001_4"),
        ];
        assert_eq!(EXPECTED.len(), urls_and_names.len());
        for ((actual_name, actual_url), &(expected_name, expected_url)) in
            urls_and_names.into_iter().zip(EXPECTED)
        {
            assert_eq!(expected_name, actual_name);
            assert_eq!(expected_url, actual_url);
        }
    }

    #[test]
    #[ignore]
    fn it_extracts_a_timelimit_from_apg4b_b() {
        let _ = env_logger::try_init();
        let mut atcoder = start().unwrap();
        let page = atcoder
            .get("/contests/apg4b/tasks/APG4b_b")
            .recv_html()
            .unwrap();
        match page
            .extract_as_suite(&AtcoderContest::new("apg4b"))
            .unwrap()
        {
            TestSuite::Unsubmittable => {}
            suite => panic!("Got {:?}", suite),
        }
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    #[test]
    #[ignore]
    fn it_extracts_timelimits_and_sample_cases_from_arc001() {
        static A: &[(&str, &str)] = &[
            ("9\n131142143\n", "4 1\n"),
            ("20\n12341234123412341234\n", "5 5\n"),
            ("4\n1111\n", "4 0\n"),
        ];
        static B: &[(&str, &str)] = &[("7 34\n", "5\n"), ("19 28\n", "2\n"), ("10 10\n", "0\n")];
        static C: &[(&str, &str)] = &[
            ("........\n........\n.......Q\n........\n..Q.....\n........\n.Q......\n........\n",
             "Q.......\n....Q...\n.......Q\n.....Q..\n..Q.....\n......Q.\n.Q......\n...Q....\n"),
            (".....Q..\n.Q......\n........\n........\n........\nQ.......\n........\n........\n",
             "No Answer\n"),
        ];
        static D: &[(&str, &str)] = &[
            ("7\n3 3\n2 5\n4 6\n2 3\n3 6\n3 4\n4 6\n2 5\n1 5\n", "8.22677276241436\n"),
            ("5\n3 3\n0 5\n0 5\n0 5\n0 5\n0 5\n0 5\n", "5\n"),
        ];
        let expected = [
            ("A", "/contests/arc001/tasks/arc001_1", 2000, A),
            ("B", "/contests/arc001/tasks/arc001_2", 2000, B),
            ("C", "/contests/arc001/tasks/arc001_3", 2000, C),
            ("D", "/contests/arc001/tasks/arc001_4", 2000, D),
        ];
        let _ = env_logger::try_init();
        test_sample_extraction("arc001", &expected);
    }

    #[test]
    #[ignore]
    fn it_extracts_timelimits_and_sample_cases_from_arc002() {
        static A: &[(&str, &str)] = &[
            ("1001\n", "NO\n"),
            ("2012\n", "YES\n"),
            ("2100\n", "NO\n"),
            ("2000\n", "YES\n"),
        ];
        static B: &[(&str, &str)] = &[
            ("2012/05/02\n", "2013/01/01\n"),
            ("2020/05/02\n", "2020/05/02\n"),
            ("2088/02/28\n", "2088/02/29\n"),
        ];
        static C: &[(&str, &str)] = &[
            ("4\nABXY\n", "2\n"),
            ("13\nABABABABXBXBX\n", "7\n"),
            ("8\nAABBAABB\n", "4\n"),
        ];
        static D: &[(&str, &str)] = &[
            ("3 10\n..o.o.xxx.\n...o.xo.x.\no.xxo..x..\n", "o\n"),
            ("3 5\n..x..\n.o...\n...x.\n", "x\n"),
        ];
        let expected = [
            ("A", "/contests/arc002/tasks/arc002_1", 2000, A),
            ("B", "/contests/arc002/tasks/arc002_2", 2000, B),
            ("C", "/contests/arc002/tasks/arc002_3", 2000, C),
            ("D", "/contests/arc002/tasks/arc002_4", 2000, D),
        ];
        let _ = env_logger::try_init();
        test_sample_extraction("arc002", &expected);
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    #[test]
    #[ignore]
    fn it_extracts_timelimits_and_sample_cases_from_arc019() {
        static A: &[(&str, &str)] = &[
            ("1Z0\n", "120\n"),
            ("4ZD6O\n", "42060\n"),
            ("BI9Z\n", "8192\n"),
        ];
        static B: &[(&str, &str)] = &[
            ("ARC\n", "73\n"),
            ("S\n", "0\n"),
            ("NOLEMONNOMELON\n", "350\n"),
        ];
        static C: &[(&str, &str)] = &[
            ("5 7 3\nGET..ET\n..T....\n.TEST..\n.E.T.ET\n...ETC.\n", "19\n"),
            ("5 7 2\nGET..ET\n..T....\n.TEST..\n.E.T.ET\n...ETC.\n", "21\n"),
            ("5 7 1\nGET..ET\n..T....\n.TEST..\n.E.T.ET\n...ETC.\n", "-1\n"),
            ("6 35 4\nT...TT.....TT...TTT...TTT..TTG.....\n..T..T.TTT.T..T..E..T..E...TTT.TTT.\n\
              .TTT.T.....E.TTTTT.TTT.TTT.TTT.....\n.....T.TT.TT.TTTTT.TTT.TTT.TTTTTTT.\n\
              .TTT.T.TT..T..T..S..T..TTT.TTTTTTT.\n.CTT.E.TTT.TT...TTT...TT.....E.....\n",
             "94\n"),
        ];
        static D: &[(&str, &str)] = &[];
        let expected = [
            ("A", "/contests/arc019/tasks/arc019_1", 2000, A),
            ("B", "/contests/arc019/tasks/arc019_2", 2000, B),
            ("C", "/contests/arc019/tasks/arc019_3", 2000, C),
            ("D", "/contests/arc019/tasks/arc019_4", 2000, D),
        ];
        let _ = env_logger::try_init();
        test_sample_extraction("arc019", &expected);
    }

    #[test]
    #[ignore]
    fn it_extracts_timelimits_and_sample_cases_from_arc058() {
        static C: &[(&str, &str)] = &[
            ("1000 8\n1 3 4 5 6 7 8 9\n", "2000\n"),
            ("9999 1\n0\n", "9999\n"),
        ];
        static D: &[(&str, &str)] = &[
            ("2 3 1 1\n", "2\n"),
            ("10 7 3 4\n", "3570\n"),
            ("100000 100000 99999 99999\n", "1\n"),
            ("100000 100000 44444 55555\n", "738162020\n"),
        ];
        static E: &[(&str, &str)] = &[
            ("3 5 7 5\n", "1\n"),
            ("4 5 7 5\n", "34\n"),
            ("37 4 2 3\n", "863912418\n"),
            ("40 5 7 5\n", "562805100\n"),
        ];
        static F: &[(&str, &str)] = &[
            ("3 7\nat\ncoder\ncodar\n", "atcodar\n"),
            ("3 7\ncoder\ncodar\nat\n", "codarat\n"),
            ("4 13\nkyuri\nnamida\nzzzzzzz\naaaaaa\n", "namidazzzzzzz\n"),
        ];
        let expected = [
            ("C", "/contests/arc058/tasks/arc058_a", 2000, C),
            ("D", "/contests/arc058/tasks/arc058_b", 2000, D),
            ("E", "/contests/arc058/tasks/arc058_c", 4000, E),
            ("F", "/contests/arc058/tasks/arc058_d", 5000, F),
        ];
        let _ = env_logger::try_init();
        test_sample_extraction("arc058", &expected);
    }

    #[test]
    #[ignore]
    fn it_extracts_timelimits_and_sample_cases_from_abc041() {
        static A: &[(&str, &str)] = &[
            ("atcoder\n3\n", "c\n"),
            ("beginner\n1\n", "b\n"),
            ("contest\n7\n", "t\n"),
            ("z\n1\n", "z\n"),
        ];
        static B: &[(&str, &str)] = &[
            ("2 3 4\n", "24\n"),
            ("10000 1000 100\n", "1000000000\n"),
            ("100000 1 100000\n", "999999937\n"),
            ("1000000000 1000000000 1000000000\n", "999999664\n"),
        ];
        static C: &[(&str, &str)] = &[
            ("3\n140 180 160\n", "2\n3\n1\n"),
            ("2\n1000000000 1\n", "1\n2\n"),
            ("8\n3 1 4 15 9 2 6 5\n", "4\n5\n7\n8\n3\n1\n6\n2\n"),
        ];
        static D: &[(&str, &str)] = &[
            ("3 2\n2 1\n2 3\n", "2\n"),
            ("5 5\n1 2\n2 3\n3 5\n1 4\n4 5\n", "3\n"),
            ("16 1\n1 2\n", "10461394944000\n"),
        ];
        let expected = [
            ("A", "/contests/abc041/tasks/abc041_a", 2000, A),
            ("B", "/contests/abc041/tasks/abc041_b", 2000, B),
            ("C", "/contests/abc041/tasks/abc041_c", 2000, C),
            ("D", "/contests/abc041/tasks/abc041_d", 3000, D),
        ];
        let _ = env_logger::try_init();
        test_sample_extraction("abc041", &expected);
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    #[test]
    #[ignore]
    fn it_extracts_timelimits_and_sample_cases_from_chokudai_s001() {
        static A: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "5\n"),
            ("6\n1 2 3 4 5 6\n", "6\n"),
            ("7\n7 6 5 4 3 2 1\n", "7\n"),
            ("20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n", "20\n"),
        ];
        static B: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "15\n"),
            ("6\n1 2 3 4 5 6\n", "21\n"),
            ("7\n7 6 5 4 3 2 1\n", "28\n"),
            ("20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n", "210\n"),
        ];
        static C: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "3,1,5,4,2\n"),
            ("6\n1 2 3 4 5 6\n", "1,2,3,4,5,6\n"),
            ("7\n7 6 5 4 3 2 1\n", "7,6,5,4,3,2,1\n"),
            ("20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n",
             "19,11,10,7,8,9,17,18,20,4,3,15,16,1,5,14,6,2,13,12\n"),
        ];
        static D: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "1 2 3 4 5\n"),
            ("6\n1 2 3 4 5 6\n", "1 2 3 4 5 6\n"),
            ("7\n7 6 5 4 3 2 1\n", "1 2 3 4 5 6 7\n"),
            ("20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n",
             "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20\n"),
        ];
        static E: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "2\n"),
            ("6\n1 2 3 4 5 6\n", "1\n"),
            ("7\n7 6 5 4 3 2 1\n", "7\n"),
            ("20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n", "14\n"),
        ];
        static F: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "2\n"),
            ("6\n1 2 3 4 5 6\n", "6\n"),
            ("7\n7 6 5 4 3 2 1\n", "1\n"),
            ("20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n", "2\n"),
        ];
        static G: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "31542\n"),
            ("6\n1 2 3 4 5 6\n", "123456\n"),
            ("7\n7 6 5 4 3 2 1\n", "7654321\n"),
            ("20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n", "370453866\n"),
        ];
        static H: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "2\n"),
            ("6\n1 2 3 4 5 6\n", "6\n"),
            ("7\n7 6 5 4 3 2 1\n", "1\n"),
            ("20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n", "6\n"),
        ];
        static I: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "1\n"),
            ("6\n1 2 3 4 5 6\n", "2\n"),
            ("7\n7 6 5 4 3 2 1\n", "2\n"),
            ("20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n", "3\n"),
        ];
        static J: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "5\n"),
            ("6\n1 2 3 4 5 6\n", "0\n"),
            ("7\n7 6 5 4 3 2 1\n", "21\n"),
            ("20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n", "114\n"),
        ];
        static K: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "54\n"),
            ("6\n1 2 3 4 5 6\n", "1\n"),
            ("7\n7 6 5 4 3 2 1\n", "5040\n"),
            ("20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n", "542869439\n"),
        ];
        static L: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "YES\n"),
            ("6\n1 2 3 4 5 6\n", "YES\n"),
            ("7\n7 6 5 4 3 2 1\n", "YES\n"),
            ("20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n", "YES\n"),
        ];
        let expected = [
            ("A", "/contests/chokudai_s001/tasks/chokudai_S001_a", 2000, A),
            ("B", "/contests/chokudai_s001/tasks/chokudai_S001_b", 2000, B),
            ("C", "/contests/chokudai_s001/tasks/chokudai_S001_c", 2000, C),
            ("D", "/contests/chokudai_s001/tasks/chokudai_S001_d", 2000, D),
            ("E", "/contests/chokudai_s001/tasks/chokudai_S001_e", 2000, E),
            ("F", "/contests/chokudai_s001/tasks/chokudai_S001_f", 2000, F),
            ("G", "/contests/chokudai_s001/tasks/chokudai_S001_g", 2000, G),
            ("H", "/contests/chokudai_s001/tasks/chokudai_S001_h", 2000, H),
            ("I", "/contests/chokudai_s001/tasks/chokudai_S001_i", 2000, I),
            ("J", "/contests/chokudai_s001/tasks/chokudai_S001_j", 2000, J),
            ("K", "/contests/chokudai_s001/tasks/chokudai_S001_k", 2000, K),
            ("L", "/contests/chokudai_s001/tasks/chokudai_S001_l", 2000, L),
        ];
        let _ = env_logger::try_init();
        test_sample_extraction("chokudai_s001", &expected);
    }

    fn test_sample_extraction(contest: &str, expected: &[(&str, &str, u64, &[(&str, &str)])]) {
        let mut atcoder = start().unwrap();
        let contest = AtcoderContest::new(contest);
        let page = atcoder.fetch_tasks_page(&contest).unwrap();
        let urls_and_names = page.extract_task_urls_with_names().unwrap();
        for (
            (actual_name, actual_url),
            (expected_name, expected_url, expected_timelimit, expected_samples),
        ) in urls_and_names.iter().zip(expected.iter())
        {
            assert_eq!(expected_name, actual_name);
            assert_eq!(expected_url, actual_url);
            let problem_page = atcoder.get(&actual_url).recv_html().unwrap();
            let expected_timelimit = Duration::from_millis(*expected_timelimit);
            let expected_suite =
                TestSuite::simple(expected_timelimit, None, None, own_pairs(expected_samples));
            let actual_suite = problem_page.extract_as_suite(&contest).unwrap();
            assert_eq!(expected_suite, actual_suite);
        }
    }

    fn own_pairs<O: Borrow<B>, B: ToOwned<Owned = O> + ?Sized>(pairs: &[(&B, &B)]) -> Vec<(O, O)> {
        pairs
            .iter()
            .map(|(l, r)| ((*l).to_owned(), (*r).to_owned()))
            .collect()
    }

    #[test]
    #[ignore]
    fn it_extracts_a_submitted_source_code() {
        static URL: &str = "/contests/utpc2011/submissions/2067";
        static EXPECTED_CODE: &str =
            "import java.util.*;\n\
             import java.math.*;\n\
             import static java.lang.Math.*;\n\
             import static java.util.Arrays.*;\n\
             import static java.util.Collections.*;\n\
             public class Main{\n\
             \tpublic static void main(String[] args) {\n\
             \t\tnew Main().run();\n\
             \t}\n\
             \tScanner sc = new Scanner(System.in);\n\
             \tvoid run() {\n\
             \t\tint m=sc.nextInt(),n=sc.nextInt();\n\
             \t\tint[] as=new int[m];\n\
             \t\tfor(int i=0;i<m;i++)for(int j=0;j<n;j++)as[i]+=sc.nextInt();\n\
             \t\t\tsort(as);\n\
             \t\t\tSystem.out.println(as[m-1]);\n\
             \t}\n\
             }\n\
             ";
        let _ = env_logger::try_init();
        let mut atcoder = start().unwrap();
        let page = atcoder.get(URL).recv_html().unwrap();
        let code = page.extract_submitted_code().unwrap();
        assert_eq!(EXPECTED_CODE, code);
    }

    fn start() -> SessionResult<AtCoder> {
        let client = service::reqwest_client(Duration::from_secs(10))?;
        let base = UrlBase::new(Host::Domain("beta.atcoder.jp"), true, None);
        let session = HttpSession::new(client, base, None)?;
        Ok(AtCoder {
            session,
            credentials: Credentials::None,
        })
    }
}
