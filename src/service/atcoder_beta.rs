use errors::{ServiceErrorKind, ServiceResult};
use service::OpenInBrowser;
use testsuite::{SuiteFileExtension, SuiteFilePath, TestSuite};

use chrono::{DateTime, Local, Utc};
use httpsession::HttpSession;
use regex::Regex;
use select::document::Document;
use select::node::Node;
use select::predicate::{And, Attr, Class, Name, Predicate, Text};

use std::fmt;
use std::io::Read;
use std::path::Path;

/// Logins to "beta.atcoder.jp".
pub fn login() -> ServiceResult<()> {
    AtCoderBeta::start()?.login_if_not(true)
}

/// Participates in a `contest_name`.
pub fn participate(contest_name: &str) -> ServiceResult<()> {
    AtCoderBeta::start()?.register_explicitly(&Contest::new(contest_name))
}

/// Accesses to pages of the problems and extracts pairs of sample input/output
/// from them.
pub fn download(
    contest_name: &str,
    dir_to_save: &Path,
    extension: SuiteFileExtension,
    open_browser: bool,
) -> ServiceResult<()> {
    AtCoderBeta::start()?.download(
        &Contest::new(contest_name),
        dir_to_save,
        extension,
        open_browser,
    )
}

/// Submits a source code.
pub fn submit(
    contest_name: &str,
    task: &str,
    lang_id: u32,
    src_path: &Path,
    open_browser: bool,
    skip_checking_if_accepted: bool,
) -> ServiceResult<()> {
    AtCoderBeta::start()?.submit(
        &Contest::new(contest_name),
        task,
        lang_id,
        src_path,
        open_browser,
        skip_checking_if_accepted,
    )
}

custom_derive! {
    #[derive(NewtypeDeref, NewtypeDerefMut)]
    struct AtCoderBeta(HttpSession);
}

impl AtCoderBeta {
    fn start() -> ServiceResult<Self> {
        let session = super::start_session("atcoderbeta", "beta.atcoder.jp")?;
        Ok(AtCoderBeta(session))
    }

    fn login_if_not(&mut self, eprints_message_if_already_logged_in: bool) -> ServiceResult<()> {
        if self.has_cookie() {
            let response = self.get_expecting("/settings", &[200, 302])?;
            if response.status().as_u16() == 200 {
                if eprints_message_if_already_logged_in {
                    eprintln!("Already logged in.");
                }
                return Ok(());
            }
        }
        while !self.try_logging_in()? {
            eprintln!("Failed to login. Try again.");
            self.clear_cookies()?;
        }
        Ok(())
    }

    fn try_logging_in(&mut self) -> ServiceResult<bool> {
        #[derive(Serialize)]
        struct Payload {
            username: String,
            password: String,
            csrf_token: String,
        }

        let csrf_token = extract_csrf_token(&Document::from_read(self.get("/login")?)?)?;
        let (username, password) = super::ask_username_and_password("Username: ")?;
        let payload = Payload {
            username,
            password,
            csrf_token,
        };
        self.post_urlencoded("/login", &payload, &[302], None)?;
        let response = self.get_expecting("/settings", &[200, 302])?;
        let success = response.status().as_u16() == 200;
        if success {
            println!("Successfully logged in.");
        }
        Ok(success)
    }

    fn register_explicitly(&mut self, contest: &Contest) -> ServiceResult<()> {
        self.register_if_active_or_explicit(contest, true)
    }

    fn fetch_tasks_page(&mut self, contest: &Contest) -> ServiceResult<Document> {
        let response = self.get_expecting(&contest.url_tasks(), &[200, 302, 404])?;
        if response.status().as_u16() == 200 {
            Ok(Document::from_read(response)?)
        } else {
            self.register_if_active_or_explicit(contest, false)?;
            Ok(Document::from_read(self.get(&contest.url_tasks())?)?)
        }
    }

    fn register_if_active_or_explicit(
        &mut self,
        contest: &Contest,
        explicit: bool,
    ) -> ServiceResult<()> {
        #[derive(Serialize)]
        struct Payload {
            csrf_token: String,
        }

        let response = self.get_expecting(&contest.url_top(), &[200, 302])?;
        if response.status().as_u16() == 302 {
            bail!(ServiceErrorKind::ContestNotFound(contest.to_string()));
        }
        let page = Document::from_read(response)?;
        let duration = extract_contest_duration(&page)?;
        let status = duration.check_current_status(contest.to_string());
        if !explicit {
            status.raise_if_not_begun()?;
        }
        if explicit || *contest == Contest::Practice || status.is_active() {
            self.login_if_not(false)?;
            let page = Document::from_read(self.get(&contest.url_top())?)?;
            let payload = Payload {
                csrf_token: extract_csrf_token(&page)?,
            };
            let url = contest.url_register();
            self.post_urlencoded(&url, &payload, &[302], None)?;
        }
        Ok(())
    }

    fn download(
        &mut self,
        contest: &Contest,
        dir_to_save: &Path,
        extension: SuiteFileExtension,
        open_browser: bool,
    ) -> ServiceResult<()> {
        let tasks_page = self.fetch_tasks_page(contest)?;
        let outputs = extract_task_urls_with_names(&tasks_page)?
            .into_iter()
            .map(|(name, url)| -> ServiceResult<_> {
                let suite = extract_cases(self.get(&url)?, &contest.style())?;
                let path = SuiteFilePath::new(dir_to_save, name.to_lowercase(), extension);
                Ok((url, suite, path))
            })
            .collect::<ServiceResult<Vec<_>>>()?;
        for &(_, ref suite, ref path) in &outputs {
            suite.save(path, true)?;
        }
        if open_browser {
            self.open_in_browser(&contest.url_submissions_me())?;
            for &(ref url, _, _) in &outputs {
                self.open_in_browser(url)?;
            }
        }
        Ok(())
    }

    #[allow(non_snake_case)]
    fn submit(
        &mut self,
        contest: &Contest,
        task: &str,
        lang_id: u32,
        src_path: &Path,
        open_browser: bool,
        skip_checking_if_accepted: bool,
    ) -> ServiceResult<()> {
        #[derive(Serialize)]
        struct Payload {
            #[serde(rename = "data.TaskScreenName")]
            dataTaskScreenName: String,
            #[serde(rename = "data.LanguageId")]
            dataLanguageId: u32,
            sourceCode: String,
            csrf_token: String,
        }

        let tasks_page = self.fetch_tasks_page(contest)?;
        let checks_if_accepted = !skip_checking_if_accepted && *contest != Contest::Practice && {
            let duration = extract_contest_duration(&tasks_page)?;
            let status = duration.check_current_status(contest.to_string());
            status.raise_if_not_begun()?;
            status.is_active()
        };
        for (name, url) in extract_task_urls_with_names(&tasks_page)? {
            if name.to_uppercase() == task.to_uppercase() {
                let task_screen_name = {
                    lazy_static! {
                        static ref SCREEN_NAME: Regex =
                            Regex::new(r"\A/contests/[a-z0-9_\-]+/tasks/([a-z0-9_]+)/?\z$").unwrap();
                    }
                    if let Some(caps) = SCREEN_NAME.captures(&url) {
                        caps[1].to_owned()
                    } else {
                        break;
                    }
                };
                if checks_if_accepted {
                    let page = self.get(&contest.url_submissions_me())?;
                    if check_if_accepted(page, &task_screen_name)? {
                        bail!(ServiceErrorKind::AlreadyAccepted);
                    }
                }
                let sourceCode = super::replace_class_name_if_necessary(src_path, "Main")?;
                let csrf_token = extract_csrf_token(&Document::from_read(self.get(&url)?)?)?;
                let url = contest.url_submit();
                let payload = Payload {
                    dataTaskScreenName: task_screen_name,
                    dataLanguageId: lang_id,
                    sourceCode,
                    csrf_token,
                };
                self.post_urlencoded(&url, &payload, &[302], None)?;
                if open_browser {
                    self.open_in_browser(&contest.url_submissions_me())?;
                }
                return Ok(());
            }
        }
        bail!(ServiceErrorKind::NoSuchProblem(task.to_owned()));
    }
}

#[derive(Clone, PartialEq, Eq)]
enum Contest {
    Practice,
    AbcBefore007(u32),
    Abc(u32),
    ArcBefore058(u32),
    Arc(u32),
    Agc(u32),
    ChokudaiS(u32),
    Other(String),
}

impl fmt::Display for Contest {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Contest::Practice => write!(f, "practice contest"),
            Contest::AbcBefore007(n) | Contest::Abc(n) => write!(f, "ABC{:>03}", n),
            Contest::ArcBefore058(n) | Contest::Arc(n) => write!(f, "ARC{:>03}", n),
            Contest::Agc(n) => write!(f, "AGC{:>03}", n),
            Contest::ChokudaiS(n) => write!(f, "Chokudai SpeedRun {:>03}", n),
            Contest::Other(ref s) => write!(f, "{}", s),
        }
    }
}

impl Contest {
    fn new(s: &str) -> Self {
        lazy_static! {
            static ref NAME: Regex = Regex::new(r"\A\s*([a-zA-Z_]+)(\d{3})\s*\z").unwrap();
        }
        if let Some(caps) = NAME.captures(s) {
            let name = caps[1].to_lowercase();
            let number = caps[2].parse::<u32>().unwrap_or(0);
            if name == "abc" && number < 7 {
                return Contest::AbcBefore007(number);
            } else if name == "abc" {
                return Contest::Abc(number);
            } else if name == "arc" && number < 58 {
                return Contest::ArcBefore058(number);
            } else if name == "arc" {
                return Contest::Arc(number);
            } else if name == "agc" {
                return Contest::Agc(number);
            } else if name == "chokudai_s" || name == "chokudais" {
                return Contest::ChokudaiS(number);
            }
        } else if s == "practice" {
            return Contest::Practice;
        }
        Contest::Other(s.to_owned())
    }

    fn style(&self) -> SampleCaseStyle {
        match *self {
            Contest::AbcBefore007(_) | Contest::ArcBefore058(_) => SampleCaseStyle::Old,
            _ => SampleCaseStyle::New,
        }
    }

    fn url_top(&self) -> String {
        static BASE: &'static str = "/contests/";
        match *self {
            Contest::Practice => format!("{}practice", BASE),
            Contest::AbcBefore007(n) | Contest::Abc(n) => format!("{}abc{:>03}", BASE, n),
            Contest::ArcBefore058(n) | Contest::Arc(n) => format!("{}arc{:>03}", BASE, n),
            Contest::Agc(n) => format!("{}agc{:>03}", BASE, n),
            Contest::ChokudaiS(n) => format!("{}chokudai_s{:>03}", BASE, n),
            Contest::Other(ref s) => format!("{}{}", BASE, s),
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

    fn url_submissions_me(&self) -> String {
        format!("{}/submissions/me", self.url_top())
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
        match *self {
            ContestStatus::NotBegun(ref s, t) => {
                bail!(ServiceErrorKind::ContestNotBegun(s.clone(), t))
            }
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

enum SampleCaseStyle {
    New,
    Old,
}

fn extract_csrf_token(document: &Document) -> ServiceResult<String> {
    fn extract(document: &Document) -> Option<String> {
        document
            .find(Attr("name", "csrf_token"))
            .next()?
            .attr("value")
            .map(str::to_owned)
    }

    super::quit_on_failure(extract(document), String::is_empty)
}

fn extract_task_urls_with_names(document: &Document) -> ServiceResult<Vec<(String, String)>> {
    fn extract(document: &Document) -> Option<Vec<(String, String)>> {
        let mut names_and_pathes = vec![];
        let predicate = Attr("id", "main-container")
            .child(And(Name("div"), Class("row")))
            .child(And(Name("div"), Class("col-sm-12")))
            .child(And(Name("div"), Class("panel")))
            .child(And(Name("table"), Class("table")))
            .child(Name("tbody"))
            .child(Name("tr"));
        for node in document.find(predicate) {
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
        Some(names_and_pathes)
    }

    super::quit_on_failure(extract(document), Vec::is_empty)
}

fn extract_cases<R: Read>(html: R, _style: &SampleCaseStyle) -> ServiceResult<TestSuite> {
    let document = Document::from_read(html)?;
    extract_cases_from_new_style(&document)
}

fn extract_cases_from_new_style(document: &Document) -> ServiceResult<TestSuite> {
    fn try_extracting_from_section(section_node: Node, regex: &Regex) -> Option<String> {
        let title = section_node.find(Name("h3")).next()?.text();
        let sample = section_node.find(Name("pre")).next()?.text();
        return_none_unless!(regex.is_match(&title));
        info!(
            "Extracting sample cases: Found h3{{{}}}+pre{{{:?}}}",
            title, sample
        );
        Some(sample)
    }

    fn extract_for_lang(
        document: &Document,
        re_input: &'static Regex,
        re_output: &'static Regex,
        lang_class_name: &'static str,
    ) -> Option<Vec<(String, String)>> {
        let predicate = Attr("id", "task-statement")
            .child(And(Name("span"), Class("lang")))
            .child(And(Name("span"), Class(lang_class_name)))
            .child(And(Name("div"), Class("part")))
            .child(Name("section"));
        let (mut samples, mut input_sample) = (vec![], None);
        for node in document.find(predicate) {
            info!(
                "Extracting sample cases: Found #task-statement>span.lang>span.{}>div.part>section\
                 >",
                lang_class_name
            );
            input_sample = if let Some(input_sample) = input_sample {
                let output_sample = try_extracting_from_section(node, re_output)?;
                samples.push((output_sample, input_sample));
                None
            } else if let Some(input_sample) = try_extracting_from_section(node, re_input) {
                Some(input_sample)
            } else {
                None
            };
        }
        return_none_if!(samples.is_empty());
        Some(samples)
    }

    fn extract(document: &Document) -> Option<TestSuite> {
        lazy_static! {
            static ref IN_JA: Regex = Regex::new(r"\A入力例 \d+.*\z").unwrap();
            static ref OUT_JA: Regex = Regex::new(r"\A出力例 \d+.*\z").unwrap();
            static ref IN_EN: Regex = Regex::new(r"\ASample Input \d+.*\z").unwrap();
            static ref OUT_EN: Regex = Regex::new(r"\ASample Output \d+.*\z").unwrap();
        }
        let timelimit = extract_timelimit_as_millis(document);
        let samples = {
            info!("Extracting sample cases: Searching \"入力例\" and \"出力例\"");
            if let Some(samples) = extract_for_lang(document, &IN_JA, &OUT_JA, "lang-ja") {
                samples
            } else {
                info!("Extracting sample cases: Searching \"Sample Input\" and \"Sample Output\"");
                extract_for_lang(document, &IN_EN, &OUT_EN, "lang-en")?
            }
        };
        Some(TestSuite::from_samples(timelimit, samples))
    }

    Ok(extract(document).unwrap_or_default())
}

fn extract_timelimit_as_millis(document: &Document) -> Option<u64> {
    lazy_static! {
        static ref TIMELIMIT: Regex = Regex::new(r"\A\D*(\d+)\s*sec.*\z").unwrap();
    }
    let predicate = Attr("id", "main-container")
        .child(And(Name("div"), Class("row")))
        .child(And(Name("div"), Class("col-sm-12")))
        .child(Name("p"))
        .child(Text);
    let text = document.find(predicate).next()?.text();
    info!(
        "Extracting timelimit: Found #main-container>div.row>div.col-sm-12>p{{{:?}}}",
        text
    );
    let caps = TIMELIMIT.captures(&text)?;
    let timelimit = 1000 * caps[1].parse::<u64>().ok()?;
    info!(
        "Extracting timelimit: Successfully extracted: {}ms",
        timelimit
    );
    Some(timelimit)
}

fn extract_contest_duration(document: &Document) -> ServiceResult<ContestDuration> {
    fn extract(document: &Document) -> Option<(String, String)> {
        let predicate = Name("time").child(Text);
        let t1 = document.find(predicate).nth(0)?.text();
        info!("Extracting contest duration: Found time{{{}}}", t1);
        let t2 = document.find(predicate).nth(1)?.text();
        info!("Extracting contest duration: Found time{{{}}}", t2);
        Some((t1, t2))
    }

    match extract(document) {
        Some((t1, t2)) => {
            static FORMAT: &'static str = "%F %T%z";
            let t1 = DateTime::parse_from_str(&t1, FORMAT)?.with_timezone(&Utc);
            let t2 = DateTime::parse_from_str(&t2, FORMAT)?.with_timezone(&Utc);
            Ok(ContestDuration(t1, t2))
        }
        None => bail!(ServiceErrorKind::Scrape),
    }
}

fn check_if_accepted<R: Read>(html: R, task_screen_name: &str) -> ServiceResult<bool> {
    fn check(document: &Document, task_screen_name: &str) -> Option<bool> {
        lazy_static! {
            static ref URL: Regex = Regex::new(r"\A/contests/\w+/tasks/(\w+)\z").unwrap();
        }
        let predicate = Attr("id", "main-container")
            .child(And(Name("div"), Class("row")))
            .child(And(Name("div"), Class("col-sm-12")))
            .child(And(Name("div"), Class("panel-submission")))
            .child(And(Name("div"), Class("table-responsive")))
            .child(And(Name("table"), Class("table")))
            .child(Name("tbody"))
            .child(Name("tr"));
        for node in document.find(predicate) {
            info!("Extracting submissions: Found #main-container>[[omitted]]>tr>");
            let url = node.find(Name("td").child(Name("a"))).nth(0)?.attr("href")?;
            info!("Extracting submissions: Found td>a[href={:?}]", url);
            if let Some(caps) = URL.captures(url) {
                if &caps[1] == task_screen_name {
                    let predicate = Name("td").child(Name("span")).child(Text);
                    if let Some(node) = node.find(predicate).nth(0) {
                        info!("Extracting submissions: Found td>span{{{}}}", node.text());
                        if node.text() == "AC" {
                            return Some(true);
                        }
                    }
                }
            }
        }
        info!("Extracting submissions: \"AC\" not found");
        Some(false)
    }

    match check(&Document::from_read(html)?, task_screen_name) {
        Some(p) => Ok(p),
        None => bail!(ServiceErrorKind::Scrape),
    }
}
