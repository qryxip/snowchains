use errors::{ServiceErrorKind, ServiceResult};
use service::OpenInBrowser;
use testsuite::{SuiteFileExtension, SuiteFilePath, TestSuite};

use chrono::{DateTime, Local, Utc};
use httpsession::HttpSession;
use regex::Regex;
use select::document::Document;
use select::predicate::{And, Attr, Class, Name, Predicate, Text};

use std::collections::BTreeMap;
use std::fmt;
use std::io::Read;
use std::ops::{Deref, DerefMut};
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

pub(self) struct AtCoderBeta(HttpSession);

impl Deref for AtCoderBeta {
    type Target = HttpSession;

    fn deref(&self) -> &HttpSession {
        &self.0
    }
}

impl DerefMut for AtCoderBeta {
    fn deref_mut(&mut self) -> &mut HttpSession {
        &mut self.0
    }
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
                let suite = extract_as_suite(self.get(&url)?, contest)?;
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
pub(self) enum Contest {
    Practice,
    Arc(u32),
    Abc(u32),
    Agc(u32),
    ChokudaiS(u32),
    Other(String),
}

impl fmt::Display for Contest {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Contest::Practice => write!(f, "practice contest"),
            Contest::Abc(n) => write!(f, "ABC{:>03}", n),
            Contest::Arc(n) => write!(f, "ARC{:>03}", n),
            Contest::Agc(n) => write!(f, "AGC{:>03}", n),
            Contest::ChokudaiS(n) => write!(f, "Chokudai SpeedRun {:>03}", n),
            Contest::Other(ref s) => write!(f, "{}", s),
        }
    }
}

impl Contest {
    pub(self) fn new(s: &str) -> Self {
        lazy_static! {
            static ref NAME: Regex = Regex::new(r"\A\s*([a-zA-Z_]+)(\d{3})\s*\z").unwrap();
        }
        if let Some(caps) = NAME.captures(s) {
            let name = caps[1].to_lowercase();
            let number = caps[2].parse::<u32>().unwrap_or(0);
            if name == "abc" {
                return Contest::Abc(number);
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

    fn url_top(&self) -> String {
        static BASE: &'static str = "/contests/";
        match *self {
            Contest::Practice => format!("{}practice", BASE),
            Contest::Abc(n) => format!("{}abc{:>03}", BASE, n),
            Contest::Arc(n) => format!("{}arc{:>03}", BASE, n),
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

pub(self) fn extract_task_urls_with_names(
    document: &Document,
) -> ServiceResult<Vec<(String, String)>> {
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

pub(self) fn extract_as_suite<R: Read>(html: R, contest: &Contest) -> ServiceResult<TestSuite> {
    fn extract_samples(document: &Document, contest: &Contest) -> Option<Vec<(String, String)>> {
        lazy_static! {
            static ref IN_JA: Regex = Regex::new(r"\A[\s\n]*入力例\s*(\d{1,3})+[.\n]*\z").unwrap();
            static ref OUT_JA: Regex = Regex::new(r"\A[\s\n]*出力例\s*(\d{1,3})+[.\n]*\z").unwrap();
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
        static INFO1: &str =
            "#task-statement>span.lang>span.lang-ja.div.part>section>h3{{...}}+pre{{...}}";
        static INFO2: &str =
            "#task-statement>span.lang>span.lang-en.div.part>section>h3{{...}}+pre{{...}}";
        static INFO3: &str = "#task-statement>div.part>section>h3{{...}}+pre{{...}}";
        static INFO4: &str = "#task-statement>div.part>h3{{...}}+section>pre{{...}}";
        static INFO5: &str = "#task-statement>h3{{...}}+section>pre{{...}}";
        static INFO6: &str = "#task-statement>section>h3{{...}}+pre{{...}}";
        let on_current = || {
            try_extract_samples(document, predicate1, &IN_JA, &OUT_JA, INFO1)
                .or_else(|| try_extract_samples(document, predicate2, &IN_EN, &OUT_EN, INFO2))
        };
        let on_arc019_to_arc057 = || {
            try_extract_samples(document, predicate3, &IN_JA, &OUT_JA, INFO3)
                .or_else(|| try_extract_samples(document, predicate4, &IN_JA, &OUT_JA, INFO4))
                .or_else(|| try_extract_samples(document, predicate5, &IN_JA, &OUT_JA, INFO5))
                .or_else(|| try_extract_samples(document, predicate6, &IN_JA, &OUT_JA, INFO6))
        };
        let on_arc002_to_arc018 = || {
            try_extract_samples(document, predicate4, &IN_JA, &OUT_JA, INFO4)
                .or_else(|| try_extract_samples(document, predicate3, &IN_JA, &OUT_JA, INFO3))
                .or_else(|| try_extract_samples(document, predicate5, &IN_JA, &OUT_JA, INFO5))
                .or_else(|| try_extract_samples(document, predicate6, &IN_JA, &OUT_JA, INFO6))
        };
        let on_arc001 = || try_extract_samples(document, predicate5, &IN_JA, &OUT_JA, INFO5);
        let on_abc041 = || try_extract_samples(document, predicate6, &IN_JA, &OUT_JA, INFO6);
        let samples = match *contest {
            Contest::Arc(n) if 19 <= n && n <= 57 => on_arc019_to_arc057(),
            Contest::Abc(n) if 7 <= n && n <= 40 => on_arc019_to_arc057(),
            Contest::Arc(n) if 2 <= n && n <= 18 => on_arc002_to_arc018(),
            Contest::Abc(n) if 1 <= n && n <= 6 => on_arc002_to_arc018(),
            Contest::Arc(1) => on_arc001(),
            Contest::Abc(41) => on_abc041(),
            _ => on_current(),
        }?;
        Some(samples)
    }

    fn try_extract_samples<P: Predicate>(
        document: &Document,
        predicate_for_h3_or_pre_or_section: P,
        re_input: &Regex,
        re_output: &Regex,
        info: &'static str,
    ) -> Option<Vec<(String, String)>> {
        info!("Extracting sample cases: Searching {}...", info);
        let mut inputs = BTreeMap::<u8, _>::new();
        let mut outputs = BTreeMap::<u8, _>::new();
        let mut next = None;
        for node in document.find(predicate_for_h3_or_pre_or_section) {
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
            Some(samples)
        }
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

    let document = Document::from_read(html)?;
    let timelimit = extract_timelimit_as_millis(&document);
    let samples = extract_samples(&document, contest).unwrap_or_default();
    Ok(TestSuite::from_samples(timelimit, samples))
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

#[cfg(test)]
mod tests {
    use service::atcoder_beta::{AtCoderBeta, Contest};
    use testsuite::TestSuite;

    use httpsession::{HttpSession, RedirectPolicy};
    use httpsession::header::UserAgent;

    use std::borrow::Borrow;
    use std::time::Duration;

    #[test]
    #[ignore]
    fn it_extracts_task_urls() {
        let mut atcoder = start();
        let page = atcoder.fetch_tasks_page(&Contest::new("agc001")).unwrap();
        let urls_and_names = super::extract_task_urls_with_names(&page).unwrap();
        static EXPECTED: &[(&str, &str)] = &[
            ("A", "/contests/agc001/tasks/agc001_a"),
            ("B", "/contests/agc001/tasks/agc001_b"),
            ("C", "/contests/agc001/tasks/agc001_c"),
            ("D", "/contests/agc001/tasks/agc001_d"),
            ("E", "/contests/agc001/tasks/agc001_e"),
            ("F", "/contests/agc001/tasks/agc001_f"),
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
    fn it_extracts_timelimits_and_sample_cases_from_arc001() {
        static A: &[(&str, &str)] = &[
            ("9\n131142143\n", "4 1\n"),
            ("20\n12341234123412341234\n", "5 5\n"),
            ("4\n1111\n", "4 0\n"),
        ];
        static B: &[(&str, &str)] = &[("7 34\n", "5\n"), ("19 28\n", "2\n"), ("10 10\n", "0\n")];
        static C: &[(&str, &str)] = &[
            (
                "........\n........\n.......Q\n........\n..Q.....\n........\n.Q......\n........\n",
                "Q.......\n....Q...\n.......Q\n.....Q..\n..Q.....\n......Q.\n.Q......\n...Q....\n",
            ),
            (
                ".....Q..\n.Q......\n........\n........\n........\nQ.......\n........\n........\n",
                "No Answer\n",
            ),
        ];
        static D: &[(&str, &str)] = &[
            (
                "7\n3 3\n2 5\n4 6\n2 3\n3 6\n3 4\n4 6\n2 5\n1 5\n",
                "8.22677276241436\n",
            ),
            ("5\n3 3\n0 5\n0 5\n0 5\n0 5\n0 5\n0 5\n", "5\n"),
        ];
        let expected = [
            ("A", "/contests/arc001/tasks/arc001_1", 2000, A),
            ("B", "/contests/arc001/tasks/arc001_2", 2000, B),
            ("C", "/contests/arc001/tasks/arc001_3", 2000, C),
            ("D", "/contests/arc001/tasks/arc001_4", 2000, D),
        ];
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
        test_sample_extraction("arc002", &expected);
    }

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
            (
                "5 7 3\nGET..ET\n..T....\n.TEST..\n.E.T.ET\n...ETC.\n",
                "19\n",
            ),
            (
                "5 7 2\nGET..ET\n..T....\n.TEST..\n.E.T.ET\n...ETC.\n",
                "21\n",
            ),
            (
                "5 7 1\nGET..ET\n..T....\n.TEST..\n.E.T.ET\n...ETC.\n",
                "-1\n",
            ),
            (
                "6 35 4\nT...TT.....TT...TTT...TTT..TTG.....\n..T..T.TTT.T..T..E..T..E...TTT.TTT.\n\
                 .TTT.T.....E.TTTTT.TTT.TTT.TTT.....\n.....T.TT.TT.TTTTT.TTT.TTT.TTTTTTT.\n\
                 .TTT.T.TT..T..T..S..T..TTT.TTTTTTT.\n.CTT.E.TTT.TT...TTT...TT.....E.....\n",
                "94\n",
            ),
        ];
        static D: &[(&str, &str)] = &[];
        let expected = [
            ("A", "/contests/arc019/tasks/arc019_1", 2000, A),
            ("B", "/contests/arc019/tasks/arc019_2", 2000, B),
            ("C", "/contests/arc019/tasks/arc019_3", 2000, C),
            ("D", "/contests/arc019/tasks/arc019_4", 2000, D),
        ];
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
        test_sample_extraction("abc041", &expected);
    }

    #[test]
    #[ignore]
    fn it_extracts_timelimits_and_sample_cases_from_chokudai_s001() {
        static A: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "5\n"),
            ("6\n1 2 3 4 5 6\n", "6\n"),
            ("7\n7 6 5 4 3 2 1\n", "7\n"),
            (
                "20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n",
                "20\n",
            ),
        ];
        static B: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "15\n"),
            ("6\n1 2 3 4 5 6\n", "21\n"),
            ("7\n7 6 5 4 3 2 1\n", "28\n"),
            (
                "20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n",
                "210\n",
            ),
        ];
        static C: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "3,1,5,4,2\n"),
            ("6\n1 2 3 4 5 6\n", "1,2,3,4,5,6\n"),
            ("7\n7 6 5 4 3 2 1\n", "7,6,5,4,3,2,1\n"),
            (
                "20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n",
                "19,11,10,7,8,9,17,18,20,4,3,15,16,1,5,14,6,2,13,12\n",
            ),
        ];
        static D: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "1 2 3 4 5\n"),
            ("6\n1 2 3 4 5 6\n", "1 2 3 4 5 6\n"),
            ("7\n7 6 5 4 3 2 1\n", "1 2 3 4 5 6 7\n"),
            (
                "20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n",
                "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20\n",
            ),
        ];
        static E: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "2\n"),
            ("6\n1 2 3 4 5 6\n", "1\n"),
            ("7\n7 6 5 4 3 2 1\n", "7\n"),
            (
                "20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n",
                "14\n",
            ),
        ];
        static F: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "2\n"),
            ("6\n1 2 3 4 5 6\n", "6\n"),
            ("7\n7 6 5 4 3 2 1\n", "1\n"),
            (
                "20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n",
                "2\n",
            ),
        ];
        static G: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "31542\n"),
            ("6\n1 2 3 4 5 6\n", "123456\n"),
            ("7\n7 6 5 4 3 2 1\n", "7654321\n"),
            (
                "20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n",
                "370453866\n",
            ),
        ];
        static H: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "2\n"),
            ("6\n1 2 3 4 5 6\n", "6\n"),
            ("7\n7 6 5 4 3 2 1\n", "1\n"),
            (
                "20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n",
                "6\n",
            ),
        ];
        static I: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "1\n"),
            ("6\n1 2 3 4 5 6\n", "2\n"),
            ("7\n7 6 5 4 3 2 1\n", "2\n"),
            (
                "20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n",
                "3\n",
            ),
        ];
        static J: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "5\n"),
            ("6\n1 2 3 4 5 6\n", "0\n"),
            ("7\n7 6 5 4 3 2 1\n", "21\n"),
            (
                "20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n",
                "114\n",
            ),
        ];
        static K: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "54\n"),
            ("6\n1 2 3 4 5 6\n", "1\n"),
            ("7\n7 6 5 4 3 2 1\n", "5040\n"),
            (
                "20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n",
                "542869439\n",
            ),
        ];
        static L: &[(&str, &str)] = &[
            ("5\n3 1 5 4 2\n", "YES\n"),
            ("6\n1 2 3 4 5 6\n", "YES\n"),
            ("7\n7 6 5 4 3 2 1\n", "YES\n"),
            (
                "20\n19 11 10 7 8 9 17 18 20 4 3 15 16 1 5 14 6 2 13 12\n",
                "YES\n",
            ),
        ];
        #[cfg_attr(rustfmt, rustfmt_skip)]
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
        test_sample_extraction("chokudai_s001", &expected);
    }

    fn start() -> AtCoderBeta {
        let session = HttpSession::builder()
            .base("beta.atcoder.jp", true, None)
            .timeout(Duration::from_secs(3))
            .redirect(RedirectPolicy::none())
            .default_header(UserAgent::new(
                "snowchains <https://github.com/wariuni/snowchains>",
            ))
            .with_robots_txt()
            .unwrap();
        AtCoderBeta(session)
    }

    fn test_sample_extraction(contest: &str, expected: &[(&str, &str, u64, &[(&str, &str)])]) {
        let mut atcoder = start();
        let contest = Contest::new(contest);
        let page = atcoder.fetch_tasks_page(&contest).unwrap();
        let urls_and_names = super::extract_task_urls_with_names(&page).unwrap();
        for (
            &(ref actual_name, ref actual_url),
            &(expected_name, expected_url, expected_timelimit, expected_samples),
        ) in urls_and_names.iter().zip(expected.iter())
        {
            assert_eq!(expected_name, actual_name);
            assert_eq!(expected_url, actual_url);
            let problem_page = atcoder.get(&actual_url).unwrap();
            let expected_suite =
                TestSuite::from_samples(Some(expected_timelimit), own_pairs(expected_samples));
            let actual_suite = super::extract_as_suite(problem_page, &contest).unwrap();
            assert_eq!(expected_suite, actual_suite);
        }
    }

    fn own_pairs<O: Borrow<B>, B: ToOwned<Owned = O> + ?Sized>(pairs: &[(&B, &B)]) -> Vec<(O, O)> {
        pairs
            .iter()
            .map(|&(ref l, ref r)| ((*l).to_owned(), (*r).to_owned()))
            .collect()
    }
}
