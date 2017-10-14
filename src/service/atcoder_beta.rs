use error::{PrintChainColored, ServiceErrorKind, ServiceResult};
use service::session::HttpSession;
use testsuite::{SuiteFileExtension, SuiteFilePath, TestSuite};

use regex::Regex;
use reqwest::StatusCode;
use select::document::Document;
use select::node::Node;
use select::predicate::{And, Attr, Class, Name, Predicate, Text};
use std::io::Read;
use std::path::Path;
use webbrowser;


/// Logins to "beta.atcoder.jp".
pub fn login() -> ServiceResult<()> {
    AtCoderBeta::start(true)?.save()
}


/// Participates in given contest.
pub fn participate(contest_name: &str) -> ServiceResult<()> {
    let contest = &Contest::new(contest_name);
    let mut atcoder = AtCoderBeta::start(false)?;
    atcoder.register_to_contest(contest)?;
    atcoder.save()
}


/// Accesses to pages of the problems and extract pairs of sample input/output from them.
pub fn download(
    contest_name: &str,
    dir_to_save: &Path,
    extension: SuiteFileExtension,
    open_browser: bool,
) -> ServiceResult<()> {
    let contest = Contest::new(contest_name);
    let mut atcoder = AtCoderBeta::start(false)?;
    atcoder.register_to_contest(&contest)?;
    atcoder.download_all_tasks(
        contest,
        dir_to_save,
        extension,
        open_browser,
    )?;
    atcoder.save()
}


/// Submits a source code.
pub fn submit(
    contest_name: &str,
    task: &str,
    lang_id: u32,
    src_path: &Path,
    open_browser: bool,
    force: bool,
) -> ServiceResult<()> {
    let contest = Contest::new(contest_name);
    let mut atcoder = AtCoderBeta::start(false)?;
    atcoder.register_to_contest(&contest)?;
    atcoder.submit_code(
        contest,
        task,
        lang_id,
        src_path,
        open_browser,
        force,
    )?;
    atcoder.save()
}


custom_derive! {
    #[derive(NewtypeDeref, NewtypeDerefMut)]
    struct AtCoderBeta(HttpSession);
}

impl AtCoderBeta {
    fn start(eprints_message_if_already_logged_in: bool) -> ServiceResult<Self> {
        let mut atcoder = AtCoderBeta(HttpSession::start("atcoder-beta.sqlite3")?);
        if atcoder.has_cookie() && atcoder.http_get("https://beta.atcoder.jp/settings").is_ok() {
            if eprints_message_if_already_logged_in {
                eprintln!("Already logged in.");
            }
        } else {
            atcoder.login()?;
        }
        Ok(atcoder)
    }

    fn login(&mut self) -> ServiceResult<()> {
        while let Err(e) = self.try_logging_in() {
            e.print_chain_colored();
            eprintln!("Failed to login. Try again.");
            self.clear_cookies();
        }
        Ok(println!("Succeeded to login."))
    }

    fn try_logging_in(&mut self) -> ServiceResult<()> {
        #[derive(Serialize)]
        struct PostData {
            username: String,
            password: String,
            csrf_token: String,
        }

        let csrf_token = extract_csrf_token(self.http_get(URL)?)?;
        let (username, password) = super::read_username_and_password("Username: ")?;
        let data = PostData {
            username: username,
            password: password,
            csrf_token: csrf_token,
        };
        static URL: &'static str = "https://beta.atcoder.jp/login";
        let _ = self.http_post_urlencoded(URL, data, StatusCode::Found)?;
        let _ = self.http_get("https://beta.atcoder.jp/settings")?;
        Ok(())
    }

    fn register_to_contest(&mut self, contest: &Contest) -> ServiceResult<()> {
        #[derive(Serialize)]
        struct PostData {
            csrf_token: String,
        }

        let csrf_token = extract_csrf_token(self.http_get(&contest.top_url())?)?;
        let data = PostData { csrf_token: csrf_token };
        self.http_post_urlencoded(&contest.registration_url(), data, StatusCode::Found)
            .map(|_| ())
    }

    fn download_all_tasks(
        &mut self,
        contest: Contest,
        dir_to_save: &Path,
        extension: SuiteFileExtension,
        open_browser: bool,
    ) -> ServiceResult<()> {
        let mut outputs = vec![];
        let urls_with_names = extract_task_urls_with_names(self.http_get(&contest.tasks_url())?)?;
        for (name, relative_url) in urls_with_names {
            let url = format!("https://beta.atcoder.jp{}", relative_url);
            let suite = extract_cases(self.http_get(&url)?, &contest.style())?;
            let path = SuiteFilePath::new(&dir_to_save, name.to_lowercase(), extension);
            outputs.push((url, suite, path));
        }
        for &(_, ref suite, ref path) in &outputs {
            suite.save(&path)?;
        }
        if open_browser {
            for (url, ..) in outputs.into_iter() {
                println!("Opening {} in default browser...", url);
                webbrowser::open(&url)?;
            }
        }
        Ok(())
    }

    #[allow(non_snake_case)]
    fn submit_code(
        &mut self,
        contest: Contest,
        task: &str,
        lang_id: u32,
        src_path: &Path,
        open_browser: bool,
        force: bool,
    ) -> ServiceResult<()> {
        #[derive(Serialize)]
        struct PostData {
            #[serde(rename = "data.TaskScreenName")]
            dataTaskScreenName: String,
            #[serde(rename = "data.LanguageId")]
            dataLanguageId: u32,
            sourceCode: String,
            csrf_token: String,
        }

        for (name, relative_url) in
            extract_task_urls_with_names(self.http_get(&contest.tasks_url())?)?
        {
            if name.to_uppercase() == task.to_uppercase() {
                let task_screen_name = {
                    lazy_static! {
                        static ref REGEX: Regex = Regex::new(r"^.*/([a-z0-9_]+)/?$").unwrap();
                    }
                    if let Some(caps) = REGEX.captures(&relative_url) {
                        caps[1].to_owned()
                    } else {
                        break;
                    }
                };
                if !(force || contest.is_practice()) {
                    let url = contest.standings_url();
                    let (c1, c2) = (StatusCode::Ok, StatusCode::Found);
                    if self.http_get_as_opt(&url, c1, c2)?.is_none() {
                        let page = self.http_get(&contest.submissions_url())?;
                        if check_if_accepted(page, &task_screen_name)? {
                            return Ok(eprintln!(
                                "Your code seems to be already accepted. Append \"--force\" to \
                                 force submit."
                            ));
                        }
                    }
                }
                let source_code = super::replace_class_name_if_necessary(src_path, "Main")?;
                let csrf_token = {
                    let url = format!("https://beta.atcoder.jp{}", relative_url);
                    extract_csrf_token(self.http_get(&url)?)?
                };
                let data = PostData {
                    dataTaskScreenName: task_screen_name,
                    dataLanguageId: lang_id,
                    sourceCode: source_code,
                    csrf_token: csrf_token,
                };
                let url = contest.submission_url();
                let _ = self.http_post_urlencoded(&url, data, StatusCode::Found)?;
                if open_browser {
                    let url = contest.submissions_url();
                    println!("Opening {} in default browser...", url);
                    webbrowser::open(&url)?;
                }
                return Ok(());
            }
        }
        bail!(ServiceErrorKind::NoSuchProblem(task.to_owned()));
    }

    fn save(self) -> ServiceResult<()> {
        self.0.save_cookie_to_db()
    }
}


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

impl Contest {
    fn new(s: &str) -> Self {
        let regex = Regex::new(r"^\s*([a-zA-Z_]+)(\d\d\d)\s*$").unwrap();
        if let Some(caps) = regex.captures(s) {
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
            Contest::AbcBefore007(_) |
            Contest::ArcBefore058(_) => SampleCaseStyle::Old,
            _ => SampleCaseStyle::New,
        }
    }

    fn is_practice(&self) -> bool {
        match *self {
            Contest::Practice => true,
            _ => false,
        }
    }

    fn top_url(&self) -> String {
        static BASE: &'static str = "https://beta.atcoder.jp/contests/";
        match *self {
            Contest::Practice => format!("{}practice", BASE),
            Contest::AbcBefore007(n) => format!("{}abc{:>03}", BASE, n),
            Contest::Abc(n) => format!("{}abc{:>03}", BASE, n),
            Contest::ArcBefore058(n) => format!("{}arc{:>03}", BASE, n),
            Contest::Arc(n) => format!("{}arc{:>03}", BASE, n),
            Contest::Agc(n) => format!("{}agc{:>03}", BASE, n),
            Contest::ChokudaiS(n) => format!("{}chokudai_s{:>03}", BASE, n),
            Contest::Other(ref s) => format!("{}{}", BASE, s),
        }
    }

    fn tasks_url(&self) -> String {
        format!("{}/tasks", self.top_url())
    }

    fn registration_url(&self) -> String {
        format!("{}/register", self.top_url())
    }

    fn submission_url(&self) -> String {
        format!("{}/submit", self.top_url())
    }

    fn submissions_url(&self) -> String {
        format!("{}/submissions/me", self.top_url())
    }

    fn standings_url(&self) -> String {
        format!("{}/standings", self.top_url())
    }
}


enum SampleCaseStyle {
    New,
    Old,
}


fn extract_csrf_token<R: Read>(html: R) -> ServiceResult<String> {
    fn extract(document: Document) -> Option<String> {
        try_opt!(document.find(Attr("name", "csrf_token")).next())
            .attr("value")
            .map(str::to_owned)
    }

    super::quit_on_failure(extract(Document::from_read(html)?), String::is_empty)
}


fn extract_task_urls_with_names<R: Read>(html: R) -> ServiceResult<Vec<(String, String)>> {
    fn extract(document: Document) -> Option<Vec<(String, String)>> {
        let mut names_and_pathes = vec![];
        let predicate = Attr("id", "main-container")
            .child(And(Name("div"), Class("row")))
            .child(And(Name("div"), Class("col-sm-12")))
            .child(And(Name("div"), Class("panel")))
            .child(And(Name("table"), Class("table")))
            .child(Name("tbody"))
            .child(Name("tr"));
        for node in document.find(predicate) {
            let node = try_opt!(node.find(And(Name("td"), Class("text-center"))).next());
            let node = try_opt!(node.find(Name("a")).next());
            let url = try_opt!(node.attr("href")).to_owned();
            let name = try_opt!(node.find(Text).next()).text();
            info!(
                "Extracting problem links: Found #main-container>[[omitted]]>a[href={:?}]{{{}}}",
                url,
                name
            );
            names_and_pathes.push((name, url));
        }
        Some(names_and_pathes)
    }

    super::quit_on_failure(extract(Document::from_read(html)?), Vec::is_empty)
}


fn extract_cases<R: Read>(html: R, style: &SampleCaseStyle) -> ServiceResult<TestSuite> {
    let document = Document::from_read(html)?;
    match *style {
        SampleCaseStyle::New => extract_cases_from_new_style(document),
        SampleCaseStyle::Old => extract_cases_from_new_style(document),
    }
}


fn extract_cases_from_new_style(document: Document) -> ServiceResult<TestSuite> {
    fn try_extracting_from_section(section_node: Node, regex: &Regex) -> Option<String> {
        let title = try_opt!(section_node.find(Name("h3")).next()).text();
        let sample = try_opt!(section_node.find(Name("pre")).next()).text();
        return_none_unless!(regex.is_match(&title));
        info!(
            "Extracting sample cases: Found h3{{{}}}+pre{{{:?}}}",
            title,
            sample
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
                "Extracting sample cases: Found #task-statement>span.lang>span.{}>div.part>section>",
                lang_class_name
            );
            input_sample = if let Some(input_sample) = input_sample {
                let output_sample = try_opt!(try_extracting_from_section(node, &re_output));
                samples.push((output_sample, input_sample));
                None
            } else if let Some(input_sample) = try_extracting_from_section(node, &re_input) {
                Some(input_sample)
            } else {
                None
            };
        }
        return_none_if!(samples.is_empty());
        Some(samples)
    }

    fn extract(document: Document) -> Option<TestSuite> {
        lazy_static! {
            static ref RE_IN_JA: Regex = Regex::new(r"^入力例 \d+.*$").unwrap();
            static ref RE_OUT_JA: Regex = Regex::new(r"^出力例 \d+.*$").unwrap();
            static ref RE_IN_EN: Regex = Regex::new(r"^Sample Input \d+.*$").unwrap();
            static ref RE_OUT_EN: Regex = Regex::new(r"^Sample Output \d+.*$").unwrap();
        }
        let timelimit = extract_timelimit_as_millis(&document);
        let samples = {
            info!("Extracting sample cases: Searching \"入力例\" and \"出力例\"");
            if let Some(samples) = extract_for_lang(&document, &RE_IN_JA, &RE_OUT_JA, "lang-ja") {
                samples
            } else {
                info!("Extracting sample cases: Searching \"Sample Input\" and \"Sample Output\"");
                try_opt!(extract_for_lang(
                    &document,
                    &RE_IN_EN,
                    &RE_OUT_EN,
                    "lang-en",
                ))
            }
        };
        Some(TestSuite::from_text(timelimit, samples))
    }

    Ok(extract(document).unwrap_or_default())
}


fn extract_timelimit_as_millis(document: &Document) -> Option<u64> {
    lazy_static! {
        static ref RE_TIMELIMIT: Regex = Regex::new(r"^\D*(\d+)\s*sec.*$").unwrap();
    }
    let predicate = Attr("id", "main-container")
        .child(And(Name("div"), Class("row")))
        .child(And(Name("div"), Class("col-sm-12")))
        .child(Name("p"))
        .child(Text);
    let text = try_opt!(document.find(predicate).next()).text();
    info!(
        "Extracting timelimit: Found #main-container>div.row>div.col-sm-12>p{{{:?}}}",
        text
    );
    let caps = try_opt!(RE_TIMELIMIT.captures(&text));
    let timelimit = 1000 * try_opt!(caps[1].parse::<u64>().ok());
    info!(
        "Extracting timelimit: Successfully extracted: {}ms",
        timelimit
    );
    Some(timelimit)
}


fn check_if_accepted<R: Read>(html: R, task_screen_name: &str) -> ServiceResult<bool> {
    fn check(document: Document, task_screen_name: &str) -> Option<bool> {
        lazy_static! {
            static ref RE_URL: Regex = Regex::new(r"^/contests/\w+/tasks/(\w+)$").unwrap();
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
            let url =
                try_opt!(try_opt!(node.find(Name("td").child(Name("a"))).nth(0)).attr("href"));
            info!("Extracting submissions: Found td>a[href={:?}]", url);
            if let Some(caps) = RE_URL.captures(url) {
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
        Some(false)
    }

    match check(Document::from_read(html)?, task_screen_name) {
        Some(p) => Ok(p),
        None => bail!(ServiceErrorKind::ScrapingFailed),
    }
}
