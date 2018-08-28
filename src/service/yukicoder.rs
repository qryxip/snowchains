use console::{ConsoleReadWrite, ConsoleWrite as _ConsoleWrite, Palette, Printer};
use errors::{ServiceError, ServiceResult, SessionResult, SubmitError};
use service::downloader::ZipDownloader;
use service::session::HttpSession;
use service::{
    Contest, DownloadProp, PrintTargets as _PrintTargets, RevelSession, Service, SessionProp,
    SubmitProp, TryIntoDocument as _TryIntoDocument,
};
use testsuite::{SuiteFilePath, TestSuite};

use cookie::Cookie;
use regex::Regex;
use reqwest::header::Location;
use reqwest::{multipart, StatusCode};
use select::document::Document;
use select::predicate::{Attr, Class, Name, Predicate as _Predicate, Text};

use std::borrow::Cow;
use std::fmt;
use std::io::Write as _Write;
use std::time::Duration;

pub(crate) fn login(sess_prop: SessionProp<impl ConsoleReadWrite>) -> ServiceResult<()> {
    Yukicoder::new(sess_prop)?.login(true)
}

pub(crate) fn download(
    mut sess_prop: SessionProp<impl ConsoleReadWrite>,
    download_prop: DownloadProp<String>,
) -> ServiceResult<()> {
    let download_prop = download_prop.parse_contest();
    download_prop.write_targets(sess_prop.console.stdout())?;
    let timeout = sess_prop.timeout;
    Yukicoder::new(sess_prop)?.download(&download_prop, timeout)
}

pub(crate) fn submit(
    sess_prop: SessionProp<impl ConsoleReadWrite>,
    submit_prop: SubmitProp<String>,
) -> ServiceResult<()> {
    let submit_prop = submit_prop.parse_contest();
    Yukicoder::new(sess_prop)?.submit(&submit_prop)
}

struct Yukicoder<RW: ConsoleReadWrite> {
    console: RW,
    session: HttpSession,
    username: Username,
    credential: RevelSession,
}

impl<RW: ConsoleReadWrite> Service for Yukicoder<RW> {
    type Console = RW;

    fn session_and_stdout(&mut self) -> (&mut HttpSession, Printer<&mut RW::Stdout>) {
        (&mut self.session, self.console.stdout())
    }

    fn console(&mut self) -> &mut RW {
        &mut self.console
    }
}

impl<RW: ConsoleReadWrite> Yukicoder<RW> {
    fn new(mut sess_prop: SessionProp<RW>) -> SessionResult<Self> {
        let credential = sess_prop.credentials.yukicoder.clone();
        let session = sess_prop.start_session()?;
        Ok(Self {
            console: sess_prop.console,
            session,
            username: Username::None,
            credential,
        })
    }

    fn login(&mut self, assure: bool) -> ServiceResult<()> {
        if let RevelSession::Some(revel_session) = self.credential.clone() {
            if !self.confirm_revel_session(revel_session.as_ref().clone())? {
                return Err(ServiceError::WrongCredentialsOnTest);
            }
        }
        self.fetch_username()?;
        if self.username.name().is_none() {
            let mut first = true;
            loop {
                if first {
                    if !assure && !self.console().ask_yes_or_no("Login? ", true)? {
                        break;
                    }
                    writeln!(
                        self.stdout(),
                        "\nInput \"REVEL_SESSION\".\n\n\
                         Firefox: sqlite3 ~/path/to/cookies.sqlite 'SELECT value FROM moz_cookies \
                         WHERE baseDomain=\"yukicoder.me\" AND name=\"REVEL_SESSION\"'\n\
                         Chrome: chrome://settings/cookies/detail?site=yukicoder.me&search=cookie\n"
                    )?;
                    self.stdout().flush()?;
                    first = false;
                }
                let revel_session = self.console().prompt_password_stderr("REVEL_SESSION: ")?;
                if self.confirm_revel_session(revel_session)? {
                    break;
                } else {
                    writeln!(self.stderr(), "Wrong \"REVEL_SESSION\".")?;
                    self.stderr().flush()?;
                }
            }
        }
        let username = self.username.clone();
        writeln!(self.stdout(), "Username: {}", username)?;
        self.stdout().flush()?;
        Ok(())
    }

    fn confirm_revel_session(&mut self, revel_session: String) -> ServiceResult<bool> {
        self.session.clear_cookies()?;
        let cookie = Cookie::new("REVEL_SESSION", revel_session);
        self.session.insert_cookie(cookie)?;
        self.fetch_username()?;
        Ok(self.username.name().is_some())
    }

    fn fetch_username(&mut self) -> SessionResult<()> {
        self.username = self.get("/").recv_html()?.extract_username();
        Ok(())
    }

    fn download(
        &mut self,
        download_prop: &DownloadProp<YukicoderContest>,
        timeout: Option<Duration>,
    ) -> ServiceResult<()> {
        let DownloadProp {
            contest,
            problems,
            download_dir,
            extension,
            open_browser,
        } = download_prop;
        self.login(false)?;
        let scrape =
            |document: &Document, problem: &str| -> ServiceResult<(TestSuite, SuiteFilePath)> {
                let suite = document.extract_samples()?;
                let path = SuiteFilePath::new(download_dir, problem, *extension);
                Ok((suite, path))
            };
        let (mut outputs, mut nos) = (vec![], vec![]);
        match (contest, problems.as_ref()) {
            (YukicoderContest::No, None) => return Err(ServiceError::PleaseSpecifyProblems),
            (YukicoderContest::No, Some(problems)) => {
                let (mut not_found, mut not_public) = (vec![], vec![]);
                for problem in problems {
                    let url = format!("/problems/no/{}", problem);
                    let res = self.get(&url).acceptable(&[200, 404]).send()?;
                    let status = res.status();
                    let document = res.try_into_document()?;
                    let public = match document.find(Attr("id", "content").child(Text)).next() {
                        None => true,
                        Some(t) => !t.text().contains("非表示"),
                    };
                    if status == StatusCode::NotFound {
                        not_found.push(problem);
                    } else if !public {
                        not_public.push(problem);
                    } else {
                        outputs.push(scrape(&document, problem).map(|(s, p)| (url, s, p))?);
                        nos.push(Cow::from(problem.as_str()));
                    }
                }
                let mut stderr = self.stderr();
                if !not_found.is_empty() {
                    writeln!(stderr.plain(Palette::Warning), "Not found: {:?}", not_found)?;
                    stderr.flush()?;
                }
                if !not_public.is_empty() {
                    writeln!(
                        stderr.plain(Palette::Warning),
                        "Not public: {:?}",
                        not_found
                    )?;
                    stderr.flush()?;
                }
            }
            (YukicoderContest::Contest(contest), problems) => {
                let target_problems = self
                    .get(&format!("/contests/{}", contest))
                    .recv_html()?
                    .extract_problems()?;
                for (name, href) in target_problems {
                    if problems.is_none() || problems.as_ref().unwrap().contains(&name) {
                        let name = name.to_lowercase();
                        let document = self.get(&href).recv_html()?;
                        outputs.push(scrape(&document, &name).map(|(s, p)| (href, s, p))?);
                        nos.push(Cow::from(name));
                    }
                }
            }
        }
        let nos = self.filter_solved(&nos)?;
        for (_, suite, path) in &outputs {
            suite.save(path, self.stdout())?;
        }
        self.stdout().flush()?;
        if !nos.is_empty() {
            static URL_PREF: &str = "https://yukicoder.me/problems/no/";
            static URL_SUF: &str = "/testcase.zip";
            let cookie = self.session.cookies_to_header();
            ZipDownloader {
                out: self.stdout().inner_writer(),
                url_pref: URL_PREF,
                url_suf: URL_SUF,
                download_dir,
                names: &nos,
                timeout,
                cookie: cookie.as_ref(),
            }.download()?;
        }
        if *open_browser {
            for (url, _, _) in &outputs {
                self.open_in_browser(url)?;
            }
        }
        Ok(())
    }

    fn submit(&mut self, prop: &SubmitProp<YukicoderContest>) -> ServiceResult<()> {
        let SubmitProp {
            contest,
            problem,
            lang_id,
            src_path,
            replacer,
            open_browser,
            skip_checking_if_accepted,
        } = prop;
        self.login(true)?;
        let code = ::fs::read_to_string(src_path)?;
        let code = match replacer {
            Some(replacer) => replacer.replace_from_local_to_submission(&problem, &code)?,
            None => code,
        };
        let mut url = match contest {
            YukicoderContest::No => format!("/problems/no/{}", problem),
            YukicoderContest::Contest(contest) => self
                .get(&format!("/contests/{}", contest))
                .recv_html()?
                .extract_problems()?
                .into_iter()
                .filter(|(name, _)| name.eq_ignore_ascii_case(problem))
                .map(|(_, href)| href)
                .next()
                .ok_or_else(|| SubmitError::NoSuchProblem(problem.clone()))?,
        };
        url += "/submit";
        let no = {
            lazy_static! {
                static ref NO: Regex =
                    Regex::new(r"\A(https://yukicoder\.me)?/problems/no/(\d+)/submit\z").unwrap();
            }
            NO.captures(&url).map(|caps| caps[2].to_owned())
        };
        if let Some(no) = no {
            if !(self.filter_solved(&[no])?.is_empty() || *skip_checking_if_accepted) {
                return Err(ServiceError::AlreadyAccepted);
            }
        }
        let document = self.get(&url).recv_html()?;
        let token = document.extract_csrf_token_from_submit_page()?;
        let form = multipart::Form::new()
            .text("csrf_token", token)
            .text("lang", lang_id.clone())
            .text("source", code.clone())
            .text("submit", "提出する");
        let url = document.extract_url_from_submit_page()?;
        let res = self.post(&url).send_multipart(form)?;
        let location = match res.headers().get::<Location>() {
            None => None,
            Some(location) => Some(self.session.resolve_url(&location)?),
        };
        if let Some(location) = location.as_ref() {
            if location
                .as_str()
                .starts_with("https://yukicoder.me/submissions/")
            {
                writeln!(self.stdout(), "Success: {}", location)?;
                self.stdout().flush()?;
                if *open_browser {
                    self.open_in_browser(location.as_str())?;
                }
                return Ok(());
            }
        }
        Err(SubmitError::Rejected(lang_id.clone(), code.len(), location).into())
    }

    fn filter_solved<'b>(
        &mut self,
        nos: &'b [impl 'b + AsRef<str>],
    ) -> ServiceResult<Vec<&'b str>> {
        #[derive(Deserialize)]
        #[serde(rename_all = "PascalCase")]
        struct Problem {
            no: u64,
        }

        if let Some(username) = self.username.name().map(ToOwned::to_owned) {
            let url = format!("/api/v1/solved/name/{}", username);
            let solved_nos = self
                .get(&url)
                .send()?
                .json::<Vec<Problem>>()?
                .into_iter()
                .map(|problem| problem.no.to_string())
                .collect::<Vec<_>>();
            Ok(nos
                .iter()
                .map(AsRef::as_ref)
                .filter(|no1| solved_nos.iter().any(|no2| no1 == no2))
                .collect())
        } else {
            Ok(vec![])
        }
    }
}

enum YukicoderContest {
    No,
    Contest(String),
}

impl fmt::Display for YukicoderContest {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            YukicoderContest::No => write!(f, "no"),
            YukicoderContest::Contest(contest) => write!(f, "{}", contest),
        }
    }
}

impl Contest for YukicoderContest {
    fn from_string(s: String) -> Self {
        if s.eq_ignore_ascii_case("no") {
            YukicoderContest::No
        } else {
            YukicoderContest::Contest(s)
        }
    }
}

#[derive(Clone, Debug)]
enum Username {
    None,
    // /public/img/anony.png (for now)
    Yukicoder(String),
    // https://avatars2.githubusercontent.com/...
    Github(String),
    // ?
    ProbablyTwitter(String),
}

impl Username {
    fn name(&self) -> Option<&str> {
        match self {
            Username::None => None,
            Username::Yukicoder(s) | Username::Github(s) | Username::ProbablyTwitter(s) => Some(&s),
        }
    }
}

impl fmt::Display for Username {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Username::None => write!(f, "<not logged in>"),
            Username::Yukicoder(s) => write!(f, "{} (yukicoder)", s),
            Username::Github(s) => write!(f, "{} (GitHub)", s),
            Username::ProbablyTwitter(s) => write!(f, "{} (probably Twitter)", s),
        }
    }
}

trait Extract {
    fn extract_username(&self) -> Username;
    fn extract_samples(&self) -> ServiceResult<TestSuite>;
    fn extract_problems(&self) -> ServiceResult<Vec<(String, String)>>;
    fn extract_csrf_token_from_submit_page(&self) -> ServiceResult<String>;
    fn extract_url_from_submit_page(&self) -> ServiceResult<String>;
}

impl Extract for Document {
    fn extract_username(&self) -> Username {
        let extract = || {
            let a = self.find(Attr("id", "usermenu").child(Name("a"))).next()?;
            let name = a.find(Text).next()?.text();
            let img = a.find(Name("img")).next()?;
            let src = img.attr("src")?;
            Some(if src == "/public/img/anony.png" {
                Username::Yukicoder(name)
            } else if src.starts_with("https://avatars2.githubusercontent.com") {
                Username::Github(name)
            } else {
                Username::ProbablyTwitter(name)
            })
        };
        extract().unwrap_or(Username::None)
    }

    fn extract_samples(&self) -> ServiceResult<TestSuite> {
        #[derive(Clone, Copy)]
        enum ProblemKind {
            Regular,
            Special,
            Reactive,
        }

        let extract = || {
            lazy_static! {
                static ref R: Regex = Regex::new(
                    "\\A / 実行時間制限 : 1ケース (\\d)\\.(\\d{3})秒 / メモリ制限 : \\d+ MB / \
                     (通常|スペシャルジャッジ|リアクティブ)問題.*\n?.*\\z"
                ).unwrap();
            }
            let text = self
                .find(Attr("id", "content").child(Name("p")).child(Text))
                .map(|text| text.text())
                .nth(1)?;
            let caps = R.captures(&text)?;
            let timelimit = {
                let s = caps[1].parse::<u64>().unwrap();
                let m = caps[2].parse::<u64>().unwrap();
                Duration::from_millis(1000 * s + m)
            };
            let kind = match &caps[3] {
                "通常" => ProblemKind::Regular,
                "スペシャルジャッジ" => ProblemKind::Special,
                "リアクティブ" => ProblemKind::Reactive,
                _ => return None,
            };
            match kind {
                ProblemKind::Regular | ProblemKind::Special => {
                    let mut samples = vec![];
                    let pred = Attr("id", "content")
                        .child(Name("div").and(Class("block")))
                        .child(Name("div").and(Class("sample")))
                        .child(Name("div").and(Class("paragraph")));
                    for paragraph in self.find(pred) {
                        let pres = paragraph.find(Name("pre").child(Text)).collect::<Vec<_>>();
                        ensure_opt!(pres.len() == 2);
                        let input = pres[0].text();
                        let output = match kind {
                            ProblemKind::Regular => Some(pres[1].text()),
                            ProblemKind::Special => None,
                            ProblemKind::Reactive => unreachable!(),
                        };
                        samples.push((input, output));
                    }
                    Some(TestSuite::simple(timelimit, None, None, samples))
                }
                ProblemKind::Reactive => Some(TestSuite::interactive(timelimit)),
            }
        };
        extract().ok_or(ServiceError::Scrape)
    }

    fn extract_problems(&self) -> ServiceResult<Vec<(String, String)>> {
        let extract = || {
            let mut problems = vec![];
            let predicate = Attr("id", "content")
                .child(Name("div").and(Class("left")))
                .child(Name("table").and(Class("table")))
                .child(Name("tbody"))
                .child(Name("tr"));
            for tr in self.find(predicate) {
                let name = tr.find(Name("td")).nth(0)?.text();
                let href = tr
                    .find(Name("td"))
                    .nth(2)?
                    .find(Name("a"))
                    .next()?
                    .attr("href")?
                    .to_owned();
                problems.push((name, href));
            }
            if problems.is_empty() {
                None
            } else {
                Some(problems)
            }
        };
        extract().ok_or(ServiceError::Scrape)
    }

    fn extract_csrf_token_from_submit_page(&self) -> ServiceResult<String> {
        self.find(Attr("id", "submit_form").child(Name("input")))
            .filter(|input| input.attr("name") == Some("csrf_token"))
            .filter_map(|input| input.attr("value").map(ToOwned::to_owned))
            .next()
            .ok_or(ServiceError::Scrape)
    }

    fn extract_url_from_submit_page(&self) -> ServiceResult<String> {
        self.find(Attr("id", "submit_form"))
            .filter_map(|form| form.attr("action").map(ToOwned::to_owned))
            .next()
            .ok_or(ServiceError::Scrape)
    }
}

#[cfg(test)]
mod tests {
    use console::{Console, ConsoleReadWrite};
    use errors::SessionResult;
    use service::session::{HttpSession, UrlBase};
    use service::yukicoder::{Extract as _Extract, Username, Yukicoder};
    use service::{self, RevelSession, Service as _Service};
    use testsuite::TestSuite;

    use env_logger;
    use url::Host;

    use std::borrow::Borrow;
    use std::io;
    use std::time::Duration;

    #[test]
    #[ignore]
    fn it_extracts_samples_from_problem1() {
        let _ = env_logger::try_init();
        test_extracting_samples(
            "/problems/no/1",
            TestSuite::simple(
                Duration::from_secs(5),
                None,
                None,
                vec![
                    ("3\n100\n3\n1 2 1\n2 3 3\n10 90 10\n10 10 50\n", "20\n"),
                    ("3\n100\n3\n1 2 1\n2 3 3\n1 100 10\n10 10 50\n", "50\n"),
                    (
                        "10\n10\n19\n1 1 2 4 5 1 3 4 6 4 6 4 5 7 8 2 3 4 9\n\
                         3 5 5 5 6 7 7 7 7 8 8 9 9 9 9 10 10 10 10\n\
                         8 6 8 7 6 6 9 9 7 6 9 7 7 8 7 6 6 8 6\n\
                         8 9 10 4 10 3 5 9 3 4 1 8 3 1 3 6 6 10 4\n",
                        "-1\n",
                    ),
                ],
            ),
        );
    }

    #[test]
    #[ignore]
    fn it_extracts_samples_from_problem188() {
        let _ = env_logger::try_init();
        test_extracting_samples(
            "/problems/no/188",
            TestSuite::simple(
                Duration::from_secs(1),
                None,
                None,
                Vec::<(&'static str, Option<&'static str>)>::new(),
            ),
        );
    }

    #[test]
    #[ignore]
    fn it_extracts_samples_from_problem192() {
        let _ = env_logger::try_init();
        test_extracting_samples(
            "/problems/no/192",
            TestSuite::simple(
                Duration::from_secs(2),
                None,
                None,
                vec![("101\n", None), ("1000\n", None)],
            ),
        );
    }

    #[test]
    #[ignore]
    fn it_extracts_samples_from_problem246() {
        let _ = env_logger::try_init();
        test_extracting_samples(
            "/problems/no/246",
            TestSuite::interactive(Duration::from_secs(2)),
        );
    }

    fn test_extracting_samples(url: &str, expected: TestSuite) {
        let mut yukicoder = start().unwrap();
        let document = yukicoder.get(url).recv_html().unwrap();
        let samples = document.extract_samples().unwrap();
        assert_eq!(expected, samples);
    }

    #[test]
    #[ignore]
    fn it_extracts_problems_names_and_hrefs_from_yukicoder_open_2015_small() {
        static EXPECTED: &[(&str, &str)] = &[
            ("A", "/problems/no/191"),
            ("B", "/problems/no/192"),
            ("C", "/problems/no/193"),
            ("D", "/problems/no/194"),
            ("E", "/problems/no/195"),
            ("F", "/problems/no/196"),
        ];
        let _ = env_logger::try_init();
        let problems = {
            let mut yukicoder = start().unwrap();
            let document = yukicoder.get("/contests/100").recv_html().unwrap();
            document.extract_problems().unwrap()
        };
        assert_eq!(own_pairs(EXPECTED), problems);
    }

    fn own_pairs<O: Borrow<B>, B: ToOwned<Owned = O> + ?Sized>(pairs: &[(&B, &B)]) -> Vec<(O, O)> {
        pairs
            .iter()
            .map(|(l, r)| ((*l).to_owned(), (*r).to_owned()))
            .collect()
    }

    fn start() -> SessionResult<Yukicoder<impl ConsoleReadWrite>> {
        let client = service::reqwest_client(Duration::from_secs(10))?;
        let base = UrlBase::new(Host::Domain("yukicoder.me"), true, None);
        let mut console = Console::null();
        let session = HttpSession::new(console.stdout(), client, base, None)?;
        Ok(Yukicoder {
            console,
            session,
            username: Username::None,
            credential: RevelSession::None,
        })
    }
}
