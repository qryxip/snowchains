use console::{ConsoleReadWrite, ConsoleWrite, Palette, Printer};
use errors::{ServiceError, ServiceResult, SessionResult};
use service::downloader::ZipDownloader;
use service::session::HttpSession;
use service::{
    Contest, DownloadProp, PrintTargets as _PrintTargets, Service, SessionProp,
    TryIntoDocument as _TryIntoDocument, UserNameAndPassword,
};
use testsuite::{SuiteFilePath, TestSuite};

use itertools::Itertools as _Itertools;
use reqwest::{Response, StatusCode};
use select::document::Document;
use select::predicate::{Attr, Class, Name, Predicate, Text};
use serde::{self, Deserialize, Deserializer};

use std::borrow::Cow;
use std::collections::HashMap;
use std::io::{self, Write as _Write};
use std::rc::Rc;
use std::time::Duration;
use std::{self, fmt};

pub(crate) fn login(sess_prop: SessionProp<impl ConsoleReadWrite>) -> ServiceResult<()> {
    Hackerrank::start(sess_prop)?.login(LoginOption::Explicit)
}

pub(crate) fn download(
    mut sess_prop: SessionProp<impl ConsoleReadWrite>,
    download_prop: DownloadProp<String>,
) -> ServiceResult<()> {
    let download_prop = download_prop.parse_contest();
    download_prop.write_targets(sess_prop.console.stdout())?;
    let timeout = sess_prop.timeout;
    Hackerrank::start(sess_prop)?.download(&download_prop, timeout)
}

struct Hackerrank<RW: ConsoleReadWrite> {
    console: RW,
    session: HttpSession,
    credentials: UserNameAndPassword,
}

impl<RW: ConsoleReadWrite> Service for Hackerrank<RW> {
    type Console = RW;

    fn session_and_stdout(&mut self) -> (&mut HttpSession, Printer<&mut RW::Stdout>) {
        (&mut self.session, self.console.stdout())
    }

    fn console(&mut self) -> &mut RW {
        &mut self.console
    }
}

impl<RW: ConsoleReadWrite> Hackerrank<RW> {
    fn start(mut sess_prop: SessionProp<RW>) -> SessionResult<Self> {
        let credentials = sess_prop.credentials.hackerrank.clone();
        let session = sess_prop.start_session()?;
        Ok(Hackerrank {
            console: sess_prop.console,
            session,
            credentials,
        })
    }

    fn login(&mut self, option: LoginOption) -> ServiceResult<()> {
        let mut res = self.get("/login").acceptable(&[200, 302]).send()?;
        if res.status() == StatusCode::Found && option == LoginOption::Explicit {
            writeln!(self.stderr(), "Already signed in.")?;
            self.stderr().flush()?;
        } else if res.status() == StatusCode::Ok {
            let (mut username, mut password, on_test) = {
                match self.credentials.clone() {
                    UserNameAndPassword::Some(username, password) => {
                        (username.clone(), password.clone(), true)
                    }
                    UserNameAndPassword::None => (
                        Rc::new(self.console().prompt_reply_stderr("Username: ")?),
                        Rc::new(self.console().prompt_password_stderr("Password: ")?),
                        false,
                    ),
                }
            };
            if option == LoginOption::NotNecessary
                && !on_test
                && !self.console().ask_yes_or_no("Login? ", false)?
            {
                return Ok(());
            }
            loop {
                if self.try_logging_in(&username, &password, res)? {
                    writeln!(self.stdout(), "Succeeded to login.")?;
                    break self.stdout().flush()?;
                }
                if on_test {
                    return Err(ServiceError::WrongCredentialsOnTest);
                }
                username = Rc::new(self.console().prompt_reply_stderr("Username: ")?);
                password = Rc::new(self.console().prompt_password_stderr("Password: ")?);
                writeln!(self.stderr(), "Failed to login. Try again.")?;
                self.stderr().flush()?;
                self.session.clear_cookies()?;
                res = self.get("/login").send()?;
            }
        }
        Ok(())
    }

    fn try_logging_in(
        &mut self,
        username: &str,
        password: &str,
        response: Response,
    ) -> ServiceResult<bool> {
        #[derive(Deserialize)]
        struct LoginResponse {
            status: bool,
        }

        let csrf_token = response.try_into_document()?.extract_csrf_token()?;
        let status = self
            .post("/auth/login")
            .raw_header("X-CSRF-Token", csrf_token)
            .acceptable(&[200])
            .send_json(&json!({
                "login": username,
                "password": password,
                "remember_me": true
            }))?.json::<LoginResponse>()?
            .status;
        Ok(status)
    }

    fn download(
        &mut self,
        download_prop: &DownloadProp<HackerrankContest>,
        timeout: Option<Duration>,
    ) -> ServiceResult<()> {
        fn warn_unless_empty(
            mut stderr: impl ConsoleWrite,
            problems: &[impl AsRef<str>],
            verb: &str,
        ) -> io::Result<()> {
            if !problems.is_empty() {
                let suf = if problems.len() == 1 { "" } else { "s" };
                let problems = problems
                    .iter()
                    .map(|problem| format!("{:?}", problem.as_ref()))
                    .format(", ");
                writeln!(
                    stderr.plain(Palette::Warning),
                    "Following problem{} {}: {}",
                    suf,
                    verb,
                    problems
                )?;
                stderr.flush()?;
            }
            Ok(())
        }

        let DownloadProp {
            contest,
            problems,
            download_dir,
            extension,
            open_browser,
        } = download_prop;
        let problems = problems.as_ref();

        let models = match (contest, problems) {
            (HackerrankContest::Master, None) => return Err(ServiceError::PleaseSpecifyProblems),
            (HackerrankContest::Master, Some(problems)) => problems
                .iter()
                .map(|problem| {
                    self.get(&format!("/rest/contests/master/challenges/{}", problem))
                        .recv_json::<ProblemQueryResponse>()
                        .map(|r| r.model)
                }).collect::<SessionResult<Vec<_>>>()?,
            (HackerrankContest::Contest(contest), problems) => {
                self.login(LoginOption::NotNecessary)?;
                self.get(&format!("/rest/contests/{}/challenges", contest))
                    .recv_json::<Models>()?
                    .models
                    .into_iter()
                    .filter(|model| match problems {
                        None => true,
                        Some(problems) => problems.contains(&model.slug),
                    }).map(|model| {
                        if model.public_test_cases {
                            Ok(model)
                        } else {
                            let (contest, problem) = (model.contest_slug, model.slug);
                            let url = format!("/rest/contests/{}/challenges/{}", contest, problem);
                            self.get(&url)
                                .recv_json::<ProblemQueryResponse>()
                                .map(|r| r.model)
                        }
                    }).collect::<SessionResult<Vec<_>>>()?
            }
        };

        let not_found = problems.map(|problems| {
            problems
                .iter()
                .filter(|&problem| !models.iter().any(|model| model.slug == *problem))
                .collect::<Vec<_>>()
        });
        if let Some(not_found) = not_found {
            warn_unless_empty(self.stderr(), &not_found, "not found")?;
        }

        let mut browser_urls = vec![];
        let mut scraped = vec![];
        let mut zip_targets = HashMap::<_, Vec<_>>::new();
        let mut cannot_view = vec![];

        for model in models {
            browser_urls.push(format!(
                "/contests/{}/challenges/{}/problem",
                model.contest_slug, model.slug
            ));
            if model.public_test_cases {
                let (contest, problem) = (model.contest_slug, model.slug);
                zip_targets
                    .entry(contest)
                    .and_modify(|problems| problems.push(problem.clone()))
                    .or_insert_with(|| vec![problem]);
            } else if let Some(body_html) = model.body_html {
                let suite = Document::from(body_html.as_str()).extract_samples()?;
                let path = SuiteFilePath::new(download_dir, &model.slug, *extension);
                scraped.push((suite, path));
            } else if !model.can_be_viewed {
                cannot_view.push(model.slug);
            } else {
                warn!("{:?}", model);
            }
        }

        warn_unless_empty(self.stderr(), &cannot_view, "cannot be viewed")?;

        for (suite, path) in scraped {
            suite.save(&path, self.stdout())?;
        }
        self.stdout().flush()?;

        if !zip_targets.is_empty() {
            let (url_pref, names);
            if zip_targets.keys().count() == 1 {
                let contest = zip_targets.keys().next().unwrap();
                url_pref = Cow::from(format!(
                    "https://www.hackerrank.com/rest/contests/{}/challenges/",
                    contest
                ));
                names = zip_targets.values().next().unwrap().clone();
            } else {
                url_pref = Cow::from("https://www.hackerrank.com/rest/contests/");
                names = zip_targets
                    .iter()
                    .flat_map(|(contest, problems)| {
                        problems
                            .iter()
                            .map(|problem| format!("{}/challenges/{}", contest, problem))
                            .collect::<Vec<_>>()
                    }).collect::<Vec<_>>();
            };
            static URL_SUF: &str = "/download_testcases";
            let cookie = self.session.cookies_to_header();
            ZipDownloader {
                out: io::sink(),
                url_pref: &url_pref,
                url_suf: URL_SUF,
                download_dir,
                names: &names,
                timeout,
                cookie: cookie.as_ref(),
            }.download()?;
        }
        if *open_browser {
            for url in browser_urls {
                self.open_in_browser(&url)?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq)]
enum LoginOption {
    Explicit,
    // Ensure,
    NotNecessary,
}

enum HackerrankContest {
    Master,
    Contest(String),
}

impl Contest for HackerrankContest {
    fn from_string(s: String) -> Self {
        if s.eq_ignore_ascii_case("master") {
            HackerrankContest::Master
        } else {
            HackerrankContest::Contest(s)
        }
    }
}

impl fmt::Display for HackerrankContest {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HackerrankContest::Master => write!(f, "master"),
            HackerrankContest::Contest(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Deserialize)]
struct Models {
    models: Vec<Model>,
}

#[derive(Deserialize)]
struct ProblemQueryResponse {
    #[allow(dead_code)]
    status: True,
    model: Model,
}

#[derive(Debug, Deserialize)]
struct Model {
    // solved: bool,
    can_be_viewed: bool,
    slug: String,
    contest_slug: String,
    public_test_cases: bool,
    body_html: Option<String>,
}

struct True;

impl<'de> Deserialize<'de> for True {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        if bool::deserialize(deserializer)? {
            Ok(True)
        } else {
            Err(serde::de::Error::custom("expected true"))
        }
    }
}

trait Extract {
    fn extract_csrf_token(&self) -> ServiceResult<String>;
    fn extract_samples(&self) -> ServiceResult<TestSuite>;
}

impl Extract for Document {
    fn extract_csrf_token(&self) -> ServiceResult<String> {
        self.find(Attr("name", "csrf-token"))
            .next()
            .and_then(|node| node.attr("content").map(str::to_owned))
            .filter(|token| !token.is_empty())
            .ok_or(ServiceError::Scrape)
    }

    fn extract_samples(&self) -> ServiceResult<TestSuite> {
        fn extract_item(this: &Document, predicate: impl Predicate) -> Vec<String> {
            this.find(predicate)
                .map(|pre| {
                    pre.find(Name("span").and(Class("err")).child(Text))
                        .map(|text| text.text())
                        .join("")
                }).collect()
        }

        let in_pred = Class("challenge_sample_input").descendant(Name("pre"));
        let out_pred = Class("challenge_sample_output").descendant(Name("pre"));
        let inputs = extract_item(self, in_pred);
        let outputs = extract_item(self, out_pred);
        if inputs.len() != outputs.len() || inputs.is_empty() {
            return Err(ServiceError::Scrape);
        }
        let samples = inputs.into_iter().zip(outputs);
        Ok(TestSuite::simple(None, None, None, samples))
    }
}

#[cfg(test)]
mod tests {
    use console::{Console, ConsoleReadWrite};
    use errors::SessionResult;
    use service::hackerrank::{Extract as _Extract, Hackerrank, ProblemQueryResponse};
    use service::session::{HttpSession, UrlBase};
    use service::{self, Service as _Service, UserNameAndPassword};
    use testsuite::TestSuite;

    use select::document::Document;
    use url::Host;

    use std::io;
    use std::time::Duration;

    #[test]
    #[ignore]
    fn it_scrapes_samples() {
        let expected = TestSuite::simple(
            None,
            None,
            None,
            vec![
                ("50 40 70 60", "YES"),
                ("55 66 66 77", "YES"),
                ("80 80 40 40", "NO"),
            ],
        );
        let actual = {
            let mut hackerrank = start().unwrap();
            let json = hackerrank
                .get("/rest/contests/hourrank-20/challenges/hot-and-cold")
                .recv_json::<ProblemQueryResponse>()
                .unwrap();
            let html = json.model.body_html.unwrap();
            Document::from(html.as_str()).extract_samples().unwrap()
        };
        assert_eq!(expected, actual);
    }

    fn start() -> SessionResult<Hackerrank<impl ConsoleReadWrite>> {
        let client = service::reqwest_client(Duration::from_secs(10))?;
        let base = UrlBase::new(Host::Domain("www.hackerrank.com"), true, None);
        let mut console = Console::null();
        let session = HttpSession::new(console.stdout(), client, base, None)?;
        Ok(Hackerrank {
            console,
            session,
            credentials: UserNameAndPassword::None,
        })
    }
}
