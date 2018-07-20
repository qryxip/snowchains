use errors::{ServiceError, ServiceResult, SessionResult};
use palette::Palette;
use service::session::{GetPost, HttpSession};
use service::{
    downloader, Contest, DownloadProp, PrintTargets as _PrintTargets, SessionProp,
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
use std::rc::Rc;
use std::{self, fmt, io};

pub(crate) fn login(sess_prop: &SessionProp) -> ServiceResult<()> {
    Hackerrank::start(sess_prop)?.login(LoginOption::Explicit)
}

pub(crate) fn download(
    sess_prop: &SessionProp,
    download_prop: DownloadProp<String>,
) -> ServiceResult<()> {
    let download_prop = download_prop.parse_contest();
    download_prop.print_targets();
    Hackerrank::start(sess_prop)?.download(sess_prop, &download_prop)
}

struct Hackerrank {
    session: HttpSession,
    credentials: UserNameAndPassword,
}

impl GetPost for Hackerrank {
    fn session(&mut self) -> &mut HttpSession {
        &mut self.session
    }
}

impl Hackerrank {
    fn start(sess_prop: &SessionProp) -> SessionResult<Self> {
        let session = sess_prop.start_session()?;
        let credentials = sess_prop.credentials.hackerrank.clone();
        Ok(Hackerrank {
            session,
            credentials,
        })
    }

    fn login(&mut self, option: LoginOption) -> ServiceResult<()> {
        let (mut username, mut password, on_test) = {
            let on_test = self.credentials.is_some();
            let (username, password) = self.credentials.or_ask("Username: ")?;
            (username, password, on_test)
        };
        let mut res = self.get("/login").acceptable(&[200, 302]).send()?;
        if res.status() == StatusCode::Found && option == LoginOption::Explicit {
            eprintln!("Already signed in.");
        } else if res.status() == StatusCode::Ok {
            if option == LoginOption::NotNecessary
                && !on_test
                && !super::ask_yes_or_no("Login? ", false)?
            {
                return Ok(());
            }
            loop {
                if self.try_logging_in(&username, &password, res)? {
                    break println!("Succeeded to login.");
                }
                if on_test {
                    return Err(ServiceError::WrongCredentialsOnTest);
                }
                let (username_, password_) = super::ask_credentials("Username: ")?;
                username = Rc::new(username_);
                password = Rc::new(password_);
                eprintln!("Failed to login. Try again.");
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
            }))?
            .json::<LoginResponse>()?
            .status;
        Ok(status)
    }

    fn download(
        &mut self,
        sess_prop: &SessionProp,
        download_prop: &DownloadProp<HackerrankContest>,
    ) -> ServiceResult<()> {
        fn warn_unless_empty(problems: &[impl AsRef<str>], verb: &str) {
            if !problems.is_empty() {
                let suf = if problems.len() == 1 { "" } else { "s" };
                let problems = problems
                    .iter()
                    .map(|problem| format!("{:?}", problem.as_ref()))
                    .format(", ");
                let mes = format!("Following problem{} {}: {}", suf, verb, problems);
                eprintln!("{}", Palette::Warning.paint(mes));
            }
        }

        let DownloadProp {
            contest,
            problems,
            download_dir,
            extension,
            open_browser,
            suppress_download_bars,
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
                })
                .collect::<SessionResult<Vec<_>>>()?,
            (HackerrankContest::Contest(contest), problems) => {
                self.login(LoginOption::NotNecessary)?;
                self.get(&format!("/rest/contests/{}/challenges", contest))
                    .recv_json::<Models>()?
                    .models
                    .into_iter()
                    .filter(|model| match problems {
                        None => true,
                        Some(problems) => problems.contains(&model.slug),
                    })
                    .map(|model| {
                        if model.public_test_cases {
                            Ok(model)
                        } else {
                            let (contest, problem) = (model.contest_slug, model.slug);
                            let url = format!("/rest/contests/{}/challenges/{}", contest, problem);
                            self.get(&url)
                                .recv_json::<ProblemQueryResponse>()
                                .map(|r| r.model)
                        }
                    })
                    .collect::<SessionResult<Vec<_>>>()?
            }
        };

        let not_found = problems.map(|problems| {
            problems
                .iter()
                .filter(|&problem| !models.iter().any(|model| model.slug == *problem))
                .collect::<Vec<_>>()
        });
        if let Some(not_found) = not_found {
            warn_unless_empty(&not_found, "not found");
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

        warn_unless_empty(&cannot_view, "cannot be viewed");

        for (suite, path) in scraped {
            suite.save(&path, true)?;
        }

        if !zip_targets.is_empty() {
            let (pref, names);
            if zip_targets.keys().count() == 1 {
                let contest = zip_targets.keys().next().unwrap();
                pref = Cow::from(format!(
                    "https://www.hackerrank.com/rest/contests/{}/challenges/",
                    contest
                ));
                names = zip_targets.values().next().unwrap().clone();
            } else {
                pref = Cow::from("https://www.hackerrank.com/rest/contests/");
                names = zip_targets
                    .iter()
                    .flat_map(|(contest, problems)| {
                        problems
                            .iter()
                            .map(|problem| format!("{}/challenges/{}", contest, problem))
                            .collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>();
            };
            let urls = downloader::Urls {
                pref,
                names,
                suf: "/download_testcases",
            };
            let cookie = self.session.cookies_to_header();
            let mut downloader = sess_prop.zip_downloader()?;
            let zips = if *suppress_download_bars {
                downloader.download(io::sink(), &urls, cookie.as_ref())
            } else {
                downloader.download(io::stdout(), &urls, cookie.as_ref())
            }?;

            for (problem, zip) in urls.names.iter().zip(&zips) {
                let path = download_dir.join(format!("{}.zip", problem));
                ::fs::write(&path, zip)?;
                println!("Saved to {}", path.display());
            }
        }

        if *open_browser {
            for url in browser_urls {
                self.session.open_in_browser(&url)?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq)]
enum LoginOption {
    Explicit,
    Ensure,
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
                })
                .collect()
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
    use errors::SessionResult;
    use service::hackerrank::{Extract as _Extract, Hackerrank, ProblemQueryResponse};
    use service::session::{GetPost as _GetPost, HttpSession, UrlBase};
    use service::{self, UserNameAndPassword};
    use testsuite::TestSuite;

    use select::document::Document;
    use url::Host;

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

    fn start() -> SessionResult<Hackerrank> {
        let client = service::reqwest_client(Duration::from_secs(10))?;
        let base = UrlBase::new(Host::Domain("www.hackerrank.com"), true, None);
        let session = HttpSession::new(client, base, None)?;
        Ok(Hackerrank {
            session,
            credentials: UserNameAndPassword::None,
        })
    }
}
