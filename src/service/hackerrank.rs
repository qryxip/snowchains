use crate::errors::{ScrapeError, ScrapeResult, ServiceErrorKind, ServiceResult};
use crate::service::download::DownloadProgress;
use crate::service::session::{self, HttpSession};
use crate::service::{
    Contest, DownloadProps, ExtractZip, PrintTargets as _PrintTargets, ProblemNameConversion,
    Service, SessionProps, UserNameAndPassword, ZipEntries, ZipEntriesSorting,
};
use crate::terminal::{HasTerm, Term, WriteAnsi};
use crate::testsuite::{SimpleSuite, TestSuite};

use itertools::Itertools as _Itertools;
use log::warn;
use once_cell::sync::Lazy;
use once_cell::sync_lazy;
use regex::Regex;
use reqwest::StatusCode;
use select::document::Document;
use select::predicate::{Predicate, Text};
use serde::{Deserialize, Deserializer};
use serde_derive::Deserialize;
use serde_json::json;
use tokio::runtime::{Runtime, TaskExecutor};

use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::io::{self, Write as _Write};

pub(crate) fn login(sess_props: SessionProps<impl Term>) -> ServiceResult<()> {
    Hackerrank::try_new(sess_props)?.login(LoginOption::Explicit)
}

pub(crate) fn download(
    mut sess_props: SessionProps<impl Term>,
    download_props: DownloadProps<String>,
) -> ServiceResult<()> {
    let download_props = download_props.convert_contest_and_problems(ProblemNameConversion::Kebab);
    download_props.print_targets(sess_props.term.stdout())?;
    Hackerrank::try_new(sess_props)?.download(&download_props)
}

struct Hackerrank<T: Term> {
    term: T,
    session: HttpSession,
    runtime: Runtime,
    credentials: UserNameAndPassword,
}

impl<T: Term> HasTerm for Hackerrank<T> {
    type Term = T;

    fn term(&mut self) -> &mut T {
        &mut self.term
    }
}

impl<T: Term> Service for Hackerrank<T> {
    type Write = T::Stdout;

    fn requirements(&mut self) -> (&mut T::Stdout, &mut HttpSession, &mut Runtime) {
        (self.term.stdout(), &mut self.session, &mut self.runtime)
    }
}

impl<T: Term> DownloadProgress for Hackerrank<T> {
    type Write = T::Stdout;

    fn requirements(&mut self) -> (&mut T::Stdout, &HttpSession, TaskExecutor) {
        (self.term.stdout(), &self.session, self.runtime.executor())
    }
}

impl<T: Term> ExtractZip for Hackerrank<T> {
    type Write = T::Stdout;

    fn out(&mut self) -> &mut T::Stdout {
        self.term.stdout()
    }
}

impl<T: Term> Hackerrank<T> {
    fn try_new(mut sess_props: SessionProps<T>) -> ServiceResult<Self> {
        let credentials = sess_props.credentials.hackerrank.clone();
        let mut runtime = Runtime::new()?;
        let session = sess_props.start_session(&mut runtime)?;
        Ok(Hackerrank {
            term: sess_props.term,
            session,
            runtime,
            credentials,
        })
    }

    fn login(&mut self, option: LoginOption) -> ServiceResult<()> {
        let mut res = self.get("/auth/login").acceptable(&[200, 302]).send()?;
        if res.status() == StatusCode::FOUND && option == LoginOption::Explicit {
            writeln!(self.stderr(), "Already signed in.")?;
            self.stderr().flush()?;
        } else if res.status() == StatusCode::OK {
            let (mut username, mut password, on_test) = {
                match self.credentials.take() {
                    UserNameAndPassword::Some(username, password) => (username, password, true),
                    UserNameAndPassword::None => (
                        self.prompt_reply_stderr("Username: ")?,
                        self.prompt_password_stderr("Password: ")?,
                        false,
                    ),
                }
            };
            if option == LoginOption::NotNecessary
                && !on_test
                && !self.ask_yes_or_no("Login? ", false)?
            {
                return Ok(());
            }
            loop {
                if self.try_logging_in(&username, &password, res)? {
                    writeln!(self.stdout(), "Succeeded to login.")?;
                    break self.stdout().flush()?;
                }
                if on_test {
                    return Err(ServiceErrorKind::LoginOnTest.into());
                }
                username = self.prompt_reply_stderr("Username: ")?;
                password = self.prompt_password_stderr("Password: ")?;
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
        response: session::Response,
    ) -> ServiceResult<bool> {
        #[derive(Deserialize)]
        struct LoginResponse {
            status: bool,
        }

        let csrf_token = response.document(&mut self.runtime)?.extract_csrf_token()?;
        let status = self
            .post("/auth/login")
            .x_csrf_token(&csrf_token)
            .acceptable(&[200])
            .send_json(&json!({
                "login": username,
                "password": password,
                "remember_me": true
            }))?
            .json::<LoginResponse>(&mut self.runtime)?
            .status;
        Ok(status)
    }

    fn download(&mut self, download_props: &DownloadProps<HackerrankContest>) -> ServiceResult<()> {
        fn warn_unless_empty(
            mut stderr: impl WriteAnsi,
            problems: &[impl AsRef<str>],
            verb: &str,
        ) -> io::Result<()> {
            if !problems.is_empty() {
                let suf = if problems.len() == 1 { "" } else { "s" };
                let problems = problems
                    .iter()
                    .map(|problem| format!("{:?}", problem.as_ref()))
                    .format(", ");
                stderr.with_reset(|o| {
                    writeln!(o.fg(11)?, "Following problem{} {}: {}", suf, verb, problems)
                })?;
                stderr.flush()?;
            }
            Ok(())
        }

        let DownloadProps {
            contest,
            problems,
            destinations,
            open_browser,
        } = download_props;
        let problems = problems.as_ref();

        let models = match (contest, problems) {
            (HackerrankContest::Master, None) => {
                return Err(ServiceErrorKind::PleaseSpecifyProblems.into())
            }
            (HackerrankContest::Master, Some(problems)) => problems
                .iter()
                .map(|problem| {
                    self.get(&format!("/rest/contests/master/challenges/{}", problem))
                        .recv_json::<ProblemQueryResponse>()
                        .map(|r| r.model)
                })
                .collect::<ServiceResult<Vec<_>>>()?,
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
                    .collect::<ServiceResult<Vec<_>>>()?
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
                let path = destinations.expand(&model.slug)?;
                scraped.push((model.slug, suite, path));
            } else if !model.can_be_viewed {
                cannot_view.push(model.slug);
            } else {
                warn!("{:?}", model);
            }
        }

        warn_unless_empty(self.stderr(), &cannot_view, "cannot be viewed")?;

        for (name, suite, path) in scraped {
            suite.save(&name, &path, self.stdout())?;
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
                    })
                    .collect::<Vec<_>>();
            };
            let urls = names
                .iter()
                .map(|name| format!("{}{}/download_testcases", url_pref, name))
                .collect::<Vec<_>>();
            for (zip, name) in self
                .download_progress(&urls, &names, None)?
                .into_iter()
                .zip(&names)
            {
                static ZIP_ENTRIES: Lazy<ZipEntries> = sync_lazy!(ZipEntries {
                    in_entry: Regex::new(r"\Ainput/input([0-9]+)\.txt\z").unwrap(),
                    in_match_group: 1,
                    in_crlf_to_lf: true,
                    out_entry: Regex::new(r"\Aoutput/output([0-9]+)\.txt\z").unwrap(),
                    out_match_group: 1,
                    out_crlf_to_lf: true,
                    sortings: vec![ZipEntriesSorting::Number],
                });
                let paths =
                    self.extract_zip(name, &zip, &destinations.text_file_dir(name)?, &ZIP_ENTRIES)?;
                TestSuite::from(SimpleSuite::new(None).paths(paths)).save(
                    name,
                    &destinations.expand(name)?,
                    self.stdout(),
                )?
            }
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
    fn extract_csrf_token(&self) -> ScrapeResult<String>;
    fn extract_samples(&self) -> ScrapeResult<TestSuite>;
}

impl Extract for Document {
    fn extract_csrf_token(&self) -> ScrapeResult<String> {
        self.find(selector!("[name=\"csrf-token\"]"))
            .next()
            .and_then(|node| node.attr("content").map(str::to_owned))
            .filter(|token| !token.is_empty())
            .ok_or_else(ScrapeError::new)
    }

    fn extract_samples(&self) -> ScrapeResult<TestSuite> {
        fn extract_item(this: &Document, predicate: impl Predicate) -> Vec<String> {
            this.find(predicate)
                .map(|pre| {
                    pre.find(selector!("span.err").child(Text))
                        .map(|text| text.text())
                        .join("")
                })
                .collect()
        }

        let inputs = extract_item(self, selector!(".challenge_sample_input pre"));
        let outputs = extract_item(self, selector!(".challenge_sample_output pre"));
        if inputs.len() != outputs.len() || inputs.is_empty() {
            return Err(ScrapeError::new());
        }
        let samples = inputs.into_iter().zip(outputs);
        Ok(SimpleSuite::new(None)
            .sample_cases(samples, |i| format!("Sample {}", i), Some("Sample"))
            .into())
    }
}

#[cfg(test)]
mod tests {
    use crate::errors::ServiceResult;
    use crate::service::hackerrank::{Extract as _Extract, Hackerrank, ProblemQueryResponse};
    use crate::service::session::{HttpSession, UrlBase};
    use crate::service::{self, Service as _Service, UserNameAndPassword};
    use crate::terminal::{Term, TermImpl};
    use crate::testsuite::{SimpleSuite, TestSuite};

    use select::document::Document;
    use tokio::runtime::Runtime;
    use url::Host;

    use std::time::Duration;

    #[test]
    fn it_scrapes_samples() {
        let expected = TestSuite::from(
            SimpleSuite::new(None).sample_cases(
                vec![
                    ("50 40 70 60", "YES"),
                    ("55 66 66 77", "YES"),
                    ("80 80 40 40", "NO"),
                ]
                .into_iter(),
                |i| format!("Sample {}", i),
                Some("Sample"),
            ),
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

    fn start() -> ServiceResult<Hackerrank<impl Term>> {
        let client = service::reqwest_client(Duration::from_secs(60))?;
        let base = UrlBase::new(Host::Domain("www.hackerrank.com"), true, None);
        let mut term = TermImpl::null();
        let mut runtime = Runtime::new()?;
        let session = HttpSession::try_new(term.stdout(), &mut runtime, client, base, None, true)?;
        Ok(Hackerrank {
            term,
            session,
            runtime,
            credentials: UserNameAndPassword::None,
        })
    }
}
