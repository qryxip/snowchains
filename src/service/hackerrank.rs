use errors::{ServiceError, ServiceResult};
use service::session::{GetPost, HttpSession};
use service::{downloader, Contest, Credentials, DownloadProp, SessionProp};

use reqwest::header::Headers;
use reqwest::{Response, StatusCode};
use select::document::Document;
use select::predicate::Attr;

use std::{fmt, io};

pub(crate) fn login(sess_prop: &SessionProp) -> ServiceResult<()> {
    Hackerrank::start(sess_prop, true).map(|_| ())
}

pub(crate) fn download(
    sess_prop: &SessionProp,
    download_prop: DownloadProp<String>,
) -> ServiceResult<()> {
    let download_prop = download_prop.parse_contest();
    Hackerrank::start(sess_prop, false)?.download(sess_prop, &download_prop)
}

struct Hackerrank {
    session: HttpSession,
}

impl GetPost for Hackerrank {
    fn session(&mut self) -> &mut HttpSession {
        &mut self.session
    }
}

impl Hackerrank {
    fn start(
        sess_prop: &SessionProp,
        prints_message_when_already_logged_in: bool,
    ) -> ServiceResult<Self> {
        let session = sess_prop.start_session()?;
        let mut this = Hackerrank { session };
        let mut res = this.get("/login").acceptable(&[200, 302]).send()?;
        if res.status() == StatusCode::Found && prints_message_when_already_logged_in {
            eprintln!("Already signed in.");
        } else if res.status() == StatusCode::Ok {
            loop {
                if this.try_logging_in(res)? {
                    break println!("Succeeded to login.");
                }
                eprintln!("Failed to login. Try again.");
                this.session.clear_cookies()?;
                res = this.get("/login").send()?;
            }
        }
        Ok(this)
    }

    fn try_logging_in(&mut self, mut html: Response) -> ServiceResult<bool> {
        #[derive(Deserialize)]
        struct ResponseData {
            status: bool,
        }

        let (username, password) = Credentials::None.or_ask("Username: ")?;
        let token = Document::from(html.text()?.as_str()).extract_csrf_token()?;
        let status = self
            .post("/auth/login")
            .headers({
                let mut headers = Headers::new();
                headers.set_raw("X-CSRF-Token", token);
                headers
            })
            .acceptable(&[200])
            .send_json(&json!({
                "login": username.as_str(),
                "password": password.as_str(),
                "remember_me": true
            }))?
            .json::<ResponseData>()?
            .status;
        Ok(status)
    }

    fn download(
        &mut self,
        sess_prop: &SessionProp,
        download_prop: &DownloadProp<HackerrankContest>,
    ) -> ServiceResult<()> {
        let DownloadProp {
            contest,
            problems,
            download_dir,
            open_browser,
            ..
        } = download_prop;
        match (contest, problems.as_ref()) {
            (HackerrankContest::Contest(_), _) => unimplemented!(),
            (HackerrankContest::Challenges, None) => Err(ServiceError::PleaseSpecifyProblems),
            (HackerrankContest::Challenges, Some(problems)) => {
                let mut downloader = sess_prop.zip_downloader()?;
                let zips = downloader.download(
                    io::stdout(),
                    &downloader::Urls {
                        pref: "https://www.hackerrank.com/rest/contests/master/challenges/",
                        names: problems,
                        suf: "/download_testcases",
                    },
                    self.session.cookies_to_header().as_ref(),
                )?;
                for (problem, zip) in problems.iter().zip(&zips) {
                    let path = download_dir.join(format!("{}.zip", problem));
                    ::fs::write(&path, zip)?;
                    println!("Saved to {}", path.display());
                }
                if *open_browser {
                    for problem in problems {
                        let url = format!("/challenges/{}/problem", problem);
                        self.session.open_in_browser(&url)?;
                    }
                }
                Ok(())
            }
        }
    }
}

enum HackerrankContest {
    Challenges,
    Contest(String),
}

impl Contest for HackerrankContest {
    fn from_string(s: String) -> Self {
        if s.eq_ignore_ascii_case("challenges") {
            HackerrankContest::Challenges
        } else {
            HackerrankContest::Contest(s)
        }
    }
}

impl fmt::Display for HackerrankContest {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HackerrankContest::Challenges => write!(f, "challenges"),
            HackerrankContest::Contest(name) => write!(f, "contests/{}", name),
        }
    }
}

trait Extract {
    fn extract_csrf_token(&self) -> ServiceResult<String>;
}

impl Extract for Document {
    fn extract_csrf_token(&self) -> ServiceResult<String> {
        self.find(Attr("name", "csrf-token"))
            .next()
            .and_then(|node| node.attr("content").map(str::to_owned))
            .filter(|token| !token.is_empty())
            .ok_or(ServiceError::Scrape)
    }
}
