use errors::{ServiceError, ServiceResult};
use service::session::{GetPost, HttpSession};
use service::{Credentials, DownloadProp, SessionProp};

use reqwest::header::Headers;
use reqwest::{Response, StatusCode};
use select::document::Document;
use select::predicate::Attr;

pub(crate) fn login(sess_prop: &SessionProp) -> ServiceResult<()> {
    Hackerrank::start(sess_prop, true).map(|_| ())
}

pub(crate) fn download(sess_prop: &SessionProp, prop: &DownloadProp<String>) -> ServiceResult<()> {
    Hackerrank::start(sess_prop, false)?.download(prop)
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
        let mut hackerrank = Hackerrank { session };
        let mut res = hackerrank.get("/login").acceptable(&[200, 302]).send()?;
        if res.status() == StatusCode::Found && prints_message_when_already_logged_in {
            eprintln!("Already signed in.");
        } else if res.status() == StatusCode::Ok {
            loop {
                if hackerrank.try_logging_in(res)? {
                    break println!("Succeeded to login.");
                }
                eprintln!("Failed to login. Try again.");
                hackerrank.session.clear_cookies()?;
                res = hackerrank.get("/login").send()?;
            }
        }
        Ok(hackerrank)
    }

    fn try_logging_in(&mut self, mut html: Response) -> ServiceResult<bool> {
        #[derive(Debug, Serialize)]
        struct PostData<'a> {
            login: &'a str,
            password: &'a str,
            remember_me: bool,
        }

        #[derive(Deserialize)]
        struct ResponseData {
            status: bool,
        }

        let (username, password) = Credentials::None.or_ask("Username: ")?;
        let csrf_token = Document::from(html.text()?.as_str()).extract_csrf_token()?;
        let data = PostData {
            login: &username,
            password: &password,
            remember_me: true,
        };
        let status = self
            .post("/auth/login")
            .headers({
                let mut headers = Headers::new();
                headers.set_raw("X-CSRF-Token", csrf_token);
                headers
            })
            .acceptable(&[200])
            .send_json(&data)?
            .json::<ResponseData>()?
            .status;
        Ok(status)
    }

    fn download(&mut self, _: &DownloadProp<String>) -> ServiceResult<()> {
        unimplemented!()
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
