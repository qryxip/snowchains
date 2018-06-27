use errors::ServiceResult;
use service::session::HttpSession;
use service::{Credentials, DownloadProp, SessionProp};

use reqwest::header::Headers;
use reqwest::Response;
use select::document::Document;
use select::predicate::Attr;

use std::io::Read;

pub(crate) fn login(sess_prop: &SessionProp) -> ServiceResult<()> {
    HackerRank::start(sess_prop, true).map(|_| ())
}

pub(crate) fn download(sess_prop: &SessionProp, prop: &DownloadProp<String>) -> ServiceResult<()> {
    HackerRank::start(sess_prop, false)?.download(prop)
}

custom_derive! {
    #[derive(NewtypeDeref, NewtypeDerefMut)]
    struct HackerRank(HttpSession);
}

impl HackerRank {
    fn start(
        sess_prop: &SessionProp,
        prints_message_when_already_logged_in: bool,
    ) -> ServiceResult<Self> {
        let mut hackerrank = HackerRank(sess_prop.start_session()?);
        let mut response = hackerrank.get_expecting("/login", &[200, 302])?;
        if response.status().as_u16() == 302 && prints_message_when_already_logged_in {
            eprintln!("Already signed in.");
        } else if response.status().as_u16() == 200 {
            loop {
                if hackerrank.try_logging_in(response)? {
                    break println!("Succeeded to login.");
                }
                eprintln!("Failed to login. Try again.");
                hackerrank.clear_cookies()?;
                response = hackerrank.get("/login")?;
            }
        }
        Ok(hackerrank)
    }

    fn try_logging_in(&mut self, html: Response) -> ServiceResult<bool> {
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
        let csrf_token = extract_csrf_token(html)?;
        let data = PostData {
            login: &username,
            password: &password,
            remember_me: true,
        };
        let status = self
            .post_json("/auth/login", &data, &[200], {
                let mut headers = Headers::new();
                headers.set_raw("X-CSRF-Token", csrf_token);
                headers
            })?
            .json::<ResponseData>()?
            .status;
        Ok(status)
    }

    fn download(&mut self, _: &DownloadProp<String>) -> ServiceResult<()> {
        unimplemented!()
    }
}

fn extract_csrf_token(html: impl Read) -> ServiceResult<String> {
    fn extract(document: &Document) -> Option<String> {
        document
            .find(Attr("name", "csrf-token"))
            .next()?
            .attr("content")
            .map(str::to_owned)
    }

    super::quit_on_failure(extract(&Document::from_read(html)?), String::is_empty)
}
