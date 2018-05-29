use errors::{
    FileIoError, FileIoErrorKind, SerializeError, SessionError, SessionResult, StartSessionError,
};
use terminal::Color;
use util;

use cookie::{self, CookieJar};
use failure::ResultExt as _ResultExt;
use reqwest::header::{ContentType, Headers, Location, SetCookie, UserAgent};
use reqwest::{self, header, Method, RedirectPolicy, RequestBuilder, Response, StatusCode};
use robots_txt::{Robots, SimpleMatcher};
use serde::Serialize;
use url::{Host, Url};
use {bincode, serde_json, serde_urlencoded};

use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::io::{self, Read, Seek, SeekFrom, Write as _IoWrite};
use std::path::PathBuf;
use std::time::Duration;

static USER_AGENT: &str = "snowchains <https://github.com/wariuni/snowchains>";

/// A wrapper of `reqwest::Client`.
#[derive(Debug)]
pub(crate) struct HttpSession {
    client: reqwest::Client,
    robots_txts: HashMap<String, String>,
    base: Option<UrlBase>,
    jar: Option<AutosavedCookieJar>,
}

impl HttpSession {
    pub fn new(
        base: impl Into<Option<UrlBase>>,
        timeout: impl Into<Option<Duration>>,
        cookies_path: impl Into<Option<PathBuf>>,
    ) -> SessionResult<Self> {
        let start = || -> SessionResult<HttpSession> {
            let client = reqwest::Client::builder()
                .redirect(RedirectPolicy::none())
                .timeout(timeout)
                .referer(false)
                .default_headers({
                    let mut headers = Headers::new();
                    headers.set(UserAgent::new(USER_AGENT));
                    headers
                })
                .build()?;
            let base = base.into();
            let host = base.as_ref().map(|base| base.host.clone());
            let jar = match cookies_path.into() {
                Some(path) => Some(AutosavedCookieJar::new(path)?),
                None => None,
            };
            let mut session = Self {
                client,
                robots_txts: hashmap!(),
                base,
                jar,
            };
            if let Some(host) = host {
                let mut response = session.get_expecting("/robots.txt", &[200, 301, 302, 404])?;
                while [301, 302].contains(&response.status().as_u16()) {
                    let location = response
                        .headers()
                        .get::<Location>()
                        .map(|location| (*location).to_owned());
                    if let Some(location) = location {
                        response = session.get_expecting(&location, &[200, 301, 302, 404])?;
                    } else {
                        return Ok(session);
                    }
                }
                match response.status().as_u16() {
                    200 => {
                        session
                            .robots_txts
                            .insert(host.to_string(), response.text()?);
                    }
                    404 => (),
                    _ => unreachable!(),
                }
            }
            Ok(session)
        };
        start().context(StartSessionError).map_err(Into::into)
    }

    /// Whether it has any cookie value.
    pub fn has_cookie(&self) -> bool {
        match self.jar.as_ref() {
            Some(jar) => jar.inner.iter().next().is_some(),
            None => false,
        }
    }

    /// Removes all cookies.
    pub fn clear_cookies(&mut self) -> SessionResult<()> {
        if let Some(jar) = self.jar.as_mut() {
            jar.inner = CookieJar::new();
            jar.save()?;
        }
        Ok(())
    }

    /// If `url` starts with '/' and the base host is present, returns
    /// http(s)://<host><url>.
    pub fn resolve_url<'a>(&self, url: &'a str) -> SessionResult<Url> {
        match self.base.as_ref() {
            Some(base) => base.with(url),
            None => Url::parse(url).map_err(|e| SessionError::ParseUrl(url.to_owned(), e)),
        }
    }

    /// Sends a GET request to `url`, expecting the response status code is 200.
    pub fn get(&mut self, url: &str) -> SessionResult<Response> {
        self.request(Method::Get, url, &[200], |mut request| request.send())
    }

    /// Sends a GET request to `url`.
    pub fn get_expecting(
        &mut self,
        url: &str,
        expected_statuses: &[u16],
    ) -> SessionResult<Response> {
        self.request(Method::Get, url, expected_statuses, |mut request| {
            request.send()
        })
    }

    /// Sends a POST request to `url`, serializing `data`.
    pub fn post_json(
        &mut self,
        url: &str,
        data: &(impl fmt::Debug + Serialize),
        expected_statuses: &[u16],
        extra_headers: impl Into<Option<Headers>>,
    ) -> SessionResult<Response> {
        self.post(
            url,
            serde_json::to_string(data).map_err(|e| SerializeError::new(data, e))?,
            ContentType::json(),
            expected_statuses,
            extra_headers,
        )
    }

    /// Sends a POST request to `url`, serializing `data`.
    pub fn post_urlencoded(
        &mut self,
        url: &str,
        data: &(impl fmt::Debug + Serialize),
        expected_statuses: &[u16],
        extra_headers: impl Into<Option<Headers>>,
    ) -> SessionResult<Response> {
        self.post(
            url,
            serde_urlencoded::to_string(data).map_err(|e| SerializeError::new(data, e))?,
            ContentType::form_url_encoded(),
            expected_statuses,
            extra_headers,
        )
    }

    fn post(
        &mut self,
        url: &str,
        body: impl Into<String>,
        content_type: ContentType,
        expected_statuses: &[u16],
        extra_headers: impl Into<Option<Headers>>,
    ) -> SessionResult<Response> {
        let body = body.into();
        self.request(Method::Post, url, expected_statuses, move |mut request| {
            request.body(body).header(content_type);
            if let Some(extra_headers) = extra_headers.into() {
                request.headers(extra_headers);
            }
            request.send()
        })
    }

    fn request(
        &mut self,
        method: Method,
        url: &str,
        expected_statuses: &[u16],
        f: impl FnOnce(RequestBuilder) -> reqwest::Result<Response>,
    ) -> SessionResult<Response> {
        let url = self.resolve_url(url)?;
        self.assert_not_forbidden_by_robots_txt(&url)?;
        let expected_statuses = statuses_from(expected_statuses);
        echo_method(&method, &url);
        let mut request = self.client.request(method, url.as_str());
        if let Some(jar) = self.jar.as_mut() {
            request.header(jar.to_header());
        }
        let response = f(request).map_err(|e| {
            println!();
            e
        })?;
        response.echo_status(&expected_statuses);
        if let Some(jar) = self.jar.as_mut() {
            jar.update(&response)?;
        }
        response.filter_by_status(expected_statuses)
    }

    fn assert_not_forbidden_by_robots_txt(&self, url: &Url) -> SessionResult<()> {
        if let Some(host) = url.host_str() {
            if let Some(robots_txt) = self.robots_txts.get(host) {
                let robots = Robots::from_str(robots_txt);
                let matcher = SimpleMatcher::new(&robots.choose_section(USER_AGENT).rules);
                if !matcher.check_path(url.path()) {
                    return Err(SessionError::ForbiddenByRobotsTxt);
                }
            }
        }
        Ok(())
    }
}

fn statuses_from(from: &[u16]) -> Vec<StatusCode> {
    from.iter()
        .map(|&n| StatusCode::try_from(n).unwrap_or_else(|_| StatusCode::Unregistered(n)))
        .collect::<Vec<StatusCode>>()
}

fn echo_method(method: &Method, url: &Url) {
    print_bold!(Color::None, "{} ", method);
    print_bold!(Color::Url, "{}", url);
    print!(" ... ");
    io::stdout().flush().unwrap();
}

trait ResponseExt
where
    Self: Sized,
{
    fn echo_status(&self, expected_statuses: &[StatusCode]);
    fn filter_by_status(self, expected: Vec<StatusCode>) -> SessionResult<Self>;
}

impl ResponseExt for Response {
    fn echo_status(&self, expected_statuses: &[StatusCode]) {
        if expected_statuses.contains(&self.status()) {
            println_bold!(Color::Success, "{}", self.status());
        } else {
            println_bold!(Color::Fatal, "{}", self.status());
        }
    }

    fn filter_by_status(self, expected: Vec<StatusCode>) -> SessionResult<Self> {
        if expected.is_empty() || expected.contains(&self.status()) {
            Ok(self)
        } else {
            Err(SessionError::UnexpectedStatusCode(expected, self.status()))
        }
    }
}

#[derive(Debug)]
pub(crate) struct UrlBase {
    host: Host<&'static str>,
    https: bool,
    port: Option<u16>,
}

impl UrlBase {
    pub fn new(host: Host<&'static str>, https: bool, port: Option<u16>) -> Self {
        Self { host, https, port }
    }

    fn with(&self, relative_or_absolute_url: &str) -> SessionResult<Url> {
        let mut url = Cow::from(relative_or_absolute_url);
        if url.starts_with('/') {
            url = format!(
                "http{}://{}{}{}",
                if self.https { "s" } else { "" },
                self.host,
                match self.port {
                    Some(port) => format!(":{}", port),
                    None => "".to_owned(),
                },
                url,
            ).into();
        }
        Url::parse(&url).map_err(|e| SessionError::ParseUrl(url.into_owned(), e))
    }
}

#[derive(Debug)]
struct AutosavedCookieJar {
    path: PathBuf,
    file: File,
    inner: CookieJar,
}

impl AutosavedCookieJar {
    fn new(path: impl Into<PathBuf>) -> SessionResult<Self> {
        let path = path.into();
        let exists = path.exists();
        let mut file = util::fs::create_and_lock(&path)?;
        let mut inner = CookieJar::new();
        if exists {
            let mut cookies =
                Vec::with_capacity(file.metadata().map(|m| m.len() as usize + 1).unwrap_or(0));
            file.read_to_end(&mut cookies)
                .map_err(|e| FileIoError::chaining(FileIoErrorKind::Read, &path, e))?;
            if !cookies.is_empty() {
                let cookies = bincode::deserialize::<Vec<String>>(&cookies)
                    .map_err(|e| FileIoError::chaining(FileIoErrorKind::Deserialize, &path, e))?;
                for cookie in cookies {
                    let cookie = cookie::Cookie::parse(cookie.clone()).map_err(|e| {
                        SessionError::ParseCookieFromPath(cookie, path.to_owned(), e)
                    })?;
                    inner.add(cookie);
                }
            }
        } else {
            file.write_all(&bincode::serialize(&Vec::<String>::new()).unwrap())
                .map_err(|e| FileIoError::chaining(FileIoErrorKind::Write, &path, e))?;
        }
        Ok(Self { file, path, inner })
    }

    fn to_header(&self) -> header::Cookie {
        self.inner
            .iter()
            .fold(header::Cookie::new(), |mut header, cookie| {
                header.append(cookie.name().to_owned(), cookie.value().to_owned());
                header
            })
    }

    fn update(&mut self, response: &Response) -> SessionResult<()> {
        if let Some(setcookie) = response.headers().get::<SetCookie>() {
            for cookie in setcookie.iter() {
                let cookie = cookie.to_owned();
                let cookie = cookie::Cookie::parse(cookie.clone()).map_err(|e| {
                    SessionError::ParseCookieFromUrl(cookie, response.url().to_owned(), e)
                })?;
                self.inner.add(cookie);
            }
        }
        self.save().map_err(Into::into)
    }

    fn save(&mut self) -> SessionResult<()> {
        let value = self
            .inner
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>();
        let value = bincode::serialize(&value).map_err(|e| SerializeError::new(&value, e))?;
        self.file
            .seek(SeekFrom::Start(0))
            .and_then(|_| self.file.set_len(0))
            .and_then(|()| self.file.write_all(&value))
            .map_err(|e| FileIoError::chaining(FileIoErrorKind::Write, &self.path, e).into())
    }
}

#[cfg(test)]
mod tests {
    use errors::{FileIoError, SessionError};
    use service::session::{HttpSession, UrlBase};

    use failure::Fail as _Fail;
    use nickel::{self, Nickel};
    use tempdir::TempDir;
    use url::Host;
    use {env_logger, reqwest};

    use std::net::Ipv4Addr;
    use std::panic;

    #[test]
    #[ignore]
    fn it_works() {
        let _ = env_logger::try_init();
        let server = {
            let mut server = Nickel::new();
            server.utilize(router! {
                get "/" => |_, mut response| {
                    response.headers_mut().set(
                        nickel::hyper::header::SetCookie(vec!["foo=bar".to_owned()]));
                    ""
                }
                get "/robots.txt" => { "User-agent: *\nDisallow: /sensitive" }
            });
            server.listen("127.0.0.1:2000").unwrap()
        };
        let result = panic::catch_unwind(|| {
            let base = UrlBase::new(Host::Ipv4(Ipv4Addr::new(127, 0, 0, 1)), false, Some(2000));
            let mut session = HttpSession::new(Some(base), None, None).unwrap();
            let response = session.get("/").unwrap();
            assert!(
                response
                    .headers()
                    .get::<reqwest::header::SetCookie>()
                    .is_some()
            );
            session.get_expecting("/nonexisting", &[404]).unwrap();
            session.get_expecting("/nonexisting", &[]).unwrap();
            match session.get("/sensitive").unwrap_err() {
                SessionError::ForbiddenByRobotsTxt => {}
                e => panic!("{:?}", e),
            }
        });
        server.detach();
        result.unwrap_or_else(|p| panic::resume_unwind(p));
    }

    #[test]
    #[ignore]
    fn it_keeps_a_file_locked_while_alive() {
        let _ = env_logger::try_init();
        let tempdir = TempDir::new("it_keeps_a_file_locked_while_alive").unwrap();
        let path = tempdir.path().join("cookies");
        HttpSession::new(None, None, path.clone()).unwrap();
        HttpSession::new(None, None, path.clone()).unwrap();
        let _session = HttpSession::new(None, None, path.clone()).unwrap();
        let err = HttpSession::new(None, None, path.clone()).unwrap_err();
        if let SessionError::Start(ref ctx) = err {
            if ctx.cause().is_some() {
                return;
            }
        }
        panic!("{:?}", err);
    }
}
