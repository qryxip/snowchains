use errors::{SessionErrorKind, SessionResult, SessionResultExt};
use terminal::Color;
use util;

use cookie::{self, CookieJar};
use reqwest::header::{ContentType, Headers, Location, SetCookie, UserAgent};
use reqwest::{self, header, Method, RedirectPolicy, RequestBuilder, Response, StatusCode};
use robots_txt::{Robots, SimpleMatcher};
use serde::Serialize;
use url::{self, Host, Url};
use {bincode, serde_json, serde_urlencoded};

use std;
use std::collections::HashMap;
use std::fmt::Write as _FmtWrite;
use std::fs::File;
use std::io::{self, Read, Seek, SeekFrom, Write as _IoWrite};
use std::ops::Deref;
use std::path::PathBuf;
use std::time::Duration;

/// A wrapper of `reqwest::Client`.
#[derive(Debug)]
pub(crate) struct HttpSession {
    client: reqwest::Client,
    cookie_jar: CookieJar,
    robots_txts: HashMap<String, String>,
    user_agent: Option<UserAgent>,
    base: Option<Url>,
    cookies_file: Option<File>,
}

impl HttpSession {
    /// Creates a `HttpSessionBuilder`.
    pub fn builder() -> HttpSessionBuilder<BaseNone> {
        HttpSessionBuilder::new()
    }

    /// Whether it has any cookie value.
    pub fn has_cookie(&self) -> bool {
        self.cookie_jar.iter().next().is_some()
    }

    /// Removes all cookies.
    pub fn clear_cookies(&mut self) -> SessionResult<()> {
        self.cookie_jar = CookieJar::new();
        self.save()
    }

    /// If `url` starts with '/' and the base host is present, returns
    /// http(s)://<host><url>.
    ///
    /// Errors
    ///
    /// Fails if `url` is relative and the host not set.
    pub fn resolve_url<'a>(&self, url: &'a str) -> SessionResult<Url> {
        if url.starts_with('/') {
            if let Some(base) = &self.base {
                return base.join(&url[1..])
                    .chain_err(|| SessionErrorKind::ParseUrl(url.to_owned()));
            }
        }
        Url::parse(url).chain_err(|| SessionErrorKind::ParseUrl(url.to_owned()))
    }

    /// Sends a GET request to `url`, expecting the response status code is 200.
    ///
    /// Errors
    ///
    /// Fails if:
    /// - `url` is invalid
    /// - The stored "robots.txt" disallows accesses to `url`
    /// - The status code is not in `expected_statuses`.
    /// - IO error occurred (file, terminal)
    /// - The HTTP request fails, or redirected too many times (see
    ///   `reqwest::RequestBuilder::send`)
    /// - Received invalid "Set-Cookie" (hardly possbile)
    pub fn get(&mut self, url: &str) -> SessionResult<Response> {
        self.request(Method::Get, url, &[200], |mut request| request.send())
    }

    /// Sends a GET request to `url`.
    ///
    /// Errors
    ///
    /// Fails if:
    /// - `url` is invalid
    /// - The stored "robots.txt" disallows accesses to `url`
    /// - The status code is not in `expected_statuses`.
    /// - IO error occurred (file, terminal)
    /// - The HTTP request fails, or redirected too many times (see
    ///   `reqwest::RequestBuilder::send`)
    /// - Received invalid "Set-Cookie" (hardly possbile)
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
    ///
    /// Errors
    ///
    /// Fails if:
    /// - `url` is invalid
    /// - The stored "robots.txt" disallows accesses to `url`
    /// - The status code is not in `expected_statuses`.
    /// - IO error occurred (file, terminal)
    /// - The HTTP request fails, or redirected too many times (see
    ///   `reqwest::RequestBuilder::send`)
    /// - Received invalid "Set-Cookie" (hardly possbile)
    pub fn post_json(
        &mut self,
        url: &str,
        data: &impl Serialize,
        expected_statuses: &[u16],
        extra_headers: impl Into<Option<Headers>>,
    ) -> SessionResult<Response> {
        self.post(
            url,
            serde_json::to_string(&data)?,
            ContentType::json(),
            expected_statuses,
            extra_headers,
        )
    }

    /// Sends a POST request to `url`, serializing `data`.
    ///
    /// Errors
    ///
    /// Fails if:
    /// - `url` is invalid
    /// - The stored "robots.txt" disallows accesses to `url`
    /// - The status code is not in `expected_statuses`.
    /// - IO error occurred (file, terminal)
    /// - The HTTP request fails, or redirected too many times (see
    ///   `reqwest::RequestBuilder::send`)
    /// - Received invalid "Set-Cookie" (hardly possbile)
    pub fn post_urlencoded(
        &mut self,
        url: &str,
        data: &impl Serialize,
        expected_statuses: &[u16],
        extra_headers: impl Into<Option<Headers>>,
    ) -> SessionResult<Response> {
        self.post(
            url,
            serde_urlencoded::to_string(&data)?,
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
        request.header(self.cookie_jar.as_cookie_header());
        let response = f(request).map_err(|e| {
            println!();
            e
        })?;
        response.echo_status(&expected_statuses);
        self.cookie_jar.update(&response)?;
        self.save()?;
        response.filter_by_status(expected_statuses)
    }

    fn assert_not_forbidden_by_robots_txt(&self, url: &Url) -> SessionResult<()> {
        if let Some(host) = url.host_str() {
            if let Some(robots_txt) = self.robots_txts.get(host) {
                let user_agent = self.user_agent.as_ref().map(Deref::deref).unwrap_or("*");
                let robots = Robots::from_str(robots_txt);
                let matcher = SimpleMatcher::new(&robots.choose_section(user_agent).rules);
                if !matcher.check_path(url.path()) {
                    bail!(SessionErrorKind::ForbiddenByRobotsTxt);
                }
            }
        }
        Ok(())
    }

    fn save(&mut self) -> SessionResult<()> {
        if let Some(file) = &mut self.cookies_file {
            let cookies = self.cookie_jar
                .iter()
                .map(cookie::Cookie::to_string)
                .collect::<Vec<_>>();
            let cookies = bincode::serialize(&cookies)?;
            file.rewrite(&cookies)?;
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

trait CookieJarExt {
    fn as_cookie_header(&self) -> header::Cookie;
    fn update(&mut self, response: &Response) -> std::result::Result<(), cookie::ParseError>;
}

impl CookieJarExt for CookieJar {
    fn as_cookie_header(&self) -> header::Cookie {
        self.iter()
            .fold(header::Cookie::new(), |mut header, cookie| {
                header.append(cookie.name().to_owned(), cookie.value().to_owned());
                header
            })
    }

    fn update(&mut self, response: &Response) -> std::result::Result<(), cookie::ParseError> {
        if let Some(setcookie) = response.headers().get::<SetCookie>() {
            for cookie in setcookie.iter() {
                let cookie = cookie::Cookie::parse(cookie.to_string())?;
                self.add(cookie);
            }
        }
        Ok(())
    }
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
            bail!(SessionErrorKind::UnexpectedStatusCode(
                expected,
                self.status()
            ))
        }
    }
}

/// Builder of `HttpSession`.
#[derive(Debug)]
pub(crate) struct HttpSessionBuilder<B: BaseOption> {
    client_builder: reqwest::ClientBuilder,
    base: B,
    default_headers: Vec<Headers>,
    cookies_file_path: Option<PathBuf>,
}

impl Default for HttpSessionBuilder<BaseNone> {
    fn default() -> Self {
        Self::new()
    }
}

impl HttpSessionBuilder<BaseNone> {
    /// Creates a `HttpSessionBuilder`.
    pub fn new() -> Self {
        Self {
            client_builder: reqwest::Client::builder(),
            base: BaseNone,
            default_headers: vec![],
            cookies_file_path: None,
        }
    }

    pub fn base<S: AsRef<str>>(
        self,
        host: Host<S>,
        https: bool,
        port: Option<u16>,
    ) -> HttpSessionBuilder<BaseSome<S>> {
        HttpSessionBuilder {
            client_builder: self.client_builder,
            base: BaseSome(host, https, port),
            default_headers: self.default_headers,
            cookies_file_path: self.cookies_file_path,
        }
    }
}

impl<S: AsRef<str>> HttpSessionBuilder<BaseSome<S>> {
    /// Builds fetching "/robots.txt".
    pub fn with_robots_txt(self) -> SessionResult<HttpSession> {
        let host = self.base.0.to_string();
        let mut session = self.build()?;
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
                let robots_txt = util::string_from_read(response, 10240)?;
                session.robots_txts.insert(host, robots_txt);
            }
            404 => (),
            _ => unreachable!(),
        }
        Ok(session)
    }
}

impl<B: BaseOption> HttpSessionBuilder<B> {
    /// Builds.
    pub fn build(mut self) -> SessionResult<HttpSession> {
        let user_agent = self.default_headers
            .iter()
            .flat_map(|headers| headers.get::<UserAgent>())
            .next()
            .cloned();
        for headers in self.default_headers {
            self.client_builder.default_headers(headers);
        }
        let client = self.client_builder.build()?;
        let (cookie_jar, cookies_file) = match &self.cookies_file_path {
            None => (CookieJar::new(), None),
            Some(path) => {
                let mut file = util::fs::create_and_lock(path)?;
                let mut jar = Vec::with_capacity(1024);
                file.read_to_end(&mut jar)?;
                let jar = if jar.is_empty() {
                    CookieJar::new()
                } else {
                    let cookies = bincode::deserialize::<Vec<String>>(&jar)?;
                    let mut jar = CookieJar::new();
                    for cookie in cookies {
                        jar.add(cookie::Cookie::parse(cookie)?);
                    }
                    jar
                };
                (jar, Some(file))
            }
        };
        let base = self.base.try_into_option_url()?;
        Ok(HttpSession {
            client,
            cookie_jar,
            robots_txts: HashMap::new(),
            user_agent,
            base,
            cookies_file,
        })
    }

    /// Adds `Headers`.
    pub fn default_headers(mut self, headers: Headers) -> Self {
        self.default_headers.push(headers);
        self
    }

    /// Sets a `RedirectPolicy`.
    ///
    /// Default will follow redirects up to maximum of 10.
    pub fn redirect(mut self, policy: RedirectPolicy) -> Self {
        self.client_builder.redirect(policy);
        self
    }

    /// Enables or disables automatic setting of the `Referer` header.
    ///
    /// Default is `true`.
    pub fn referer(mut self, enable: bool) -> Self {
        self.client_builder.referer(enable);
        self
    }

    /// Sets a timeout for connect and r/w operations.
    ///
    /// Default is 30 seconds.
    ///
    /// To disable timeout, pass `None`.
    pub fn timeout(mut self, timeout: impl Into<Option<Duration>>) -> Self {
        self.client_builder.timeout(timeout);
        self
    }

    /// Sets a file path to save cookies
    pub fn autosave_cookies(mut self, path: impl Into<PathBuf>) -> Self {
        self.cookies_file_path = Some(path.into());
        self
    }
}

trait Rewrite {
    fn rewrite(&mut self, data: &[u8]) -> io::Result<()>;
}

impl Rewrite for File {
    fn rewrite(&mut self, data: &[u8]) -> io::Result<()> {
        self.seek(SeekFrom::Start(0))?;
        self.set_len(0)?;
        self.write_all(data)
    }
}

pub(crate) trait BaseOption {
    fn try_into_option_url(&self) -> std::result::Result<Option<Url>, url::ParseError>;
}

#[derive(Clone)]
pub(crate) struct BaseSome<S: AsRef<str>>(Host<S>, bool, Option<u16>);

impl<S: AsRef<str>> BaseOption for BaseSome<S> {
    fn try_into_option_url(&self) -> std::result::Result<Option<Url>, url::ParseError> {
        let (host, https, port) = (&self.0, self.1, self.2);
        let mut s = "http".to_owned();
        if https {
            s += "s";
        }
        write!(s, "://{}", host).unwrap();
        if let Some(port) = port {
            write!(s, ":{}", port).unwrap();
        }
        s += "/";
        Url::parse(&s).map(Option::Some)
    }
}

pub(crate) struct BaseNone;

impl BaseOption for BaseNone {
    fn try_into_option_url(&self) -> std::result::Result<Option<Url>, url::ParseError> {
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use errors::{FileIoErrorKind, SessionErrorKind, SessionResult};
    use service::session::HttpSession;

    use nickel::{self, Nickel};
    use tempdir::TempDir;
    use url::Host;
    use {env_logger, reqwest};

    use std::net::Ipv4Addr;
    use std::panic;
    use std::path::Path;

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
            let mut session = HttpSession::builder()
                .base::<&'static str>(Host::Ipv4(Ipv4Addr::new(127, 0, 0, 1)), false, Some(2000))
                .with_robots_txt()
                .unwrap();
            let response = session.get("/").unwrap();
            assert!(
                response
                    .headers()
                    .get::<reqwest::header::SetCookie>()
                    .is_some()
            );
            session.get_expecting("/nonexisting", &[404]).unwrap();
            session.get_expecting("/nonexisting", &[]).unwrap();
            let e = session.get("/sensitive").unwrap_err();
            match e.kind() {
                SessionErrorKind::ForbiddenByRobotsTxt => {}
                e => panic!("{}", e),
            }
        });
        server.detach();
        result.unwrap_or_else(|p| panic::resume_unwind(p));
    }

    #[test]
    #[ignore]
    fn it_keeps_a_file_locked_while_alive() {
        fn session(dir: &Path) -> SessionResult<HttpSession> {
            HttpSession::builder()
                .autosave_cookies(&dir.join("cookies"))
                .build()
        }

        let _ = env_logger::try_init();
        let tempdir = TempDir::new("it_keeps_a_file_locked_while_alive").unwrap();
        let _ = session(tempdir.path()).unwrap();
        let _session = session(tempdir.path()).unwrap();
        let e = session(tempdir.path()).unwrap_err();
        if let &SessionErrorKind::FileIo(ref e) = e.kind() {
            if let &FileIoErrorKind::Lock(_) = e.kind() {
                return;
            }
        }
        panic!("{}", e);
    }
}
