use console::{ConsoleWrite, Palette};
use errors::{FileIoError, FileIoErrorKind, SessionError, SessionResult, StartSessionError};
use path::AbsPathBuf;
use service::USER_AGENT;

use cookie::{self, CookieJar};
use failure::ResultExt as _ResultExt;
use reqwest::header::{self, HeaderValue, InvalidHeaderValue};
use reqwest::{self, multipart, Method, Response, StatusCode};
use robots_txt::{Robots, SimpleMatcher};
use select::document::Document;
use serde::de::DeserializeOwned;
use serde::Serialize;
use url::{Host, Url};
use {bincode, webbrowser};

use std;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Write as _FmtWrite;
use std::fs::File;
use std::io::{self, Read, Seek, SeekFrom, Write};

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
        mut stdout: impl ConsoleWrite,
        client: reqwest::Client,
        base: impl Into<Option<UrlBase>>,
        cookies_path: impl Into<Option<AbsPathBuf>>,
    ) -> SessionResult<Self> {
        let start = move || -> SessionResult<Self> {
            let base = base.into();
            let host = base.as_ref().map(|base| base.host.clone());
            let jar = match cookies_path.into() {
                Some(path) => Some(AutosavedCookieJar::new(path)?),
                None => None,
            };
            let mut this = Self {
                client,
                robots_txts: hashmap!(),
                base,
                jar,
            };
            if let Some(host) = host {
                let mut res = this
                    .get("/robots.txt", &mut stdout)
                    .acceptable(&[200, 301, 302, 404])
                    .send()?;
                while [301, 302, 404].contains(&res.status().as_u16()) {
                    if let Some(location) =
                        res.headers().get(header::LOCATION).map(ToOwned::to_owned)
                    {
                        let location = location.to_str()?;
                        res = this
                            .get(location, &mut stdout)
                            .acceptable(&[200, 301, 302, 404])
                            .send()?;
                    } else {
                        return Ok(this);
                    }
                }
                match res.status() {
                    StatusCode::OK => {
                        this.robots_txts.insert(host.to_string(), res.text()?);
                    }
                    StatusCode::NOT_FOUND => (),
                    _ => unreachable!(),
                }
            }
            Ok(this)
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

    pub fn cookies_to_header_value(&self) -> SessionResult<Option<HeaderValue>> {
        match self.jar.as_ref().map(AutosavedCookieJar::to_header_value) {
            None => Ok(None),
            Some(Ok(v)) => Ok(Some(v)),
            Some(Err(e)) => Err(e.into()),
        }
    }

    pub fn insert_cookie(&mut self, cookie: cookie::Cookie<'static>) -> SessionResult<()> {
        match self.jar.as_mut() {
            None => Ok(()),
            Some(jar) => jar.insert_cookie(cookie),
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
    pub fn resolve_url(&self, url: &str) -> SessionResult<Url> {
        match self.base.as_ref() {
            Some(base) => base.with(url),
            None => Url::parse(url).map_err(|e| SessionError::ParseUrl(url.to_owned(), e)),
        }
    }

    /// Opens `url`, which is relative or absolute, with default browser
    /// printing a message.
    pub fn open_in_browser(
        &mut self,
        url: &str,
        mut stdout: impl ConsoleWrite,
    ) -> SessionResult<()> {
        let url = self.resolve_url(url)?;
        writeln!(stdout, "Opening {} in default browser...", url)?;
        stdout.flush()?;
        let status = webbrowser::open(url.as_str())?.status;
        if status.success() {
            Ok(())
        } else {
            Err(SessionError::Webbrowser(status))
        }
    }

    pub fn get<O: ConsoleWrite>(&mut self, url: &str, stdout: O) -> self::Request<O> {
        self.request(url, Method::GET, vec![StatusCode::OK], stdout)
    }

    pub fn post<O: ConsoleWrite>(&mut self, url: &str, stdout: O) -> self::Request<O> {
        self.request(url, Method::POST, vec![StatusCode::FOUND], stdout)
    }

    fn request<O: ConsoleWrite>(
        &mut self,
        url: &str,
        method: Method,
        acceptable: Vec<StatusCode>,
        stdout: O,
    ) -> self::Request<O> {
        self::Request {
            inner: self.try_request(url, method),
            stdout,
            session: self,
            acceptable,
        }
    }

    fn try_request(&mut self, url: &str, method: Method) -> SessionResult<reqwest::RequestBuilder> {
        let url = self.resolve_url(url)?;
        self.assert_not_forbidden_by_robots_txt(&url)?;
        let mut req = self.client.request(method, url.as_str());
        if let Some(jar) = self.jar.as_ref() {
            req.header(header::COOKIE, jar.to_header_value()?);
        }
        Ok(req)
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

pub(crate) struct Request<'a, O: ConsoleWrite> {
    inner: SessionResult<reqwest::RequestBuilder>,
    stdout: O,
    session: &'a mut HttpSession,
    acceptable: Vec<StatusCode>,
}

impl<'a, O: ConsoleWrite> Request<'a, O> {
    pub fn x_csrf_token(mut self, token: &str) -> Self {
        if let Ok(inner) = self.inner.as_mut() {
            inner.header("X-CSRF-Token", token);
        }
        self
    }

    /// Panics:
    ///
    /// Panics if `statuses` contains `n` such that `n < 100 || 600 <= n`.
    pub fn acceptable(self, statuses: &'static [u16]) -> Self {
        let acceptable = statuses
            .iter()
            .map(|&n| StatusCode::from_u16(n))
            .collect::<std::result::Result<_, _>>()
            .unwrap();
        Self { acceptable, ..self }
    }

    pub fn send(mut self) -> SessionResult<Response> {
        let req = self.inner?.build()?;
        let client = &self.session.client;
        req.echo_method(&mut self.stdout)?;
        let res = match client.execute(req) {
            Ok(res) => Ok(res),
            Err(err) => {
                writeln!(self.stdout)?;
                self.stdout.flush()?;
                Err(err)
            }
        }?;
        res.echo_status(&self.acceptable, self.stdout)?;
        if let Some(jar) = self.session.jar.as_mut() {
            jar.update(&res)?;
        }
        res.filter_by_status(self.acceptable)
    }

    pub fn send_form(mut self, form: &(impl Serialize + ?Sized)) -> SessionResult<Response> {
        if let Ok(inner) = self.inner.as_mut() {
            inner.form(form);
        }
        self.send()
    }

    pub fn send_json(mut self, json: &(impl Serialize + ?Sized)) -> SessionResult<Response> {
        if let Ok(inner) = self.inner.as_mut() {
            inner.json(json);
        }
        self.send()
    }

    pub fn send_multipart(mut self, multipart: multipart::Form) -> SessionResult<Response> {
        if let Ok(inner) = self.inner.as_mut() {
            inner.multipart(multipart);
        }
        self.send()
    }

    pub fn recv_html(self) -> SessionResult<Document> {
        Ok(Document::from(self.send()?.text()?.as_str()))
    }

    pub fn recv_json<T: DeserializeOwned>(self) -> SessionResult<T> {
        Ok(self.send()?.json()?)
    }
}

trait EchoMethod {
    fn echo_method(&self, stdout: impl ConsoleWrite) -> io::Result<()>;
}

impl EchoMethod for reqwest::Request {
    fn echo_method(&self, mut stdout: impl ConsoleWrite) -> io::Result<()> {
        write!(stdout.bold(None), "{}", self.method())?;
        write!(stdout.plain(Palette::Url), " {}", self.url())?;
        write!(stdout, " ... ")?;
        stdout.flush()
    }
}

trait ResponseExt
where
    Self: Sized,
{
    fn echo_status(
        &self,
        expected_statuses: &[StatusCode],
        stdout: impl ConsoleWrite,
    ) -> io::Result<()>;
    fn filter_by_status(self, expected: Vec<StatusCode>) -> SessionResult<Self>;
}

impl ResponseExt for Response {
    fn echo_status(
        &self,
        expected_statuses: &[StatusCode],
        mut stdout: impl ConsoleWrite,
    ) -> io::Result<()> {
        let palette = if expected_statuses.contains(&self.status()) {
            Palette::Success
        } else {
            Palette::Fatal
        };
        writeln!(stdout.bold(palette), "{}", self.status())?;
        stdout.flush()
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
    path: AbsPathBuf,
    file: File,
    inner: CookieJar,
}

impl AutosavedCookieJar {
    fn new(path: impl Into<AbsPathBuf>) -> SessionResult<Self> {
        let path = path.into();
        let exists = path.exists();
        let mut file = ::fs::create_and_lock(&path)?;
        let mut inner = CookieJar::new();
        if exists {
            let mut cookies =
                Vec::with_capacity(file.metadata().map(|m| m.len() as usize + 1).unwrap_or(0));
            file.read_to_end(&mut cookies)
                .map_err(|err| FileIoError::new(FileIoErrorKind::Read, path.as_ref()).with(err))?;
            if !cookies.is_empty() {
                let cookies = bincode::deserialize::<Vec<String>>(&cookies).map_err(|err| {
                    FileIoError::new(FileIoErrorKind::Deserialize, path.as_ref()).with(err)
                })?;
                for cookie in cookies {
                    let cookie = cookie::Cookie::parse(cookie.clone()).map_err(|e| {
                        SessionError::ParseCookieFromPath(cookie, path.to_owned(), e)
                    })?;
                    inner.add(cookie);
                }
            }
        } else {
            file.write_all(&bincode::serialize(&Vec::<String>::new()).unwrap())
                .map_err(|err| FileIoError::new(FileIoErrorKind::Write, path.as_ref()).with(err))?;
        }
        Ok(Self { file, path, inner })
    }

    fn to_header_value(&self) -> std::result::Result<HeaderValue, InvalidHeaderValue> {
        let s = self.inner.iter().fold("".to_owned(), |mut s, cookie| {
            if !s.is_empty() {
                s.push_str(";");
            }
            write!(s, "{}={}", cookie.name(), cookie.value()).unwrap();
            s
        });
        HeaderValue::from_str(&s)
    }

    fn insert_cookie(&mut self, cookie: cookie::Cookie<'static>) -> SessionResult<()> {
        self.inner.add(cookie);
        self.save()
    }

    fn update(&mut self, response: &Response) -> SessionResult<()> {
        fn parse_setcookie(setcookie: &HeaderValue) -> Option<cookie::Cookie<'static>> {
            let setcookie = setcookie.to_str().ok()?;
            let cookie = cookie::Cookie::parse(setcookie).ok()?;
            Some(cookie.into_owned())
        }

        for setcookie in response.headers().get_all(header::SET_COOKIE) {
            let cookie = parse_setcookie(setcookie).ok_or_else(|| {
                SessionError::ParseCookieFromUrl(setcookie.to_owned(), response.url().to_owned())
            })?;
            self.inner.add(cookie);
        }
        Ok(())
    }

    fn save(&mut self) -> SessionResult<()> {
        let value = self
            .inner
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>();
        let value = bincode::serialize(&value)?;
        self.file
            .seek(SeekFrom::Start(0))
            .and_then(|_| self.file.set_len(0))
            .and_then(|()| self.file.write_all(&value))
            .map_err(|err| FileIoError::new(FileIoErrorKind::Write, self.path.as_ref()).with(err))
            .map_err(Into::into)
    }
}

#[cfg(test)]
mod tests {
    use console::Printer;
    use errors::SessionError;
    use path::AbsPathBuf;
    use service;
    use service::session::{HttpSession, UrlBase};

    use env_logger;
    use failure::Fail as _Fail;
    use nickel::{self, Nickel};
    use reqwest::header;
    use tempdir::TempDir;
    use url::Host;

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
            let client = service::reqwest_client(None).unwrap();
            let base = UrlBase::new(Host::Ipv4(Ipv4Addr::new(127, 0, 0, 1)), false, Some(2000));
            let mut null = Printer::null();
            let mut sess = HttpSession::new(&mut null, client, Some(base), None).unwrap();
            let res = sess.get("/", &mut null).send().unwrap();
            assert!(res.headers().get(header::SET_COOKIE).is_some());
            sess.get("/nonexisting", &mut null)
                .acceptable(&[404])
                .send()
                .unwrap();
            sess.get("/nonexisting", &mut null)
                .acceptable(&[])
                .send()
                .unwrap();
            match sess.get("/sensitive", null).send().unwrap_err() {
                SessionError::ForbiddenByRobotsTxt => {}
                err => panic!("{:?}", err),
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
        let path = AbsPathBuf::new_or_panic(tempdir.path().join("cookies"));
        let client = service::reqwest_client(None).unwrap();
        let mut null = Printer::null();
        HttpSession::new(&mut null, client.clone(), None, path.clone()).unwrap();
        HttpSession::new(&mut null, client.clone(), None, path.clone()).unwrap();
        let _session = HttpSession::new(&mut null, client.clone(), None, path.clone()).unwrap();
        let err = HttpSession::new(&mut null, client, None, path.clone()).unwrap_err();
        if let SessionError::Start(ref ctx) = err {
            if ctx.cause().is_some() {
                return;
            }
        }
        panic!("{:?}", err);
    }
}
