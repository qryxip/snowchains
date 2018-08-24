use console::{self, Console, Palette};
use errors::{FileIoError, FileIoErrorKind, SessionError, SessionResult, StartSessionError};
use path::AbsPathBuf;
use service::USER_AGENT;

use cookie::{self, CookieJar};
use failure::ResultExt as _ResultExt;
use reqwest::header::{self, Headers, Location, SetCookie};
use reqwest::{self, multipart, Method, Response, StatusCode};
use robots_txt::{Robots, SimpleMatcher};
use select::document::Document;
use serde::de::DeserializeOwned;
use serde::Serialize;
use url::{Host, Url};
use {bincode, webbrowser};

use std::borrow::Cow;
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, Read, Seek, SeekFrom, Write};

pub(super) trait HasSession<'a> {
    type Stdin: BufRead + 'a;
    type Stdout: Write + 'a;
    type Stderr: Write + 'a;

    fn session<'b>(
        &'b mut self,
    ) -> &'b mut HttpSession<'a, Self::Stdin, Self::Stdout, Self::Stderr>
    where
        'a: 'b;

    fn console<'b>(&'b mut self) -> &'b mut Console<Self::Stdin, Self::Stdout, Self::Stderr>
    where
        'a: 'b,
    {
        &mut self.session().console
    }

    fn get<'b>(
        &'b mut self,
        url: &str,
    ) -> self::Request<'b, 'a, Self::Stdin, Self::Stdout, Self::Stderr>
    where
        'a: 'b,
    {
        self.session().get(url)
    }

    fn post<'b>(
        &'b mut self,
        url: &str,
    ) -> self::Request<'b, 'a, Self::Stdin, Self::Stdout, Self::Stderr>
    where
        'a: 'b,
    {
        self.session().post(url)
    }

    fn stdout<'b>(&'b mut self) -> console::Stdout<'b, Self::Stdout>
    where
        'a: 'b,
    {
        self.console().stdout()
    }

    fn stderr<'b>(&'b mut self) -> console::Stderr<'b, Self::Stderr>
    where
        'a: 'b,
    {
        self.console().stderr()
    }
}

/// A wrapper of `reqwest::Client`.
#[derive(Debug)]
pub(crate) struct HttpSession<'a, I: BufRead + 'a, O: Write + 'a, E: Write + 'a> {
    console: &'a mut Console<I, O, E>,
    client: reqwest::Client,
    robots_txts: HashMap<String, String>,
    base: Option<UrlBase>,
    jar: Option<AutosavedCookieJar>,
}

impl<'a, I: BufRead + 'a, O: Write + 'a, E: Write + 'a> HttpSession<'a, I, O, E> {
    pub fn new(
        console: &'a mut Console<I, O, E>,
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
                console,
                client,
                robots_txts: hashmap!(),
                base,
                jar,
            };
            if let Some(host) = host {
                let mut res = this
                    .get("/robots.txt")
                    .acceptable(&[200, 301, 302, 404])
                    .send()?;
                while [301, 302].contains(&res.status().as_u16()) {
                    let location = res
                        .headers()
                        .get::<Location>()
                        .map(|location| (*location).to_owned());
                    if let Some(location) = location {
                        res = this
                            .get(&location)
                            .acceptable(&[200, 301, 302, 404])
                            .send()?;
                    } else {
                        return Ok(this);
                    }
                }
                match res.status().as_u16() {
                    200 => {
                        this.robots_txts.insert(host.to_string(), res.text()?);
                    }
                    404 => (),
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

    pub fn cookies_to_header(&self) -> Option<header::Cookie> {
        self.jar.as_ref().map(AutosavedCookieJar::to_header)
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
    pub fn open_in_browser(&mut self, url: &str) -> SessionResult<()> {
        let url = self.resolve_url(url)?;
        let mut stdout = self.console.stdout();
        writeln!(stdout, "Opening {} in default browser...", url)?;
        stdout.flush()?;
        let status = webbrowser::open(url.as_str())?.status;
        if status.success() {
            Ok(())
        } else {
            Err(SessionError::Webbrowser(status))
        }
    }

    pub fn get<'b>(&'b mut self, url: &str) -> self::Request<'b, 'a, I, O, E> {
        self.request(url, Method::Get, vec![StatusCode::Ok])
    }

    pub fn post<'b>(&'b mut self, url: &str) -> self::Request<'b, 'a, I, O, E> {
        self.request(url, Method::Post, vec![StatusCode::Found])
    }

    fn request<'b>(
        &'b mut self,
        url: &str,
        method: Method,
        acceptable: Vec<StatusCode>,
    ) -> self::Request<'b, 'a, I, O, E> {
        self::Request {
            inner: self.try_request(url, method),
            session: self,
            acceptable,
        }
    }

    fn try_request(&mut self, url: &str, method: Method) -> SessionResult<reqwest::RequestBuilder> {
        let url = self.resolve_url(url)?;
        self.assert_not_forbidden_by_robots_txt(&url)?;
        let mut req = self.client.request(method, url.as_str());
        if let Some(jar) = self.jar.as_ref() {
            req.header(jar.to_header());
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

pub(crate) struct Request<'a, 'b: 'a, I: BufRead + 'b, O: Write + 'b, E: Write + 'b> {
    session: &'a mut HttpSession<'b, I, O, E>,
    inner: SessionResult<reqwest::RequestBuilder>,
    acceptable: Vec<StatusCode>,
}

impl<'a, 'b, I: BufRead, O: Write, E: Write> Request<'a, 'b, I, O, E> {
    pub fn raw_header(
        mut self,
        name: impl Into<Cow<'static, str>>,
        value: impl Into<header::Raw>,
    ) -> Self {
        if let Ok(inner) = self.inner.as_mut() {
            let mut headers = Headers::new();
            headers.set_raw(name, value);
            inner.headers(headers);
        }
        self
    }

    pub fn acceptable(self, statuses: &[u16]) -> Self {
        let acceptable = statuses
            .iter()
            .map(|&n| StatusCode::try_from(n).unwrap_or_else(|_| StatusCode::Unregistered(n)))
            .collect();
        Self { acceptable, ..self }
    }

    pub fn send(self) -> SessionResult<Response> {
        let req = self.inner?.build()?;
        let mut stdout = self.session.console.stdout();
        let client = &self.session.client;
        req.echo_method(stdout.reborrow())?;
        let res = match client.execute(req) {
            Ok(res) => Ok(res),
            Err(err) => {
                writeln!(stdout)?;
                stdout.flush()?;
                Err(err)
            }
        }?;
        res.echo_status(&self.acceptable, stdout)?;
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
    fn echo_method(&self, stdout: console::Stdout<impl Write>) -> io::Result<()>;
}

impl EchoMethod for reqwest::Request {
    fn echo_method(&self, mut stdout: console::Stdout<impl Write>) -> io::Result<()> {
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
        stdout: console::Stdout<impl Write>,
    ) -> io::Result<()>;
    fn filter_by_status(self, expected: Vec<StatusCode>) -> SessionResult<Self>;
}

impl ResponseExt for Response {
    fn echo_status(
        &self,
        expected_statuses: &[StatusCode],
        mut stdout: console::Stdout<impl Write>,
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

    fn to_header(&self) -> header::Cookie {
        self.inner
            .iter()
            .fold(header::Cookie::new(), |mut header, cookie| {
                header.append(cookie.name().to_owned(), cookie.value().to_owned());
                header
            })
    }

    fn insert_cookie(&mut self, cookie: cookie::Cookie<'static>) -> SessionResult<()> {
        self.inner.add(cookie);
        self.save()
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
            self.save()?;
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
    use console::Console;
    use errors::SessionError;
    use path::AbsPathBuf;
    use service;
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
            let client = service::reqwest_client(None).unwrap();
            let base = UrlBase::new(Host::Ipv4(Ipv4Addr::new(127, 0, 0, 1)), false, Some(2000));
            let mut null = Console::null();
            let mut sess = HttpSession::new(&mut null, client, Some(base), None).unwrap();
            let res = sess.get("/").send().unwrap();
            assert!(res.headers().get::<reqwest::header::SetCookie>().is_some());
            sess.get("/nonexisting").acceptable(&[404]).send().unwrap();
            sess.get("/nonexisting").acceptable(&[]).send().unwrap();
            match sess.get("/sensitive").send().unwrap_err() {
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
        let (mut null1, mut null2) = (Console::null(), Console::null());
        HttpSession::new(&mut null1, client.clone(), None, path.clone()).unwrap();
        HttpSession::new(&mut null1, client.clone(), None, path.clone()).unwrap();
        let _session = HttpSession::new(&mut null1, client.clone(), None, path.clone()).unwrap();
        let err = HttpSession::new(&mut null2, client, None, path.clone()).unwrap_err();
        if let SessionError::Start(ref ctx) = err {
            if ctx.cause().is_some() {
                return;
            }
        }
        panic!("{:?}", err);
    }
}
