use crate::errors::{FileResult, ServiceErrorKind, ServiceResult};
use crate::fs::LockedFile;
use crate::path::AbsPath;
use crate::service::USER_AGENT;
use crate::terminal::WriteAnsi;

use cookie::CookieJar;
use derive_new::new;
use failure::{Fail as _Fail, Fallible, ResultExt as _ResultExt};
use log::info;
use maplit::hashmap;
use reqwest::header::{self, HeaderValue};
use reqwest::{multipart, Method, Response, StatusCode};
use robots_txt::{Robots, SimpleMatcher};
use select::document::Document;
use serde::de::DeserializeOwned;
use serde::Serialize;
use url::{Host, Url};

use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Write as _FmtWrite;
use std::io;

/// A wrapper of `reqwest::Client`.
#[derive(Debug)]
pub(crate) struct HttpSession {
    client: reqwest::Client,
    robots_txts: HashMap<String, String>,
    base: Option<UrlBase>,
    jar: Option<AutosavedCookieJar>,
    silent: bool,
}

impl HttpSession {
    pub fn try_new<'a>(
        mut out: impl WriteAnsi,
        client: reqwest::Client,
        base: impl Into<Option<UrlBase>>,
        cookies_path: impl Into<Option<&'a AbsPath>>,
        silent: bool,
    ) -> ServiceResult<Self> {
        let base = base.into();
        let host = base.as_ref().map(|base| base.host.clone());
        let jar = match cookies_path.into() {
            Some(path) => Some(AutosavedCookieJar::try_new(path)?),
            None => None,
        };
        let mut this = Self {
            client,
            robots_txts: hashmap!(),
            base,
            jar,
            silent,
        };
        if let Some(host) = host {
            let mut res = this
                .get("/robots.txt", &mut out)
                .acceptable(&[200, 301, 302, 404])
                .send()?;
            while [301, 302, 404].contains(&res.status().as_u16()) {
                if let Some(location) = res.headers().get(header::LOCATION).map(ToOwned::to_owned) {
                    let location = location
                        .to_str()
                        .with_context(|_| ServiceErrorKind::ReadHeader(header::LOCATION))?;
                    res = this
                        .get(location, &mut out)
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
    }

    /// Whether it has any cookie value.
    pub fn has_cookie(&self) -> bool {
        match self.jar.as_ref() {
            Some(jar) => jar.inner.iter().next().is_some(),
            None => false,
        }
    }

    pub fn cookies_to_header_value(&self) -> ServiceResult<Option<HeaderValue>> {
        match self.jar.as_ref().map(AutosavedCookieJar::to_header_value) {
            None => Ok(None),
            Some(Ok(v)) => Ok(Some(v)),
            Some(Err(e)) => Err(e),
        }
    }

    pub fn insert_cookie(&mut self, cookie: cookie::Cookie<'static>) -> ServiceResult<()> {
        match self.jar.as_mut() {
            None => Ok(()),
            Some(jar) => jar.insert_cookie(cookie),
        }
    }

    /// Removes all cookies.
    pub fn clear_cookies(&mut self) -> ServiceResult<()> {
        if let Some(jar) = self.jar.as_mut() {
            jar.inner = CookieJar::new();
            jar.save()?;
        }
        Ok(())
    }

    /// If `url` starts with '/' and the base host is present, returns
    /// http(s)://<host><url>.
    pub fn resolve_url(&self, url: &str) -> ServiceResult<Url> {
        match self.base.as_ref() {
            Some(base) => base.with(url),
            None => Url::parse(url)
                .map_err(|e| e.context(ServiceErrorKind::ParseUrl(url.to_owned())).into()),
        }
    }

    /// Opens `url`, which is relative or absolute, with default browser
    /// printing a message.
    pub fn open_in_browser(&mut self, url: &str, mut out: impl WriteAnsi) -> ServiceResult<()> {
        let url = self.resolve_url(url)?;
        writeln!(out, "Opening {} in default browser...", url)?;
        out.flush()?;
        let status = webbrowser::open(url.as_str())?.status;
        if status.success() {
            Ok(())
        } else {
            Err(ServiceErrorKind::Webbrowser(status).into())
        }
    }

    pub fn get<O: WriteAnsi>(&mut self, url: &str, out: O) -> self::Request<O> {
        self.request(url, Method::GET, vec![StatusCode::OK], out)
    }

    pub fn post<O: WriteAnsi>(&mut self, url: &str, out: O) -> self::Request<O> {
        self.request(url, Method::POST, vec![StatusCode::FOUND], out)
    }

    fn request<O: WriteAnsi>(
        &mut self,
        url: &str,
        method: Method,
        acceptable: Vec<StatusCode>,
        out: O,
    ) -> self::Request<O> {
        self::Request {
            inner: self.try_request(url, method),
            out: if self.silent { None } else { Some(out) },
            session: self,
            acceptable,
        }
    }

    fn try_request(&mut self, url: &str, method: Method) -> ServiceResult<reqwest::RequestBuilder> {
        let url = self.resolve_url(url)?;
        self.assert_not_forbidden_by_robots_txt(&url)?;
        let mut req = self.client.request(method, url.as_str());
        if let Some(jar) = self.jar.as_ref() {
            req = req.header(header::COOKIE, jar.to_header_value()?);
        }
        Ok(req)
    }

    fn assert_not_forbidden_by_robots_txt(&self, url: &Url) -> ServiceResult<()> {
        if let Some(host) = url.host_str() {
            if let Some(robots_txt) = self.robots_txts.get(host) {
                let robots = Robots::from_str(robots_txt);
                let matcher = SimpleMatcher::new(&robots.choose_section(USER_AGENT).rules);
                if !matcher.check_path(url.path()) {
                    return Err(ServiceErrorKind::ForbiddenByRobotsTxt.into());
                }
            }
        }
        Ok(())
    }
}

pub(crate) struct Request<'a, O: WriteAnsi> {
    inner: ServiceResult<reqwest::RequestBuilder>,
    out: Option<O>,
    session: &'a mut HttpSession,
    acceptable: Vec<StatusCode>,
}

impl<'a, O: WriteAnsi> Request<'a, O> {
    pub(crate) fn x_csrf_token(self, token: &str) -> Self {
        Self {
            inner: self.inner.map(|inner| inner.header("X-CSRF-Token", token)),
            ..self
        }
    }

    /// Panics:
    ///
    /// Panics if `statuses` contains `n` such that `n < 100 || 600 <= n`.
    pub(crate) fn acceptable(self, statuses: &'static [u16]) -> Self {
        let acceptable = statuses
            .iter()
            .map(|&n| StatusCode::from_u16(n))
            .collect::<std::result::Result<_, _>>()
            .unwrap();
        Self { acceptable, ..self }
    }

    pub(crate) fn send(mut self) -> ServiceResult<Response> {
        let req = self.inner?.build()?;
        let client = &self.session.client;
        req.log_method();
        if let Some(out) = self.out.as_mut() {
            req.echo_method(out)?;
        }
        let res = match client.execute(req) {
            Ok(res) => Ok(res),
            Err(err) => {
                if let Some(out) = self.out.as_mut() {
                    writeln!(out)?;
                    out.flush()?;
                }
                Err(err)
            }
        }?;
        if let Some(out) = self.out.as_mut() {
            res.echo_status(&self.acceptable, out)?;
        }
        res.log_status();
        if let Some(jar) = self.session.jar.as_mut() {
            jar.update(&res)?;
        }
        res.filter_by_status(self.acceptable)
    }

    pub(crate) fn send_form(self, form: &(impl Serialize + ?Sized)) -> ServiceResult<Response> {
        Self {
            inner: self.inner.map(|inner| inner.form(form)),
            ..self
        }.send()
    }

    pub(crate) fn send_json(self, json: &(impl Serialize + ?Sized)) -> ServiceResult<Response> {
        Self {
            inner: self.inner.map(|inner| inner.json(json)),
            ..self
        }.send()
    }

    pub(crate) fn send_multipart(self, multipart: multipart::Form) -> ServiceResult<Response> {
        Self {
            inner: self.inner.map(|inner| inner.multipart(multipart)),
            ..self
        }.send()
    }

    pub(crate) fn recv_html(self) -> ServiceResult<Document> {
        Ok(Document::from(self.send()?.text()?.as_str()))
    }

    pub(crate) fn recv_json<T: DeserializeOwned>(self) -> ServiceResult<T> {
        Ok(self.send()?.json()?)
    }
}

trait RequestExt {
    fn log_method(&self);
    fn echo_method(&self, out: impl WriteAnsi) -> io::Result<()>;
}

impl RequestExt for reqwest::Request {
    fn log_method(&self) {
        info!("{}: {}", self.method(), self.url());
    }

    fn echo_method(&self, mut out: impl WriteAnsi) -> io::Result<()> {
        out.with_reset(|o| o.bold()?.write_str(self.method()))?;
        out.write_str(" ")?;
        out.with_reset(|o| o.fg(14)?.write_str(self.url()))?;
        out.write_str(" ... ")?;
        out.flush()
    }
}

trait ResponseExt
where
    Self: Sized,
{
    fn log_status(&self);
    fn echo_status(&self, expected_statuses: &[StatusCode], out: impl WriteAnsi) -> io::Result<()>;
    fn filter_by_status(self, expected: Vec<StatusCode>) -> ServiceResult<Self>;
}

impl ResponseExt for Response {
    fn log_status(&self) {
        info!("{}", self.status());
    }

    fn echo_status(
        &self,
        expected_statuses: &[StatusCode],
        mut out: impl WriteAnsi,
    ) -> io::Result<()> {
        let color = if expected_statuses.contains(&self.status()) {
            10
        } else {
            9
        };
        out.with_reset(|o| write!(o.fg(color)?.bold()?, "{}", self.status()))?;
        out.write_str("\n")?;
        out.flush()
    }

    fn filter_by_status(self, expected: Vec<StatusCode>) -> ServiceResult<Self> {
        if expected.is_empty() || expected.contains(&self.status()) {
            Ok(self)
        } else {
            Err(ServiceErrorKind::UnexpectedStatusCode(
                self.url().to_owned(),
                self.status(),
                expected,
            ).into())
        }
    }
}

#[derive(Debug, new)]
pub(crate) struct UrlBase {
    host: Host<&'static str>,
    https: bool,
    port: Option<u16>,
}

impl UrlBase {
    fn with(&self, relative_or_absolute_url: &str) -> ServiceResult<Url> {
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
        Url::parse(&url).map_err(|e| {
            e.context(ServiceErrorKind::ParseUrl(url.into_owned()))
                .into()
        })
    }
}

#[derive(Debug)]
struct AutosavedCookieJar {
    file: LockedFile,
    inner: CookieJar,
}

impl AutosavedCookieJar {
    fn try_new(path: &AbsPath) -> ServiceResult<Self> {
        let mut file = LockedFile::try_new(path)?;
        let mut inner = CookieJar::new();
        if !file.is_empty()? {
            let cookies = file.bincode::<Vec<String>>()?;
            if !cookies.is_empty() {
                for cookie in cookies {
                    let cookie = cookie::Cookie::parse(cookie.clone()).with_context(|_| {
                        ServiceErrorKind::ParseCookieFromPath(path.to_owned(), cookie)
                    })?;
                    inner.add(cookie);
                }
                return Ok(Self { file, inner });
            }
        }
        file.write_bincode(&Vec::<String>::new())?;
        Ok(Self { file, inner })
    }

    fn to_header_value(&self) -> ServiceResult<HeaderValue> {
        let s = self.inner.iter().fold("".to_owned(), |mut s, cookie| {
            if !s.is_empty() {
                s.push_str(";");
            }
            write!(s, "{}={}", cookie.name(), cookie.value()).unwrap();
            s
        });
        HeaderValue::from_str(&s)
            .with_context(|_| ServiceErrorKind::ParseCookieFromPath(self.file.path().to_owned(), s))
            .map_err(Into::into)
    }

    fn insert_cookie(&mut self, cookie: cookie::Cookie<'static>) -> ServiceResult<()> {
        self.inner.add(cookie);
        self.save().map_err(Into::into)
    }

    fn update(&mut self, response: &Response) -> ServiceResult<()> {
        fn parse_setcookie(setcookie: &HeaderValue) -> Fallible<cookie::Cookie<'static>> {
            let setcookie = setcookie.to_str()?;
            let cookie = cookie::Cookie::parse(setcookie)?;
            Ok(cookie.into_owned())
        }

        for setcookie in response.headers().get_all(header::SET_COOKIE) {
            let cookie = parse_setcookie(setcookie).with_context(|_| {
                ServiceErrorKind::ParseCookieFromUrl(
                    response.url().to_owned(),
                    setcookie.to_owned(),
                )
            })?;
            self.inner.add(cookie);
        }
        self.save().map_err(Into::into)
    }

    fn save(&mut self) -> FileResult<()> {
        let value = self
            .inner
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>();
        self.file.write_bincode(&value)
    }
}

#[cfg(test)]
mod tests {
    use crate::errors::{FileError, FileErrorKind, ServiceError, ServiceErrorKind};
    use crate::path::AbsPathBuf;
    use crate::service;
    use crate::service::session::{HttpSession, UrlBase};
    use crate::terminal::tests::Ansi;

    use if_chain::if_chain;
    use nickel::{Nickel, _middleware_inner, _router_inner, as_block, as_pat, middleware, router};
    use reqwest::header;
    use tempdir::TempDir;
    use url::Host;

    use std::io::{self, Cursor};
    use std::net::Ipv4Addr;
    use std::{panic, str};

    #[test]
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
            let mut wtr = Ansi::new(Cursor::new(Vec::<u8>::new()));
            let mut sess = HttpSession::try_new(&mut wtr, client, Some(base), None, false).unwrap();
            let res = sess.get("/", &mut wtr).send().unwrap();
            assert!(res.headers().get(header::SET_COOKIE).is_some());
            sess.get("/nonexisting", &mut wtr)
                .acceptable(&[404])
                .send()
                .unwrap();
            sess.get("/nonexisting", &mut wtr)
                .acceptable(&[])
                .send()
                .unwrap();
            if_chain! {
                let err = sess.get("/sensitive", &mut wtr).send().unwrap_err();
                if let ServiceError::Context(ctx) = &err;
                if let ServiceErrorKind::ForbiddenByRobotsTxt = ctx.get_context();
                then {
                } else {
                    panic!("{:?}", err);
                }
            }
            assert_eq!(
                str::from_utf8(wtr.get_ref().get_ref()).unwrap(),
                format!(
                    "{get} {robots_txt} ... {expected_200}\n\
                     {get} {root} ... {expected_200}\n\
                     {get} {nonexisting} ... {expected_404}\n\
                     {get} {nonexisting} ... {unexpected_404}\n",
                    get = "\x1b[1mGET\x1b[0m",
                    robots_txt = "\x1b[38;5;14mhttp://127.0.0.1:2000/robots.txt\x1b[0m",
                    root = "\x1b[38;5;14mhttp://127.0.0.1:2000/\x1b[0m",
                    nonexisting = "\x1b[38;5;14mhttp://127.0.0.1:2000/nonexisting\x1b[0m",
                    expected_200 = "\x1b[38;5;10m\x1b[1m200 OK\x1b[0m",
                    expected_404 = "\x1b[38;5;10m\x1b[1m404 Not Found\x1b[0m",
                    unexpected_404 = "\x1b[38;5;9m\x1b[1m404 Not Found\x1b[0m",
                ),
            );
        });
        server.detach();
        result.unwrap_or_else(|p| panic::resume_unwind(p));
    }

    #[test]
    fn it_keeps_a_file_locked_while_alive() {
        let _ = env_logger::try_init();
        let tempdir = TempDir::new("it_keeps_a_file_locked_while_alive").unwrap();
        let path = AbsPathBuf::new_or_panic(tempdir.path().join("cookies"));
        let path = path.as_path();
        let client = service::reqwest_client(None).unwrap();
        let mut wtr = Ansi::new(io::sink());
        HttpSession::try_new(&mut wtr, client.clone(), None, path, true).unwrap();
        HttpSession::try_new(&mut wtr, client.clone(), None, path, true).unwrap();
        let _session = HttpSession::try_new(&mut wtr, client.clone(), None, path, true).unwrap();
        let err = HttpSession::try_new(&mut wtr, client, None, path, true).unwrap_err();
        if let ServiceError::File(FileError::Context(kind)) = &err {
            if let FileErrorKind::Lock(_) = kind.get_context() {
                return tempdir.close().unwrap();
            }
        }
        panic!("{:?}", err);
    }
}
