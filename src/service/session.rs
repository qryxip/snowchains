use crate::errors::{FileResult, ServiceError, ServiceErrorKind, ServiceResult};
use crate::fs::{LazyLockedFile, LockedFile};
use crate::path::AbsPath;
use crate::service::USER_AGENT;
use crate::terminal::WriteExt as _;

use cookie::CookieJar;
use derive_new::new;
use failure::{Fail as _, Fallible, ResultExt as _};
use futures::{task, try_ready, Async, Future, Poll, Stream as _};
use maplit::hashmap;
use mime::Mime;
use reqwest::header::{self, HeaderValue};
use reqwest::r#async::Decoder;
use reqwest::{Method, StatusCode};
use robots_txt::{Robots, SimpleMatcher};
use scraper::Html;
use serde::de::DeserializeOwned;
use serde::Serialize;
use termcolor::WriteColor;
use tokio::runtime::Runtime;
use url::{Host, Url};

use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{self, Write as _};
use std::ops::Deref;
use std::{io, mem};

#[derive(Debug)]
pub(super) struct HttpSessionInitParams<'a, W: WriteColor> {
    pub(super) out: W,
    pub(super) runtime: &'a mut Runtime,
    pub(super) robots: bool,
    pub(super) client: reqwest::r#async::Client,
    pub(super) base: Option<UrlBase>,
    pub(super) cookies_path: Option<&'a AbsPath>,
    pub(super) api_token_path: Option<&'a AbsPath>,
    pub(super) silent: bool,
}

#[derive(Debug)]
pub(super) struct HttpSession {
    client: reqwest::r#async::Client,
    robots_txts: HashMap<String, String>,
    base: Option<UrlBase>,
    jar: Option<AutosavedCookieJar>,
    api_token: LazyLockedFile,
    silent: bool,
}

impl HttpSession {
    pub(super) fn try_new(params: HttpSessionInitParams<impl WriteColor>) -> ServiceResult<Self> {
        let HttpSessionInitParams {
            mut out,
            mut runtime,
            robots,
            client,
            base,
            cookies_path,
            api_token_path,
            silent,
        } = params;
        let host = base.as_ref().map(|base| base.host.clone());
        let mut this = Self {
            client,
            robots_txts: hashmap!(),
            base,
            jar: cookies_path.map(AutosavedCookieJar::try_new).transpose()?,
            api_token: match api_token_path {
                None => LazyLockedFile::Null,
                Some(path) => LazyLockedFile::Uninited(path.to_owned()),
            },
            silent,
        };
        if robots {
            if let Some(host) = host {
                let mut res = this
                    .get("/robots.txt", &mut out, &mut runtime)
                    .acceptable(&[200, 301, 302, 404])
                    .send()?;
                while [301, 302, 404].contains(&res.status().as_u16()) {
                    if let Some(location) =
                        res.headers().get(header::LOCATION).map(ToOwned::to_owned)
                    {
                        let location = location
                            .to_str()
                            .with_context(|_| ServiceErrorKind::ReadHeader(header::LOCATION))?;
                        res = this
                            .get(location, &mut out, &mut runtime)
                            .acceptable(&[200, 301, 302, 404])
                            .send()?;
                    } else {
                        return Ok(this);
                    }
                }
                match res.status() {
                    StatusCode::OK => {
                        this.robots_txts
                            .insert(host.to_string(), res.text(runtime)?);
                    }
                    StatusCode::NOT_FOUND => (),
                    _ => unreachable!(),
                }
            }
        }
        Ok(this)
    }

    pub(super) fn api_token(&mut self) -> &mut LazyLockedFile {
        &mut self.api_token
    }

    pub(super) fn client(&self) -> reqwest::r#async::Client {
        self.client.clone()
    }

    /// Whether it has any cookie value.
    pub(super) fn has_cookie(&self) -> bool {
        match self.jar.as_ref() {
            Some(jar) => jar.inner.iter().next().is_some(),
            None => false,
        }
    }

    pub(super) fn cookies_to_header_value(&self) -> ServiceResult<Option<HeaderValue>> {
        match self.jar.as_ref().map(AutosavedCookieJar::to_header_value) {
            None => Ok(None),
            Some(Ok(v)) => Ok(Some(v)),
            Some(Err(e)) => Err(e),
        }
    }

    pub(super) fn insert_cookie(&mut self, cookie: cookie::Cookie<'static>) -> ServiceResult<()> {
        match self.jar.as_mut() {
            None => Ok(()),
            Some(jar) => jar.insert_cookie(cookie),
        }
    }

    /// Removes all cookies.
    pub(super) fn clear_cookies(&mut self) -> ServiceResult<()> {
        if let Some(jar) = self.jar.as_mut() {
            jar.inner = CookieJar::new();
            jar.save()?;
        }
        Ok(())
    }

    /// If `url` starts with '/' and the base host is present, returns
    /// http(s)://<host><url>.
    pub(super) fn resolve_url(&self, url: impl IntoRelativeOrAbsoluteUrl) -> ServiceResult<Url> {
        url.with(self.base.as_ref())
    }

    /// Opens `url`, which is relative or absolute, with default browser
    /// printing a message.
    pub(super) fn open_in_browser(
        &mut self,
        url: impl IntoRelativeOrAbsoluteUrl,
        mut out: impl WriteColor,
    ) -> ServiceResult<()> {
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

    pub(super) fn get<'a, 'b, O: WriteColor>(
        &'a mut self,
        url: impl IntoRelativeOrAbsoluteUrl,
        out: O,
        runtime: &'b mut Runtime,
    ) -> self::Request<'a, 'b, O> {
        self.request(url, Method::GET, vec![StatusCode::OK], out, runtime)
    }

    pub(super) fn post<'a, 'b, O: WriteColor>(
        &'a mut self,
        url: impl IntoRelativeOrAbsoluteUrl,
        out: O,
        runtime: &'b mut Runtime,
    ) -> self::Request<'a, 'b, O> {
        self.request(url, Method::POST, vec![StatusCode::FOUND], out, runtime)
    }

    fn request<'a, 'b, O: WriteColor>(
        &'a mut self,
        url: impl IntoRelativeOrAbsoluteUrl,
        method: Method,
        acceptable: Vec<StatusCode>,
        out: O,
        runtime: &'b mut Runtime,
    ) -> self::Request<'a, 'b, O> {
        self::Request {
            inner: self.try_request(url, method),
            out: if self.silent { None } else { Some(out) },
            session: self,
            runtime,
            acceptable,
            no_cookie: false,
        }
    }

    fn try_request(
        &mut self,
        url: impl IntoRelativeOrAbsoluteUrl,
        method: Method,
    ) -> ServiceResult<reqwest::r#async::RequestBuilder> {
        let url = self.resolve_url(url)?;
        self.assert_not_forbidden_by_robots_txt(&url)?;
        Ok(self.client().request(method, url.as_str()))
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

#[derive(Debug)]
pub(super) struct Request<'a, 'b, O: WriteColor> {
    inner: ServiceResult<reqwest::r#async::RequestBuilder>,
    out: Option<O>,
    session: &'a mut HttpSession,
    runtime: &'b mut Runtime,
    acceptable: Vec<StatusCode>,
    no_cookie: bool,
}

impl<'a, 'b, O: WriteColor> Request<'a, 'b, O> {
    pub(super) fn basic_auth(
        self,
        username: impl fmt::Display,
        password: Option<impl fmt::Display>,
    ) -> Self {
        Self {
            inner: self.inner.map(|inner| inner.basic_auth(username, password)),
            ..self
        }
    }

    pub(super) fn bearer_auth(self, token: impl fmt::Display) -> Self {
        Self {
            inner: self.inner.map(|inner| inner.bearer_auth(token)),
            ..self
        }
    }

    pub(super) fn form(mut self, form: &(impl Serialize + ?Sized)) -> Self {
        self.inner = self.inner.map(|inner| inner.form(form));
        self
    }

    pub(super) fn json(mut self, json: &(impl Serialize + ?Sized)) -> Self {
        self.inner = self.inner.map(|inner| inner.json(json));
        self
    }

    /// Panics:
    ///
    /// Panics if `statuses` contains `n` such that `n < 100 || 600 <= n`.
    pub(super) fn acceptable(self, statuses: &'static [u16]) -> Self {
        let acceptable = statuses
            .iter()
            .map(|&n| StatusCode::from_u16(n))
            .collect::<std::result::Result<_, _>>()
            .unwrap();
        Self { acceptable, ..self }
    }

    pub(super) fn no_cookie(mut self) -> Self {
        self.no_cookie = true;
        self
    }

    pub(super) fn send(self) -> ServiceResult<self::Response> {
        self.send_internal().map(|(r, _)| r)
    }

    fn send_internal(mut self) -> ServiceResult<(self::Response, &'b mut Runtime)> {
        let mut req = self.inner?;
        if !self.no_cookie {
            if let Some(jar) = self.session.jar.as_ref() {
                req = req.header(header::COOKIE, jar.to_header_value()?);
            }
        }
        let req = req.build()?;
        let client = &self.session.client;
        let runtime = self.runtime;
        if let Some(out) = self.out.as_mut() {
            req.echo_method(out)?;
        }
        let fut = client
            .execute(req)
            .map_err(ServiceError::from)
            .select(crate::signal::ctrl_c())
            .map(|(r, _)| r)
            .map_err(|(e, _)| e);
        let res = match runtime.block_on(fut) {
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
        if !self.no_cookie {
            if let Some(jar) = self.session.jar.as_mut() {
                jar.update(&res)?;
            }
        }
        let inner = res.filter_by_status(self.acceptable)?;
        Ok((self::Response { inner }, runtime))
    }

    pub(super) fn send_form(
        self,
        form: &(impl Serialize + ?Sized),
    ) -> ServiceResult<self::Response> {
        self.form(form).send()
    }

    pub(super) fn send_multipart(
        mut self,
        form: reqwest::r#async::multipart::Form,
    ) -> ServiceResult<self::Response> {
        self.inner = self.inner.map(|inner| inner.multipart(form));
        self.send()
    }

    pub(super) fn recv_html(self) -> ServiceResult<Html> {
        let (res, runtime) = self.send_internal()?;
        res.html(runtime)
    }

    pub(super) fn recv_json<T: DeserializeOwned + Send + Sync + 'static>(self) -> ServiceResult<T> {
        let (res, runtime) = self.send_internal()?;
        res.json(runtime)
    }

    pub(super) fn status(self) -> ServiceResult<StatusCode> {
        self.send_internal().map(|(r, _)| r.status())
    }
}

#[derive(Debug)]
pub(super) struct Response {
    inner: reqwest::r#async::Response,
}

impl Response {
    pub(super) fn header_location(&self) -> ServiceResult<Option<&str>> {
        self.headers()
            .get(header::LOCATION)
            .map(|location| {
                location.to_str().map_err(|e| {
                    e.context(ServiceErrorKind::ReadHeader(header::LOCATION))
                        .into()
                })
            })
            .transpose()
    }

    fn text(mut self, runtime: &mut Runtime) -> ServiceResult<String> {
        struct BufDecoder {
            decoder: Decoder,
            buf: Vec<u8>,
        }

        impl Future for BufDecoder {
            type Item = Vec<u8>;
            type Error = ServiceError;

            fn poll(&mut self) -> Poll<Vec<u8>, ServiceError> {
                if let Some(chunk) = try_ready!(self.decoder.poll()) {
                    self.buf.extend(chunk);
                    task::current().notify();
                    Ok(Async::NotReady)
                } else {
                    Ok(Async::Ready(mem::replace(&mut self.buf, vec![])))
                }
            }
        }

        let content_type = self
            .inner
            .headers()
            .get(header::CONTENT_TYPE)
            .and_then(|v| v.to_str().ok())
            .and_then(|v| v.parse::<Mime>().ok());
        let encoding = content_type
            .as_ref()
            .and_then(|mime| mime.get_param("charset"));
        if let Some(encoding) = &encoding {
            if *encoding != "utf-8" {
                let encoding = encoding.as_str().to_owned();
                return Err(ServiceErrorKind::NonUtf8Content(Some(encoding)).into());
            }
        }
        let cap = self
            .inner
            .headers()
            .get(header::CONTENT_LENGTH)
            .and_then(|s| s.to_str().ok())
            .and_then(|s| s.parse::<usize>().ok())
            .unwrap_or(0);
        let buf = Vec::with_capacity(cap);
        let decoder = mem::replace(self.inner.body_mut(), Decoder::empty());
        let fut = BufDecoder { decoder, buf }.select(crate::signal::ctrl_c());
        let content = runtime.block_on(fut).map(|(x, _)| x).map_err(|(e, _)| e)?;
        String::from_utf8(content).map_err(|e| {
            let encoding = encoding.map(|e| e.as_str().to_owned());
            e.context(ServiceErrorKind::NonUtf8Content(encoding)).into()
        })
    }

    pub(super) fn html(self, runtime: &mut Runtime) -> ServiceResult<Html> {
        let text = self.text(runtime)?;
        Ok(Html::parse_document(&text))
    }

    pub(super) fn json<T: DeserializeOwned + Send + Sync + 'static>(
        mut self,
        runtime: &mut Runtime,
    ) -> ServiceResult<T> {
        let fut = self
            .inner
            .json()
            .map_err(ServiceError::from)
            .select(crate::signal::ctrl_c::<T, _>());
        runtime.block_on(fut).map(|(x, _)| x).map_err(|(e, _)| e)
    }
}

impl Deref for Response {
    type Target = reqwest::r#async::Response;

    fn deref(&self) -> &reqwest::r#async::Response {
        &self.inner
    }
}

trait RequestExt {
    fn echo_method(&self, out: impl WriteColor) -> io::Result<()>;
}

impl RequestExt for reqwest::r#async::Request {
    fn echo_method(&self, mut out: impl WriteColor) -> io::Result<()> {
        out.set_color(color!(bold))?;
        out.write_str(self.method())?;
        out.write_str(" ")?;
        out.set_color(color!(fg(Cyan), intense))?;
        out.write_str(self.url())?;
        out.reset()?;
        out.write_str(" ... ")?;
        out.flush()
    }
}

trait ResponseExt: Sized {
    fn echo_status(&self, expected_statuses: &[StatusCode], out: impl WriteColor)
        -> io::Result<()>;
    fn filter_by_status(self, expected: Vec<StatusCode>) -> ServiceResult<Self>;
}

impl ResponseExt for reqwest::r#async::Response {
    fn echo_status(
        &self,
        expected_statuses: &[StatusCode],
        mut out: impl WriteColor,
    ) -> io::Result<()> {
        if expected_statuses.contains(&self.status()) {
            out.set_color(color!(fg(Green), intense, bold))?;
        } else {
            out.set_color(color!(fg(Red), intense, bold))?;
        };
        write!(out, "{}", self.status())?;
        out.reset()?;
        writeln!(out)?;
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
            )
            .into())
        }
    }
}

#[derive(Debug, new)]
pub(super) struct UrlBase {
    host: Host<&'static str>,
    https: bool,
    port: Option<u16>,
}

pub(super) trait IntoRelativeOrAbsoluteUrl {
    fn with(self, base: Option<&UrlBase>) -> ServiceResult<Url>;
}

impl IntoRelativeOrAbsoluteUrl for Url {
    fn with(self, _: Option<&UrlBase>) -> ServiceResult<Url> {
        Ok(self)
    }
}

impl<'a> IntoRelativeOrAbsoluteUrl for &'a Url {
    fn with(self, _: Option<&UrlBase>) -> ServiceResult<Url> {
        Ok(self.clone())
    }
}

impl<'a> IntoRelativeOrAbsoluteUrl for &'a str {
    fn with(self, base: Option<&UrlBase>) -> ServiceResult<Url> {
        let mut url = Cow::from(self);
        if let Some(base) = base {
            if url.starts_with('/') {
                url = format!(
                    "http{}://{}{}{}",
                    if base.https { "s" } else { "" },
                    base.host,
                    match base.port {
                        Some(port) => format!(":{}", port),
                        None => "".to_owned(),
                    },
                    url,
                )
                .into();
            }
        }
        Url::parse(&url).map_err(|e| {
            e.context(ServiceErrorKind::ParseUrl(url.into_owned()))
                .into()
        })
    }
}

impl<'a> IntoRelativeOrAbsoluteUrl for &'a String {
    fn with(self, base: Option<&UrlBase>) -> ServiceResult<Url> {
        self.as_str().with(base)
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

    fn update(&mut self, response: &reqwest::r#async::Response) -> ServiceResult<()> {
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
    use crate::errors::{FileError, FileErrorKind, ServiceError, ServiceErrorKind, ServiceResult};
    use crate::path::{AbsPath, AbsPathBuf};
    use crate::service;
    use crate::service::session::{HttpSession, HttpSessionInitParams, UrlBase};

    use failure::Fallible;
    use futures::Future as _;
    use if_chain::if_chain;
    use once_cell::sync::Lazy;
    use once_cell::sync_lazy;
    use pretty_assertions::assert_eq;
    use reqwest::StatusCode;
    use tempdir::TempDir;
    use termcolor::{Ansi, Color, ColorSpec, NoColor, WriteColor};
    use tokio::runtime::Runtime;
    use url::Host;
    use warp::Filter;

    use std::io::{self, Write as _};
    use std::net::Ipv4Addr;
    use std::{env, panic, str};

    #[test]
    fn it_works() -> Fallible<()> {
        const LOCALHOST_PORT: u16 = 2000;

        let filter_ua = warp::filters::header::exact("User-Agent", service::USER_AGENT);
        let index = warp::path::end()
            .and(filter_ua)
            .map(|| warp::reply::with_header(warp::reply(), "Set-Cookie", "foo=bar"));
        let confirm_cookie = warp::path("confirm-cookie")
            .and(filter_ua)
            .and(warp::filters::cookie::cookie("foo"))
            .map(|value: String| match value.as_ref() {
                "bar" => warp::reply::with_status("", StatusCode::OK),
                _ => warp::reply::with_status("", StatusCode::BAD_REQUEST),
            });
        let robots_txt = warp::path("robots.txt")
            .and(filter_ua)
            .map(|| "User-agent: *\nDisallow: /sensitive");
        let server = warp::serve(index.or(confirm_cookie).or(robots_txt));

        let mut runtime = Runtime::new()?;
        runtime.spawn(server.bind(([127, 0, 0, 1], LOCALHOST_PORT)));

        let tempdir = dunce::canonicalize(&env::temp_dir())?;
        let tempdir = TempDir::new_in(&tempdir, "it_keeps_a_file_locked_while_alive")?;

        let result = panic::catch_unwind::<_, Fallible<()>>(|| {
            let cookies = AbsPathBuf::try_new(tempdir.path().join("cookies")).unwrap();
            let client = service::reqwest_async_client(None)?;
            let base = UrlBase::new(Host::Ipv4(Ipv4Addr::new(127, 0, 0, 1)), false, Some(2000));
            let mut wtr = Ansi::new(vec![]);
            let mut runtime = Runtime::new()?;
            let mut sess = HttpSession::try_new(HttpSessionInitParams {
                out: &mut wtr,
                runtime: &mut runtime,
                robots: true,
                client,
                base: Some(base),
                cookies_path: Some(&cookies),
                api_token_path: None,
                silent: false,
            })?;

            sess.get("/", &mut wtr, &mut runtime).send()?;
            sess.get("/confirm-cookie", &mut wtr, &mut runtime).send()?;
            sess.get("/nonexisting", &mut wtr, &mut runtime)
                .acceptable(&[404])
                .send()?;
            sess.get("/nonexisting", &mut wtr, &mut runtime)
                .acceptable(&[])
                .send()?;
            if_chain! {
                let err = sess.get("/sensitive", &mut wtr, &mut runtime).send().unwrap_err();
                if let ServiceError::Context(ctx) = &err;
                if let ServiceErrorKind::ForbiddenByRobotsTxt = ctx.get_context();
                then {} else { return Err(err.into()) }
            }

            static EXPECTED: Lazy<String> = sync_lazy! {
                let mut expected = Ansi::new(vec![]);

                let mut print_line = |path: &str, status_color: u8, status: &str| {
                    expected.set_color(ColorSpec::new().set_bold(true)).unwrap();
                    expected.write_all(b"GET ").unwrap();
                    expected
                        .set_color(ColorSpec::new().set_fg(Some(Color::Ansi256(14))))
                        .unwrap();
                    write!(expected, "http://127.0.0.1:{}{}", LOCALHOST_PORT, path).unwrap();
                    expected.reset().unwrap();
                    expected.write_all(b" ... ").unwrap();
                    expected
                        .set_color(
                            ColorSpec::new()
                                .set_fg(Some(Color::Ansi256(status_color)))
                                .set_bold(true),
                        )
                        .unwrap();
                    expected.write_all(status.as_ref()).unwrap();
                    expected.reset().unwrap();
                    expected.write_all(b"\n").unwrap();
                };

                print_line("/robots.txt", 10, "200 OK");
                print_line("/", 10, "200 OK");
                print_line("/confirm-cookie", 10, "200 OK");
                print_line("/nonexisting", 10, "404 Not Found");
                print_line("/nonexisting", 9, "404 Not Found");

                String::from_utf8(expected.into_inner()).unwrap()
            };

            assert_eq!(str::from_utf8(wtr.get_ref())?, &*EXPECTED);
            Ok(())
        });

        runtime.shutdown_now().wait().unwrap();
        tempdir.close()?;
        result.unwrap_or_else(|p| panic::resume_unwind(p))
    }

    #[test]
    fn it_keeps_a_file_locked_while_alive() -> Fallible<()> {
        fn construct_session(
            out: impl WriteColor,
            runtime: &mut Runtime,
            client: &reqwest::r#async::Client,
            path: &AbsPath,
        ) -> ServiceResult<HttpSession> {
            HttpSession::try_new(HttpSessionInitParams {
                out,
                runtime,
                robots: true,
                client: client.clone(),
                base: None,
                cookies_path: Some(path),
                api_token_path: None,
                silent: true,
            })
        }

        let tempdir = dunce::canonicalize(&env::temp_dir())?;
        let tempdir = TempDir::new_in(&tempdir, "it_keeps_a_file_locked_while_alive")?;
        let path = AbsPathBuf::try_new(tempdir.path().join("cookies")).unwrap();
        let path = path.as_path();
        let mut sink = NoColor::new(io::sink());
        let mut rt = Runtime::new()?;
        let client = service::reqwest_async_client(None)?;
        construct_session(&mut sink, &mut rt, &client, path)?;
        construct_session(&mut sink, &mut rt, &client, path)?;
        let _session = construct_session(&mut sink, &mut rt, &client, path)?;
        if_chain! {
            let err = construct_session(&mut sink, &mut rt, &client, path).unwrap_err();
            if let ServiceError::File(FileError::Context(kind)) = &err;
            if let FileErrorKind::Lock(_) = kind.get_context();
            then { Ok(()) } else { Err(err.into()) }
        }
    }
}
