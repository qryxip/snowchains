use crate::errors::{FileResult, ServiceError, ServiceErrorKind, ServiceResult};
use crate::fs::{LazyLockedFile, LockedFile};
use crate::path::AbsPath;
use crate::service::session::owned_robots::OwnedRobots;
use crate::terminal::{HasTermProps, Input, WriteExt as _};
use crate::util::collections::NonEmptyIndexSet;
use crate::util::indexmap::IndexSetAsRefStrExt as _;

use cookie::CookieJar;
use derive_new::new;
use failure::{Fail as _, Fallible, ResultExt as _};
use futures::{task, try_ready, Async, Future, Poll, Stream as _};
use if_chain::if_chain;
use indexmap::IndexSet;
use maplit::hashmap;
use mime::Mime;
use reqwest::header::{self, HeaderValue};
use reqwest::r#async::Decoder;
use reqwest::{Method, StatusCode};
use scraper::Html;
use serde::de::DeserializeOwned;
use serde::Serialize;
use termcolor::WriteColor;
use tokio::runtime::Runtime;
use url::{Host, Url};

use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{self, Write as _};
use std::io::{self, Write as _};
use std::mem;
use std::ops::Deref;

pub(super) trait Session {
    type Stdin: Input;
    type Stderr: WriteColor + HasTermProps;

    fn state_ref(&self) -> &State<Self::Stdin, Self::Stderr>;

    fn state_mut(&mut self) -> &mut State<Self::Stdin, Self::Stderr>;

    fn stderr(&mut self) -> &mut Self::Stderr {
        &mut self.state_mut().stderr
    }

    fn runtime(&mut self) -> &mut Runtime {
        &mut self.state_mut().runtime
    }

    fn client(&self) -> reqwest::r#async::Client {
        self.state_ref().client.clone()
    }

    fn api_token(&mut self) -> &mut LazyLockedFile {
        &mut self.state_mut().api_token
    }

    /// If `url` starts with '/' and the base host is present, returns
    /// http(s)://<host><url>.
    fn resolve_url(&self, url: impl IntoRelativeOrAbsoluteUrl) -> ServiceResult<Url> {
        url.with(self.state_ref().url_base.as_ref())
    }

    fn prompt_reply_stderr(&mut self, prompt: &str) -> io::Result<String> {
        let State { stdin, stderr, .. } = self.state_mut();
        stderr.write_str(prompt)?;
        stderr.flush()?;
        stdin.read_reply()
    }

    fn prompt_password_stderr(&mut self, prompt: &str) -> io::Result<String> {
        let State { stdin, stderr, .. } = self.state_mut();
        stderr.write_str(prompt)?;
        stderr.flush()?;
        stdin.read_password()
    }

    fn ask_yn(&mut self, prompt: &str, default: bool) -> io::Result<bool> {
        let State { stdin, stderr, .. } = self.state_mut();
        let prompt = format!("{}{} ", prompt, if default { "(Y/n)" } else { "(y/N)" });
        loop {
            stderr.write_str(&prompt)?;
            stderr.flush()?;
            match &stdin.read_reply()? {
                s if s.is_empty() => break Ok(default),
                s if s.eq_ignore_ascii_case("y") || s.eq_ignore_ascii_case("yes") => {
                    break Ok(true)
                }
                s if s.eq_ignore_ascii_case("n") || s.eq_ignore_ascii_case("no") => {
                    break Ok(false)
                }
                _ => {
                    stderr.set_color(color!(fg(Yellow), intense))?;
                    stderr.write_str(r#"Answer "y", "yes", "n", "no", or ""."#)?;
                    stderr.reset()?;
                }
            }
        }
    }

    /// Opens `url`, which is relative or absolute, with default browser
    /// printing a message.
    fn open_in_browser(&mut self, url: impl IntoRelativeOrAbsoluteUrl) -> ServiceResult<()> {
        let url = self.resolve_url(url)?;
        writeln!(self.stderr(), "Opening {} in default browser...", url)?;
        self.stderr().flush()?;
        let status = webbrowser::open(url.as_str())?.status;
        if status.success() {
            Ok(())
        } else {
            Err(ServiceErrorKind::Webbrowser(status).into())
        }
    }

    /// Whether it has any cookie value.
    fn has_cookie(&self) -> bool {
        self.state_ref()
            .cookie_jar
            .as_ref()
            .map_or(false, |j| j.inner.iter().next().is_some())
    }

    fn cookies_to_header_value(&self) -> ServiceResult<Option<HeaderValue>> {
        self.state_ref()
            .cookie_jar
            .as_ref()
            .map(AutosavedCookieJar::to_header_value)
            .transpose()
    }

    fn insert_cookie(&mut self, cookie: cookie::Cookie<'static>) -> ServiceResult<()> {
        if let Some(jar) = &mut self.state_mut().cookie_jar {
            jar.insert_cookie(cookie)?;
        }
        Ok(())
    }

    /// Removes all cookies.
    fn clear_cookies(&mut self) -> ServiceResult<()> {
        if let Some(jar) = &mut self.state_mut().cookie_jar {
            jar.inner = CookieJar::new();
            jar.save()?;
        }
        Ok(())
    }

    fn get(&mut self, url: impl IntoRelativeOrAbsoluteUrl) -> self::Request<&mut Self::Stderr> {
        self.state_mut()
            .request(url, Method::GET, vec![StatusCode::OK])
    }

    fn post(&mut self, url: impl IntoRelativeOrAbsoluteUrl) -> self::Request<&mut Self::Stderr> {
        self.state_mut()
            .request(url, Method::POST, vec![StatusCode::FOUND])
    }

    fn warn_not_found(
        &mut self,
        problems: &NonEmptyIndexSet<String>,
        found: &IndexSet<String>,
    ) -> io::Result<()> {
        let not_found = problems.intersection(found).collect::<IndexSet<_>>();

        if !not_found.is_empty() {
            let stderr = &mut self.state_mut().stderr;
            stderr.set_color(color!(fg(Yellow), intense))?;
            write!(stderr, "Not found: {}", not_found.format_as_str_list())?;
            stderr.reset()?;
            writeln!(stderr)?;
            stderr.flush()?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub(super) struct State<I: Input, E: WriteColor + HasTermProps> {
    stdin: I,
    stderr: E,
    runtime: Runtime,
    client: reqwest::r#async::Client,
    robots: HashMap<String, OwnedRobots>,
    cookie_jar: Option<AutosavedCookieJar>,
    api_token: LazyLockedFile,
    url_base: Option<UrlBase>,
    http_silent: bool,
}

impl<I: Input, E: WriteColor + HasTermProps> State<I, E> {
    pub(super) fn start(args: StateStartArgs<I, E>) -> ServiceResult<Self> {
        let StateStartArgs {
            stdin,
            stderr,
            runtime,
            robots,
            client,
            url_base,
            cookies_path,
            api_token_path,
            http_silent,
        } = args;

        let host = url_base.as_ref().map(|base| base.host.clone());
        let mut this = Self {
            stdin,
            stderr,
            runtime,
            client,
            robots: hashmap!(),
            cookie_jar: cookies_path.map(AutosavedCookieJar::try_new).transpose()?,
            api_token: match api_token_path {
                None => LazyLockedFile::Null,
                Some(path) => LazyLockedFile::Uninited(path.to_owned()),
            },
            url_base,
            http_silent,
        };
        if robots {
            if let Some(host) = host {
                let mut res = this
                    .get("/robots.txt")
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
                            .get(location)
                            .acceptable(&[200, 301, 302, 404])
                            .send()?;
                    } else {
                        return Ok(this);
                    }
                }
                match res.status() {
                    StatusCode::OK => {
                        let txt = res.text(&mut this.runtime)?;
                        this.robots.insert(host.to_string(), OwnedRobots::new(txt));
                    }
                    StatusCode::NOT_FOUND => (),
                    _ => unreachable!(),
                }
            }
        }
        Ok(this)
    }

    fn request<'a>(
        &'a mut self,
        url: impl IntoRelativeOrAbsoluteUrl,
        method: Method,
        acceptable: Vec<StatusCode>,
    ) -> self::Request<'a, &'a mut E> {
        self::Request {
            inner: self.try_request(url, method),
            stderr: if self.http_silent {
                None
            } else {
                Some(&mut self.stderr)
            },
            runtime: &mut self.runtime,
            client: &self.client,
            cookie_jar: self.cookie_jar.as_mut(),
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
        Ok(self.client.request(method, url.as_str()))
    }

    fn assert_not_forbidden_by_robots_txt(&self, url: &Url) -> ServiceResult<()> {
        if_chain! {
            if let Some(host) = url.host_str();
            if let Some(robots) = self.robots.get(host);
            if !robots.check_path_for_ua(url.path());
            then {
                Err(ServiceErrorKind::ForbiddenByRobotsTxt.into())
            } else {
                Ok(())
            }
        }
    }
}

impl<I: Input, E: WriteColor + HasTermProps> Session for State<I, E> {
    type Stdin = I;
    type Stderr = E;

    fn state_ref(&self) -> &Self {
        self
    }

    fn state_mut(&mut self) -> &mut Self {
        self
    }
}

#[derive(Debug)]
pub(super) struct StateStartArgs<'a, I: Input, E: WriteColor + HasTermProps> {
    pub(super) stdin: I,
    pub(super) stderr: E,
    pub(super) runtime: Runtime,
    pub(super) robots: bool,
    pub(super) client: reqwest::r#async::Client,
    pub(super) url_base: Option<UrlBase>,
    pub(super) cookies_path: Option<&'a AbsPath>,
    pub(super) api_token_path: Option<&'a AbsPath>,
    pub(super) http_silent: bool,
}

#[derive(Debug)]
pub(super) struct Request<'a, E: WriteColor> {
    inner: ServiceResult<reqwest::r#async::RequestBuilder>,
    stderr: Option<E>,
    runtime: &'a mut Runtime,
    client: &'a reqwest::r#async::Client,
    cookie_jar: Option<&'a mut AutosavedCookieJar>,
    acceptable: Vec<StatusCode>,
    no_cookie: bool,
}

impl<'a, E: WriteColor> Request<'a, E> {
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

    fn send_internal(mut self) -> ServiceResult<(self::Response, &'a mut Runtime)> {
        let mut req = self.inner?;
        if !self.no_cookie {
            if let Some(jar) = self.cookie_jar.as_ref() {
                req = req.header(header::COOKIE, jar.to_header_value()?);
            }
        }
        let req = req.build()?;
        let client = self.client;
        let runtime = self.runtime;
        if let Some(stderr) = &mut self.stderr {
            req.echo_method(stderr)?;
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
                if let Some(stderr) = &mut self.stderr {
                    writeln!(stderr)?;
                    stderr.flush()?;
                }
                Err(err)
            }
        }?;
        if let Some(stderr) = &mut self.stderr {
            res.echo_status(&self.acceptable, stderr)?;
        }
        if !self.no_cookie {
            if let Some(jar) = &mut self.cookie_jar {
                jar.update(&res)?;
            }
        }
        let inner = res.filter_by_status(self.acceptable)?;
        Ok((self::Response(inner), runtime))
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
pub(super) struct Response(reqwest::r#async::Response);

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
            .0
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
            .0
            .headers()
            .get(header::CONTENT_LENGTH)
            .and_then(|s| s.to_str().ok())
            .and_then(|s| s.parse::<usize>().ok())
            .unwrap_or(0);
        let buf = Vec::with_capacity(cap);
        let decoder = mem::replace(self.0.body_mut(), Decoder::empty());
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
            .0
            .json()
            .map_err(ServiceError::from)
            .select(crate::signal::ctrl_c::<T, _>());
        runtime.block_on(fut).map(|(x, _)| x).map_err(|(e, _)| e)
    }
}

impl Deref for Response {
    type Target = reqwest::r#async::Response;

    fn deref(&self) -> &reqwest::r#async::Response {
        &self.0
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
        if_chain! {
            if !file.is_empty()?;
            let cookies = file.json::<Vec<String>>()?;
            if !cookies.is_empty();
            then {
                for cookie in cookies {
                    let cookie = cookie::Cookie::parse(cookie.clone()).with_context(|_| {
                        ServiceErrorKind::ParseCookieFromPath(path.to_owned(), cookie)
                    })?;
                    inner.add(cookie);
                }
            } else {
                file.write_json(&Vec::<String>::new())?;
            }
        }
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
        self.file.write_json(&value)
    }
}

mod owned_robots {
    use crate::service::USER_AGENT;

    use robots_txt::{Robots, SimpleMatcher};
    use std::mem;

    /// **NOTE:** this is a self-referential struct.
    #[derive(Debug)]
    pub(super) struct OwnedRobots {
        string: String,                  // `String` is a `StableDeref`
        robots: Robots<'static>,         // This `'static` is fake
        matcher: SimpleMatcher<'static>, // This `'static` is also fake
    }

    impl OwnedRobots {
        pub(super) fn new(string: String) -> Self {
            unsafe {
                let robots = Robots::from_str(&string);
                let matcher = SimpleMatcher::new(&robots.choose_section(USER_AGENT).rules);
                Self {
                    matcher: mem::transmute(matcher),
                    robots: mem::transmute(robots),
                    string,
                }
            }
        }

        pub(super) fn check_path_for_ua(&self, path: &str) -> bool {
            self.matcher.check_path(path)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::errors::{FileError, FileErrorKind, ServiceError, ServiceErrorKind, ServiceResult};
    use crate::fs::LazyLockedFile;
    use crate::path::{AbsPath, AbsPathBuf};
    use crate::service;
    use crate::service::session::{Session, State, StateStartArgs, UrlBase};
    use crate::terminal::{AnsiWithProps, Dumb, TtyOrPiped};

    use failure::Fallible;
    use futures::Future as _;
    use if_chain::if_chain;
    use maplit::hashmap;
    use once_cell::sync::Lazy;
    use pretty_assertions::assert_eq;
    use reqwest::StatusCode;
    use tempdir::TempDir;
    use termcolor::{Ansi, Color, ColorSpec, WriteColor};
    use tokio::runtime::Runtime;
    use url::Host;
    use warp::Filter;

    use std::convert::TryFrom as _;
    use std::io::{self, Empty, Write as _};
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
            let mut stderr = AnsiWithProps::new();

            let url_base = UrlBase::new(Host::Ipv4(Ipv4Addr::new(127, 0, 0, 1)), false, Some(2000));
            let cookies = AbsPathBuf::try_new(tempdir.path().join("cookies")).unwrap();
            let mut sess = State::start(StateStartArgs {
                stdin: TtyOrPiped::Piped(io::empty()),
                stderr: &mut stderr,
                runtime: Runtime::new()?,
                robots: true,
                client: service::reqwest_async_client(None)?,
                url_base: Some(url_base),
                cookies_path: Some(&cookies),
                api_token_path: None,
                http_silent: false,
            })?;

            sess.get("/").send()?;
            sess.get("/confirm-cookie").send()?;
            sess.get("/nonexisting").acceptable(&[404]).send()?;
            sess.get("/nonexisting").acceptable(&[]).send()?;
            if_chain! {
                let err = sess.get("/sensitive").send().unwrap_err();
                if let ServiceError::Context(ctx) = &err;
                if let ServiceErrorKind::ForbiddenByRobotsTxt = ctx.get_context();
                then {} else { return Err(err.into()) }
            }

            static EXPECTED: Lazy<String> = Lazy::new(|| {
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
            });

            assert_eq!(String::try_from(stderr)?, *EXPECTED);
            Ok(())
        });

        runtime.shutdown_now().wait().unwrap();
        tempdir.close()?;
        result.unwrap_or_else(|p| panic::resume_unwind(p))
    }

    #[test]
    fn it_keeps_a_file_locked_while_alive() -> Fallible<()> {
        fn construct_state(
            client: &reqwest::r#async::Client,
            path: &AbsPath,
        ) -> ServiceResult<State<TtyOrPiped<Empty>, Dumb>> {
            let runtime = Runtime::new()?;
            State::start(StateStartArgs {
                stdin: TtyOrPiped::Piped(io::empty()),
                stderr: Dumb::new(),
                runtime,
                robots: true,
                client: client.clone(),
                url_base: None,
                cookies_path: Some(path),
                api_token_path: None,
                http_silent: true,
            })
        }

        let tempdir = dunce::canonicalize(&env::temp_dir())?;
        let tempdir = TempDir::new_in(&tempdir, "it_keeps_a_file_locked_while_alive")?;
        let path = AbsPathBuf::try_new(tempdir.path().join("cookies")).unwrap();
        let client = service::reqwest_async_client(None)?;
        construct_state(&client, path.as_path())?;
        construct_state(&client, path.as_path())?;
        let _state = construct_state(&client, path.as_path())?;
        if_chain! {
            let err = construct_state(&client, path.as_path()).unwrap_err();
            if let ServiceError::File(FileError::Context(kind)) = &err;
            if let FileErrorKind::Lock(_) = kind.get_context();
            then {
                Ok(())
            } else {
                Err(err.into())
            }
        }
    }

    #[test]
    fn test_ask_yn() -> Fallible<()> {
        let mut rdr = "y\nn\n\ny\nn\nヌ\n\n".as_bytes();
        let mut stderr = AnsiWithProps::new();

        let mut state = State {
            stdin: TtyOrPiped::Piped(&mut rdr),
            stderr: &mut stderr,
            runtime: Runtime::new()?,
            client: reqwest::r#async::Client::builder().build()?,
            robots: hashmap!(),
            cookie_jar: None,
            api_token: LazyLockedFile::Null,
            url_base: None,
            http_silent: true,
        };

        assert_eq!(state.ask_yn("Yes?: ", true)?, true);
        assert_eq!(state.ask_yn("Yes?: ", true)?, false);
        assert_eq!(state.ask_yn("Yes?: ", true)?, true);
        assert_eq!(state.ask_yn("No?: ", false)?, true);
        assert_eq!(state.ask_yn("No?: ", false)?, false);
        assert_eq!(state.ask_yn("No?: ", false)?, false);

        let err = state.ask_yn("Yes?: ", true).unwrap_err();
        if err.kind() != io::ErrorKind::UnexpectedEof {
            return Err(err.into());
        }

        assert_eq!(rdr, &b""[..]);

        static EXPECTED: Lazy<String> = Lazy::new(|| {
            let mut expected = Ansi::new(vec![]);
            expected
                .write_all(b"Yes?: (Y/n) Yes?: (Y/n) Yes?: (Y/n) No?: (y/N) No?: (y/N) No?: (y/N) ")
                .unwrap();
            expected
                .set_color(ColorSpec::new().set_fg(Some(Color::Ansi256(11))))
                .unwrap();
            expected
                .write_all(br#"Answer "y", "yes", "n", "no", or ""."#)
                .unwrap();
            expected.reset().unwrap();
            expected.write_all(b"No?: (y/N) Yes?: (Y/n) ").unwrap();
            String::from_utf8(expected.into_inner()).unwrap()
        });
        assert_eq!(String::try_from(stderr)?, *EXPECTED);
        Ok(())
    }
}
