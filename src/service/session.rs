use crate::errors::{
    FileError, FileErrorKind, FileResult, PseudoReqwestError, PseudoReqwestResult, ServiceError,
    ServiceErrorKind, ServiceResult,
};
use crate::fs::{LazyLockedFile, LockedFile};
use crate::path::AbsPath;
use crate::service::session::owned_robots::OwnedRobots;
use crate::terminal::{HasTermProps, Input, WriteExt as _};
use crate::util::collections::NonEmptyIndexSet;
use crate::util::indexmap::IndexSetAsRefStrExt as _;

use cookie_store::CookieStore;
use derive_new::new;
use failure::{Fail as _, ResultExt as _};
use futures::{task, try_ready, Async, Future, Poll, Stream as _};
use http::{HttpTryFrom, Uri};
use if_chain::if_chain;
use indexmap::IndexSet;
use itertools::Itertools as _;
use maplit::hashmap;
use mime::Mime;
use reqwest::header::{self, HeaderMap, HeaderName, HeaderValue};
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
use std::io::{self, BufReader, Write as _};
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

    /// Whether it has any **unexpired** cookie.
    fn has_cookie(&self) -> bool {
        self.state_ref()
            .cookie_store
            .as_ref()
            .map_or(false, |s| s.store.iter_unexpired().next().is_some())
    }

    fn cookies_to_header_value(&self, url: &Url) -> Option<HeaderValue> {
        self.state_ref()
            .cookie_store
            .as_ref()
            .and_then(|s| s.to_header_value(url))
    }

    fn insert_cookie(&mut self, cookie: &cookie::Cookie, url: &Url) -> ServiceResult<()> {
        if let Some(store) = &mut self.state_mut().cookie_store {
            store.insert_cookie(cookie, url)?;
        }
        Ok(())
    }

    /// Removes all cookies.
    fn clear_cookies(&mut self) -> ServiceResult<()> {
        if let Some(store) = &mut self.state_mut().cookie_store {
            store.clear()?;
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
    cookie_store: Option<AutosavedCookieStore>,
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
            cookie_store: cookies_path
                .map(AutosavedCookieStore::try_new)
                .transpose()?,
            api_token: match api_token_path {
                None => LazyLockedFile::Null,
                Some(path) => LazyLockedFile::Uninited(path.to_owned()),
            },
            url_base,
            http_silent,
        };
        if robots {
            if let Some(host) = host {
                let res = this
                    .get("/robots.txt")
                    .acceptable(&[200, 404])
                    .redirect_unlimited()
                    .send()?;
                if res.status() == 200 {
                    let txt = res.text(&mut this.runtime)?;
                    this.robots.insert(host.to_string(), OwnedRobots::new(txt));
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
            req: self.resolve_url(url).and_then(|url| {
                self.assert_not_forbidden_by_robots_txt(&url)?;
                Ok(self::RequestBuilder::new(self.client(), method, url))
            }),
            stderr: if self.http_silent {
                None
            } else {
                Some(&mut self.stderr)
            },
            runtime: &mut self.runtime,
            cookie_store: self.cookie_store.as_mut(),
            acceptable,
            redirect_unlimited: false,
            no_cookie: false,
        }
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
    req: ServiceResult<self::RequestBuilder>,
    stderr: Option<E>,
    runtime: &'a mut Runtime,
    cookie_store: Option<&'a mut AutosavedCookieStore>,
    acceptable: Vec<StatusCode>,
    redirect_unlimited: bool,
    no_cookie: bool,
}

impl<'a, E: WriteColor> Request<'a, E> {
    pub(super) fn basic_auth(
        self,
        username: impl fmt::Display,
        password: Option<impl fmt::Display>,
    ) -> Self {
        Self {
            req: self
                .req
                .and_then(|r| r.basic_auth(username, password).map_err(Into::into)),
            ..self
        }
    }

    pub(super) fn bearer_auth(self, token: impl fmt::Display) -> Self {
        Self {
            req: self
                .req
                .and_then(|r| r.bearer_auth(token).map_err(Into::into)),
            ..self
        }
    }

    pub(super) fn form(self, form: &(impl Serialize + ?Sized)) -> Self {
        Self {
            req: self.req.and_then(|r| r.form(form).map_err(Into::into)),
            ..self
        }
    }

    pub(super) fn json(self, json: &(impl Serialize + ?Sized)) -> Self {
        Self {
            req: self.req.and_then(|r| r.json(json).map_err(Into::into)),
            ..self
        }
    }

    /// # Panics:
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

    /// # Panics:
    ///
    /// Panics if `self.acceptable` contains any 3xx status code.
    fn redirect_unlimited(self) -> Self {
        if self.acceptable.iter().any(|s| s.is_redirection()) {
            panic!("`acceptable` contains 3xx status code(s)");
        }
        Self {
            redirect_unlimited: true,
            ..self
        }
    }

    pub(super) fn no_cookie(mut self) -> Self {
        self.no_cookie = true;
        self
    }

    pub(super) fn send(self) -> ServiceResult<self::Response> {
        self.send_internal().map(|(r, _)| r)
    }

    fn send_internal(mut self) -> ServiceResult<(self::Response, &'a mut Runtime)> {
        if_chain! {
            if !self.no_cookie;
            if let Ok(req) = &mut self.req;
            if let Some(store) = &self.cookie_store;
            if let Some(value) = store.to_header_value(&req.url);
            then {
                req.headers.insert(header::COOKIE, value);
            }
        }

        let runtime = self.runtime;
        let mut req = self.req?;

        loop {
            if let Some(stderr) = &mut self.stderr {
                stderr.set_color(color!(bold))?;
                stderr.write_str(&req.method)?;
                stderr.write_str(" ")?;
                stderr.set_color(color!(fg(Cyan), intense))?;
                stderr.write_str(&req.url)?;
                stderr.reset()?;
                stderr.write_str(" ... ")?;
                stderr.flush()?;
            }

            let res = runtime.block_on(WithCtrlC(req.send()));
            if res.is_err() {
                if let Some(stderr) = &mut self.stderr {
                    writeln!(stderr)?;
                    stderr.flush()?;
                }
            }
            let res = res?;

            let redirect = self.redirect_unlimited && res.status().is_redirection();
            let acceptable = redirect || self.acceptable.contains(&res.status());

            if let Some(stderr) = &mut self.stderr {
                if acceptable {
                    stderr.set_color(color!(fg(Green), intense, bold))?;
                } else {
                    stderr.set_color(color!(fg(Red), intense, bold))?;
                };
                write!(stderr, "{}", res.status())?;
                stderr.reset()?;
                writeln!(stderr)?;
                stderr.flush()?;
            }

            if !self.no_cookie {
                if let Some(store) = &mut self.cookie_store {
                    store.update(&res)?;
                }
            }

            if redirect {
                let loc = res.headers().get(header::LOCATION).and_then(|loc| {
                    let loc = req.url.join(loc.to_str().ok()?).ok()?;
                    guard!(loc.as_str().parse::<Uri>().is_ok());
                    Some(loc)
                });
                if let Some(loc) = loc {
                    req.url = loc;
                    continue;
                }
            }

            break if acceptable {
                Ok((self::Response(res), runtime))
            } else {
                Err(ServiceErrorKind::UnexpectedStatusCode(
                    res.url().to_owned(),
                    res.status(),
                    self.acceptable.clone(),
                )
                .into())
            };
        }
    }

    pub(super) fn send_form(
        self,
        form: &(impl Serialize + ?Sized),
    ) -> ServiceResult<self::Response> {
        self.form(form).send()
    }

    pub(super) fn send_multipart(mut self, form: FormBuilder) -> ServiceResult<self::Response> {
        self.req = self.req.map(|r| r.multipart(form));
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

/// A builder that can construct a `reqwest::async::Request` any number of times.
#[derive(Debug)]
struct RequestBuilder {
    client: reqwest::r#async::Client,
    method: Method,
    url: Url,
    headers: HeaderMap,
    body: self::Body,
}

impl RequestBuilder {
    fn new(client: reqwest::r#async::Client, method: Method, url: Url) -> Self {
        Self {
            client,
            method,
            url,
            headers: HeaderMap::new(),
            body: self::Body::None,
        }
    }

    fn header<V>(mut self, key: HeaderName, value: V) -> PseudoReqwestResult<Self>
    where
        HeaderValue: HttpTryFrom<V>,
    {
        let value = <HeaderValue as HttpTryFrom<_>>::try_from(value)
            .map_err(|e| PseudoReqwestError::new(self.url.clone(), Into::<http::Error>::into(e)))?;
        self.headers.insert(key, value);
        Ok(self)
    }

    fn basic_auth<U: fmt::Display, P: fmt::Display>(
        self,
        username: U,
        password: Option<P>,
    ) -> PseudoReqwestResult<Self> {
        let auth = match password {
            None => format!("{}:", username),
            Some(p) => format!("{}:{}", username, p),
        };
        let auth = base64::encode(&auth);
        self.header(header::AUTHORIZATION, format!("Basic {}", auth))
    }

    fn bearer_auth<T: fmt::Display>(self, token: T) -> PseudoReqwestResult<Self> {
        self.header(header::AUTHORIZATION, format!("Bearer {}", token))
    }

    fn multipart(mut self, multipart: FormBuilder) -> Self {
        self.body = self::Body::Multipart(multipart);
        self
    }

    fn form<T: Serialize + ?Sized>(mut self, form: &T) -> PseudoReqwestResult<Self> {
        self.headers.insert(
            header::CONTENT_TYPE,
            HeaderValue::from_static("application/x-www-form-urlencoded"),
        );
        let form = serde_urlencoded::to_string(form)
            .map_err(|e| PseudoReqwestError::new(self.url.clone(), e))?;
        self.body = self::Body::Serialized(form);
        Ok(self)
    }

    fn json<T: Serialize + ?Sized>(mut self, json: &T) -> PseudoReqwestResult<Self> {
        self.headers.insert(
            header::CONTENT_TYPE,
            HeaderValue::from_static("application/json"),
        );
        let json = serde_json::to_string(json)
            .map_err(|e| PseudoReqwestError::new(self.url.clone(), e))?;
        self.body = self::Body::Serialized(json);
        Ok(self)
    }

    fn send(&self) -> impl Future<Item = reqwest::r#async::Response, Error = reqwest::Error> {
        let mut req = self
            .client
            .request(self.method.clone(), self.url.clone())
            .headers(self.headers.clone());
        match &self.body {
            self::Body::None => {}
            self::Body::Serialized(s) => req = req.body(s.clone()),
            self::Body::Multipart(f) => req = req.multipart(f.build()),
        };
        req.send()
    }
}

#[derive(Debug)]
enum Body {
    None,
    Serialized(String),
    Multipart(FormBuilder),
}

#[derive(Debug, Default)]
pub(crate) struct FormBuilder(Vec<(Cow<'static, str>, Cow<'static, str>)>);

impl FormBuilder {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn text<T: Into<Cow<'static, str>>, U: Into<Cow<'static, str>>>(
        mut self,
        name: T,
        value: U,
    ) -> Self {
        self.0.push((name.into(), value.into()));
        self
    }

    fn build(&self) -> reqwest::r#async::multipart::Form {
        let mut ret = reqwest::r#async::multipart::Form::new();
        for (key, value) in &self.0 {
            ret = ret.text(key.clone(), value.clone());
        }
        ret
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
                crate::signal::check_ctrl_c()?;

                if let Some(chunk) = try_ready!(self.decoder.poll()) {
                    self.buf.extend(chunk);
                    task::current().notify();
                    Ok(Async::NotReady)
                } else {
                    Ok(Async::Ready(mem::replace(&mut self.buf, vec![])))
                }
            }
        }

        let encoding = self
            .0
            .headers()
            .get(header::CONTENT_TYPE)
            .and_then(|v| v.to_str().ok())
            .and_then(|v| v.parse::<Mime>().ok())
            .and_then(|m| m.get_param("charset").map(|m| m.as_str().to_owned()));

        if let Some(encoding) = &encoding {
            if encoding != "utf-8" {
                return Err(ServiceErrorKind::NonUtf8Content(Some(encoding.clone())).into());
            }
        }

        let buf =
            Vec::with_capacity(self.0.content_length().map(|n| n + 1).unwrap_or(1024) as usize);
        let decoder = mem::replace(self.0.body_mut(), Decoder::empty());
        let content = runtime.block_on(BufDecoder { decoder, buf })?;
        String::from_utf8(content)
            .map_err(|e| e.context(ServiceErrorKind::NonUtf8Content(encoding)).into())
    }

    pub(super) fn html(self, runtime: &mut Runtime) -> ServiceResult<Html> {
        let text = self.text(runtime)?;
        Ok(Html::parse_document(&text))
    }

    pub(super) fn json<T: DeserializeOwned + Send + Sync + 'static>(
        mut self,
        runtime: &mut Runtime,
    ) -> ServiceResult<T> {
        runtime.block_on(WithCtrlC(self.0.json()))
    }
}

impl Deref for Response {
    type Target = reqwest::r#async::Response;

    fn deref(&self) -> &reqwest::r#async::Response {
        &self.0
    }
}

struct WithCtrlC<F: Future<Error = reqwest::Error>>(F);

impl<F: Future<Error = reqwest::Error>> Future for WithCtrlC<F> {
    type Item = F::Item;
    type Error = ServiceError;

    fn poll(&mut self) -> Poll<F::Item, ServiceError> {
        crate::signal::check_ctrl_c()?;
        self.0.poll().map_err(Into::into)
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
struct AutosavedCookieStore {
    file: LockedFile,
    store: CookieStore,
}

impl AutosavedCookieStore {
    fn try_new(path: &AbsPath) -> FileResult<Self> {
        let mut file = LockedFile::try_new(path)?;

        let store = if file.is_empty()? {
            CookieStore::default()
        } else {
            let array = file.json::<Vec<serde_json::Value>>()?;
            let json_lines = array.iter().join("\n");
            CookieStore::load_json(BufReader::new(json_lines.as_bytes()))
                .with_context(|_| FileErrorKind::Read(path.to_owned()))
                .map_err(FileError::from)?
        };

        Ok(Self { file, store })
    }

    fn to_header_value(&self, url: &Url) -> Option<HeaderValue> {
        use percent_encoding::USERINFO_ENCODE_SET;

        // https://github.com/seanmonstar/reqwest/pull/522
        let header =
            self.store
                .get_request_cookies(url)
                .fold("".to_owned(), |mut header, cookie| {
                    let name = cookie.name().as_ref();
                    let name = percent_encoding::percent_encode(name, USERINFO_ENCODE_SET);
                    let value = cookie.value().as_ref();
                    let value = percent_encoding::percent_encode(value, USERINFO_ENCODE_SET);
                    if !header.is_empty() {
                        header.push_str("; ");
                    }
                    write!(header, "{}={}", name, value).unwrap();
                    header
                });

        guard!(!header.is_empty());
        let result = header.parse();
        debug_assert!(result.is_ok());
        result.ok()
    }

    fn insert_cookie(&mut self, cookie: &cookie::Cookie, url: &Url) -> ServiceResult<()> {
        self.store
            .insert_raw(cookie, url)
            .with_context(|_| ServiceErrorKind::InvalidCookie)?;
        self.save().map_err(Into::into)
    }

    fn update(&mut self, response: &reqwest::r#async::Response) -> ServiceResult<()> {
        let mut inserted = false;
        for setcookie in response.headers().get_all(header::SET_COOKIE) {
            // Ignores invalid cookies as `reqwest` does.
            if let Ok(cookie) = setcookie.to_str() {
                inserted |= self.store.parse(cookie, response.url()).is_ok();
            }
        }
        if inserted {
            self.save()?;
        }
        Ok(())
    }

    fn clear(&mut self) -> ServiceResult<()> {
        self.store.clear();
        self.save().map_err(Into::into)
    }

    fn save(&mut self) -> FileResult<()> {
        let array = self
            .store
            .iter_unexpired()
            .filter(|c| c.is_persistent())
            .collect::<Vec<_>>();
        self.file.write_json(&array).map_err(Into::into)
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
    use http::Uri;
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
        let redirect = warp::path("redirect")
            .and(filter_ua)
            .map(|| warp::redirect(Uri::from_static("/")));
        let server = warp::serve(index.or(confirm_cookie).or(robots_txt).or(redirect));

        let mut runtime = Runtime::new()?;
        runtime.spawn(server.bind(([127, 0, 0, 1], LOCALHOST_PORT)));

        let tempdir = dunce::canonicalize(&env::temp_dir())?;
        let tempdir = TempDir::new_in(&tempdir, "it_keeps_a_file_locked_while_alive")?;

        let result = panic::catch_unwind::<_, Fallible<()>>(|| {
            let mut stderr = AnsiWithProps::new();

            let url_base = UrlBase::new(Host::Ipv4(Ipv4Addr::new(127, 0, 0, 1)), false, Some(2000));
            let cookies = AbsPathBuf::try_new(tempdir.path().join("cookies.json")).unwrap();
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
            sess.get("/redirect").redirect_unlimited().send()?;
            sess.get("/nonexisting").acceptable(&[404]).send()?;
            if_chain! {
                let err = sess.get("/nonexisting").send().unwrap_err();
                if let ServiceError::Context(ctx) = &err;
                if let ServiceErrorKind::UnexpectedStatusCode(
                    url,
                    StatusCode::NOT_FOUND,
                    expected,
                ) = ctx.get_context();
                if url.as_str() == format!("http://127.0.0.1:{}/nonexisting", LOCALHOST_PORT);
                if expected == &[StatusCode::OK];
                then {
                } else {
                    return Err(err.into())
                }
            }
            if_chain! {
                let err = sess.get("/sensitive").send().unwrap_err();
                if let ServiceError::Context(ctx) = &err;
                if let ServiceErrorKind::ForbiddenByRobotsTxt = ctx.get_context();
                then {
                } else {
                    return Err(err.into())
                }
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
                print_line("/redirect", 10, "301 Moved Permanently");
                print_line("/", 10, "200 OK");
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
            cookie_store: None,
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
