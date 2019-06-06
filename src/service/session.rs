use crate::errors::{
    FileError, FileErrorKind, FileResult, PseudoReqwestError, PseudoReqwestResult, ServiceError,
    ServiceErrorKind, ServiceResult,
};
use crate::fs::{LazyLockedFile, LockedFile};
use crate::path::AbsPath;
use crate::service::session::owned_robots::OwnedRobots;
use crate::service::USER_AGENT;
use crate::terminal::{HasTermProps, Input, WriteExt as _};
use crate::util::collections::NonEmptyIndexSet;
use crate::util::indexmap::IndexSetAsRefStrExt as _;

use cookie_store::CookieStore;
use derive_new::new;
use failure::{Fail, ResultExt as _};
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
use robots_txt::{Robots, SimpleMatcher};
use scraper::Html;
use serde::de::DeserializeOwned;
use serde::Serialize;
use termcolor::WriteColor;
use tokio::runtime::Runtime;
use url::{Host, Url};

use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt::{self, Write as _};
use std::io::{self, BufReader, Write as _};
use std::marker::PhantomData;
use std::ops::Deref;
use std::{cmp, mem};

pub(super) trait Session: Sized {
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

    fn get(&mut self, url: impl IntoRelativeOrAbsoluteUrl) -> self::Request<&mut Self, Get> {
        self.request::<Get, _>(url, vec![StatusCode::OK])
    }

    fn post(&mut self, url: impl IntoRelativeOrAbsoluteUrl) -> self::Request<&mut Self, Post> {
        self.request::<Post, _>(url, vec![StatusCode::FOUND])
    }

    fn request<M: StaticMethod, U: IntoRelativeOrAbsoluteUrl>(
        &mut self,
        url: U,
        acceptable: Vec<StatusCode>,
    ) -> self::Request<&mut Self, M> {
        self::Request {
            inner: self.resolve_url(url).and_then(|url| {
                self.state_mut().assert_not_forbidden_by_robots_txt(&url)?;
                Ok(RequestInner {
                    inner: RequestBuilderBuilder::new::<M>(self.client(), url),
                    eprint: !self.state_ref().http_silent,
                    acceptable,
                    redirect_unlimited: false,
                    retries_on_get: self.state_ref().retries_on_get,
                })
            }),
            session: self,
            phantom: PhantomData,
        }
    }

    fn retry_recv_text(&mut self, res: self::Response<Get>) -> ServiceResult<String> {
        self.retry_recv(res, |res, rt| res.text(rt))
    }

    fn retry_recv_html(&mut self, res: self::Response<Get>) -> ServiceResult<Html> {
        self.retry_recv(res, |res, rt| res.html(rt))
    }

    fn retry_recv_json<T: DeserializeOwned + Send + Sync + 'static>(
        &mut self,
        res: self::Response<Get>,
    ) -> ServiceResult<T> {
        self.retry_recv(res, |res, rt| res.json(rt))
    }

    fn retry_recv<
        R,
        F: for<'a, 'b> FnMut(&'a mut self::Response<Get>, &'b mut Runtime) -> ServiceResult<R>,
    >(
        &mut self,
        mut res: self::Response<Get>,
        mut f: F,
    ) -> ServiceResult<R> {
        f(&mut res, self.runtime()).or_else(|err| {
            res.req.retries_on_get = cmp::max(res.req.retries_on_get, 1) - 1;
            while res.req.retries_on_get > 0 {
                res = self::Request::<_, Get> {
                    inner: Ok(res.req),
                    session: &mut *self,
                    phantom: PhantomData,
                }
                .retry_send()?;
                match f(&mut res, self.runtime()) {
                    Ok(ret) => return Ok(ret),
                    Err(_) => {
                        res.req.retries_on_get = cmp::max(res.req.retries_on_get, 1) - 1;
                    }
                }
            }
            Err(err)
        })
    }

    fn recv_html(&mut self, mut res: self::Response<Post>) -> ServiceResult<Html> {
        res.html(self.runtime())
    }

    fn recv_json<T: DeserializeOwned + Send + Sync + 'static>(
        &mut self,
        mut res: self::Response<Post>,
    ) -> ServiceResult<T> {
        res.json(self.runtime())
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
    retries_on_get: u32,
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
            retries_on_get,
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
            retries_on_get,
            http_silent,
        };
        if robots {
            if let Some(host) = host {
                let res = this
                    .get("/robots.txt")
                    .acceptable(&[200, 404])
                    .redirect_unlimited()
                    .retry_send()?;
                if res.status() == 200 {
                    let robots = OwnedRobots::new(
                        this.retry_recv_text(res)?,
                        |s| Robots::from_str(s),
                        |robots| SimpleMatcher::new(&robots.choose_section(USER_AGENT).rules),
                    );
                    this.robots.insert(host.to_string(), robots);
                }
            }
        }
        Ok(this)
    }

    fn assert_not_forbidden_by_robots_txt(&self, url: &Url) -> ServiceResult<()> {
        if_chain! {
            if let Some(host) = url.host_str();
            if let Some(robots) = self.robots.get(host);
            if !robots.rent_matcher(|m| m.check_path(url.path()));
            then {
                Err(ServiceErrorKind::ForbiddenByRobotsTxt.into())
            } else {
                Ok(())
            }
        }
    }
}

impl<'a, S: Session> Session for &'a mut S {
    type Stdin = S::Stdin;
    type Stderr = S::Stderr;

    fn state_ref(&self) -> &State<S::Stdin, S::Stderr> {
        (**self).state_ref()
    }

    fn state_mut(&mut self) -> &mut State<S::Stdin, S::Stderr> {
        (**self).state_mut()
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
    pub(super) retries_on_get: u32,
    pub(super) http_silent: bool,
}

#[derive(Debug)]
pub(super) struct Request<S: Session, M: StaticMethod> {
    inner: ServiceResult<RequestInner>,
    session: S,
    phantom: PhantomData<fn() -> M>,
}

impl<S: Session, M: StaticMethod> Request<S, M> {
    pub(super) fn basic_auth(
        mut self,
        username: impl fmt::Display,
        password: Option<impl fmt::Display>,
    ) -> Self {
        self.inner = self.inner.and_then(|mut inner| {
            inner.inner = inner.inner.basic_auth(username, password)?;
            Ok(inner)
        });
        self
    }

    pub(super) fn bearer_auth(mut self, token: impl fmt::Display) -> Self {
        self.inner = self.inner.and_then(|mut inner| {
            inner.inner = inner.inner.bearer_auth(token)?;
            Ok(inner)
        });
        self
    }

    /// # Panics:
    ///
    /// Panics if:
    /// - `statuses` contains `n` such that `n < 100 || 600 <= n`
    /// - `this.inner.redirect_unlimited` is `true` where `Ok(this) = self`
    pub(super) fn acceptable(mut self, statuses: &'static [u16]) -> Self {
        if let Ok(inner) = &mut self.inner {
            if inner.redirect_unlimited {
                panic!("`redirect_unlimited` is `true`");
            }
            inner.acceptable = statuses
                .iter()
                .map(|&n| StatusCode::from_u16(n))
                .collect::<std::result::Result<_, _>>()
                .unwrap();
        }
        self
    }

    /// # Panics:
    ///
    /// Panics if `this.inner.acceptable` contains any 3xx status code where `Ok(this) = self`.
    pub(super) fn redirect_unlimited(mut self) -> Self {
        if let Ok(inner) = &mut self.inner {
            if inner.acceptable.iter().any(|s| s.is_redirection()) {
                panic!("`acceptable` contains 3xx status code(s)");
            }
            inner.redirect_unlimited = true;
        }
        self
    }

    fn send_internal(mut self, allow_retry: bool) -> ServiceResult<(S, self::Response<M>)> {
        if_chain! {
            if let Ok(inner) = &mut self.inner;
            if let Some(store) = &self.session.state_ref().cookie_store;
            if let Some(value) = store.to_header_value(&inner.inner.url);
            then {
                inner.inner.headers.insert(header::COOKIE, value);
            }
        }

        let RequestInner {
            mut inner,
            eprint,
            acceptable,
            redirect_unlimited,
            retries_on_get,
        } = self.inner?;

        let mut retries_on_get = if allow_retry { retries_on_get } else { 0 };

        let st = self.session.state_mut();
        let (rt, stderr, mut cookie_store) =
            (&mut st.runtime, &mut st.stderr, st.cookie_store.as_mut());

        loop {
            if eprint {
                stderr.set_color(color!(bold))?;
                stderr.write_str(&inner.method)?;
                stderr.write_str(" ")?;
                stderr.set_color(color!(fg(Cyan), intense))?;
                stderr.write_str(&inner.url)?;
                stderr.reset()?;
                stderr.write_str(" ... ")?;
                stderr.flush()?;
            }

            let res = rt.block_on(WithCtrlC(inner.send()));
            if let Err(err) = &res {
                if eprint {
                    stderr.set_color(color!(fg(Red), intense, bold))?;
                    stderr.write_str(err.prompt())?;
                    stderr.reset()?;
                    writeln!(stderr)?;
                    stderr.flush()?;
                }
                if inner.method == Method::GET && retries_on_get > 0 && err.is_http_or_timeout() {
                    retries_on_get -= 1;
                    continue;
                }
            }
            let res = res?;

            let is_redirect = redirect_unlimited && res.status().is_redirection();
            let is_acceptable = is_redirect || acceptable.contains(&res.status());

            if eprint {
                if is_acceptable {
                    stderr.set_color(color!(fg(Green), intense, bold))?;
                } else {
                    stderr.set_color(color!(fg(Red), intense, bold))?;
                };
                write!(stderr, "{}", res.status())?;
                stderr.reset()?;
                writeln!(stderr)?;
                stderr.flush()?;
            }

            if let Some(cookie_store) = &mut cookie_store {
                cookie_store.update(&res)?;
            }

            if is_redirect {
                let loc = res.headers().get(header::LOCATION).and_then(|loc| {
                    let loc = inner.url.join(loc.to_str().ok()?).ok()?;
                    guard!(loc.as_str().parse::<Uri>().is_ok());
                    Some(loc)
                });
                if let Some(loc) = loc {
                    inner = inner.redirect(loc);
                    continue;
                }
            }

            if is_acceptable {
                let req = RequestInner {
                    inner,
                    eprint,
                    acceptable,
                    redirect_unlimited,
                    retries_on_get,
                };
                break Ok((self.session, self::Response::new(res, req)));
            }

            let retry_after = res
                .headers()
                .get(header::RETRY_AFTER)
                .and_then(|v| v.to_str().ok());

            if inner.method == Method::GET
                && retries_on_get > 0
                && res.status().is_server_error()
                && retry_after.is_none()
            {
                retries_on_get -= 1;
                continue;
            }

            let err = ServiceErrorKind::UnexpectedStatusCode(
                res.url().to_owned(),
                res.status(),
                acceptable,
            );

            break Err(if let Some(retry_after) = retry_after {
                let msg = format!("The response contains `Retry-After: {}`", retry_after);
                failure::err_msg(msg).context(err).into()
            } else {
                err.into()
            });
        }
    }
}

impl<S: Session> Request<S, Get> {
    pub(super) fn retry_send(self) -> ServiceResult<self::Response<Get>> {
        self.send_internal(true).map(|(_, res)| res)
    }

    pub(super) fn retry_status(self) -> ServiceResult<StatusCode> {
        self.send_internal(true).map(|(_, res)| res.status())
    }

    pub(super) fn retry_recv_html(self) -> ServiceResult<Html> {
        let (mut sess, res) = self.send_internal(true)?;
        sess.retry_recv_html(res)
    }

    pub(super) fn retry_recv_json<T: DeserializeOwned + Send + Sync + 'static>(
        self,
    ) -> ServiceResult<T> {
        let (mut sess, res) = self.send_internal(true)?;
        sess.retry_recv_json(res)
    }
}

impl<S: Session> Request<S, Post> {
    pub(super) fn form(mut self, form: &(impl Serialize + ?Sized)) -> Self {
        self.inner = self.inner.and_then(|mut inner| {
            inner.inner = inner.inner.form(form)?;
            Ok(inner)
        });
        self
    }

    pub(super) fn json(mut self, json: &(impl Serialize + ?Sized)) -> Self {
        self.inner = self.inner.and_then(|mut inner| {
            inner.inner = inner.inner.json(json)?;
            Ok(inner)
        });
        self
    }

    pub(super) fn multipart(mut self, form: self::FormBuilder) -> Self {
        self.inner = self.inner.map(|mut inner| {
            inner.inner = inner.inner.multipart(form);
            inner
        });
        self
    }

    pub(super) fn send(self) -> ServiceResult<self::Response<Post>> {
        self.send_internal(false).map(|(_, res)| res)
    }

    pub(super) fn status(self) -> ServiceResult<StatusCode> {
        self.send_internal(false).map(|(_, res)| res.status())
    }

    pub(super) fn recv_json<T: DeserializeOwned + Send + Sync + 'static>(self) -> ServiceResult<T> {
        let (mut sess, res) = self.send_internal(false)?;
        sess.recv_json(res)
    }
}

#[derive(Debug)]
struct RequestInner {
    inner: RequestBuilderBuilder,
    eprint: bool,
    acceptable: Vec<StatusCode>,
    redirect_unlimited: bool,
    retries_on_get: u32,
}

/// A builder that can construct a `reqwest::async::RequestBuilder` any number of times.
#[derive(Debug)]
struct RequestBuilderBuilder {
    client: reqwest::r#async::Client,
    method: Method,
    url: Url,
    headers: HeaderMap,
    body: self::Body,
}

impl RequestBuilderBuilder {
    fn new<M: StaticMethod>(client: reqwest::r#async::Client, url: Url) -> Self {
        Self {
            client,
            method: M::value(),
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
            .map_err(|e| PseudoReqwestError::new(self.url.clone(), e.into()))?;
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

    fn multipart(mut self, multipart: FormBuilder) -> Self {
        self.body = self::Body::Multipart(multipart);
        self
    }

    fn redirect(mut self, url: Url) -> Self {
        if (self.url.host(), self.url.port_or_known_default())
            != (url.host(), url.port_or_known_default())
        {
            // Removes sensitive headers as `reqwest` does.
            self.headers.remove(header::AUTHORIZATION);
            self.headers.remove(header::COOKIE);
            self.headers.remove("cookie2");
            self.headers.remove(header::PROXY_AUTHORIZATION);
            self.headers.remove(header::WWW_AUTHENTICATE);
        }
        self.method = Method::GET;
        self.url = url;
        self.body = self::Body::None;
        self
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
pub(super) struct Response<M: StaticMethod> {
    inner: reqwest::r#async::Response,
    req: RequestInner,
    phantom: PhantomData<fn() -> M>,
}

impl<M: StaticMethod> Response<M> {
    fn new(inner: reqwest::r#async::Response, req: RequestInner) -> Self {
        Self {
            inner,
            req,
            phantom: PhantomData,
        }
    }

    pub(super) fn location_uri(&self) -> ServiceResult<Option<Uri>> {
        fn ctx(err: impl Fail) -> ServiceError {
            err.context(ServiceErrorKind::ReadHeader(header::LOCATION))
                .into()
        }

        self.headers()
            .get(header::LOCATION)
            .map(|loc| loc.to_str().map_err(ctx)?.parse().map_err(ctx))
            .transpose()
    }

    fn text(&mut self, runtime: &mut Runtime) -> ServiceResult<String> {
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

        let buf = Vec::with_capacity(self.content_length().map(|n| n + 1).unwrap_or(1024) as usize);
        let decoder = mem::replace(self.inner.body_mut(), Decoder::empty());
        let content = runtime.block_on(BufDecoder { decoder, buf })?;
        String::from_utf8(content)
            .map_err(|e| e.context(ServiceErrorKind::NonUtf8Content(encoding)).into())
    }

    fn html(&mut self, runtime: &mut Runtime) -> ServiceResult<Html> {
        let text = self.text(runtime)?;
        Ok(Html::parse_document(&text))
    }

    fn json<T: DeserializeOwned + Send + Sync + 'static>(
        &mut self,
        runtime: &mut Runtime,
    ) -> ServiceResult<T> {
        runtime
            .block_on(WithCtrlC(self.inner.json()))
            .map_err(Into::into)
    }
}

impl TryFrom<Response<Post>> for Response<Get> {
    type Error = ();

    fn try_from(res: Response<Post>) -> std::result::Result<Self, ()> {
        if res.req.inner.method == Method::GET {
            Ok(Self {
                inner: res.inner,
                req: res.req,
                phantom: PhantomData,
            })
        } else {
            Err(())
        }
    }
}

impl<M: StaticMethod> Deref for Response<M> {
    type Target = reqwest::r#async::Response;

    fn deref(&self) -> &reqwest::r#async::Response {
        &self.inner
    }
}

// `'static` is necessary?
pub(crate) trait StaticMethod: 'static {
    fn value() -> Method;
}

#[derive(Debug)]
pub(crate) enum Get {}

impl StaticMethod for Get {
    fn value() -> Method {
        Method::GET
    }
}

#[derive(Debug)]
pub(crate) enum Post {}

impl StaticMethod for Post {
    fn value() -> Method {
        Method::POST
    }
}

struct WithCtrlC<F: Future<Error = reqwest::Error>>(F);

impl<F: Future<Error = reqwest::Error>> Future for WithCtrlC<F> {
    type Item = F::Item;
    type Error = ReqwestOrCtrlCError;

    fn poll(&mut self) -> Poll<F::Item, ReqwestOrCtrlCError> {
        crate::signal::check_ctrl_c().map_err(ReqwestOrCtrlCError::CtrlC)?;
        self.0.poll().map_err(ReqwestOrCtrlCError::Reqwest)
    }
}

#[derive(Debug)]
enum ReqwestOrCtrlCError {
    Reqwest(reqwest::Error),
    CtrlC(io::Error),
}

impl ReqwestOrCtrlCError {
    fn is_http_or_timeout(&self) -> bool {
        match self {
            ReqwestOrCtrlCError::Reqwest(e) => e.is_http() || e.is_timeout(),
            ReqwestOrCtrlCError::CtrlC(_) => false,
        }
    }

    fn prompt(&self) -> &'static str {
        match self {
            ReqwestOrCtrlCError::Reqwest(e) if e.is_http() => "<failed> (http)",
            ReqwestOrCtrlCError::Reqwest(e) if e.is_timeout() => "<failed> (timeout)",
            ReqwestOrCtrlCError::Reqwest(_) => "<failed>",
            ReqwestOrCtrlCError::CtrlC(_) => "<ctrl-c>",
        }
    }
}

impl From<ReqwestOrCtrlCError> for ServiceError {
    fn from(from: ReqwestOrCtrlCError) -> Self {
        match from {
            ReqwestOrCtrlCError::Reqwest(e) => e.into(),
            ReqwestOrCtrlCError::CtrlC(e) => e.into(),
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
    use robots_txt::{Robots, SimpleMatcher};
    use std::mem;

    /// **NOTE:** this is a self-referential struct.
    #[derive(Debug)]
    pub(super) struct OwnedRobots {
        // https://github.com/rust-lang/rfcs/blob/master/text/1857-stabilize-drop-order.md
        matcher: SimpleMatcher<'static>, // This `'static` is fake
        robots: Robots<'static>,         // This `'static` is also fake
        string: String,                  // `String` is a `StableDeref`
    }

    impl OwnedRobots {
        pub(super) fn new(
            string: String,
            robots: for<'a> fn(&'a str) -> Robots<'a>,
            matcher: for<'a, 'b> fn(&'a Robots<'b>) -> SimpleMatcher<'a>,
        ) -> Self {
            unsafe {
                let robots = robots(&string);
                let matcher = matcher(&robots);
                Self {
                    matcher: mem::transmute(matcher),
                    robots: mem::transmute(robots),
                    string,
                }
            }
        }

        pub(super) fn rent_matcher<R, F: for<'a, 'b> FnOnce(&'a SimpleMatcher<'b>) -> R>(
            &self,
            f: F,
        ) -> R {
            f(&self.matcher)
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
                retries_on_get: 1,
                http_silent: false,
            })?;

            sess.get("/").retry_send()?;
            sess.get("/confirm-cookie").retry_send()?;
            sess.get("/redirect").redirect_unlimited().retry_send()?;
            sess.get("/nonexisting").acceptable(&[404]).retry_send()?;
            if_chain! {
                let err = sess.get("/nonexisting").retry_send().unwrap_err();
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
                let err = sess.get("/sensitive").retry_send().unwrap_err();
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
                retries_on_get: 0,
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
            retries_on_get: 0,
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
