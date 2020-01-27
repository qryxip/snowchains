use crate::errors::{
    FileError, FileErrorKind, FileResult, PseudoReqwestError, PseudoReqwestResult, ServiceError,
    ServiceErrorKind, ServiceResult,
};
use crate::fs::{LazyLockedFile, LockedFile};
use crate::path::AbsPath;
use crate::service::context::owned_robots::OwnedRobots;
use crate::service::{ResponseExt as _, USER_AGENT};
use crate::terminal::{HasTermProps, Input, WriteExt as _};
use crate::util::collections::NonEmptyIndexSet;
use crate::util::indexmap::IndexSetAsRefStrExt as _;

use cookie_store::CookieStore;
use failure::{Fail, ResultExt as _};
use futures01::{task, try_ready, Async, Future, Poll, Stream as _};
use http01::{HttpTryFrom, Uri};
use if_chain::if_chain;
use indexmap::IndexSet;
use itertools::Itertools as _;
use maplit::{btreeset, hashmap};
use mime::Mime;
use reqwest::header::{self, HeaderMap, HeaderName, HeaderValue};
use reqwest::r#async::Decoder;
use reqwest::{Method, RedirectPolicy, StatusCode};
use robots_txt::{Robots, SimpleMatcher};
use scraper::Html;
use serde::de::DeserializeOwned;
use serde::Serialize;
use termcolor::WriteColor;
use tokio01::runtime::Runtime;
use url::Url;

use std::borrow::Cow;
use std::collections::{BTreeSet, HashMap};
use std::convert::TryFrom;
use std::io::{self, BufReader, Write as _};
use std::marker::PhantomData;
use std::ops::Deref;
use std::time::Duration;
use std::{cmp, fmt, mem};

pub(super) trait HasContextMut: Sized {
    type Stdin: Input;
    type Stderr: WriteColor + HasTermProps;

    fn context(&self) -> &Context<Self::Stdin, Self::Stderr>;

    fn context_mut(&mut self) -> &mut Context<Self::Stdin, Self::Stderr>;

    fn stderr(&mut self) -> &mut Self::Stderr {
        &mut self.context_mut().stderr
    }

    fn runtime(&mut self) -> &mut Runtime {
        &mut self.context_mut().runtime
    }

    fn client(&self) -> reqwest::r#async::Client {
        self.context().client.clone()
    }

    fn api_token(&mut self) -> &mut LazyLockedFile {
        &mut self.context_mut().api_token
    }

    fn wait_enter(&mut self, prompt: &str) -> io::Result<()> {
        let Context { stdin, stderr, .. } = self.context_mut();
        stderr.write_str(prompt)?;
        stderr.flush()?;
        stdin.ignore_line()
    }

    fn prompt_reply_stderr(&mut self, prompt: &str) -> io::Result<String> {
        let Context { stdin, stderr, .. } = self.context_mut();
        stderr.write_str(prompt)?;
        stderr.flush()?;
        stdin.read_reply()
    }

    fn prompt_password_stderr(&mut self, prompt: &str) -> io::Result<String> {
        let Context { stdin, stderr, .. } = self.context_mut();
        stderr.write_str(prompt)?;
        stderr.flush()?;
        stdin.read_password()
    }

    fn ask_yn(&mut self, prompt: &str, default: bool) -> io::Result<bool> {
        let Context { stdin, stderr, .. } = self.context_mut();
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

    fn open_in_browser(&mut self, url: impl ParseWithBaseUrl) -> ServiceResult<()> {
        let url = url.parse_with_base_url(self.context().base_url)?;
        write!(self.stderr(), "Opening {} in default browser...", url)?;
        self.stderr().flush()?;
        let status = webbrowser::open(url.as_str())?.status;
        writeln!(self.stderr())?;
        self.stderr().flush()?;
        if status.success() {
            Ok(())
        } else {
            Err(ServiceErrorKind::Webbrowser(status).into())
        }
    }

    /// Whether it has any **unexpired** cookie.
    fn has_cookie(&self) -> bool {
        self.context()
            .cookie_store
            .as_ref()
            .map_or(false, |s| s.store.iter_unexpired().next().is_some())
    }

    fn cookies_to_header_value(&self, url: &Url) -> Option<HeaderValue> {
        self.context()
            .cookie_store
            .as_ref()
            .and_then(|s| s.to_header_value(url))
    }

    fn insert_cookie(&mut self, cookie: &cookie::Cookie, url: &Url) -> ServiceResult<()> {
        if let Some(store) = &mut self.context_mut().cookie_store {
            store.insert_cookie(cookie, url)?;
        }
        Ok(())
    }

    /// Removes all cookies.
    fn clear_cookies(&mut self) -> ServiceResult<()> {
        if let Some(store) = &mut self.context_mut().cookie_store {
            store.clear()?;
        }
        Ok(())
    }

    fn get(&mut self, url: impl ParseWithBaseUrl) -> self::Request<&mut Self, Get> {
        self.request::<Get, _>(url, btreeset![StatusCode::OK])
    }

    fn post(&mut self, url: impl ParseWithBaseUrl) -> self::Request<&mut Self, Post> {
        self.request::<Post, _>(url, btreeset![StatusCode::FOUND])
    }

    fn request<M: StaticMethod, U: ParseWithBaseUrl>(
        &mut self,
        url: U,
        acceptable: BTreeSet<StatusCode>,
    ) -> self::Request<&mut Self, M> {
        self::Request {
            inner: url
                .parse_with_base_url(self.context().base_url)
                .and_then(|url| {
                    self.context_mut()
                        .assert_not_forbidden_by_robots_txt(&url)?;
                    Ok(RequestInner {
                        inner: RequestBuilderBuilder::new::<M>(self.client(), url),
                        eprint: !self.context().http_silent,
                        acceptable,
                        warn: btreeset![],
                        redirect_unlimited: false,
                        retries_on_get: self.context().retries_on_get,
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

    fn retry_login<C: Credentials, F: FnMut(&mut Self, C::Items) -> ServiceResult<bool>>(
        &mut self,
        prompts: C::Prompts,
        success_msg: &'static str,
        fail_msg: &'static str,
        mut f: F,
    ) -> ServiceResult<()> {
        let mut login_retries = self.context().login_retries;
        loop {
            let credentials = C::ask(prompts, &mut *self)?;
            if f(&mut *self, credentials)? {
                writeln!(self.stderr(), "{}", success_msg)?;
                self.stderr().flush()?;
                break Ok(());
            }
            if login_retries == Some(0) {
                break Err(ServiceErrorKind::LoginRetriesExceeded.into());
            }
            login_retries = login_retries.map(|n| n - 1);
            writeln!(self.stderr(), "{}", fail_msg)?;
        }
    }

    fn warn_not_found(
        &mut self,
        problems: &NonEmptyIndexSet<String>,
        found: &IndexSet<String>,
    ) -> io::Result<()> {
        let not_found = problems.difference(found).collect::<IndexSet<_>>();

        if !not_found.is_empty() {
            let stderr = &mut self.context_mut().stderr;
            stderr.set_color(color!(fg(Yellow), intense))?;
            write!(stderr, "Not found: {}", not_found.format_as_str_list())?;
            stderr.reset()?;
            writeln!(stderr)?;
            stderr.flush()?;
        }
        Ok(())
    }
}

pub(crate) struct ContextBuilder<'a, I: Input, E: WriteColor + HasTermProps> {
    pub(crate) stdin: I,
    pub(crate) stderr: E,
    pub(crate) base_url: Option<&'static Url>,
    pub(crate) cookies_path: Option<&'a AbsPath>,
    pub(crate) api_token_path: Option<&'a AbsPath>,
    pub(crate) retries_on_get: u32,
    pub(crate) timeout: Option<Duration>,
    pub(crate) robots: bool,
    pub(crate) http_silent: bool,
    pub(crate) login_retries: Option<u32>,
}

impl<I: Input, E: WriteColor + HasTermProps> ContextBuilder<'_, I, E> {
    pub(crate) fn build(self) -> ServiceResult<Context<I, E>> {
        let Self {
            stdin,
            stderr,
            base_url,
            cookies_path,
            api_token_path,
            retries_on_get,
            robots,
            http_silent,
            timeout,
            login_retries,
        } = self;

        let runtime = tokio01::runtime::Runtime::new()?;
        let client = {
            let mut builder = reqwest::r#async::Client::builder()
                .referer(false)
                .redirect(RedirectPolicy::none()) // Redirects manually
                .cookie_store(false) // Uses `CookieStore` directly
                .default_headers({
                    let mut headers = HeaderMap::new();
                    headers.insert(header::USER_AGENT, USER_AGENT.parse().unwrap());
                    headers
                });
            if let Some(timeout) = timeout {
                builder = builder.timeout(timeout);
            }
            builder.build()?
        };
        let robots = if robots { Some(hashmap!()) } else { None };
        let cookie_store = cookies_path
            .map(AutosavedCookieStore::try_new)
            .transpose()?;
        let api_token = match api_token_path {
            None => LazyLockedFile::Null,
            Some(path) => LazyLockedFile::Uninited(path.to_owned()),
        };

        Ok(Context {
            stdin,
            stderr,
            runtime,
            client,
            robots,
            cookie_store,
            api_token,
            base_url,
            retries_on_get,
            http_silent,
            login_retries,
        })
    }
}

#[derive(Debug)]
pub(crate) struct Context<I: Input, E: WriteColor + HasTermProps> {
    stdin: I,
    stderr: E,
    runtime: Runtime,
    client: reqwest::r#async::Client,
    robots: Option<HashMap<String, OwnedRobots>>,
    cookie_store: Option<AutosavedCookieStore>,
    api_token: LazyLockedFile,
    base_url: Option<&'static Url>,
    retries_on_get: u32,
    http_silent: bool,
    login_retries: Option<u32>,
}

impl<I: Input, E: WriteColor + HasTermProps> Context<I, E> {
    pub(super) fn get_robots_txt(&mut self) -> ServiceResult<()> {
        if_chain! {
            if self.robots.is_some();
            if let Some(host) = self.base_url.and_then(Url::host_str);
            let res = self
                .get("/robots.txt")
                .acceptable(&[200, 404])
                .redirect_unlimited()
                .retry_send()?;
            if res.status() == 200;
            then {
                let robots = OwnedRobots::new(
                    self.retry_recv_text(res)?,
                    |s| Robots::from_str(s),
                    |robots| SimpleMatcher::new(&robots.choose_section(USER_AGENT).rules),
                );
                self.robots.as_mut().unwrap().insert(host.to_string(), robots);
            }
        }
        Ok(())
    }

    fn assert_not_forbidden_by_robots_txt(&self, url: &Url) -> ServiceResult<()> {
        if_chain! {
            if let Some(host) = url.host_str();
            if let Some(robots) = &self.robots;
            if let Some(robots) = robots.get(host);
            if !robots.rent_matcher(|m| m.check_path(url.path()));
            then {
                Err(ServiceErrorKind::ForbiddenByRobotsTxt.into())
            } else {
                Ok(())
            }
        }
    }
}

impl<'a, S: HasContextMut> HasContextMut for &'a mut S {
    type Stdin = S::Stdin;
    type Stderr = S::Stderr;

    fn context(&self) -> &Context<S::Stdin, S::Stderr> {
        (**self).context()
    }

    fn context_mut(&mut self) -> &mut Context<S::Stdin, S::Stderr> {
        (**self).context_mut()
    }
}

impl<I: Input, E: WriteColor + HasTermProps> HasContextMut for Context<I, E> {
    type Stdin = I;
    type Stderr = E;

    fn context(&self) -> &Self {
        self
    }

    fn context_mut(&mut self) -> &mut Self {
        self
    }
}

#[derive(Debug)]
pub(crate) struct ContextStartArgs<'a, I: Input, E: WriteColor + HasTermProps> {
    pub(super) stdin: I,
    pub(super) stderr: E,
    pub(super) runtime: Runtime,
    pub(super) robots: bool,
    pub(super) client: reqwest::r#async::Client,
    pub(super) base_url: Option<&'static Url>,
    pub(super) cookies_path: Option<&'a AbsPath>,
    pub(super) api_token_path: Option<&'a AbsPath>,
    pub(super) retries_on_get: u32,
    pub(super) http_silent: bool,
    pub(super) login_retries: Option<u32>,
}

#[derive(Debug)]
pub(super) struct Request<S: HasContextMut, M: StaticMethod> {
    inner: ServiceResult<RequestInner>,
    session: S,
    phantom: PhantomData<fn() -> M>,
}

impl<S: HasContextMut, M: StaticMethod> Request<S, M> {
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

    pub(super) fn push_path_segment(mut self, segment: &str) -> Self {
        if let Ok(inner) = &mut self.inner {
            if let Ok(mut segments) = inner.inner.url.path_segments_mut() {
                segments.push(segment);
            }
        }
        self
    }

    pub(super) fn extend_path_segments<I: IntoIterator>(mut self, segments: I) -> Self
    where
        I::Item: AsRef<str>,
    {
        if let Ok(inner) = &mut self.inner {
            if let Ok(mut segments_mut) = inner.inner.url.path_segments_mut() {
                segments_mut.extend(segments);
            }
        }
        self
    }

    pub(super) fn push_url_query(mut self, key: &str, value: &str) -> Self {
        if let Ok(inner) = &mut self.inner {
            inner.inner.url.query_pairs_mut().append_pair(key, value);
        }
        self
    }

    /// # Panics:
    ///
    /// Panics if:
    /// - `statuses` contains `n` such that `n < 100 || 600 <= n`
    /// - `this.inner.warn` is not a subset of `statused` where `Ok(this) = self`
    /// - `this.inner.redirect_unlimited` and `statuses` contains any 3xx status code where `Ok(this) = self`
    pub(super) fn acceptable(mut self, statuses: &'static [u16]) -> Self {
        if let Ok(inner) = &mut self.inner {
            inner.acceptable = statuses
                .iter()
                .map(|&n| StatusCode::from_u16(n))
                .collect::<std::result::Result<_, _>>()
                .unwrap();
            inner.assert_constraints();
        }
        self
    }

    /// # Panics:
    ///
    /// Panics if `statuses` is not a subset of `acceptable`.
    pub(super) fn warn(mut self, statuses: &'static [u16]) -> Self {
        if let Ok(inner) = &mut self.inner {
            inner.warn = statuses
                .iter()
                .map(|&n| StatusCode::from_u16(n))
                .collect::<std::result::Result<_, _>>()
                .unwrap();
            inner.assert_constraints();
        }
        self
    }

    /// # Panics:
    ///
    /// Panics if `this.inner.acceptable` contains any 3xx status code where `Ok(this) = self`.
    pub(super) fn redirect_unlimited(mut self) -> Self {
        if let Ok(inner) = &mut self.inner {
            inner.redirect_unlimited = true;
            inner.assert_constraints();
        }
        self
    }

    fn send_internal(mut self, allow_retry: bool) -> ServiceResult<(S, self::Response<M>)> {
        if_chain! {
            if let Ok(inner) = &mut self.inner;
            if let Some(store) = &self.session.context().cookie_store;
            if let Some(value) = store.to_header_value(&inner.inner.url);
            then {
                inner.inner.headers.insert(header::COOKIE, value);
            }
        }

        let RequestInner {
            mut inner,
            eprint,
            acceptable,
            warn,
            redirect_unlimited,
            retries_on_get,
        } = self.inner?;

        let mut retries_on_get = if allow_retry { retries_on_get } else { 0 };

        let st = self.session.context_mut();
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
                if warn.contains(&res.status()) {
                    stderr.set_color(color!(fg(Yellow), intense, bold))?;
                } else if is_acceptable {
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
                    warn,
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

            let err = ServiceErrorKind::UnexpectedStatusCode(res.url2(), res.status(), acceptable);

            break Err(if let Some(retry_after) = retry_after {
                let msg = format!("The response contains `Retry-After: {}`", retry_after);
                failure::err_msg(msg).context(err).into()
            } else {
                err.into()
            });
        }
    }
}

impl<S: HasContextMut> Request<S, Get> {
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

impl<S: HasContextMut> Request<S, Post> {
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
    acceptable: BTreeSet<StatusCode>,
    warn: BTreeSet<StatusCode>,
    redirect_unlimited: bool,
    retries_on_get: u32,
}

impl RequestInner {
    /// # Panics
    ///
    /// Panics if:
    /// - `warn` is not a subset of `acceptable`
    /// - `redirect_unlimited` and `acceptable` contains any 3xx status code
    fn assert_constraints(&self) {
        if !self.warn.is_subset(&self.acceptable) {
            panic!("`warn` is not a subset of `acceptable`");
        }
        if self.redirect_unlimited && self.acceptable.iter().any(|s| s.is_redirection()) {
            panic!("`redirect_unlimited` and `acceptable` contains 3xx status codes");
        }
    }
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
            .request(self.method.clone(), self.url.as_str())
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
                    Ok(Async::Ready(mem::take(&mut self.buf)))
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

pub(super) trait ParseWithBaseUrl {
    fn parse_with_base_url(self, base: Option<&Url>) -> ServiceResult<Url>;
}

impl ParseWithBaseUrl for Url {
    fn parse_with_base_url(self, _: Option<&Url>) -> ServiceResult<Url> {
        Ok(self)
    }
}

impl<'a> ParseWithBaseUrl for &'a Url {
    fn parse_with_base_url(self, _: Option<&Url>) -> ServiceResult<Url> {
        Ok(self.clone())
    }
}

impl ParseWithBaseUrl for Uri {
    fn parse_with_base_url(self, base: Option<&Url>) -> ServiceResult<Url> {
        self.to_string().parse_with_base_url(base)
    }
}

impl<'a> ParseWithBaseUrl for &'a Uri {
    fn parse_with_base_url(self, base: Option<&Url>) -> ServiceResult<Url> {
        self.to_string().parse_with_base_url(base)
    }
}

impl ParseWithBaseUrl for String {
    fn parse_with_base_url(self, base: Option<&Url>) -> ServiceResult<Url> {
        (*self).parse_with_base_url(base)
    }
}

impl<'a> ParseWithBaseUrl for &'a String {
    fn parse_with_base_url(self, base: Option<&Url>) -> ServiceResult<Url> {
        (**self).parse_with_base_url(base)
    }
}

impl<'a> ParseWithBaseUrl for &'a str {
    fn parse_with_base_url(self, base: Option<&Url>) -> ServiceResult<Url> {
        self.parse()
            .or_else(|_| Url::options().base_url(base).parse(self))
            .with_context(|_| ServiceErrorKind::ParseUrl(self.to_owned()))
            .map_err(Into::into)
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
                .map_err(failure::err_msg)
                .with_context(|_| FileErrorKind::Read(path.to_owned()))
                .map_err(FileError::from)?
        };

        Ok(Self { file, store })
    }

    fn to_header_value(&self, url: &Url) -> Option<HeaderValue> {
        let header = self
            .store
            .get_request_cookies(url)
            .map(|c| format!("{}={}", c.name(), c.value()))
            .join("; ");

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
                inserted |= self.store.parse(cookie, &response.url2()).is_ok();
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

pub(super) trait Credentials: Sized {
    type Prompts: Copy;
    type Items;

    fn ask(prompts: Self::Prompts, sess: impl HasContextMut) -> io::Result<Self::Items>;
}

pub(super) enum Password {}

impl Credentials for Password {
    type Prompts = (&'static str,);
    type Items = (String,);

    fn ask((prompt,): (&'static str,), mut sess: impl HasContextMut) -> io::Result<(String,)> {
        let password = sess.prompt_password_stderr(prompt)?;
        Ok((password,))
    }
}

pub(super) enum UsernameAndPassword {}

impl Credentials for UsernameAndPassword {
    type Prompts = (&'static str, &'static str);
    type Items = (String, String);

    fn ask(
        (username_prompt, password_prompt): (&'static str, &'static str),
        mut sess: impl HasContextMut,
    ) -> io::Result<(String, String)> {
        let username = sess.prompt_reply_stderr(username_prompt)?;
        let password = sess.prompt_password_stderr(password_prompt)?;
        Ok((username, password))
    }
}

mod owned_robots {
    use robots_txt::{Robots, SimpleMatcher};
    use stable_deref_trait::StableDeref;
    use static_assertions::assert_impl_all;

    use std::mem;

    assert_impl_all!(String: StableDeref<Target = str>);

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
    use crate::service::context::{Context, HasContextMut};
    use crate::terminal::{AnsiWithProps, Dumb, TtyOrPiped};

    use failure::Fallible;
    use http::{StatusCode, Uri};
    use if_chain::if_chain;
    use maplit::btreeset;
    use once_cell::sync::Lazy;
    use pretty_assertions::assert_eq;
    use tempdir::TempDir;
    use termcolor::{Ansi, Color, ColorSpec, WriteColor};
    use url::Url;
    use warp::Filter;

    use std::convert::TryFrom as _;
    use std::io::{self, Empty, Write as _};
    use std::{env, panic, str};

    #[tokio::test(threaded_scheduler)]
    async fn it_works() -> Fallible<()> {
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

        let (tx, rx) = tokio::sync::oneshot::channel();

        let (_, server) = warp::serve(index.or(confirm_cookie).or(robots_txt).or(redirect))
            .bind_with_graceful_shutdown(([127, 0, 0, 1], LOCALHOST_PORT), async {
                rx.await.unwrap();
            });
        let handle = tokio::task::spawn(server);

        let tempdir = dunce::canonicalize(&env::temp_dir())?;
        let tempdir = TempDir::new_in(&tempdir, "it_keeps_a_file_locked_while_alive")?;

        let result = panic::catch_unwind::<_, Fallible<()>>(|| {
            static BASE_URL: Lazy<Url> = Lazy::new(|| {
                format!("http://127.0.0.1:{}", LOCALHOST_PORT)
                    .parse()
                    .unwrap()
            });

            let mut stderr = AnsiWithProps::new();

            let cookies = AbsPathBuf::try_new(tempdir.path().join("cookies.json")).unwrap();
            let mut sess = service::ContextBuilder {
                stdin: TtyOrPiped::Piped(io::empty()),
                stderr: &mut stderr,
                base_url: Some(&BASE_URL),
                cookies_path: Some(&cookies),
                api_token_path: None,
                timeout: None,
                http_silent: false,
                robots: true,
                retries_on_get: 1,
                login_retries: None,
            }
            .build()?;

            sess.get_robots_txt()?;
            sess.get("/").retry_send()?;
            sess.get("/confirm-cookie").retry_send()?;
            sess.get("/redirect").redirect_unlimited().retry_send()?;
            sess.get("/nonexisting").acceptable(&[404]).retry_send()?;
            if_chain! {
                let err = sess.get("/nonexisting").retry_send().unwrap_err();
                if let ServiceError::Context(ctx) = &err;
                if let ServiceErrorKind::UnexpectedStatusCode(
                    url,
                    http01::StatusCode::NOT_FOUND,
                    expected,
                ) = ctx.get_context();
                if url.as_str() == format!("http://127.0.0.1:{}/nonexisting", LOCALHOST_PORT);
                if *expected == btreeset![http01::StatusCode::OK];
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

        tx.send(()).unwrap();
        handle.await?;

        tempdir.close()?;
        result.unwrap_or_else(|p| panic::resume_unwind(p))
    }

    #[test]
    fn it_keeps_a_file_locked_while_alive() -> Fallible<()> {
        fn construct_ctx(path: &AbsPath) -> ServiceResult<Context<TtyOrPiped<Empty>, Dumb>> {
            service::ContextBuilder {
                stdin: TtyOrPiped::Piped(io::empty()),
                stderr: Dumb::new(),
                base_url: None,
                cookies_path: Some(path),
                api_token_path: None,
                timeout: None,
                http_silent: true,
                robots: false,
                retries_on_get: 0,
                login_retries: None,
            }
            .build()
        }

        let tempdir = dunce::canonicalize(&env::temp_dir())?;
        let tempdir = TempDir::new_in(&tempdir, "it_keeps_a_file_locked_while_alive")?;
        let path = AbsPathBuf::try_new(tempdir.path().join("cookies")).unwrap();
        construct_ctx(&path)?;
        construct_ctx(&path)?;
        let _ctx = construct_ctx(&path)?;
        if_chain! {
            let err = construct_ctx(&path).unwrap_err();
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

        let mut ctx = Context {
            stdin: TtyOrPiped::Piped(&mut rdr),
            stderr: &mut stderr,
            runtime: tokio01::runtime::Runtime::new()?,
            client: reqwest::r#async::Client::builder().build()?,
            robots: None,
            cookie_store: None,
            api_token: LazyLockedFile::Null,
            base_url: None,
            retries_on_get: 0,
            http_silent: true,
            login_retries: None,
        };

        assert_eq!(ctx.ask_yn("Yes?: ", true)?, true);
        assert_eq!(ctx.ask_yn("Yes?: ", true)?, false);
        assert_eq!(ctx.ask_yn("Yes?: ", true)?, true);
        assert_eq!(ctx.ask_yn("No?: ", false)?, true);
        assert_eq!(ctx.ask_yn("No?: ", false)?, false);
        assert_eq!(ctx.ask_yn("No?: ", false)?, false);

        let err = ctx.ask_yn("Yes?: ", true).unwrap_err();
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
