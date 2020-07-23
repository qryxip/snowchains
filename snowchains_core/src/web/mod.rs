macro_rules! lazy_url {
    ($url:literal $(,)?) => {
        ::once_cell::sync::Lazy::new(|| $url.parse().unwrap());
    };
}

macro_rules! static_url {
    ($url:literal $(,)?) => {{
        static URL: ::once_cell::sync::Lazy<::url::Url> = lazy_url!($url);
        &*URL
    }};
}

macro_rules! lazy_selector {
    ($selectors:literal $(,)?) => {
        ::once_cell::sync::Lazy::new(|| ::scraper::Selector::parse($selectors).unwrap())
    };
}

macro_rules! static_selector {
    ($selectors:literal $(,)?) => {{
        static SELECTOR: ::once_cell::sync::Lazy<::scraper::Selector> = lazy_selector!($selectors);
        &*SELECTOR
    }};
}

macro_rules! lazy_regex {
    ($regex:literal $(,)?) => {
        ::once_cell::sync::Lazy::new(|| ::regex::Regex::new($regex).unwrap());
    };
}

macro_rules! static_regex {
    ($regex:literal $(,)?) => {{
        static REGEX: ::once_cell::sync::Lazy<::regex::Regex> = lazy_regex!($regex);
        &*REGEX
    }};
}

macro_rules! url {
    ($fmt:literal $(, $expr:expr)* $(,)*) => {
        // `self::url_from_rel` is defined in each module.

        self::url_from_rel(format!(
            $fmt,
            $(
                ::percent_encoding::utf8_percent_encode(
                    &$expr.to_string(),
                    ::percent_encoding::NON_ALPHANUMERIC
                ),
            )*
        )).unwrap()
    };
}

mod atcoder;
mod codeforces;
mod yukicoder;

pub use crate::web::{
    atcoder::{
        Atcoder, AtcoderLoginCredentials, AtcoderParticipateCredentials, AtcoderParticipateTarget,
        AtcoderRetrieveFullTestCasesCredentials, AtcoderRetrieveLanguagesCredentials,
        AtcoderRetrieveLanguagesTarget, AtcoderRetrieveSampleTestCasesCredentials,
        AtcoderRetrieveTestCasesTargets, AtcoderSubmitCredentials, AtcoderSubmitTarget,
    },
    codeforces::{
        Codeforces, CodeforcesLoginCredentials, CodeforcesParticipateCredentials,
        CodeforcesParticipateTarget, CodeforcesRetrieveLanguagesCredentials,
        CodeforcesRetrieveLanguagesTarget, CodeforcesRetrieveSampleTestCasesCredentials,
        CodeforcesRetrieveTestCasesTargets, CodeforcesSubmitCredentials, CodeforcesSubmitTarget,
    },
    yukicoder::{
        Yukicoder, YukicoderRetrieveFullTestCasesCredentials, YukicoderRetrieveTestCasesTargets,
        YukicoderSubmitCredentials, YukicoderSubmitTarget,
    },
};

use crate::testsuite::TestSuite;
use anyhow::{anyhow, bail, Context as _};
use cookie_store::CookieStore;
use derivative::Derivative;
use derive_more::{Display, From};
use easy_ext::ext;
use futures_util::StreamExt as _;
use indexmap::IndexMap;
use indicatif::{MultiProgress, ProgressBar, ProgressDrawTarget, ProgressStyle};
use itertools::Itertools as _;
use reqwest::{header, redirect::Policy, Method, StatusCode};
use scraper::Html;
use serde::{Deserialize, Serialize, Serializer};
use std::{
    any,
    borrow::Borrow,
    convert::TryInto,
    fmt,
    hash::Hash,
    io::Write as _,
    marker::PhantomData,
    ops::{Deref, RangeFull, RangeInclusive},
    str,
    time::Duration,
};
use strum::EnumString;
use termcolor::{BufferedStandardStream, Color, ColorChoice, ColorSpec, WriteColor as _};
use tokio::runtime::Runtime;
use unicode_width::UnicodeWidthStr as _;
use url::Url;

pub trait Platform: Sized {
    type Cookies;
    type LoginCredentials;
    type ParticipateTarget;
    type ParticipateCredentials;
    type RetrieveLanguagesTarget;
    type RetrieveLanguagesCredentials;
    type RetrieveTestCasesTargets;
    type RetrieveTestCasesCredentials;
    type RetrieveFullTestCasesCredentials;
    type SubmitTarget;
    type SubmitCredentials;

    const VARIANT: PlatformVariant;
}

#[derive(
    EnumString,
    Debug,
    strum::Display,
    Copy,
    Clone,
    Ord,
    PartialOrd,
    Eq,
    PartialEq,
    Hash,
    Deserialize,
    Serialize,
)]
#[strum(serialize_all = "lowercase")]
#[serde(rename_all = "PascalCase")]
pub enum PlatformVariant {
    Atcoder,
    Codeforces,
    Yukicoder,
}

impl PlatformVariant {
    pub const KEBAB_CASE_VARIANTS: &'static [&'static str] =
        &["atcoder", "codeforces", "yukicoder"];

    pub fn to_kebab_case_str(self) -> &'static str {
        match self {
            Self::Atcoder => "atcoder",
            Self::Codeforces => "codeforces",
            Self::Yukicoder => "yukicoder",
        }
    }

    pub fn to_pascal_case_str(self) -> &'static str {
        match self {
            Self::Atcoder => "Atcoder",
            Self::Codeforces => "Codeforces",
            Self::Yukicoder => "Yukicoder",
        }
    }
}

pub trait Exec<A>: Platform {
    type Output;
    fn exec(args: A) -> anyhow::Result<Self::Output>;
}

pub struct Login<P: Platform, S: Shell> {
    pub timeout: Option<Duration>,
    pub cookies: P::Cookies,
    pub shell: S,
    pub credentials: P::LoginCredentials,
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Deserialize, Serialize)]
pub enum LoginOutcome {
    Success,
    AlreadyLoggedIn,
}

impl LoginOutcome {
    pub fn to_json(self) -> String {
        serde_json::to_string(&self).expect("should not fail")
    }
}

pub struct Participate<P: Platform, S: Shell> {
    pub target: P::ParticipateTarget,
    pub timeout: Option<Duration>,
    pub cookies: P::Cookies,
    pub shell: S,
    pub credentials: P::ParticipateCredentials,
}

#[derive(Debug, From, Serialize)]
pub enum ParticipateOutcome {
    Success,
    AlreadyParticipated,
    ContestIsFinished,
}

#[derive(Debug)]
pub struct RetrieveLanguages<P: Platform, S: Shell> {
    pub target: P::RetrieveLanguagesTarget,
    pub timeout: Option<Duration>,
    pub cookies: P::Cookies,
    pub shell: S,
    pub credentials: P::RetrieveLanguagesCredentials,
}

#[derive(Debug)]
pub struct RetrieveLanguagesOutcome {
    pub names_by_id: IndexMap<String, String>,
}

pub struct RetrieveTestCases<P: Platform, S: Shell> {
    pub targets: P::RetrieveTestCasesTargets,
    pub timeout: Option<Duration>,
    pub cookies: P::Cookies,
    pub shell: S,
    pub credentials: P::RetrieveTestCasesCredentials,
    pub full: Option<RetrieveFullTestCases<P>>,
}

pub struct RetrieveFullTestCases<P: Platform> {
    pub credentials: P::RetrieveFullTestCasesCredentials,
}

#[derive(Debug)]
pub struct RetrieveTestCasesOutcome {
    pub contest: Option<RetrieveTestCasesOutcomeContest>,
    pub problems: Vec<RetrieveTestCasesOutcomeProblem>,
}

#[derive(Debug)]
pub struct RetrieveTestCasesOutcomeContest {
    pub id: String,
    pub submissions_url: Url,
}

#[derive(Debug, Serialize)]
pub struct RetrieveTestCasesOutcomeProblem {
    pub slug: String,
    pub url: Url,
    pub screen_name: String,
    pub display_name: String,
    pub test_suite: TestSuite,
    pub text_files: IndexMap<String, RetrieveTestCasesOutcomeProblemTextFiles>,
}

#[derive(Debug, Serialize)]
pub struct RetrieveTestCasesOutcomeProblemTextFiles {
    pub r#in: String,
    pub out: Option<String>,
}

#[derive(Debug)]
pub struct Submit<P: Platform, S: Shell> {
    pub target: P::SubmitTarget,
    pub language_id: String,
    pub code: String,
    pub watch_submission: bool,
    pub timeout: Option<Duration>,
    pub cookies: P::Cookies,
    pub shell: S,
    pub credentials: P::SubmitCredentials,
}

#[derive(Debug)]
pub struct SubmitOutcome {
    problem_screen_name: String,
    submission_url: Url,
    submissions_url: Url,
}

pub struct Cookies<'closures> {
    pub cookie_store: CookieStore,
    pub on_update_cookie_store: &'closures mut dyn FnMut(&CookieStore) -> anyhow::Result<()>,
}

pub trait Shell {
    fn progress_draw_target(&self) -> ProgressDrawTarget {
        ProgressDrawTarget::hidden()
    }

    fn info<T: fmt::Display>(&mut self, _: T) -> anyhow::Result<()> {
        Ok(())
    }

    fn warn<T: fmt::Display>(&mut self, _: T) -> anyhow::Result<()> {
        Ok(())
    }

    fn on_request(&mut self, _: &reqwest::blocking::Request) -> anyhow::Result<()> {
        Ok(())
    }

    fn on_response(
        &mut self,
        _: &reqwest::blocking::Response,
        _: StatusCodeColor,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SinkShell;

impl Shell for SinkShell {}

pub struct StandardStreamShell {
    wtr: BufferedStandardStream,
}

impl StandardStreamShell {
    pub fn new(color_choice: ColorChoice) -> Self {
        Self {
            wtr: BufferedStandardStream::stderr(color_choice),
        }
    }
}

impl fmt::Debug for StandardStreamShell {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("StandardStreamShell")
            .field("wtr", &format_args!("_"))
            .finish()
    }
}

impl Shell for StandardStreamShell {
    fn progress_draw_target(&self) -> ProgressDrawTarget {
        if self.wtr.supports_color() {
            ProgressDrawTarget::stderr()
        } else {
            ProgressDrawTarget::hidden()
        }
    }

    fn info<T: fmt::Display>(&mut self, message: T) -> anyhow::Result<()> {
        self.wtr.set_color(
            ColorSpec::new()
                .set_reset(false)
                .set_bold(true)
                .set_fg(Some(Color::Cyan)),
        )?;
        write!(self.wtr, "info:")?;
        self.wtr.reset()?;
        writeln!(self.wtr, " {}", message)?;
        self.wtr.flush().map_err(Into::into)
    }

    fn warn<T: fmt::Display>(&mut self, message: T) -> anyhow::Result<()> {
        self.wtr.set_color(
            ColorSpec::new()
                .set_reset(false)
                .set_bold(true)
                .set_fg(Some(Color::Yellow)),
        )?;
        write!(self.wtr, "warning:")?;
        self.wtr.reset()?;
        writeln!(self.wtr, " {}", message)?;
        self.wtr.flush().map_err(Into::into)
    }

    fn on_request(&mut self, req: &reqwest::blocking::Request) -> anyhow::Result<()> {
        self.wtr
            .set_color(ColorSpec::new().set_reset(false).set_bold(true))?;
        write!(self.wtr, "{}", req.method())?;
        self.wtr.reset()?;

        write!(self.wtr, " ")?;

        self.wtr
            .set_color(ColorSpec::new().set_reset(false).set_fg(Some(Color::Cyan)))?;
        write!(self.wtr, "{}", req.url())?;
        self.wtr.reset()?;

        write!(self.wtr, " ... ")?;

        self.wtr.flush().map_err(Into::into)
    }

    fn on_response(
        &mut self,
        res: &reqwest::blocking::Response,
        status_code_color: StatusCodeColor,
    ) -> anyhow::Result<()> {
        let fg = match status_code_color {
            StatusCodeColor::Ok => Some(Color::Green),
            StatusCodeColor::Warn => Some(Color::Yellow),
            StatusCodeColor::Error => Some(Color::Red),
            StatusCodeColor::Unknown => None,
        };

        self.wtr
            .set_color(ColorSpec::new().set_reset(false).set_bold(true).set_fg(fg))?;
        write!(self.wtr, "{}", res.status())?;
        self.wtr.reset()?;
        writeln!(self.wtr)?;
        self.wtr.flush().map_err(Into::into)
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum StatusCodeColor {
    Ok,
    Warn,
    Error,
    Unknown,
}

struct Session<S, U> {
    async_client: reqwest::Client,
    blocking_client: reqwest::blocking::Client,
    cookie_store: Option<CookieStore>,
    on_update_cookie_store: U,
    shell: S,
}

struct SessionBuilder<S, U> {
    timeout: Option<Duration>,
    cookie_store: Option<CookieStore>,
    on_update_cookie_store: U,
    shell: S,
}

impl SessionBuilder<SinkShell, fn(&CookieStore) -> anyhow::Result<()>> {
    fn new() -> Self {
        Self {
            timeout: None,
            cookie_store: None,
            on_update_cookie_store: |_| Ok(()),
            shell: SinkShell,
        }
    }
}

impl<S: Shell, U: FnMut(&CookieStore) -> anyhow::Result<()>> SessionBuilder<S, U> {
    fn timeout(self, timeout: Option<Duration>) -> Self {
        Self { timeout, ..self }
    }

    fn cookie_store(self, cookie_store: Option<CookieStore>) -> Self {
        Self {
            cookie_store,
            ..self
        }
    }

    fn on_update_cookie_store<U2: FnMut(&CookieStore) -> anyhow::Result<()>>(
        self,
        on_update_cookie_store: U2,
    ) -> SessionBuilder<S, U2> {
        SessionBuilder {
            timeout: self.timeout,
            cookie_store: self.cookie_store,
            on_update_cookie_store,
            shell: self.shell,
        }
    }

    fn shell<S2: Shell>(self, shell: S2) -> SessionBuilder<S2, U> {
        SessionBuilder {
            timeout: self.timeout,
            cookie_store: self.cookie_store,
            on_update_cookie_store: self.on_update_cookie_store,
            shell,
        }
    }

    fn build(self) -> reqwest::Result<Session<S, U>> {
        let Self {
            timeout,
            cookie_store,
            on_update_cookie_store,
            shell,
        } = self;

        macro_rules! client(($builder:path) => {{
            let client = $builder()
                .user_agent(USER_AGENT)
                .cookie_store(false)
                .redirect(Policy::none());

            if let Some(timeout) = timeout {
                client.timeout(timeout).build()
            } else {
                client.build()
            }
        }});

        let async_client = client!(reqwest::ClientBuilder::new)?;
        let blocking_client = client!(reqwest::blocking::ClientBuilder::new)?;

        return Ok(Session {
            async_client,
            blocking_client,
            cookie_store,
            on_update_cookie_store,
            shell,
        });

        static USER_AGENT: &str = "snowchains <https://github.com/qryxip/snowchains>";

        trait DummyMethod: Sized {
            fn cookie_store(self, _: bool) -> Self {
                self
            }
        }

        impl DummyMethod for reqwest::ClientBuilder {}
        impl DummyMethod for reqwest::blocking::ClientBuilder {}
    }
}

trait SessionMut: Sized {
    type Shell: Shell;
    type OnUpdateCookieStore: FnMut(&CookieStore) -> anyhow::Result<()>;

    fn async_client(&self) -> &reqwest::Client;

    fn shell(&mut self) -> &mut Self::Shell;

    fn cookie_store(&self) -> Option<&CookieStore>;

    fn request(
        &mut self,
        method: Method,
        url: Url,
    ) -> SessionRequestBuilder<'_, Self::Shell, Self::OnUpdateCookieStore>;

    fn cookie_header(&self, url: &Url) -> String {
        self.cookie_store()
            .into_iter()
            .flat_map(|c| c.get_request_cookies(url))
            .join("; ")
    }

    fn get(
        &mut self,
        url: Url,
    ) -> SessionRequestBuilder<'_, Self::Shell, Self::OnUpdateCookieStore> {
        self.request(Method::GET, url)
    }

    fn post(
        &mut self,
        url: Url,
    ) -> SessionRequestBuilder<'_, Self::Shell, Self::OnUpdateCookieStore> {
        self.request(Method::POST, url)
    }
}

impl<S: Shell, U: FnMut(&CookieStore) -> anyhow::Result<()>> SessionMut for Session<S, U> {
    type Shell = S;
    type OnUpdateCookieStore = U;

    fn async_client(&self) -> &reqwest::Client {
        &self.async_client
    }

    fn shell(&mut self) -> &mut S {
        &mut self.shell
    }

    fn cookie_store(&self) -> Option<&CookieStore> {
        self.cookie_store.as_ref()
    }

    fn request(&mut self, method: Method, url: Url) -> SessionRequestBuilder<'_, S, U> {
        SessionRequestBuilder {
            inner: self.blocking_client.request(method, url.clone()),
            url,
            redirects: 0,
            colorize_status_code: Box::new(|_| StatusCodeColor::Unknown),
            sess: self,
        }
    }
}

impl<S: SessionMut> SessionMut for &'_ mut S {
    type Shell = S::Shell;
    type OnUpdateCookieStore = S::OnUpdateCookieStore;

    fn async_client(&self) -> &reqwest::Client {
        (**self).async_client()
    }

    fn shell(&mut self) -> &mut S::Shell {
        (**self).shell()
    }

    fn cookie_store(&self) -> Option<&CookieStore> {
        (**self).cookie_store()
    }

    fn request(
        &mut self,
        method: Method,
        url: Url,
    ) -> SessionRequestBuilder<'_, S::Shell, S::OnUpdateCookieStore> {
        (**self).request(method, url)
    }
}

struct SessionRequestBuilder<'a, S, U> {
    inner: reqwest::blocking::RequestBuilder,
    url: Url,
    redirects: usize,
    colorize_status_code: Box<dyn FnOnce(StatusCode) -> StatusCodeColor>,
    sess: &'a mut Session<S, U>,
}

impl<S: Shell, U: FnMut(&CookieStore) -> anyhow::Result<()>> SessionRequestBuilder<'_, S, U> {
    fn bearer_auth<T>(self, token: T) -> Self
    where
        T: fmt::Display,
    {
        Self {
            inner: self.inner.bearer_auth(token),
            ..self
        }
    }

    fn form<T: Serialize + ?Sized>(self, form: &T) -> Self {
        Self {
            inner: self.inner.form(form),
            ..self
        }
    }

    fn json<T: Serialize + ?Sized>(self, json: &T) -> Self {
        Self {
            inner: self.inner.json(json),
            ..self
        }
    }

    fn colorize_status_code(
        self,
        ok: impl StatusCodeRange,
        warn: impl StatusCodeRange,
        error: impl StatusCodeRange,
    ) -> Self {
        Self {
            colorize_status_code: Box::new(move |status| -> _ {
                if ok.contains(status) {
                    StatusCodeColor::Ok
                } else if warn.contains(status) {
                    StatusCodeColor::Warn
                } else if error.contains(status) {
                    StatusCodeColor::Error
                } else {
                    StatusCodeColor::Unknown
                }
            }),
            ..self
        }
    }

    fn send(self) -> anyhow::Result<reqwest::blocking::Response> {
        let Self {
            mut inner,
            url,
            redirects,
            colorize_status_code,
            sess,
        } = self;

        if redirects > 0 {
            todo!();
        }

        let cookie_header = sess.cookie_header(&url);
        if !cookie_header.is_empty() {
            inner = inner.header(header::COOKIE, cookie_header);
        }

        let req = inner.build()?;
        sess.shell.on_request(&req)?;

        let res = sess.blocking_client.execute(req)?;
        sess.shell
            .on_response(&res, colorize_status_code(res.status()))?;

        if let Some(cookie_store) = &mut sess.cookie_store {
            for set_cookie in res.headers().get_all(header::SET_COOKIE) {
                let set_cookie = str::from_utf8(set_cookie.as_bytes())
                    .map_err(|e| anyhow!("{}: {}", e, &url))?;
                let cookie = cookie_store::Cookie::parse(set_cookie, &url)?.into_owned();
                cookie_store.insert(cookie, &url)?;
            }

            if res.headers().contains_key(header::SET_COOKIE) {
                (sess.on_update_cookie_store)(cookie_store)?;
            }
        }

        Ok(res)
    }
}

trait StatusCodeRange: 'static {
    fn contains(&self, status: StatusCode) -> bool;
}

impl StatusCodeRange for () {
    fn contains(&self, _: StatusCode) -> bool {
        false
    }
}

impl StatusCodeRange for RangeInclusive<u16> {
    fn contains(&self, status: StatusCode) -> bool {
        self.contains(&status.as_u16())
    }
}

impl StatusCodeRange for RangeFull {
    fn contains(&self, _: StatusCode) -> bool {
        true
    }
}

impl StatusCodeRange for &'static [u16; 1] {
    fn contains(&self, status: StatusCode) -> bool {
        <[_]>::contains(*self, &status.as_u16())
    }
}

impl StatusCodeRange for &'static [u16; 2] {
    fn contains(&self, status: StatusCode) -> bool {
        <[_]>::contains(*self, &status.as_u16())
    }
}

#[derive(Derivative, Display)]
#[derivative(Default, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
#[display(fmt = "{}", inner)]
struct CaseConverted<C> {
    inner: String,
    phantom: PhantomData<fn() -> C>,
}

impl<C: CaseConvertion> CaseConverted<C> {
    pub fn new<S: AsRef<str>>(s: S) -> Self {
        Self {
            inner: C::CONVERT(s.as_ref()),
            phantom: PhantomData,
        }
    }
}

impl<C> fmt::Debug for CaseConverted<C> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple(&format!(
            "CaseConverted::<{}>",
            any::type_name::<C>().split("::").last().unwrap_or_default(),
        ))
        .field(&self.inner)
        .finish()
    }
}

impl<C> Deref for CaseConverted<C> {
    type Target = str;

    fn deref(&self) -> &str {
        &self.inner
    }
}

impl<C> Borrow<str> for CaseConverted<C> {
    fn borrow(&self) -> &str {
        &self.inner
    }
}

impl<C> From<CaseConverted<C>> for String {
    fn from(from: CaseConverted<C>) -> String {
        from.inner
    }
}

impl<C> Serialize for CaseConverted<C> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.inner.serialize(serializer)
    }
}

trait CaseConvertion: fmt::Debug {
    const CONVERT: fn(&str) -> String;
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
enum LowerCase {}

impl CaseConvertion for LowerCase {
    const CONVERT: fn(&str) -> String = str::to_lowercase;
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
enum UpperCase {}

impl CaseConvertion for UpperCase {
    const CONVERT: fn(&str) -> String = str::to_uppercase;
}

#[ext(ResponseExt)]
impl reqwest::blocking::Response
where
    Self: Sized,
{
    fn location(&self) -> anyhow::Result<&str> {
        self.headers()
            .get(header::LOCATION)
            .with_context(|| "Missing `Location` header")?
            .to_str()
            .with_context(|| "Invalid `Location` header")
    }

    fn html(self) -> reqwest::Result<Html> {
        let text = self.text()?;
        Ok(Html::parse_document(&text))
    }

    fn ensure_status(self, statuses: &'static [u16]) -> anyhow::Result<Self> {
        if !statuses.contains(&self.status().as_u16()) {
            bail!("expected {:?}, got {}", statuses, self.status());
        }
        Ok(self)
    }
}

fn download_with_progress(
    draw_target: ProgressDrawTarget,
    dl_targets: Vec<(String, reqwest::RequestBuilder)>,
) -> anyhow::Result<Vec<String>> {
    let mut rt = Runtime::new()?;
    let mp = MultiProgress::with_draw_target(draw_target);
    let name_width = dl_targets.iter().map(|(s, _)| s.width()).max().unwrap_or(0);

    let handles = dl_targets
        .into_iter()
        .map(|(name, req)| {
            let pb = mp.add(ProgressBar::new(0));
            pb.set_style(progress_style("{prefix:.bold} Waiting..."));
            pb.set_prefix(&align_left(&name, name_width));

            rt.spawn(async move {
                let res = req.send().await?;

                tokio::task::block_in_place(|| {
                    if let Some(content_len) = res.content_length() {
                        pb.set_length(content_len);
                    }

                    pb.set_style(progress_style(
                        "{prefix:.bold} {bytes:9} {bytes_per_sec:11} {elapsed_precise} {bar} \
                         {percent}%",
                    ));
                });

                let mut content = vec![];
                let mut stream = res.bytes_stream();

                while let Some(chunk) = stream.next().await {
                    let chunk = chunk?;

                    content.extend_from_slice(chunk.as_ref());

                    tokio::task::block_in_place(|| {
                        pb.inc(chunk.len().try_into().unwrap_or(u64::MAX));
                    });
                }

                tokio::task::block_in_place(|| pb.finish_at_current_pos());

                reqwest::Result::Ok(content)
            })
        })
        .collect::<Vec<_>>();

    mp.join()?;

    return handles
        .into_iter()
        .map(|handle| {
            String::from_utf8(rt.block_on(handle)??).with_context(|| "Invalid UTF-8 content")
        })
        .collect();

    fn progress_style(template: &str) -> ProgressStyle {
        ProgressStyle::default_bar().template(template)
    }

    fn align_left(s: &str, n: usize) -> String {
        let spaces = n.saturating_sub(s.width());
        s.chars().chain(itertools::repeat_n(' ', spaces)).collect()
    }
}
