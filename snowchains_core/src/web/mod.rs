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
        // `self::BASE_URL` is defined in each module.

        self::BASE_URL.join(&format!(
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
        AtcoderWatchSubmissionsCredentials, AtcoderWatchSubmissionsTarget,
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
use fs2::FileExt as _;
use futures_util::StreamExt as _;
use indexmap::IndexMap;
use indicatif::{MultiProgress, ProgressBar, ProgressDrawTarget, ProgressStyle};
use itertools::Itertools as _;
use prettytable::{
    cell,
    format::{FormatBuilder, LinePosition, LineSeparator},
    row, Table,
};
use reqwest::{header, redirect::Policy, Method, StatusCode};
use scraper::Html;
use serde::{Deserialize, Serialize, Serializer};
use std::{
    any,
    borrow::Borrow,
    cell::RefCell,
    convert::TryInto,
    fmt,
    fs::File,
    hash::Hash,
    io::{self, BufReader, Seek as _, SeekFrom},
    marker::PhantomData,
    ops::{Deref, RangeFull, RangeInclusive},
    path::{Path, PathBuf},
    str,
    sync::Mutex,
    time::Duration,
};
use strum::EnumString;
use termcolor::Ansi;
use tokio::runtime::Runtime;
use unicode_width::UnicodeWidthStr as _;
use url::Url;

pub trait Platform: Sized {
    type CookieStorage;
    type LoginCredentials;
    type ParticipateTarget;
    type ParticipateCredentials;
    type RetrieveLanguagesTarget;
    type RetrieveLanguagesCredentials;
    type RetrieveTestCasesTargets;
    type RetrieveTestCasesCredentials;
    type RetrieveFullTestCasesCredentials;
    type WatchSubmissionsTarget;
    type WatchSubmissionsCredentials;
    type SubmitTarget;
    type SubmitCredentials;
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
pub enum PlatformKind {
    Atcoder,
    Codeforces,
    Yukicoder,
}

impl PlatformKind {
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
    pub credentials: P::LoginCredentials,
    pub cookie_storage: P::CookieStorage,
    pub timeout: Option<Duration>,
    pub shell: S,
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
    pub credentials: P::ParticipateCredentials,
    pub cookie_storage: P::CookieStorage,
    pub timeout: Option<Duration>,
    pub shell: S,
}

#[derive(Debug, Clone, Copy, From, Serialize)]
pub enum ParticipateOutcome {
    Success,
    AlreadyParticipated,
    ContestIsFinished,
}

impl ParticipateOutcome {
    pub fn to_json(self) -> String {
        serde_json::to_string(&self).expect("should not fail")
    }

    pub fn message(self) -> &'static str {
        match self {
            Self::Success => "Successfully participated.",
            Self::AlreadyParticipated => "Already participated.",
            Self::ContestIsFinished => "The contest is already finished.",
        }
    }
}

#[derive(Debug)]
pub struct RetrieveLanguages<P: Platform, S: Shell> {
    pub target: P::RetrieveLanguagesTarget,
    pub credentials: P::RetrieveLanguagesCredentials,
    pub cookie_storage: P::CookieStorage,
    pub timeout: Option<Duration>,
    pub shell: S,
}

#[derive(Debug, Serialize)]
pub struct RetrieveLanguagesOutcome {
    pub names_by_id: IndexMap<String, String>,
}

impl RetrieveLanguagesOutcome {
    pub fn to_json(&self) -> String {
        serde_json::to_string(self).expect("should not fail")
    }

    pub fn to_table(&self) -> impl fmt::Display {
        let mut table = Table::new();

        *table.get_format() = FormatBuilder::new()
            .padding(1, 1)
            .column_separator('│')
            .borders('│')
            .separator(LinePosition::Top, LineSeparator::new('─', '┬', '┌', '┐'))
            .separator(LinePosition::Title, LineSeparator::new('─', '┼', '├', '┤'))
            .separator(LinePosition::Intern, LineSeparator::new('─', '┼', '├', '┤'))
            .separator(LinePosition::Bottom, LineSeparator::new('─', '┴', '└', '┘'))
            .build();

        table.set_titles(row!["ID", "Name"]);

        for (id, name) in &self.names_by_id {
            table.add_row(row![id, name]);
        }

        table
    }
}

pub struct RetrieveTestCases<P: Platform, S: Shell> {
    pub targets: P::RetrieveTestCasesTargets,
    pub credentials: P::RetrieveTestCasesCredentials,
    pub full: Option<RetrieveFullTestCases<P>>,
    pub cookie_storage: P::CookieStorage,
    pub timeout: Option<Duration>,
    pub shell: S,
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
    pub index: String,
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

#[derive(Debug, Serialize)]
pub struct WatchSubmissions<P: Platform, S: Shell> {
    pub target: P::WatchSubmissionsTarget,
    pub credentials: P::WatchSubmissionsCredentials,
    pub cookie_storage: P::CookieStorage,
    pub timeout: Option<Duration>,
    pub shell: S,
}

struct AnsiColored(Vec<u8>);

impl AnsiColored {
    fn new(f: impl FnOnce(&mut Ansi<Vec<u8>>) -> io::Result<()>) -> io::Result<Self> {
        let mut wtr = Ansi::new(vec![]);
        f(&mut wtr)?;
        Ok(Self(wtr.into_inner()))
    }

    fn get(&self) -> &[u8] {
        &self.0
    }
}

#[derive(Debug)]
pub struct Submit<P: Platform, S: Shell> {
    pub target: P::SubmitTarget,
    pub credentials: P::SubmitCredentials,
    pub language_id: String,
    pub code: String,
    pub watch_submission: bool,
    pub cookie_storage: P::CookieStorage,
    pub timeout: Option<Duration>,
    pub shell: S,
}

#[derive(Debug, Serialize)]
pub struct SubmitOutcome {
    pub problem_screen_name: String,
    pub submission_url: Url,
    pub submissions_url: Url,
}

impl SubmitOutcome {
    pub fn to_json(&self) -> String {
        serde_json::to_string(self).expect("should not fail")
    }
}

pub struct CookieStorage {
    pub cookie_store: CookieStore,
    pub on_update: Box<dyn Fn(&CookieStore) -> anyhow::Result<()>>,
}

impl CookieStorage {
    pub fn with_jsonl<P: AsRef<Path>>(path: P) -> anyhow::Result<Self> {
        let path = path.as_ref();

        let cookie_store = if path.exists() {
            File::open(&path)
                .map_err(anyhow::Error::from)
                .and_then(|h| {
                    CookieStore::load_json(BufReader::new(h)).map_err(|e| anyhow!("{}", e))
                })
                .with_context(|| format!("Could not load cookies from `{}`", path.display()))?
        } else {
            CookieStore::default()
        };

        let file = LazyLockedFile::new(&path);

        let on_update = Box::new(move |cookie_store: &CookieStore| -> _ {
            file.overwrite(|file| {
                cookie_store.save_json(file).map_err(|e| anyhow!("{}", e))?;
                Ok(())
            })
        });

        return Ok(Self {
            cookie_store,
            on_update,
        });

        #[derive(Debug)]
        struct LazyLockedFile {
            path: PathBuf,
            file: Mutex<Option<File>>,
        }

        impl LazyLockedFile {
            fn new(path: &Path) -> Self {
                Self {
                    path: path.to_owned(),
                    file: Mutex::new(None),
                }
            }

            fn overwrite(
                &self,
                f: impl FnOnce(&mut File) -> anyhow::Result<()>,
            ) -> anyhow::Result<()> {
                let Self { path, file } = self;

                let mut file = file.lock().unwrap();

                let new_file = if file.is_none() {
                    if let Some(parent) = path.parent() {
                        if !parent.exists() {
                            std::fs::create_dir_all(parent).with_context(|| {
                                format!("Could not create `{}`", parent.display())
                            })?;
                        }
                    }

                    let new_file = File::create(&path)
                        .with_context(|| format!("Could not open `{}`", path.display()))?;

                    new_file
                        .try_lock_exclusive()
                        .with_context(|| format!("Could not lock `{}`", path.display()))?;

                    Some(new_file)
                } else {
                    None
                };

                let file = file.get_or_insert_with(|| new_file.unwrap());

                file.seek(SeekFrom::Start(0))
                    .and_then(|_| file.set_len(0))
                    .map_err(Into::into)
                    .and_then(|()| f(file))
                    .and_then(|()| file.sync_data().map_err(Into::into))
                    .with_context(|| format!("Could not write `{}`", path.display()))
            }
        }
    }
}

pub trait Shell {
    fn progress_draw_target(&self) -> ProgressDrawTarget {
        ProgressDrawTarget::hidden()
    }

    fn print_ansi(&mut self, _message: &[u8]) -> io::Result<()> {
        Ok(())
    }

    fn warn<T: fmt::Display>(&mut self, _message: T) -> io::Result<()> {
        Ok(())
    }

    fn on_request(&mut self, _request: &reqwest::blocking::Request) -> io::Result<()> {
        Ok(())
    }

    fn on_response(
        &mut self,
        _response: &reqwest::blocking::Response,
        _status_code_color: StatusCodeColor,
    ) -> io::Result<()> {
        Ok(())
    }
}

impl<S: Shell> Shell for &'_ mut S {
    fn progress_draw_target(&self) -> ProgressDrawTarget {
        (**self).progress_draw_target()
    }

    fn print_ansi(&mut self, message: &[u8]) -> io::Result<()> {
        (**self).print_ansi(message)
    }

    fn warn<T: fmt::Display>(&mut self, message: T) -> io::Result<()> {
        (**self).warn(message)
    }

    fn on_request(&mut self, request: &reqwest::blocking::Request) -> io::Result<()> {
        (**self).on_request(request)
    }

    fn on_response(
        &mut self,
        response: &reqwest::blocking::Response,
        status_code_color: StatusCodeColor,
    ) -> io::Result<()> {
        (**self).on_response(response, status_code_color)
    }
}

impl<S: Shell> Shell for RefCell<S> {
    fn progress_draw_target(&self) -> ProgressDrawTarget {
        self.borrow().progress_draw_target()
    }

    fn print_ansi(&mut self, message: &[u8]) -> io::Result<()> {
        self.borrow_mut().print_ansi(message)
    }

    fn warn<T: fmt::Display>(&mut self, message: T) -> io::Result<()> {
        self.borrow_mut().warn(message)
    }

    fn on_request(&mut self, request: &reqwest::blocking::Request) -> io::Result<()> {
        self.borrow_mut().on_request(request)
    }

    fn on_response(
        &mut self,
        response: &reqwest::blocking::Response,
        status_code_color: StatusCodeColor,
    ) -> io::Result<()> {
        self.borrow_mut().on_response(response, status_code_color)
    }
}

impl<S: Shell> Shell for &'_ RefCell<S> {
    fn progress_draw_target(&self) -> ProgressDrawTarget {
        (*self).borrow().progress_draw_target()
    }

    fn print_ansi(&mut self, message: &[u8]) -> io::Result<()> {
        (*self).borrow_mut().print_ansi(message)
    }

    fn warn<T: fmt::Display>(&mut self, message: T) -> io::Result<()> {
        (*self).borrow_mut().warn(message)
    }

    fn on_request(&mut self, request: &reqwest::blocking::Request) -> io::Result<()> {
        (*self).borrow_mut().on_request(request)
    }

    fn on_response(
        &mut self,
        response: &reqwest::blocking::Response,
        status_code_color: StatusCodeColor,
    ) -> io::Result<()> {
        (*self)
            .borrow_mut()
            .on_response(response, status_code_color)
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum StatusCodeColor {
    Ok,
    Warn,
    Error,
    Unknown,
}

struct Session<S> {
    async_client: reqwest::Client,
    blocking_client: reqwest::blocking::Client,
    cookie_storage: Option<CookieStorage>,
    shell: S,
}

impl<S: Shell> Session<S> {
    fn new(
        timeout: Option<Duration>,
        cookie_storage: Option<CookieStorage>,
        shell: S,
    ) -> anyhow::Result<Self> {
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

        return Ok(Self {
            async_client,
            blocking_client,
            cookie_storage,
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

    fn async_client(&self) -> &reqwest::Client;

    fn shell(&mut self) -> &mut Self::Shell;

    fn cookie_store(&self) -> Option<&CookieStore>;

    fn request(&mut self, method: Method, url: Url) -> SessionRequestBuilder<'_, Self::Shell>;

    fn cookie_header(&self, url: &Url) -> String {
        self.cookie_store()
            .into_iter()
            .flat_map(|c| c.get_request_cookies(url))
            .join("; ")
    }

    fn get(&mut self, url: Url) -> SessionRequestBuilder<'_, Self::Shell> {
        self.request(Method::GET, url)
    }

    fn post(&mut self, url: Url) -> SessionRequestBuilder<'_, Self::Shell> {
        self.request(Method::POST, url)
    }
}

impl<S: Shell> SessionMut for Session<S> {
    type Shell = S;

    fn async_client(&self) -> &reqwest::Client {
        &self.async_client
    }

    fn shell(&mut self) -> &mut S {
        &mut self.shell
    }

    fn cookie_store(&self) -> Option<&CookieStore> {
        self.cookie_storage
            .as_ref()
            .map(|CookieStorage { cookie_store, .. }| cookie_store)
    }

    fn request(&mut self, method: Method, url: Url) -> SessionRequestBuilder<'_, S> {
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

    fn async_client(&self) -> &reqwest::Client {
        (**self).async_client()
    }

    fn shell(&mut self) -> &mut S::Shell {
        (**self).shell()
    }

    fn cookie_store(&self) -> Option<&CookieStore> {
        (**self).cookie_store()
    }

    fn request(&mut self, method: Method, url: Url) -> SessionRequestBuilder<'_, S::Shell> {
        (**self).request(method, url)
    }
}

struct SessionRequestBuilder<'a, S> {
    inner: reqwest::blocking::RequestBuilder,
    url: Url,
    redirects: usize,
    colorize_status_code: Box<dyn FnOnce(StatusCode) -> StatusCodeColor>,
    sess: &'a mut Session<S>,
}

impl<S: Shell> SessionRequestBuilder<'_, S> {
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

        if let Some(CookieStorage {
            cookie_store,
            on_update,
        }) = &mut sess.cookie_storage
        {
            for set_cookie in res.headers().get_all(header::SET_COOKIE) {
                let set_cookie = str::from_utf8(set_cookie.as_bytes())
                    .map_err(|e| anyhow!("{}: {}", e, &url))?;
                let cookie = cookie_store::Cookie::parse(set_cookie, &url)?.into_owned();
                cookie_store.insert(cookie, &url)?;
            }

            if res.headers().contains_key(header::SET_COOKIE) {
                (on_update)(cookie_store)?;
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

    fn location_url(&self) -> anyhow::Result<Url> {
        let mut url = static_url!("https://-").clone();
        url.set_host(self.url().host_str())?;
        url.join(self.location()?).map_err(Into::into)
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
