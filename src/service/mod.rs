pub(crate) mod atcoder;
pub(crate) mod hackerrank;

pub mod session;

use config::Config;
use errors::{
    ServiceError, ServiceErrorKind, ServiceResult, ServiceResultExt as _ServiceResultExt,
    SessionResult,
};
use replacer::CodeReplacer;
use service::session::HttpSession;
use template::{BaseDirSome, PathTemplate};
use terminal::Color;
use testsuite::SerializableExtension;
use {util, ServiceName};

use futures::{executor, future, task, Async, Future, Poll};
use pbr::{MultiBar, Pipe, ProgressBar, Units};
use reqwest::header::{ContentLength, Headers, UserAgent};
use reqwest::{RedirectPolicy, Response};
use url::Host;
use zip::result::ZipResult;
use zip::ZipArchive;
use {rpassword, rprompt, webbrowser};

use std::collections::BTreeMap;
use std::io::{self, Cursor, Read, Write};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::time::{Duration, Instant};
use std::{self, env, mem, panic, thread};

/// Gets the value `x` if `Some(x) = o` and `!f(x)`.
///
/// # Errors
///
/// Returns `Err` if the above condition is not satisfied.
pub(self) fn quit_on_failure<T>(o: Option<T>, f: for<'a> fn(&'a T) -> bool) -> ServiceResult<T> {
    if let Some(x) = o {
        if !f(&x) {
            return Ok(x);
        }
    }
    bail!(ServiceErrorKind::Scrape);
}

pub(self) trait OpenInBrowser {
    /// Opens `url`, which is relative or absolute, with default browser
    /// printing a message.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the exit status code is not 0 or an IO error occures.
    fn open_in_browser(&self, url: &str) -> ServiceResult<()>;
}

impl OpenInBrowser for HttpSession {
    fn open_in_browser(&self, url: &str) -> ServiceResult<()> {
        let url = self.resolve_url(url)?;
        println!("Opening {} in default browser...", url);
        let status = webbrowser::open(url.as_str())?.status;
        ensure!(status.success(), ServiceErrorKind::Webbrowser(status));
        Ok(())
    }
}

pub(self) trait DownloadZips {
    /// Sends GET requests to `urls` expecting the response data are all zips.
    ///
    /// # Errors
    ///
    /// Fails if:
    /// - Any of `urls` is invalid
    /// - Any of http access fails
    /// - Any of response code is not 200
    /// - Any of downloaded zip is invalid
    fn download_zips<S: AsRef<str>, W: Write + Send + 'static>(
        &mut self,
        out: W,
        alt_capacity: usize,
        urls: &[S],
    ) -> ServiceResult<Vec<ZipArchive<Cursor<Vec<u8>>>>>;
}

impl DownloadZips for HttpSession {
    fn download_zips<S: AsRef<str>, W: Write + Send + 'static>(
        &mut self,
        out: W,
        alt_capacity: usize,
        urls: &[S],
    ) -> ServiceResult<Vec<ZipArchive<Cursor<Vec<u8>>>>> {
        let responses = urls.iter()
            .map(|url| self.get(url.as_ref()))
            .collect::<SessionResult<Vec<_>>>()?;
        download_zip_files(out, alt_capacity, responses).map_err(Into::into)
    }
}

fn download_zip_files<W: Write + Send + 'static, I: IntoIterator<Item = Response>>(
    mut out: W,
    alt_capacity: usize,
    responses: I,
) -> ZipResult<Vec<ZipArchive<Cursor<Vec<u8>>>>> {
    let _ = out.write(b"Downloading zip files...\n")?;
    out.flush()?;
    let mut mb = MultiBar::on(out);
    let downloads = responses
        .into_iter()
        .map(|response| Downloading::new(response, alt_capacity, &mut mb))
        .collect::<Vec<_>>();
    let thread = thread::spawn(move || mb.listen());
    let zips = executor::block_on(future::join_all(downloads))?;
    thread.join().unwrap_or_else(|p| panic::resume_unwind(p));
    zips.into_iter().map(ZipArchive::new).collect()
}

struct Downloading {
    size_unknown: bool,
    response: Response,
    progress_bar: ProgressBar<Pipe>,
    content: Cursor<Vec<u8>>,
    buf: Vec<u8>,
    bar_updated: Instant,
}

impl Downloading {
    fn new<W: Write>(response: Response, alt_capacity: usize, mb: &mut MultiBar<W>) -> Self {
        let content_length = response.headers().get::<ContentLength>().map(|l| **l);
        let mut progress_bar = mb.create_bar(content_length.unwrap_or(0));
        progress_bar.set_units(Units::Bytes);
        let capacity = content_length.map(|n| n as usize).unwrap_or(alt_capacity);
        Self {
            size_unknown: content_length.is_none(),
            response,
            progress_bar,
            content: Cursor::new(Vec::with_capacity(capacity)),
            buf: vec![unsafe { mem::uninitialized() }; 1024 * 1024],
            bar_updated: Instant::now(),
        }
    }

    fn read(&mut self) -> Poll<Cursor<Vec<u8>>, io::Error> {
        match self.response.read(&mut self.buf)? {
            0 => {
                self.update_progress_bar();
                let content = mem::replace(&mut self.content, Cursor::new(vec![]));
                Ok(Async::Ready(content))
            }
            n => {
                let _ = self.content.write(&self.buf[0..n])?;
                if n == self.buf.len() {
                    self.buf
                        .extend(vec![unsafe { mem::uninitialized::<u8>() }; n]);
                } else if Instant::now() - self.bar_updated > Duration::from_millis(20) {
                    self.update_progress_bar();
                }
                Ok(Async::Pending)
            }
        }
    }

    fn update_progress_bar(&mut self) {
        let pos = self.content.position();
        if self.size_unknown {
            self.progress_bar.total = pos;
        }
        self.progress_bar.set(pos);
        self.bar_updated = Instant::now();
    }
}

impl Future for Downloading {
    type Item = Cursor<Vec<u8>>;
    type Error = io::Error;

    fn poll(&mut self, cx: &mut task::Context) -> Poll<Cursor<Vec<u8>>, io::Error> {
        let result = self.read();
        match result {
            Ok(Async::Ready(_)) => self.progress_bar.finish(),
            Ok(Async::Pending) => cx.waker().wake(),
            Err(_) => self.progress_bar.finish_print("Failed"),
        }
        result
    }
}

#[derive(Clone)]
pub enum Credentials {
    None,
    Some {
        username: Rc<String>,
        password: Rc<String>,
    },
}

impl Credentials {
    pub fn from_env_vars(
        username: &str,
        password: &str,
    ) -> std::result::Result<Self, env::VarError> {
        let (username, password) = (Rc::new(env::var(username)?), Rc::new(env::var(password)?));
        Ok(Credentials::Some { username, password })
    }

    pub(self) fn or_ask(&self, username_prompt: &str) -> io::Result<(Rc<String>, Rc<String>)> {
        if let Credentials::Some {
            ref username,
            ref password,
        } = *self
        {
            Ok((username.clone(), password.clone()))
        } else {
            let username = rprompt::prompt_reply_stderr(username_prompt)?;
            let password =
                rpassword::prompt_password_stderr("Password: ").or_else(|e| match e.kind() {
                    io::ErrorKind::BrokenPipe => {
                        eprintln_bold!(Color::Warning, "broken pipe");
                        rprompt::prompt_reply_stderr("Password (not hidden): ")
                    }
                    _ => Err(e),
                })?;
            Ok((Rc::new(username), Rc::new(password)))
        }
    }

    pub(self) fn is_some(&self) -> bool {
        match *self {
            Credentials::None => false,
            Credentials::Some { .. } => true,
        }
    }
}

#[derive(Default, Serialize, Deserialize)]
pub(crate) struct SessionConfig {
    #[serde(serialize_with = "util::ser::secs", deserialize_with = "util::de::non_zero_secs")]
    timeout: Option<Duration>,
}

pub(crate) struct SessionProp {
    domain: Option<&'static str>,
    cookies_path: PathBuf,
    timeout: Option<Duration>,
    credentials: Credentials,
}

impl SessionProp {
    pub fn new(
        domain: Option<&'static str>,
        cookies_path: PathBuf,
        credentials: Credentials,
        sess_conf: Option<&SessionConfig>,
    ) -> Self {
        Self {
            domain,
            cookies_path,
            timeout: sess_conf.and_then(|c| c.timeout),
            credentials,
        }
    }

    pub(self) fn credentials(&self) -> Credentials {
        self.credentials.clone()
    }

    pub(self) fn start_session(&self) -> ServiceResult<HttpSession> {
        static USER_AGENT: &str = "snowchains <https://github.com/wariuni/snowchains>";
        let builder = HttpSession::builder()
            .autosave_cookies(&self.cookies_path)
            .redirect(RedirectPolicy::none())
            .timeout(self.timeout)
            .referer(false)
            .default_headers({
                let mut headers = Headers::new();
                headers.set(UserAgent::new(USER_AGENT));
                headers
            });
        if let Some(domain) = self.domain {
            builder
                .base(Host::Domain(domain), true, None)
                .with_robots_txt()
        } else {
            builder.build()
        }.chain_err(|| ServiceError::from(ServiceErrorKind::HttpSessionStart))
    }
}

pub(crate) trait Contest {
    fn new(s: &str) -> Self;
}

impl<'a> Contest for &'a str {
    fn new(_: &str) -> Self {
        unreachable!()
    }
}

pub(crate) struct DownloadProp<C: Contest> {
    contest: C,
    download_dir: PathBuf,
    extension: SerializableExtension,
    open_browser: bool,
}

impl<'a> DownloadProp<&'a str> {
    pub fn new(config: &'a Config, open_browser: bool) -> ::Result<Self> {
        let contest = config.contest();
        let download_dir = config.testfiles_dir().expand("")?;
        let extension = config.extension_on_scrape();
        Ok(Self {
            contest,
            download_dir,
            extension,
            open_browser,
        })
    }

    pub(self) fn transform<C: Contest>(self) -> DownloadProp<C> {
        DownloadProp {
            contest: C::new(self.contest),
            download_dir: self.download_dir,
            extension: self.extension,
            open_browser: self.open_browser,
        }
    }
}

impl<C: Contest> DownloadProp<C> {
    pub(self) fn values(&self) -> (&C, &Path, SerializableExtension, bool) {
        (
            &self.contest,
            &self.download_dir,
            self.extension,
            self.open_browser,
        )
    }
}

pub(crate) struct RestoreProp<'a, C: Contest> {
    contest: C,
    src_paths: BTreeMap<u32, PathTemplate<BaseDirSome<'a>>>,
    replacers: BTreeMap<u32, CodeReplacer>,
}

impl<'a> RestoreProp<'a, &'a str> {
    pub fn new(config: &'a Config) -> ::Result<Self> {
        let contest = config.contest();
        let replacers = config.code_replacers_on_atcoder()?;
        let src_paths = config.src_paths();
        Ok(Self {
            contest,
            src_paths,
            replacers,
        })
    }

    pub(self) fn transform<C: Contest>(self) -> RestoreProp<'a, C> {
        RestoreProp {
            contest: C::new(self.contest),
            src_paths: self.src_paths,
            replacers: self.replacers,
        }
    }
}

impl<'a, C: Contest> RestoreProp<'a, C> {
    pub(self) fn values(
        &self,
    ) -> (
        &C,
        &BTreeMap<u32, PathTemplate<BaseDirSome<'a>>>,
        &BTreeMap<u32, CodeReplacer>,
    ) {
        (&self.contest, &self.src_paths, &self.replacers)
    }
}

pub(crate) struct SubmitProp<C: Contest> {
    contest: C,
    target: String,
    lang_id: u32,
    src_path: PathBuf,
    replacer: Option<CodeReplacer>,
    open_browser: bool,
    skip_checking_if_accepted: bool,
}

impl<'a> SubmitProp<&'a str> {
    pub fn new(
        config: &'a Config,
        target: String,
        language: Option<&str>,
        open_browser: bool,
        skip_checking_if_accepted: bool,
    ) -> ::Result<Self> {
        let service = config.service();
        let contest = config.contest();
        let src_path = config.src_to_submit(language)?.expand(&target)?;
        let replacer = config.code_replacer(language)?;
        let lang_id = match service {
            ServiceName::AtCoder => config.atcoder_lang_id(language)?,
            _ => bail!(::ErrorKind::Unimplemented),
        };
        Ok(Self {
            contest,
            target,
            lang_id,
            src_path,
            replacer,
            open_browser,
            skip_checking_if_accepted,
        })
    }

    pub(self) fn transform<C: Contest>(self) -> SubmitProp<C> {
        SubmitProp {
            contest: C::new(self.contest),
            target: self.target,
            lang_id: self.lang_id,
            src_path: self.src_path,
            replacer: self.replacer,
            open_browser: self.open_browser,
            skip_checking_if_accepted: self.skip_checking_if_accepted,
        }
    }
}

impl<C: Contest> SubmitProp<C> {
    pub(self) fn values(&self) -> (&C, &str, u32, &Path, Option<&CodeReplacer>, bool, bool) {
        (
            &self.contest,
            &self.target,
            self.lang_id,
            &self.src_path,
            self.replacer.as_ref(),
            self.open_browser,
            self.skip_checking_if_accepted,
        )
    }
}
