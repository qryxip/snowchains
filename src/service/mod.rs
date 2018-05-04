pub(crate) mod atcoder_beta;
pub(crate) mod hackerrank;

use ServiceName;
use config::Config;
use errors::{ServiceError, ServiceErrorKind, ServiceResult, ServiceResultExt as _SericeResultExt};
use replacer::CodeReplacer;
use template::{BaseDirSome, PathTemplate};
use terminal::Color;
use testsuite::SuiteFileExtension;

use {rpassword, rprompt, webbrowser};
use futures::{executor, future, task, Async, Future, Poll};
use httpsession::{self, ColorMode, CookieStoreOption, HttpSession, RedirectPolicy, Response};
use httpsession::header::{ContentLength, UserAgent};
use pbr::{MultiBar, Pipe, ProgressBar, Units};
use zip::ZipArchive;
use zip::result::ZipResult;

use std::{mem, panic, thread};
use std::collections::BTreeMap;
use std::io::{self, Cursor, Read, Write};
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

/// Constructs a `HttpSession`.
pub(self) fn start_session(
    domain: &'static str,
    cookie_path: PathBuf,
) -> ServiceResult<HttpSession> {
    HttpSession::builder()
        .base(domain, true, None)
        .cookie_store(CookieStoreOption::AutoSave(cookie_path))
        .echo_actions(ColorMode::Prefer256.disable_on("NO_COLOR"))
        .timeout(Duration::from_secs(20))
        .redirect(RedirectPolicy::none())
        .default_header(UserAgent::new(
            "snowchains <https://github.com/wariuni/snowchains>",
        ))
        .with_robots_txt()
        .chain_err(|| ServiceError::from(ServiceErrorKind::HttpSessionStart))
}

/// Reads username and password from stdin, showing the prompts on stderr.
///
/// If fails to read a password because of OS error 6 or 32, askes a password
/// again without hiding the input.
pub(self) fn ask_username_and_password(
    username_prompt: &'static str,
) -> io::Result<(String, String)> {
    let errno_brokenpipe = if cfg!(target_os = "windows") { 6 } else { 32 };
    let username = rprompt::prompt_reply_stderr(username_prompt)?;
    let password =
        rpassword::prompt_password_stderr("Password: ").or_else(|e| match e.raw_os_error() {
            Some(n) if n == errno_brokenpipe => {
                eprintln_bold!(Color::Warning, "os error {}", n);
                rprompt::prompt_reply_stderr("Password (not hidden): ")
            }
            _ => Err(e),
        })?;
    Ok((username, password))
}

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
            .collect::<httpsession::Result<Vec<_>>>()?;
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

pub(crate) struct InitProp {
    cookie_path: PathBuf,
    color_mode: ColorMode,
    timeout: Option<Duration>,
    credentials: Option<(String, String)>,
}

impl InitProp {
    pub fn new<T: Into<Option<Duration>>, C: Into<Option<(String, String)>>>(
        cookie_path: PathBuf,
        color_mode: ColorMode,
        timeout: T,
        credentials: C,
    ) -> Self {
        Self {
            cookie_path,
            color_mode,
            timeout: timeout.into(),
            credentials: credentials.into(),
        }
    }

    pub(self) fn credentials(&self) -> Option<(String, String)> {
        self.credentials.clone()
    }

    pub(self) fn start_session(&self, domain: &'static str) -> ServiceResult<HttpSession> {
        static UA: &str = "snowchains <https://github.com/wariuni/snowchains>";
        HttpSession::builder()
            .base(domain, true, None)
            .cookie_store(CookieStoreOption::AutoSave(self.cookie_path.clone()))
            .echo_actions(self.color_mode)
            .timeout(self.timeout)
            .redirect(RedirectPolicy::none())
            .default_header(UserAgent::new(UA))
            .with_robots_txt()
            .chain_err(|| ServiceError::from(ServiceErrorKind::HttpSessionStart))
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
    extension: SuiteFileExtension,
    open_browser: bool,
}

impl<'a> DownloadProp<&'a str> {
    pub fn new(config: &'a Config, open_browser: bool) -> ::Result<Self> {
        let contest = config.contest();
        let download_dir = config.testfiles_dir().expand("")?;
        let extension = config.extension_on_download();
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
    pub(self) fn values(&self) -> (&C, &Path, SuiteFileExtension, bool) {
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
        let service = config.service();
        let contest = config.contest();
        let replacers = config.code_replacers_on_atcoder()?;
        let src_paths = match service {
            ServiceName::AtCoderBeta => config.src_paths_on_atcoder(),
            _ => bail!(::ErrorKind::Unimplemented),
        };
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
            ServiceName::AtCoderBeta => config.atcoder_lang_id(language)?,
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

#[cfg(all(test, unix))]
mod tests {
    use errors::{ServiceError, ServiceErrorKind, ServiceResult};
    use service::DownloadZips as _DownloadZips;

    use env_logger;
    use httpsession::HttpSession;
    use nickel::{self, ListeningServer, Nickel, QueryString as _QueryString};
    use nickel::hyper::header::ContentLength;
    use zip::ZipWriter;
    use zip::result::{ZipError, ZipResult};
    use zip::write::FileOptions;

    use std::{self, mem, thread};
    use std::io::{self, Cursor, Write as _Write};
    use std::time::Duration;

    #[test]
    #[ignore]
    fn it_downloads_zip_files() {
        const PORT: u16 = 2000;
        const TIMEOUT: Duration = Duration::from_secs(20);
        let _ = env_logger::try_init();
        let server = start_server(PORT).unwrap();
        let (tx, rx) = std::sync::mpsc::channel();
        thread::spawn(move || tx.send(download_zip_files(PORT)).unwrap());
        server.detach();
        rx.recv_timeout(TIMEOUT).unwrap().unwrap();
    }

    fn download_zip_files(port: u16) -> ServiceResult<()> {
        static URLS1: &[&[&str]] = &[&["/a?addcontentlength=1", "/b?addcontentlength=1", "/c"]];
        static URLS2: &[&[&str]] = &[&["/empty", "/invalid"]];
        let mut session = HttpSession::builder()
            .base("127.0.0.1", false, Some(port))
            .build()?;
        for urls in URLS1 {
            session.download_zips(io::sink(), 10240, urls)?;
        }
        for urls in URLS2 {
            match session.download_zips(io::sink(), 10240, urls) {
                Err(ServiceError(ServiceErrorKind::Zip(ZipError::InvalidArchive(_)), _)) => {}
                Err(e) => bail!(e),
                Ok(_) => bail!("Should fail"),
            }
        }
        Ok(())
    }

    fn start_server(port: u16) -> ServiceResult<ListeningServer> {
        fn create_zip_file(each_inner_filesize: usize) -> ZipResult<Vec<u8>> {
            let buf = Vec::<u8>::with_capacity(each_inner_filesize);
            let mut zip = ZipWriter::new(Cursor::new(buf));
            for &name in &["a", "b", "c"] {
                zip.start_file(name, FileOptions::default())?;
                let _ = zip.write(&vec![unsafe { mem::uninitialized() }; each_inner_filesize])?;
            }
            Ok(zip.finish()?.into_inner())
        }

        fn respond<'a>(
            file: &'a [u8],
            request: &mut nickel::Request,
            response: &mut nickel::Response,
            path: &'static str,
        ) -> &'a [u8] {
            info!("Recieved: {}, {:?}", path, request.query());
            if request.query().get("addcontentlength") == Some("1") {
                response.headers_mut().set(ContentLength(file.len() as u64));
            }
            file
        }

        let a = create_zip_file(100)?;
        let b = create_zip_file(1000)?;
        let c = create_zip_file(10000)?;
        let invalid = vec![unsafe { mem::uninitialized() }; 5 * 1024 * 1024];
        let empty = vec![];
        let mut server = Nickel::new();
        server.utilize(router! {
            get "/a"       => |req, mut resp| { respond(&a, req, &mut resp, "/a") }
            get "/b"       => |req, mut resp| { respond(&b, req, &mut resp, "/b") }
            get "/c"       => |req, mut resp| { respond(&c, req, &mut resp, "/c") }
            get "/invalid" => |req, mut resp| { respond(&invalid, req, &mut resp, "/invalid") }
            get "/empty"   => |req, mut resp| { respond(&empty, req, &mut resp, "/empty") }
        });
        server
            .listen(format!("127.0.0.1:{}", port))
            .map_err(|e| format!("Failed to start Nickel: {}", e).into())
    }
}
