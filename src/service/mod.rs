pub mod atcoder;
pub mod atcoder_beta;
pub mod hackerrank;

use errors::{ServiceError, ServiceErrorKind, ServiceResult, ServiceResultExt as _SericeResultExt};
use replacer::CodeReplacer;
use template::PathTemplate;
use terminal::Color;
use testsuite::SuiteFileExtension;
use util;

use {rpassword, rprompt, webbrowser};
use futures::{future, task, Async, Future};
use httpsession::{ColorMode, CookieStoreOption, HttpSession, RedirectPolicy, Response};
use httpsession::header::{ContentLength, UserAgent};
use pbr::{MultiBar, Pipe, ProgressBar, Units};
use zip::ZipArchive;
use zip::result::{ZipError, ZipResult};

use std::{mem, thread};
use std::collections::BTreeMap;
use std::io::{self, Cursor, Read, Write};
use std::path::{Path, PathBuf};
use std::time::Duration;

/// Constructs a `HttpSession`.
pub(self) fn start_session(
    filename: &'static str,
    domain: &'static str,
) -> ServiceResult<HttpSession> {
    HttpSession::builder()
        .base(domain, true, None)
        .cookie_store(CookieStoreOption::AutoSave(util::path_under_home(&[
            ".local",
            "share",
            "snowchains",
            filename,
        ])?))
        .echo_actions(ColorMode::Prefer256.disable_on("NO_COLOR"))
        .timeout(Duration::from_secs(20))
        .redirect(RedirectPolicy::none())
        .default_header(UserAgent::new(
            "snowchains <https://github.com/wariuni/snowchains>",
        ))
        .with_robots_txt()
        .chain_err::<_, ServiceError>(|| ServiceErrorKind::HttpSessionStart.into())
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
        if !status.success() {
            bail!(ServiceErrorKind::Webbrowser(status));
        }
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
    fn download_zips<S: AsRef<str>>(
        &mut self,
        urls: &[S],
        each_capacity_on_content_length_missing: usize,
    ) -> ServiceResult<Vec<ZipArchive<Cursor<Vec<u8>>>>>;
}

impl DownloadZips for HttpSession {
    fn download_zips<S: AsRef<str>>(
        &mut self,
        urls: &[S],
        each_capacity_on_content_length_missing: usize,
    ) -> ServiceResult<Vec<ZipArchive<Cursor<Vec<u8>>>>> {
        let responses = urls.iter()
            .map(|url| self.get(url.as_ref()))
            .collect::<Result<Vec<_>, _>>()?;
        println!("Downloading zip files...");
        let mut mb = MultiBar::new();
        let downloads = responses
            .into_iter()
            .map(|resp| ZipDownloading::new(resp, each_capacity_on_content_length_missing, &mut mb))
            .collect::<Vec<_>>();
        thread::spawn(move || mb.listen());
        future::join_all(downloads).wait().map_err(Into::into)
    }
}

struct ZipDownloading<R: Read> {
    size_unknown: bool,
    resource: R,
    progress_bar: ProgressBar<Pipe>,
    content: Cursor<Vec<u8>>,
    buf: [u8; 10240],
}

impl ZipDownloading<Response> {
    fn new<W: Write>(response: Response, alt_capacity: usize, mb: &mut MultiBar<W>) -> Self {
        let content_length = response.headers().get::<ContentLength>().map(|l| **l);
        let mut progress_bar = mb.create_bar(content_length.unwrap_or(0));
        progress_bar.set_units(Units::Bytes);
        let size = content_length.map(|n| n as usize).unwrap_or(alt_capacity);
        Self {
            size_unknown: content_length.is_none(),
            resource: response,
            progress_bar,
            content: Cursor::new(Vec::with_capacity(size)),
            buf: [0; 10240],
        }
    }
}

impl<R: Read> Future for ZipDownloading<R> {
    type Item = ZipArchive<Cursor<Vec<u8>>>;
    type Error = ZipError;

    fn poll(&mut self) -> ZipResult<Async<ZipArchive<Cursor<Vec<u8>>>>> {
        match self.resource.read(&mut self.buf)? {
            0 => {
                self.progress_bar.finish();
                let zip = ZipArchive::new(mem::replace(&mut self.content, Cursor::new(vec![])))?;
                Ok(Async::Ready(zip))
            }
            n => {
                let _ = self.content.write(&self.buf[0..n])?;
                let pos = self.content.position();
                if self.size_unknown {
                    self.progress_bar.total = pos;
                }
                self.progress_bar.set(pos);
                task::current().notify();
                Ok(Async::NotReady)
            }
        }
    }
}

pub trait Contest {
    fn new(s: &str) -> Self;
}

impl<'a> Contest for &'a str {
    fn new(_: &str) -> Self {
        unreachable!()
    }
}

pub struct DownloadProp<C: Contest> {
    contest: C,
    dir_to_save: PathBuf,
    extension: SuiteFileExtension,
    open_browser: bool,
}

impl<'a> DownloadProp<&'a str> {
    pub fn new(
        contest: &'a str,
        dir_to_save: PathBuf,
        extension: SuiteFileExtension,
        open_browser: bool,
    ) -> Self {
        Self {
            contest,
            dir_to_save,
            extension,
            open_browser,
        }
    }

    pub(self) fn transform<C: Contest>(self) -> DownloadProp<C> {
        DownloadProp {
            contest: C::new(self.contest),
            dir_to_save: self.dir_to_save,
            extension: self.extension,
            open_browser: self.open_browser,
        }
    }
}

impl<C: Contest> DownloadProp<C> {
    pub(self) fn values(&self) -> (&C, &Path, SuiteFileExtension, bool) {
        (
            &self.contest,
            &self.dir_to_save,
            self.extension,
            self.open_browser,
        )
    }
}

pub struct RestoreProp<'a, C: Contest> {
    contest: C,
    src_paths: &'a BTreeMap<u32, PathTemplate<'a>>,
    replacers: &'a BTreeMap<u32, CodeReplacer>,
}

impl<'a> RestoreProp<'a, &'a str> {
    pub fn new(
        contest: &'a str,
        src_paths: &'a BTreeMap<u32, PathTemplate<'a>>,
        replacers: &'a BTreeMap<u32, CodeReplacer>,
    ) -> Self {
        Self {
            contest,
            src_paths,
            replacers,
        }
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
        &BTreeMap<u32, PathTemplate>,
        &BTreeMap<u32, CodeReplacer>,
    ) {
        (&self.contest, self.src_paths, self.replacers)
    }
}

pub struct SubmitProp<'a, C: Contest> {
    contest: C,
    target: String,
    lang_id: u32,
    src_path: PathBuf,
    replacer: Option<&'a CodeReplacer>,
    open_browser: bool,
    skip_checking_if_accepted: bool,
}

impl<'a> SubmitProp<'a, &'a str> {
    pub fn new(
        contest: &'a str,
        target: String,
        lang_id: u32,
        src_path: PathBuf,
        replacer: Option<&'a CodeReplacer>,
        open_browser: bool,
        skip_checking_if_accepted: bool,
    ) -> Self {
        Self {
            contest,
            target,
            lang_id,
            src_path,
            replacer,
            open_browser,
            skip_checking_if_accepted,
        }
    }

    pub(self) fn transform<C: Contest>(self) -> SubmitProp<'a, C> {
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

impl<'a, C: Contest> SubmitProp<'a, C> {
    pub(self) fn values(&self) -> (&C, &str, u32, &Path, Option<&CodeReplacer>, bool, bool) {
        (
            &self.contest,
            &self.target,
            self.lang_id,
            &self.src_path,
            self.replacer,
            self.open_browser,
            self.skip_checking_if_accepted,
        )
    }
}

#[cfg(test)]
mod tests {
    use errors::{ServiceError, ServiceErrorKind, ServiceResult};
    use service::DownloadZips as _DownloadZips;

    use httpsession::HttpSession;
    use nickel::{Nickel, QueryString as _QueryString};
    use nickel::hyper::header::ContentLength;
    use zip::ZipWriter;
    use zip::write::FileOptions;

    use std::io::{Cursor, Write as _Write};

    #[test]
    #[ignore]
    fn it_downloads_zip_files() {
        let mut server = Nickel::new();
        server.utilize(router! {
            get "/" => |request, mut response| {
                let filesize = request.query().get("filesize").and_then(|s| s.parse::<usize>().ok())
                    .unwrap_or(0);
                let add_content_length = request.query().get("addcontentlength") == Some("1");
                let mut zip = ZipWriter::new(Cursor::new(Vec::<u8>::with_capacity(filesize)));
                for &name in &["a", "b", "c"] {
                    zip.start_file(name, FileOptions::default()).unwrap();
                    zip.write(&vec![0u8; filesize]).unwrap();
                }
                let content = zip.finish().unwrap().into_inner();
                if add_content_length {
                    response.headers_mut().set(ContentLength(content.len() as u64));
                }
                content
            }
            get "/invalid" => |_request, mut response| {
                response.headers_mut().set(ContentLength(5));
                "aaaaa"
            }
        });
        let server = server.listen("127.0.0.1:2000").unwrap();
        if let Err(e) = || -> ServiceResult<()> {
            let mut session = HttpSession::builder()
                .base("127.0.0.1", false, Some(2000))
                .build()?;
            session.download_zips(
                &[
                    "/?filesize=20000000&addcontentlength=1",
                    "/?filesize=20000000",
                    "/?filesize=10000",
                    "/",
                ],
                0,
            )?;
            match session.download_zips(&["/invalid"], 0) {
                Ok(_) => bail!("Should be an error"),
                Err(ServiceError(ServiceErrorKind::Zip(_), _)) => Ok(()),
                Err(e) => Err(e),
            }
        }() {
            server.detach();
            panic!("{}", e);
        } else {
            server.detach();
        };
    }
}
