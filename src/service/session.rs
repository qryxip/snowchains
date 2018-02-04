use errors::{ServiceErrorKind, ServiceResult};
use util;

use bincode::{self, Infinite};
use cookie::{self, Cookie, CookieJar};
use pbr::{MultiBar, Units};
use reqwest::{Client, RedirectPolicy, Response, StatusCode, Url, UrlError};
use reqwest::header::{self, ContentLength, ContentType, Headers, Location, SetCookie, UserAgent};
use robots_txt::{Robots, SimpleMatcher};
use serde::Serialize;
use serde_json;
use serde_urlencoded;
use term::color;
use webbrowser;
use zip::ZipArchive;
use zip::result::ZipResult;

use std::collections::HashMap;
use std::io::{Cursor, Read, Write};
use std::path::PathBuf;
use std::thread;

static USER_AGENT: &'static str = "snowchains <https://github.com/wariuni/snowchains>";

pub struct HttpSession {
    base_domain: &'static str,
    base_https: bool,
    path_to_save_cookies: PathBuf,
    reqwest_client: Client,
    cookie_jar: CookieJar,
    robots_txts: HashMap<&'static str, String>,
}

impl HttpSession {
    /// Creates a new `HttpSession` loading
    /// `~/.local/share/snowchains/<file_name>`.
    pub fn start(
        file_name: &'static str,
        base_domain: &'static str,
        base_https: bool,
    ) -> ServiceResult<Self> {
        let path = util::path_under_home(&[".local", "share", "snowchains", file_name])?;
        let jar = if path.exists() {
            let mut file = util::open_file(&path)?;
            let jar = bincode::deserialize_from::<_, Vec<String>, _>(&mut file, Infinite)?
                .into_iter()
                .map(Cookie::parse)
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .fold(CookieJar::new(), |mut jar, cookie| {
                    jar.add(cookie);
                    jar
                });
            println!("Loaded cookies from {}", path.display());
            jar
        } else {
            println!("{} not found. New one will be created.", path.display());
            CookieJar::new()
        };
        let client = Client::builder()
            .redirect(RedirectPolicy::none())
            .default_headers({
                let mut headers = Headers::new();
                headers.set(UserAgent::new(USER_AGENT));
                headers
            })
            .build()?;
        Ok(Self {
            base_domain: base_domain,
            base_https: base_https,
            path_to_save_cookies: path,
            reqwest_client: client,
            cookie_jar: jar,
            robots_txts: HashMap::new(),
        })
    }

    /// Moves `self` and save its cookies to a file.
    pub fn save_cookies(self) -> ServiceResult<()> {
        let (jar, path) = (self.cookie_jar, self.path_to_save_cookies);
        let mut file = util::create_file_and_dirs(&path)?;
        let cookies = jar.iter().map(Cookie::to_string).collect::<Vec<_>>();
        bincode::serialize_into(&mut file, &cookies, Infinite)?;
        Ok(println!("Saved cookies to {}", path.display()))
    }

    /// Whether `self` has any cookie.
    pub fn has_cookie(&self) -> bool {
        self.cookie_jar.iter().next().is_some()
    }

    /// Deletes all cookies.
    pub fn clear_cookies(&mut self) {
        self.cookie_jar = CookieJar::new();
    }

    /// Opens `url`, which is relative or absolute, with default browser printing a message.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the exit status code is not 0 or an IO error occures.
    pub fn open_in_browser(&self, url: &str) -> ServiceResult<()> {
        let url = resolve_url(self.base_domain, self.base_https, url)?;
        println!("Opening {} in default browser...", url);
        let status = webbrowser::open(url.as_str())?.status;
        if !status.success() {
            bail!(ServiceErrorKind::Webbrowser(status));
        }
        Ok(())
    }

    /// Sends a GET request to "/robots.txt" allowing redirects and store the response.
    ///
    /// Does nothing when the HTTP status code is 404.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the GET request fails.
    pub fn fetch_robots_txt(&mut self) -> ServiceResult<()> {
        let mut response = self.http_get_expecting("/robots.txt", &[200, 301, 302, 404])?;
        while [301, 302].contains(&response.status().as_u16()) {
            if let Some(location) = response.headers().get::<Location>().map(Location::to_owned) {
                response = self.http_get_expecting(&location, &[200, 301, 302, 404])?;
            } else {
                return Ok(());
            }
        }
        if response.status().as_u16() == 200 {
            let robots_txt = util::string_from_read(response)?;
            self.robots_txts.insert(self.base_domain, robots_txt);
        }
        Ok(())
    }

    /// Sends a GET request to `url`, expecting the response code is 200.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - `url` is invalid
    /// - The http access fails
    /// - The response code is not 200
    pub fn http_get(&mut self, url: &str) -> ServiceResult<Response> {
        self.http_get_expecting(url, &[200])
    }

    /// Sends GET requests to `urls` expecting the response data are all zips.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - Any of `urls` is invalid
    /// - Any of http access fails
    /// - Any of response code is not 200
    /// - Any of downloaded zip is invalid
    pub fn http_get_zips(
        &mut self,
        urls: &[String],
    ) -> ServiceResult<Vec<ZipArchive<Cursor<Vec<u8>>>>> {
        let responses = urls.iter()
            .map(|url| self.http_get(url))
            .collect::<Result<Vec<_>, _>>()?;

        println!("Downloading...");
        let mut mb = MultiBar::new();
        let handles = responses
            .into_iter()
            .map(|mut response| {
                let content_length = response.headers().get::<ContentLength>().map(|l| **l);
                let mut pb = mb.create_bar(content_length.unwrap_or(0));
                pb.set_units(Units::Bytes);
                thread::spawn(move || -> ZipResult<_> {
                    let mut cursor = Cursor::new(Vec::with_capacity(content_length
                        .unwrap_or(50 * 1024 * 1024)
                        as usize));
                    let mut buf = [0; 10 * 1024];
                    loop {
                        let n = response.read(&mut buf)?;
                        if n == 0 {
                            pb.finish();
                            break ZipArchive::new(cursor);
                        }
                        cursor.write(&buf[0..n])?;
                        let pos = cursor.position();
                        if content_length.is_none() {
                            pb.total = pos;
                        }
                        pb.set(pos);
                    }
                })
            })
            .collect::<Vec<_>>();
        mb.listen();

        let mut zips = vec![];
        for handle in handles.into_iter() {
            zips.push(match handle.join() {
                Ok(zip) => zip,
                Err(_) => bail!(ServiceErrorKind::Thread),
            }?);
        }
        Ok(zips)
    }

    /// Sends a GET request to `url`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - `url` is invalid
    /// - The http access fails
    /// - The response code not in `expected_statuses`
    pub fn http_get_expecting(
        &mut self,
        url: &str,
        expected_statuses: &[u16],
    ) -> ServiceResult<Response> {
        let url = resolve_url(self.base_domain, self.base_https, url)?;
        let expected_statuses = statuses_from(expected_statuses);
        self.assert_not_forbidden_by_robots_txt(&url)?;
        print_bold!(None, "GET");
        print_and_flush!(" {} ... ", url);
        let response = self.reqwest_client
            .get(url.as_ref())
            .header(self.cookie_jar.as_cookie_header())
            .send()?;
        self.cookie_jar.update(&response)?;
        response.print_status(&expected_statuses);
        response.filter_by_status(expected_statuses)
    }

    /// Sends a POST request, serializing `body`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - `url` is invalid
    /// - The http access or serialization fails
    /// - The response code differs from `expected_status`
    ///
    /// # Panics
    ///
    /// Panics when `expected_status` is invalid.
    pub fn http_post_urlencoded<T: Serialize>(
        &mut self,
        url: &str,
        body: T,
        expected_status: u16,
    ) -> ServiceResult<Response> {
        self.http_post(
            url,
            serde_urlencoded::to_string(body)?,
            &[expected_status],
            ContentType::form_url_encoded(),
            &[],
        )
    }

    /// Sends a POST request, serializing `data` and appending a "X-CSRF-Token" header.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - `url` is invalid
    /// - The http access or serialization fails
    /// - The response code differs from `expected_status`
    ///
    /// # Panics
    ///
    /// Panics when `expected_status` is invalid.
    pub fn http_post_json_with_csrf_token<T: Serialize>(
        &mut self,
        url: &str,
        body: T,
        expected_status: u16,
        csrf_token: String,
    ) -> ServiceResult<Response> {
        self.http_post(
            url,
            serde_json::to_string(&body)?,
            &[expected_status],
            ContentType::json(),
            &[("X-CSRF-Token", csrf_token)],
        )
    }

    fn http_post(
        &mut self,
        url: &str,
        body: String,
        expected_statuses: &[u16],
        content_type: ContentType,
        extra_headers: &[(&'static str, String)],
    ) -> ServiceResult<Response> {
        let url = resolve_url(self.base_domain, self.base_https, url)?;
        let expected_statuses = statuses_from(expected_statuses);
        self.assert_not_forbidden_by_robots_txt(&url)?;
        print_bold!(None, "POST");
        print_and_flush!(" {} ... ", url);
        let response = self.reqwest_client
            .post(url.as_ref())
            .body(body)
            .headers({
                let mut headers = Headers::new();
                headers.set(self.cookie_jar.as_cookie_header());
                headers.set(content_type);
                for &(name, ref value) in extra_headers {
                    headers.set_raw(name, value.as_str());
                }
                headers
            })
            .send()?;
        self.cookie_jar.update(&response)?;
        response.print_status(&expected_statuses);
        response.filter_by_status(expected_statuses)
    }

    fn assert_not_forbidden_by_robots_txt(&self, url: &Url) -> ServiceResult<()> {
        if let Some(domain) = url.domain() {
            if let Some(robots_txt) = self.robots_txts.get(domain) {
                let robots = Robots::from_str(robots_txt);
                let matcher = SimpleMatcher::new(&robots.choose_section(USER_AGENT).rules);
                if !matcher.check_path(url.path()) {
                    bail!(ServiceErrorKind::ForbiddenByRobotsTxt(url.clone()));
                }
            }
        }
        Ok(())
    }
}

fn resolve_url(
    base_domain: &'static str,
    base_https: bool,
    relative_or_absolute_url: &str,
) -> Result<Url, UrlError> {
    let base_https = if base_https { "s" } else { "" };
    let joined = format!(
        "http{}://{}{}",
        base_https, base_domain, relative_or_absolute_url
    );
    Url::parse(if relative_or_absolute_url.chars().next() == Some('/') {
        &joined
    } else {
        relative_or_absolute_url
    })
}

fn statuses_from(from: &[u16]) -> Vec<StatusCode> {
    from.iter()
        .map(|&n| StatusCode::try_from(n).unwrap_or(StatusCode::Unregistered(n)))
        .collect::<Vec<StatusCode>>()
}

trait CookieJarExt {
    fn as_cookie_header(&self) -> header::Cookie;
    fn update(&mut self, response: &Response) -> Result<(), cookie::ParseError>;
}

impl CookieJarExt for CookieJar {
    fn as_cookie_header(&self) -> header::Cookie {
        self.iter()
            .fold(header::Cookie::new(), |mut header, cookie| {
                header.append(cookie.name().to_owned(), cookie.value().to_owned());
                header
            })
    }

    fn update(&mut self, response: &Response) -> Result<(), cookie::ParseError> {
        if let Some(setcookie) = response.headers().get::<SetCookie>() {
            for cookie in setcookie.iter() {
                self.add(cookie::Cookie::parse(cookie.to_string())?);
            }
        }
        Ok(())
    }
}

trait ResponseExt
where
    Self: Sized,
{
    fn print_status(&self, expected_statuses: &[StatusCode]);
    fn filter_by_status(self, expected_statuses: Vec<StatusCode>) -> ServiceResult<Self>;
}

impl ResponseExt for Response {
    fn print_status(&self, expected_statuses: &[StatusCode]) {
        let color = if expected_statuses.contains(&self.status()) {
            color::GREEN
        } else {
            color::RED
        };
        println_bold!(Some(color), "{}", self.status());
    }

    fn filter_by_status(self, expected_statuses: Vec<StatusCode>) -> ServiceResult<Self> {
        if expected_statuses.contains(&self.status()) {
            Ok(self)
        } else {
            bail!(ServiceErrorKind::UnexpectedHttpCode(
                expected_statuses,
                self.status(),
            ))
        }
    }
}
