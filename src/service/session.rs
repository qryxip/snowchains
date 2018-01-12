use errors::{ServiceErrorKind, ServiceResult};
use util;

use bincode::{self, Infinite};
use cookie::{self, Cookie, CookieJar};
use pbr::{MultiBar, Units};
use reqwest::{Client, RedirectPolicy, Response, StatusCode};
use reqwest::header::{ContentLength, ContentType, Cookie as RequestCookie, Headers, SetCookie,
                      UserAgent};
use robots_txt::{Robots, SimpleMatcher};
use serde::Serialize;
use serde_json;
use serde_urlencoded;
use term::color;
use webbrowser;
use zip::ZipArchive;
use zip::result::ZipResult;

use std::borrow::Cow;
use std::io::{Cursor, Read, Write};
use std::path::PathBuf;
use std::thread;

static USER_AGENT: &'static str = "snowchains <https://github.com/wariuni/snowchains>";

pub fn assert_not_forbidden_by_robots_txt(
    url: &'static str,
    paths: &'static [&'static str],
) -> ServiceResult<()> {
    let client = Client::builder().redirect(RedirectPolicy::none()).build()?;
    print_bold!(None, "GET");
    print_and_flush!(" {} ... ", url);
    let response = client.get(url).header(UserAgent::new(USER_AGENT)).send()?;
    response.print_status(&[StatusCode::Ok, StatusCode::NotFound]);
    let response = response.filter_by_status(&[StatusCode::Ok, StatusCode::NotFound])?;
    if response.status() == StatusCode::Ok {
        let text = util::string_from_read(response)?;
        let robots = Robots::from_str(&text);
        let matcher = SimpleMatcher::new(&robots.choose_section(USER_AGENT).rules);
        if !paths.iter().all(|path| matcher.check_path(path)) {
            bail!(ServiceErrorKind::ForbiddenByRobotsTxt);
        }
    }
    Ok(())
}

pub struct HttpSession {
    base_url: &'static str,
    cookie_jar: CookieJar,
    reqwest_client: Client,
    path_to_save_cookies: PathBuf,
}

impl HttpSession {
    /// Creates a new `HttpSession` loading `~/.local/share/snowchains/<file_name>` if it exists.
    pub fn start(file_name: &'static str, base_url: &'static str) -> ServiceResult<Self> {
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
        let mut headers = Headers::new();
        headers.set(UserAgent::new(USER_AGENT));
        let client = Client::builder()
            .redirect(RedirectPolicy::none())
            .default_headers(headers)
            .build()?;
        Ok(Self {
            base_url: base_url,
            cookie_jar: jar,
            reqwest_client: client,
            path_to_save_cookies: path,
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
        let url = to_absolute(self.base_url, url);
        println!("Opening {} in default browser...", url);
        let status = webbrowser::open(&url)?.status;
        if !status.success() {
            bail!(ServiceErrorKind::Webbrowser(status));
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
        self.send_get(url, &[StatusCode::Ok])
    }

    /// Sends a GET request to `url`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - `url` is invalid
    /// - The http access fails
    /// - The response code differs from `expected_status`
    ///
    /// # Panics
    ///
    /// Panics when `expected_status` is invalid.
    pub fn http_get_expecting(
        &mut self,
        url: &str,
        expected_status: &'static u16,
    ) -> ServiceResult<Response> {
        let expected_status = status_from_u16(expected_status);
        self.send_get(url, &[expected_status])
    }

    /// Sends a GET request to `url` and if the response code is `status1`, returns `Ok(Some)` else
    /// if it is `status2`, returns `Ok(None)`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - `url` is invalid
    /// - The http access fails
    /// - The response code is not `status1` nor `status2`.
    ///
    /// # Panics
    ///
    /// Panics when `status1` or `status2` is invalid.
    pub fn http_get_as_opt(
        &mut self,
        url: &str,
        status1: &'static u16,
        status2: &'static u16,
    ) -> ServiceResult<Option<Response>> {
        let (status1, status2) = (status_from_u16(status1), status_from_u16(status2));
        let response = self.send_get(url, &[status1, status2])?;
        Ok(Some(response)
            .into_iter()
            .find(|response| response.status() == status1))
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

    /// Sends a POST request, serializing `data`.
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
        data: T,
        expected_status: &'static u16,
    ) -> ServiceResult<Response> {
        self.send_post(
            url,
            serde_urlencoded::to_string(data)?,
            status_from_u16(expected_status),
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
        data: T,
        expected_status: &'static u16,
        csrf_token: String,
    ) -> ServiceResult<Response> {
        self.send_post(
            url,
            serde_json::to_string(&data)?,
            status_from_u16(expected_status),
            ContentType::json(),
            &[("X-CSRF-Token", csrf_token)],
        )
    }

    fn send_get(&mut self, url: &str, expected_statuses: &[StatusCode]) -> ServiceResult<Response> {
        let url = to_absolute(self.base_url, url);
        print_bold!(None, "GET");
        print_and_flush!(" {} ... ", url);
        let response = self.reqwest_client
            .get(url.as_ref())
            .header(self.cookie_jar.as_request_cookie())
            .send()?;
        self.add_setcookie_to_jar(&response)?;
        response.print_status(expected_statuses);
        response.filter_by_status(expected_statuses)
    }

    fn send_post(
        &mut self,
        url: &str,
        data: String,
        expected_status: StatusCode,
        content_type: ContentType,
        extra_headers: &[(&'static str, String)],
    ) -> ServiceResult<Response> {
        let url = to_absolute(self.base_url, url);
        print_bold!(None, "POST");
        print_and_flush!(" {} ... ", url);
        let response = self.reqwest_client
            .post(url.as_ref())
            .body(data)
            .headers({
                let mut headers = Headers::new();
                headers.set(self.cookie_jar.as_request_cookie());
                headers.set(content_type);
                for &(name, ref value) in extra_headers {
                    headers.set_raw(name, value.as_str());
                }
                headers
            })
            .send()?;
        self.add_setcookie_to_jar(&response)?;
        response.print_status(&[expected_status]);
        response.filter_by_status(&[expected_status])
    }

    fn add_setcookie_to_jar(&mut self, response: &Response) -> Result<(), cookie::ParseError> {
        for cookie in response
            .headers()
            .get::<SetCookie>()
            .map(|setcookie| setcookie.iter())
            .unwrap_or(vec![].iter())
        {
            self.cookie_jar.add(Cookie::parse(cookie.to_string())?);
        }
        Ok(())
    }
}

fn to_absolute<'a>(base_url: &'static str, relative_or_absolute_url: &'a str) -> Cow<'a, str> {
    if relative_or_absolute_url.chars().next() == Some('/') {
        Cow::Owned(format!("{}{}", base_url, relative_or_absolute_url))
    } else {
        Cow::Borrowed(relative_or_absolute_url)
    }
}

fn status_from_u16(number: &'static u16) -> StatusCode {
    StatusCode::try_from(*number).unwrap()
}

trait AsRequestCookie {
    fn as_request_cookie(&self) -> RequestCookie;
}

impl AsRequestCookie for CookieJar {
    fn as_request_cookie(&self) -> RequestCookie {
        self.iter()
            .fold(RequestCookie::new(), |mut header, cookie| {
                header.append(cookie.name().to_owned(), cookie.value().to_owned());
                header
            })
    }
}

trait ResponseExt
where
    Self: Sized,
{
    fn print_status(&self, expected_statuses: &[StatusCode]);
    fn filter_by_status(self, expected_statuses: &[StatusCode]) -> ServiceResult<Self>;
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

    fn filter_by_status(self, expected_statuses: &[StatusCode]) -> ServiceResult<Self> {
        if expected_statuses.contains(&self.status()) {
            Ok(self)
        } else {
            bail!(ServiceErrorKind::UnexpectedHttpCode(
                expected_statuses.iter().cloned().collect(),
                self.status(),
            ))
        }
    }
}
