use error::{ServiceErrorKind, ServiceResult};
use util;

use bincode::{self, Infinite};
use cookie::{self, Cookie, CookieJar};
use pbr::{MultiBar, Units};
use reqwest::{Client, RedirectPolicy, Response, StatusCode};
use reqwest::header::{ContentLength, ContentType, Cookie as RequestCookie, Headers, SetCookie,
                      UserAgent};
use serde::Serialize;
use serde_json;
use serde_urlencoded;
use term::color;
use zip::ZipArchive;
use zip::result::ZipResult;

use std::borrow::Cow;
use std::io::{Cursor, Read, Write};
use std::path::PathBuf;
use std::thread;

pub struct HttpSession {
    base_url: &'static str,
    cookie_jar: CookieJar,
    reqwest_client: Client,
    path_to_save_cookies: PathBuf,
}

impl HttpSession {
    /// Creates a new `HttpSession` loading a file in `~/.local/share/snowchains/` if it exists.
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
            println!("{} not found. It will be created.", path.display());
            CookieJar::new()
        };
        let client = Client::builder().redirect(RedirectPolicy::none()).build()?;
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

    /// Sends a GET request to `url`, expecting the response code is 200.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - `url` is invalid
    /// - The http access fails
    /// - The response code is not 200
    pub fn http_get(&mut self, url: &str) -> ServiceResult<Response> {
        self.send_get(url, StatusCode::Ok, &[StatusCode::Ok])
    }

    /// Sends a GET request to `url`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - `url` is invalid
    /// - The http access fails
    /// - The response code differs from `expected_status`
    pub fn http_get_expecting(
        &mut self,
        url: &str,
        expected_status: u16,
    ) -> ServiceResult<Response> {
        let expected_status = status_from_u16(expected_status)?;
        self.send_get(url, expected_status, &[expected_status])
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
    pub fn http_get_as_opt(
        &mut self,
        url: &str,
        status1: u16,
        status2: u16,
    ) -> ServiceResult<Option<Response>> {
        let (status1, status2) = (status_from_u16(status1)?, status_from_u16(status2)?);
        let response = self.send_get(url, status1, &[status1, status2])?;
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
    pub fn http_post_urlencoded<T: Serialize>(
        &mut self,
        url: &str,
        data: T,
        expected_status: u16,
    ) -> ServiceResult<Response> {
        self.send_post(
            url,
            serde_urlencoded::to_string(data)?,
            status_from_u16(expected_status)?,
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
    pub fn http_post_json_with_csrf_token<T: Serialize>(
        &mut self,
        url: &str,
        data: T,
        expected_status: u16,
        csrf_token: String,
    ) -> ServiceResult<Response> {
        self.send_post(
            url,
            serde_json::to_string(&data)?,
            status_from_u16(expected_status)?,
            ContentType::json(),
            &[("X-CSRF-Token", csrf_token)],
        )
    }

    fn send_get(
        &mut self,
        url: &str,
        expected_status: StatusCode,
        acceptable_statuses: &[StatusCode],
    ) -> ServiceResult<Response> {
        let url = to_absolute(self.base_url, url);
        print_bold!(None, "GET");
        print_and_flush!(" {} ... ", url);
        let response = self.reqwest_client
            .get(url.as_ref())
            .header(user_agent())
            .header(self.cookie_jar.as_request_cookie())
            .send()?;
        self.add_setcookie_to_jar(&response)?;
        response.print_status(expected_status, acceptable_statuses);
        response.filter_by_status(acceptable_statuses)
    }

    fn send_post(
        &mut self,
        url: &str,
        data: String,
        acceptable_status: StatusCode,
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
                headers.set(user_agent());
                headers.set(self.cookie_jar.as_request_cookie());
                headers.set(content_type);
                for &(header_name, ref header_value) in extra_headers {
                    headers.set_raw(header_name, header_value.as_str());
                }
                headers
            })
            .send()?;
        self.add_setcookie_to_jar(&response)?;
        response.print_status(acceptable_status, &[]);
        response.filter_by_status(&[acceptable_status])
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

fn status_from_u16(number: u16) -> ServiceResult<StatusCode> {
    StatusCode::try_from(number).map_err(|_| ServiceErrorKind::InvalidStatusCode(number).into())
}

fn user_agent() -> UserAgent {
    UserAgent::new("snowchains <https://github.com/wariuni/snowchains>")
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
    fn print_status(&self, expected_status: StatusCode, other_acceptable_statuses: &[StatusCode]);
    fn filter_by_status(self, acceptable_statuses: &[StatusCode]) -> ServiceResult<Self>;
}

impl ResponseExt for Response {
    fn print_status(&self, expected_status: StatusCode, other_acceptable_statuses: &[StatusCode]) {
        let color = if self.status() == expected_status {
            color::GREEN
        } else if other_acceptable_statuses.contains(&self.status()) {
            color::YELLOW
        } else {
            color::RED
        };
        println_bold!(Some(color), "{}", self.status());
    }

    fn filter_by_status(self, acceptable_statuses: &[StatusCode]) -> ServiceResult<Self> {
        if acceptable_statuses.contains(&self.status()) {
            Ok(self)
        } else {
            bail!(ServiceErrorKind::UnexpectedHttpCode(
                acceptable_statuses.iter().cloned().collect(),
                self.status(),
            ))
        }
    }
}
