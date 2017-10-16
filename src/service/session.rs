use error::{ServiceErrorKind, ServiceResult};
use util;

use bincode::{self, Infinite};
use cookie::{self, Cookie, CookieJar};
use pbr::{MultiBar, Units};
use reqwest::{Client, IntoUrl, RedirectPolicy, Response, StatusCode};
use reqwest::header::{ContentLength, ContentType, Cookie as RequestCookie, Headers, SetCookie,
                      UserAgent};
use serde::Serialize;
use serde_json;
use serde_urlencoded;
use std::fmt;
use std::io::{Cursor, Read, Write};
use std::path::PathBuf;
use std::thread;
use term::{Attr, color};
use zip::ZipArchive;
use zip::result::ZipResult;


pub struct HttpSession {
    cookie_jar: CookieJar,
    reqwest_client: Client,
    path_to_save_cookies: PathBuf,
}

impl HttpSession {
    /// Creates a new `HttpSession` loading `~/.local/share/snowchains/<file_name>` if it exists.
    pub fn start(file_name: &'static str) -> ServiceResult<Self> {
        let path = {
            let mut path = util::home_dir_as_io_result()?;
            path.push(".local");
            path.push("share");
            path.push("snowchains");
            path.push(file_name);
            path
        };
        let jar = if path.exists() {
            let mut file = util::open_file(&path)?;
            println!("Loaded cookies from {}", path.display());
            let mut jar = CookieJar::new();
            for cookie in bincode::deserialize_from::<_, Vec<String>, _>(&mut file, Infinite)?
                .into_iter()
            {
                jar.add(Cookie::parse(cookie)?);
            }
            jar
        } else {
            println!("{} not found. It will be created.", path.display());
            CookieJar::new()
        };
        let client = Client::builder().redirect(RedirectPolicy::none()).build()?;
        Ok(Self {
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

    /// Delete all cookies.
    pub fn clear_cookies(&mut self) {
        self.cookie_jar = CookieJar::new();
    }

    /// Sends a GET request to `url`, expecting the response code is 200.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the http access fails, or the response code is not 200.
    ///
    /// # Panics
    ///
    /// Panics when `url` is invalid.
    pub fn http_get(&mut self, url: &str) -> ServiceResult<Response> {
        self.http_get_expecting(url, StatusCode::Ok)
    }

    /// Sends a GET request to `url`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the http access fails, or the response code differs from `expected_status`.
    ///
    /// # Panics
    ///
    /// Panics when `url` is invalid.
    pub fn http_get_expecting(
        &mut self,
        url: &str,
        expected_status: StatusCode,
    ) -> ServiceResult<Response> {
        let response = self.send_get(url)?;
        if response.status() == expected_status {
            println_decorated!(Attr::Bold, Some(color::GREEN), "{}", response.status());
            Ok(response)
        } else {
            println_decorated!(Attr::Bold, Some(color::RED), "{}", response.status());
            bail!(ServiceErrorKind::UnexpectedHttpCode(
                vec![expected_status],
                response.status(),
            ))
        }
    }

    /// Sends a GET request to `url` and if the response code is `status1`, returns `Ok(Some)` else
    /// if it is `status2`, returns `Ok(None)`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the http access fails, or the response code is not `status1` nor `status2`.
    ///
    /// # Panics
    ///
    /// Panics when `url` is invalid.
    pub fn http_get_as_opt(
        &mut self,
        url: &str,
        status1: StatusCode,
        status2: StatusCode,
    ) -> ServiceResult<Option<Response>> {
        let response = self.send_get(url)?;
        if response.status() == status1 {
            println_decorated!(Attr::Bold, Some(color::GREEN), "{}", response.status());
            Ok(Some(response))
        } else if response.status() == status2 {
            println_decorated!(Attr::Bold, Some(color::GREEN), "{}", response.status());
            Ok(None)
        } else {
            println_decorated!(Attr::Bold, Some(color::RED), "{}", response.status());
            bail!(ServiceErrorKind::UnexpectedHttpCode(
                vec![status1, status2],
                response.status(),
            ))
        }
    }

    /// Sends GET requests to `urls` expecting the response data are all zips.
    ///
    /// # Errors
    ///
    /// Returns `Err` if:
    /// - Any of http access fails
    /// - Any of response code is not 200
    /// - Any of downloaded zip is invalid
    ///
    /// # Panics
    ///
    /// Panics when any of `urls` is invalid.
    pub fn http_get_zips(
        &mut self,
        urls: &[String],
    ) -> ServiceResult<Vec<ZipArchive<Cursor<Vec<u8>>>>> {
        let mut responses = vec![];
        for url in urls {
            responses.push(self.http_get(&url)?);
        }

        println!("Downloading...");
        let mut mb = MultiBar::new();
        let handles = responses
            .into_iter()
            .map(|mut response| {
                let content_length = response.headers().get::<ContentLength>().map(|l| **l);
                let mut pb = mb.create_bar(content_length.unwrap_or(0));
                pb.set_units(Units::Bytes);
                thread::spawn(move || -> ZipResult<_> {
                    let mut cursor = Cursor::new(Vec::with_capacity(
                        content_length.unwrap_or(50 * 1024 * 1024) as usize,
                    ));
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
    /// Returns `Err` if the http access or serialization fails, or the response code differs from
    /// `expected_status`.
    ///
    /// # Panics
    ///
    /// Panics when `url` is invalid.
    pub fn http_post_urlencoded<T: Serialize>(
        &mut self,
        url: &str,
        data: T,
        expected_status: StatusCode,
    ) -> ServiceResult<Response> {
        self.http_post(
            url,
            serde_urlencoded::to_string(data)?,
            expected_status,
            ContentType::form_url_encoded(),
            &[],
        )
    }

    /// Sends a POST request, serializing `data` and appending a "X-CSRF-Token" header.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the http access or serialization fails, or the response code differs from
    /// `expected_status`.
    ///
    /// # Panics
    ///
    /// Panics when `url` is invalid.
    pub fn http_post_json_with_csrf_token<T: Serialize>(
        &mut self,
        url: &str,
        data: T,
        expected_status: StatusCode,
        csrf_token: String,
    ) -> ServiceResult<Response> {
        self.http_post(
            url,
            serde_json::to_string(&data)?,
            expected_status,
            ContentType::json(),
            &[("X-CSRF-Token", csrf_token)],
        )
    }

    fn send_get<U: Clone + fmt::Display + IntoUrl>(&mut self, url: U) -> ServiceResult<Response> {
        print_decorated!(Attr::Bold, None, "GET ");
        print_and_flush!("{} ... ", url);
        let response = self.reqwest_client
            .get(url.clone())
            .header(user_agent())
            .header(self.cookie_jar.as_request_cookie())
            .send()?;
        self.add_setcookie_to_jar(&response)?;
        Ok(response)
    }

    fn http_post<U: Clone + fmt::Display + IntoUrl>(
        &mut self,
        url: U,
        data: String,
        expected_status: StatusCode,
        content_type: ContentType,
        extra_headers: &[(&'static str, String)],
    ) -> ServiceResult<Response> {
        print_decorated!(Attr::Bold, None, "POST ");
        print_and_flush!("{} ... ", url);

        let mut headers = Headers::new();
        headers.set(user_agent());
        headers.set(self.cookie_jar.as_request_cookie());
        headers.set(content_type);
        for &(header_name, ref header_value) in extra_headers {
            headers.set_raw(header_name, header_value.as_str());
        }
        let response = self.reqwest_client
            .post(url.clone())
            .body(data)
            .headers(headers)
            .send()?;

        self.add_setcookie_to_jar(&response)?;

        if response.status() == expected_status {
            println_decorated!(Attr::Bold, Some(color::GREEN), "{}", response.status());
            Ok(response)
        } else {
            println_decorated!(Attr::Bold, Some(color::RED), "{}", response.status());
            bail!(ServiceErrorKind::UnexpectedHttpCode(
                vec![expected_status],
                response.status(),
            ))
        }
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


fn user_agent() -> UserAgent {
    UserAgent::new("snowchains <https://github.com/wariuni/snowchains>")
}


trait AsRequestCookie {
    fn as_request_cookie(&self) -> RequestCookie;
}

impl AsRequestCookie for CookieJar {
    fn as_request_cookie(&self) -> RequestCookie {
        let mut request_cookie = RequestCookie::new();
        for cookie in self.iter() {
            request_cookie.append(cookie.name().to_owned(), cookie.value().to_owned());
        }
        request_cookie
    }
}
