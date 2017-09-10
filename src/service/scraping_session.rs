use super::super::error::{ServiceErrorKind, ServiceResult};
use super::super::util;
use cookie::{Cookie, CookieJar};
use reqwest::{Client, IntoUrl, RedirectPolicy, Response, StatusCode};
use reqwest::header::{ContentType, Cookie as RequestCookie, SetCookie, UserAgent};
use serde::Serialize;
use serde_json;
use serde_urlencoded;
use std::fmt::Display;
use std::fs::{self, File};
use std::io::{self, Write};
use term::{Attr, color};


pub struct ScrapingSession {
    cookie_jar: CookieJar,
}

impl ScrapingSession {
    pub fn new() -> Self {
        Self { cookie_jar: CookieJar::new() }
    }

    pub fn from_cookie_file(name_without_extension: &str) -> ServiceResult<Self> {
        let file = {
            let mut pathbuf = util::home_dir_as_io_result()?;
            pathbuf.push(".local");
            pathbuf.push("share");
            pathbuf.push("snowchains");
            pathbuf.push(name_without_extension);
            pathbuf.set_extension("jar");
            File::open(pathbuf)?
        };
        let mut cookie_jar = CookieJar::new();
        for cookie in serde_json::from_reader::<_, Vec<String>>(file)?.into_iter() {
            cookie_jar.add(Cookie::parse(cookie)?);
        }
        Ok(Self { cookie_jar: cookie_jar })
    }

    pub fn save_cookie_to_file(&self, name_without_extension: &str) -> io::Result<()> {
        let (mut file, pathbuf) = {
            let mut pathbuf = util::home_dir_as_io_result()?;
            pathbuf.push(".local");
            pathbuf.push("share");
            pathbuf.push("snowchains");
            fs::create_dir_all(&pathbuf)?;
            pathbuf.push(name_without_extension);
            pathbuf.set_extension("jar");
            (File::create(&pathbuf)?, pathbuf)
        };
        let cookies = self.cookie_jar
            .iter()
            .map(|c| c.to_string())
            .collect::<Vec<_>>();
        let cookies_text = &serde_json::to_vec::<Vec<String>>(&cookies)?;
        file.write_all(cookies_text)?;
        Ok(println!("The cookie was saved to {}", pathbuf.display()))
    }

    pub fn cookie_value(&self, name: &str) -> Option<&str> {
        self.cookie_jar.get(name).map(Cookie::value)
    }

    /// Panics when `url` is invalid.
    pub fn http_get<U: Clone + Display + IntoUrl>(&mut self, url: U) -> ServiceResult<Response> {
        self.http_get_expecting(url, StatusCode::Ok)
    }

    /// Panics when `url` is invalid.
    pub fn http_get_expecting<U: Clone + Display + IntoUrl>(
        &mut self,
        url: U,
        expected_status: StatusCode,
    ) -> ServiceResult<Response> {
        print_decorated!(Attr::Bold, None, "GET ");
        print_and_flush!("{} ... ", url);

        let response = {
            let client = Client::builder()?.redirect(RedirectPolicy::none()).build()?;
            client
                .get(url.clone())?
                .header(UserAgent::new(format!(
                    "snowchains <https://github.com/wariuni/snowchains>"
                )))
                .header(self.cookie_jar.as_request_cookie())
                .send()?
        };

        for cookie in response
            .headers()
            .get::<SetCookie>()
            .map(|setcookie| setcookie.iter())
            .unwrap_or(vec![].iter())
        {
            self.cookie_jar.add(Cookie::parse(cookie.to_string())?);
        }

        if response.status() == expected_status {
            println_decorated!(Attr::Bold, Some(color::GREEN), "{}", response.status());
            Ok(response)
        } else {
            println_decorated!(Attr::Bold, Some(color::RED), "{}", response.status());
            bail!(ServiceErrorKind::UnexpectedHttpCode(
                expected_status,
                response.status(),
            ))
        }
    }

    /// Panics when `url` is invalid.
    pub fn http_post_urlencoded<U, T>(
        &mut self,
        url: U,
        data: T,
        expected_status: StatusCode,
    ) -> ServiceResult<Response>
    where
        U: Clone + Display + IntoUrl,
        T: Serialize,
    {
        self.http_post(
            url,
            serde_urlencoded::to_string(data)?,
            expected_status,
            ContentType::form_url_encoded(),
        )
    }

    fn http_post<U: Clone + Display + IntoUrl>(
        &mut self,
        url: U,
        data: String,
        expected_status: StatusCode,
        content_type: ContentType,
    ) -> ServiceResult<Response> {
        print_decorated!(Attr::Bold, None, "POST ");
        print_and_flush!("{} ... ", url);
        let response = {
            let client = Client::builder()?.redirect(RedirectPolicy::none()).build()?;
            client
                .post(url.clone())?
                .body(data)
                .header(UserAgent::new(format!(
                    "snowchains <https://github.com/wariuni/snowchains>"
                )))
                .header(self.cookie_jar.as_request_cookie())
                .header(content_type)
                .send()?
        };

        for cookie in response
            .headers()
            .get::<SetCookie>()
            .map(|setcookie| setcookie.iter())
            .unwrap_or(vec![].iter())
        {
            self.cookie_jar.add(Cookie::parse(cookie.to_string())?);
        }

        if response.status() == expected_status {
            println_decorated!(Attr::Bold, Some(color::GREEN), "{}", response.status());
            Ok(response)
        } else {
            println_decorated!(Attr::Bold, Some(color::RED), "{}", response.status());
            bail!(ServiceErrorKind::UnexpectedHttpCode(
                expected_status,
                response.status(),
            ))
        }
    }
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
