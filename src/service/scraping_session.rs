use error::{ServiceErrorKind, ServiceResult};
use util;

use cookie::{self, Cookie, CookieJar};
use reqwest::{Client, IntoUrl, RedirectPolicy, Response, StatusCode};
use reqwest::header::{ContentType, Cookie as RequestCookie, SetCookie, UserAgent};
use rusqlite::Connection;
use serde::Serialize;
use serde_urlencoded;
use std::fmt;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use term::{Attr, color};


pub struct ScrapingSession {
    sqlite_path: PathBuf,
    sqlite_connection: Option<Connection>,
    client: Client,
    cookie_jar: CookieJar,
}

impl ScrapingSession {
    /// Crates a new `ScrapingSession`.
    pub fn new(sqlite_name: &'static str) -> ServiceResult<Self> {
        let path = get_schema_path_creating_dirs(sqlite_name)?;
        let client = Client::builder()?.redirect(RedirectPolicy::none()).build()?;
        Ok(Self {
            sqlite_path: path,
            sqlite_connection: None,
            client: client,
            cookie_jar: CookieJar::new(),
        })
    }

    /// Creates a new `ScrapingSession` connecting `~/.local/share/snowchains/<name>`.
    pub fn from_db(sqlite_name: &'static str) -> ServiceResult<Self> {
        let mut session = Self::new(sqlite_name)?;
        let conn = connect_and_try_crating_table(&session.sqlite_path)?;
        let cookie_jar = {
            let mut cookie_jar = CookieJar::new();
            let mut stmt = conn.prepare("SELECT cookie FROM cookies")?;
            for cookie in stmt.query_map::<String, _>(&[], |row| row.get(0))? {
                cookie_jar.add(Cookie::parse(cookie?)?);
            }
            cookie_jar
        };
        session.sqlite_connection = Some(conn);
        session.cookie_jar = cookie_jar;
        Ok(session)
    }

    /// Save all cookies to the sqlite database, erasing previous cookies.
    pub fn save_cookie_to_db(self) -> ServiceResult<()> {
        let conn = match self.sqlite_connection {
            Some(conn) => conn,
            None => connect_and_try_crating_table(&self.sqlite_path)?,
        };
        conn.execute("DELETE FROM cookies", &[])?;
        for cookie in self.cookie_jar.iter().map(Cookie::to_string) {
            conn.execute(
                "INSERT INTO cookies (cookie) VALUES (?1)",
                &[&cookie],
            )?;
        }
        let path = self.sqlite_path.display();
        match conn.close() {
            Ok(()) => println!("Closed {}", path),
            Err(_) => eprint_decorated!(Attr::Bold, Some(color::RED), "Failed to close {}", path),
        }
        Ok(())
    }

    /// Whether `self` has no cookie.
    pub fn no_cookie(&self) -> bool {
        self.cookie_jar.iter().next().is_none()
    }

    /// Delete all cookies.
    pub fn clear_cookies(&mut self) {
        self.cookie_jar = CookieJar::new();
    }

    /// Sends a GET request to `url`, expecting the response code is 200.
    ///
    /// # Errors
    ///
    /// Returns `Err` if an IO error occurs, or the response code is not 200.
    ///
    /// # Panics
    ///
    /// Panics when `url` is invalid.
    pub fn http_get<U>(&mut self, url: U) -> ServiceResult<Response>
    where
        U: Clone + fmt::Display + IntoUrl,
    {
        self.http_get_expecting(url, StatusCode::Ok)
    }

    /// Sends a GET request to `url`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if an IO error occurs, or the response code differs from `expected_status`.
    ///
    /// # Panics
    ///
    /// Panics when `url` is invalid.
    pub fn http_get_expecting<U>(
        &mut self,
        url: U,
        expected_status: StatusCode,
    ) -> ServiceResult<Response>
    where
        U: Clone + fmt::Display + IntoUrl,
    {
        print_decorated!(Attr::Bold, None, "GET ");
        print_and_flush!("{} ... ", url);

        let response = self.client
            .get(url.clone())?
            .header(user_agent())
            .header(self.cookie_jar.as_request_cookie())
            .send()?;

        self.add_setcookie_to_jar(&response)?;

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

    /// Sends a POST request, serializing `data`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if an IO error occurs, or the response code differs from `expected_status`.
    ///
    /// # Panics
    ///
    /// Panics when `url` is invalid.
    pub fn http_post_urlencoded<U, T>(
        &mut self,
        url: U,
        data: T,
        expected_status: StatusCode,
    ) -> ServiceResult<Response>
    where
        U: Clone + fmt::Display + IntoUrl,
        T: Serialize,
    {
        self.http_post(
            url,
            serde_urlencoded::to_string(data)?,
            expected_status,
            ContentType::form_url_encoded(),
        )
    }

    fn http_post<U: Clone + fmt::Display + IntoUrl>(
        &mut self,
        url: U,
        data: String,
        expected_status: StatusCode,
        content_type: ContentType,
    ) -> ServiceResult<Response> {
        print_decorated!(Attr::Bold, None, "POST ");
        print_and_flush!("{} ... ", url);

        let response = self.client
            .post(url.clone())?
            .body(data)
            .header(user_agent())
            .header(self.cookie_jar.as_request_cookie())
            .header(content_type)
            .send()?;

        self.add_setcookie_to_jar(&response)?;

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


fn get_schema_path_creating_dirs(name: &str) -> io::Result<PathBuf> {
    let mut path = util::home_dir_as_io_result()?;
    path.push(".local");
    path.push("share");
    path.push("snowchains");
    fs::create_dir_all(&path)?;
    path.push(name);
    Ok(path)
}


fn connect_and_try_crating_table(path: &Path) -> ServiceResult<Connection> {
    let conn = Connection::open(&path)?;
    println!("Opened {}", path.display());
    conn.execute(
        "CREATE TABLE IF NOT EXISTS cookies (cookie TEXT NOT NULL)",
        &[],
    )?;
    Ok(conn)
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
