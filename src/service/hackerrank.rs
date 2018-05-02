use errors::ServiceResult;
use service::{DownloadProp, DownloadZips, OpenInBrowser};
use testsuite::{SuiteFilePath, TestSuite};
use util;

use httpsession::{HttpSession, Response};
use httpsession::header::Headers;
use regex::Regex;
use select::document::Document;
use select::predicate::Attr;
use serde_json;
use zip::ZipArchive;
use zip::result::ZipResult;

use std::io::{self, Read, Seek};

pub fn login() -> ServiceResult<()> {
    HackerRank::start(true).map(|_| ())
}

pub fn download(prop: &DownloadProp<&str>) -> ServiceResult<()> {
    HackerRank::start(false)?.download(prop)
}

custom_derive! {
    #[derive(NewtypeDeref, NewtypeDerefMut)]
    struct HackerRank(HttpSession);
}

impl HackerRank {
    fn start(prints_message_when_already_logged_in: bool) -> ServiceResult<Self> {
        let cookie_path =
            util::fs::join_from_home(&[".local", "share", "snowchains", "hackerrank"])?;
        let mut hackerrank = HackerRank(super::start_session("www.hackerrank.com", cookie_path)?);
        let mut response = hackerrank.get_expecting("/login", &[200, 302])?;
        if response.status().as_u16() == 302 && prints_message_when_already_logged_in {
            eprintln!("Already signed in.");
        } else if response.status().as_u16() == 200 {
            loop {
                if hackerrank.try_logging_in(response)? {
                    break println!("Succeeded to login.");
                }
                eprintln!("Failed to login. Try again.");
                hackerrank.clear_cookies()?;
                response = hackerrank.get("/login")?;
            }
        }
        Ok(hackerrank)
    }

    fn try_logging_in(&mut self, html: Response) -> ServiceResult<bool> {
        #[derive(Serialize)]
        struct PostData {
            login: String,
            password: String,
            remember_me: bool,
        }

        #[derive(Deserialize)]
        struct ResponseData {
            status: bool,
        }

        let (username, password) = super::ask_username_and_password("Username: ")?;
        let csrf_token = extract_csrf_token(html)?;
        let data = PostData {
            login: username,
            password,
            remember_me: true,
        };
        let response = self.post_json("/auth/login", &data, &[200], {
            let mut headers = Headers::new();
            headers.set_raw("X-CSRF-Token", csrf_token);
            headers
        })?;
        Ok(serde_json::from_reader::<_, ResponseData>(response)?.status)
    }

    fn download(&mut self, prop: &DownloadProp<&str>) -> ServiceResult<()> {
        #[derive(Deserialize)]
        struct Challenges {
            models: Vec<Model>,
        }

        #[derive(Deserialize)]
        struct Model {
            slug: String,
        }

        let (contest, dir_to_save, extension, open_browser) = prop.values();
        let url = format!("/rest/contests/{}/challenges", contest);
        let (mut zip_urls, mut paths, mut urls) = (vec![], vec![], vec![]);
        for slug in serde_json::from_reader::<_, Challenges>(self.get(&url)?)?
            .models
            .into_iter()
            .map(|model| model.slug)
        {
            zip_urls.push(format!("{}/{}/download_testcases", url, slug));
            paths.push(SuiteFilePath::new(dir_to_save, &slug, extension));
            urls.push(format!(
                "https://www.hackerrank.com/{}/challenges/{}",
                contest, slug
            ));
        }
        let zips = self.download_zips(io::stdout(), 50 * 1024 * 1024, &zip_urls)?;
        println!("Extracting zip files...");
        let extracted = zips.into_iter()
            .map(extract_samples_from_zip)
            .collect::<Result<Vec<_>, _>>()?;
        for (suite, path) in extracted.into_iter().zip(paths) {
            suite.save(&path, true)?;
        }
        if open_browser {
            for url in &urls {
                self.open_in_browser(url)?;
            }
        }
        Ok(())
    }
}

fn extract_csrf_token(html: Response) -> ServiceResult<String> {
    fn extract(document: &Document) -> Option<String> {
        document
            .find(Attr("name", "csrf-token"))
            .next()?
            .attr("content")
            .map(str::to_owned)
    }

    super::quit_on_failure(extract(&Document::from_read(html)?), String::is_empty)
}

fn extract_samples_from_zip<R: Read + Seek>(zip: ZipArchive<R>) -> ZipResult<TestSuite> {
    lazy_static! {
        static ref IN_REGEX: Regex = Regex::new(r"input/input[0-9]+\.txt").unwrap();
        static ref OUT_REGEX: Regex = Regex::new(r"output/output[0-9]+\.txt").unwrap();
    }
    let mut zip = zip;
    let (mut inputs, mut outputs) = (vec![], vec![]);
    for i in 0..zip.len() {
        let file = zip.by_index(i)?;
        let size = file.size() as usize;
        if IN_REGEX.is_match(file.name()) {
            inputs.push(util::string_from_read(file, size)?);
        } else if OUT_REGEX.is_match(file.name()) {
            outputs.push(util::string_from_read(file, size)?);
        }
    }
    Ok(TestSuite::simple(
        None,
        None,
        None,
        outputs.into_iter().zip(inputs).collect(),
    ))
}
