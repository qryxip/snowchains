use error::ServiceResult;
use service::session::HttpSession;
use testsuite::{SuiteFileExtension, SuiteFilePath, TestSuite};
use util;

use regex::Regex;
use reqwest::Response;
use select::document::Document;
use select::predicate::Attr;
use serde_json;
use std::io::{Read, Seek};
use std::path::Path;
use zip::ZipArchive;
use zip::result::ZipResult;


pub fn login() -> ServiceResult<()> {
    HackerRank::start(true)?.save()
}


pub fn download(
    contest: &str,
    dir_to_save: &Path,
    extension: SuiteFileExtension,
    open_browser: bool,
) -> ServiceResult<()> {
    HackerRank::start(false)?
        .download(contest, dir_to_save, extension, open_browser)?
        .save()
}


custom_derive! {
    #[derive(NewtypeDeref, NewtypeDerefMut)]
    struct HackerRank(HttpSession);
}

impl HackerRank {
    fn start(prints_message_when_already_logged_in: bool) -> ServiceResult<Self> {
        static URL: &'static str = "https://www.hackerrank.com/login";
        let mut hackerrank = HackerRank(HttpSession::start("hackerrank")?);
        if let Some(response) = hackerrank.http_get_as_opt(URL, 200, 302)? {
            let mut response = response;
            loop {
                if hackerrank.try_logging_in(response)? {
                    break println!("Succeeded to login.");
                }
                eprintln!("Failed to login. Try again.");
                hackerrank.clear_cookies();
                response = hackerrank.http_get(URL)?;
            }
        } else if prints_message_when_already_logged_in {
            eprintln!("Already signed in.");
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

        let (username, password) = super::read_username_and_password("Username: ")?;
        let csrf_token = extract_csrf_token(html)?;
        let data = PostData {
            login: username,
            password: password,
            remember_me: true,
        };
        let response = self.http_post_json_with_csrf_token(
            "https://www.hackerrank.com/auth/login",
            data,
            200,
            csrf_token,
        )?;
        Ok(serde_json::from_reader::<_, ResponseData>(response)?.status)
    }

    fn download(
        mut self,
        contest: &str,
        dir_to_save: &Path,
        extension: SuiteFileExtension,
        open_browser: bool,
    ) -> ServiceResult<Self> {
        #[derive(Deserialize)]
        struct Challenges {
            models: Vec<Model>,
        }

        #[derive(Deserialize)]
        struct Model {
            slug: String,
        }

        let url = format!(
            "https://www.hackerrank.com/rest/contests/{}/challenges",
            contest
        );
        let (mut zip_urls, mut paths, mut urls) = (vec![], vec![], vec![]);
        for slug in serde_json::from_reader::<_, Challenges>(self.http_get(&url)?)?
            .models
            .into_iter()
            .map(|model| model.slug)
        {
            zip_urls.push(format!("{}/{}/download_testcases", url, slug));
            paths.push(SuiteFilePath::new(dir_to_save, slug.clone(), extension));
            urls.push(format!(
                "https://www.hackerrank.com/contests/{}/challenges/{}",
                contest,
                slug
            ));
        }
        let zips = self.http_get_zips(&zip_urls)?;
        println!("Extracting...");
        let mut extracted = vec![];
        for zip in zips {
            extracted.push(extract_samples_from_zip(zip)?);
        }
        for (suite, path) in extracted.into_iter().zip(paths) {
            suite.save(&path, true)?;
        }
        if open_browser {
            for ref url in &urls {
                super::open_browser_with_message(&url)?;
            }
        }
        Ok(self)
    }

    fn save(self) -> ServiceResult<()> {
        self.0.save_cookies()
    }
}


fn extract_csrf_token(html: Response) -> ServiceResult<String> {
    fn extract(document: Document) -> Option<String> {
        try_opt!(document.find(Attr("name", "csrf-token")).next())
            .attr("content")
            .map(str::to_owned)
    }

    super::quit_on_failure(extract(Document::from_read(html)?), String::is_empty)
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
        if IN_REGEX.is_match(file.name()) {
            inputs.push(util::string_from_read(file)?);
        } else if OUT_REGEX.is_match(file.name()) {
            outputs.push(util::string_from_read(file)?);
        }
    }
    Ok(TestSuite::from_text(None, outputs.into_iter().zip(inputs)))
}
