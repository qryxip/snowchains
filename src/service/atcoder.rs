use errors::{ServiceError, ServiceErrorKind, ServiceResult, ServiceResultExt};
use testsuite::{SuiteFileExtension, SuiteFilePath, TestSuite};

use httpsession::HttpSession;
use regex::Regex;
use select::document::Document;
use select::node::Node;
use select::predicate::{Attr as HtmlAttr, Class, Name, Predicate, Text};

use std::io::Read;
use std::path::Path;

pub fn login() -> ServiceResult<()> {
    static URL: &'static str = "https://practice.contest.atcoder.jp/settings";
    let mut atcoder = AtCoder(super::start_session("atcoder", "atcoder.jp")?);
    if atcoder.http_get(URL).is_err() {
        atcoder.login()?;
    } else {
        println!("Already logged in.");
    }
    Ok(())
}

pub fn participate(contest_name: &str) -> ServiceResult<()> {
    AtCoder::load_or_login()?.participate(contest_name)
}

pub fn download(
    contest_name: &str,
    dir_to_save: &Path,
    extension: SuiteFileExtension,
) -> ServiceResult<()> {
    let mut atcoder = AtCoder::load_or_login()?;
    atcoder.download_all_tasks(contest_name, dir_to_save, extension)
}

custom_derive! {
    #[derive(NewtypeDeref, NewtypeDerefMut)]
    struct AtCoder(HttpSession);
}

impl AtCoder {
    fn load_or_login() -> ServiceResult<Self> {
        static URL: &'static str = "https://practice.contest.atcoder.jp/settings";
        let mut atcoder = AtCoder(super::start_session("atcoder", "atcoder.jp")?);
        if atcoder.http_get(URL).is_err() {
            atcoder.login()?;
        }
        Ok(atcoder)
    }

    fn participate(&mut self, contest_name: &str) -> ServiceResult<()> {
        let url = format!(
            "https://{}.contest.atcoder.jp/participants/insert",
            contest_name
        );
        self.http_get_expecting(&url, &[302])?;
        Ok(())
    }

    fn download_all_tasks(
        &mut self,
        contest_name: &str,
        dir_to_save: &Path,
        extension: SuiteFileExtension,
    ) -> ServiceResult<()> {
        let names_and_pathes = {
            let url = format!("http://{}.contest.atcoder.jp/assignments", contest_name);
            extract_names_and_pathes(self.http_get(&url)?).chain_err(|| "Probably 404")?
        };
        for (alphabet, path) in names_and_pathes {
            let url = format!("http://{}.contest.atcoder.jp{}", contest_name, path);
            match extract_cases(self.http_get(&url)?) {
                Ok(suite) => {
                    let path = SuiteFilePath::new(dir_to_save, alphabet.to_lowercase(), extension);
                    suite.save(&path, true)?;
                }
                Err(ServiceError(ServiceErrorKind::Scrape, _)) => {
                    println!("Failed to scrape. Ignoring.");
                }
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }

    fn login(&mut self) -> ServiceResult<()> {
        #[derive(Serialize)]
        struct PostData {
            name: String,
            password: String,
        }

        fn post_data() -> ServiceResult<PostData> {
            let (user_id, password) = super::ask_username_and_password("User ID: ")?;
            Ok(PostData {
                name: user_id,
                password,
            })
        }

        static URL: &'static str = "https://practice.contest.atcoder.jp/login";
        let _ = self.http_get(URL)?;
        while self.http_post_urlencoded(URL, &post_data()?, &[302], None)
            .is_err()
        {
            println!("Failed to sign in. try again.")
        }
        Ok(())
    }
}

fn extract_names_and_pathes<R: Read>(html: R) -> ServiceResult<Vec<(String, String)>> {
    fn extract(document: &Document) -> Option<Vec<(String, String)>> {
        let mut names_and_pathes = vec![];
        let predicate = HtmlAttr("id", "outer-inner")
            .child(Name("table"))
            .child(Name("tbody"))
            .child(Name("tr"));
        for node in document.find(predicate) {
            let node = node.find(Name("td")).next()?;
            let node = node.find(Name("a")).next()?;
            let url = node.attr("href")?.to_owned();
            let text = node.find(Text).next()?.text();
            names_and_pathes.push((text, url));
        }
        Some(names_and_pathes)
    }

    super::quit_on_failure(extract(&Document::from_read(html)?), Vec::is_empty)
}

fn extract_cases<R: Read>(html: R) -> ServiceResult<TestSuite> {
    fn try_extracting_sample(section_node: Node, regex: &'static Regex) -> Option<String> {
        let title = section_node.find(Name("h3")).next()?.text();
        let sample = section_node.find(Name("pre")).next()?.text();
        return_none_unless!(regex.is_match(&title));
        Some(sample)
    }

    fn extract(document: &Document) -> Option<TestSuite> {
        let timelimit = {
            let re_timelimit = Regex::new("\\D*([0-9]+)sec.*").unwrap();
            let predicate = HtmlAttr("id", "outer-inner").child(Name("p")).child(Text);
            let text = document.find(predicate).nth(0)?.text();
            let caps = re_timelimit.captures(&text)?;
            1000 * caps[1].parse::<u64>().unwrap()
        };
        let samples = {
            lazy_static! {
                static ref RE_INPUT: Regex = Regex::new("^入力例 ([0-9]+)$").unwrap();
                static ref RE_OUTPUT: Regex = Regex::new("^出力例 ([0-9]+)$").unwrap();
            }
            let predicate = HtmlAttr("id", "task-statement")
                .child(Class("lang"))
                .child(Class("lang-ja"))
                .child(Class("part"))
                .child(Name("section"));
            let (mut samples, mut input_sample) = (vec![], None);
            for node in document.find(predicate) {
                input_sample = if let Some(input_sample) = input_sample {
                    let output_sample = try_extracting_sample(node, &RE_OUTPUT)?;
                    samples.push((output_sample, input_sample));
                    None
                } else if let Some(input_sample) = try_extracting_sample(node, &RE_INPUT) {
                    Some(input_sample)
                } else {
                    None
                };
            }
            samples
        };
        Some(TestSuite::from_samples(Some(timelimit), samples))
    }

    super::quit_on_failure(extract(&Document::from_read(html)?), TestSuite::is_empty)
}
