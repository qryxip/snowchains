use crate::errors::{ScrapeError, ScrapeResult, ServiceErrorKind, ServiceResult};
use crate::service::download::DownloadProgress;
use crate::service::session::HttpSession;
use crate::service::{
    Contest, DownloadOutcome, DownloadOutcomeProblem, DownloadProps, ExtractZip, ListLangsOutcome,
    ListLangsProps, LoginOutcome, PrintTargets as _, Service, SessionProps, SubmitOutcome,
    SubmitProps, ZipEntries, ZipEntriesSorting,
};
use crate::terminal::{HasTerm, Term, WriteAnsi as _};
use crate::testsuite::{self, BatchSuite, InteractiveSuite, SuiteFilePath, TestSuite};
use crate::util::collections::NonEmptyIndexMap;
use crate::util::lang_unstable::Never;
use crate::util::str::CaseConversion;

use cookie::Cookie;
use failure::{Fail as _, ResultExt as _};
use indexmap::IndexMap;
use itertools::Itertools as _;
use once_cell::sync::Lazy;
use once_cell::sync_lazy;
use regex::Regex;
use reqwest::{header, StatusCode};
use select::document::Document;
use select::predicate::{Predicate as _, Text};
use serde_derive::Deserialize;
use tokio::runtime::Runtime;
use url::Url;

use std::borrow::Cow;
use std::io::Write;
use std::str::FromStr;
use std::time::Duration;
use std::{fmt, mem};

pub(crate) fn login(props: SessionProps, term: impl Term) -> ServiceResult<LoginOutcome> {
    Yukicoder::try_new(props, term)?.login(true)
}

pub(crate) fn download(
    props: (SessionProps, DownloadProps<String>),
    mut term: impl Term,
) -> ServiceResult<DownloadOutcome> {
    let (sess_props, download_props) = props;
    let download_props = download_props
        .convert_problems(CaseConversion::Upper)
        .parse_contest()
        .unwrap();
    download_props.print_targets(term.stderr())?;
    Yukicoder::try_new(sess_props, term)?.download(&download_props)
}

pub(crate) fn submit(
    props: (SessionProps, SubmitProps<String>),
    mut term: impl Term,
) -> ServiceResult<SubmitOutcome> {
    let (sess_props, submit_props) = props;
    let submit_props = submit_props
        .convert_problem(CaseConversion::Upper)
        .parse_contest()
        .unwrap();
    submit_props.print_targets(term.stderr())?;
    Yukicoder::try_new(sess_props, term)?.submit(&submit_props)
}

pub(crate) fn list_langs(
    props: (SessionProps, ListLangsProps<String>),
    mut term: impl Term,
) -> ServiceResult<ListLangsOutcome> {
    let (sess_props, list_langs_props) = props;
    let list_langs_props = list_langs_props
        .convert_problem(CaseConversion::Upper)
        .parse_contest()
        .unwrap();
    list_langs_props.print_targets(term.stderr())?;
    Yukicoder::try_new(sess_props, term)?.list_langs(list_langs_props)
}

struct Yukicoder<T: Term> {
    login_retries: Option<u32>,
    term: T,
    session: HttpSession,
    runtime: Runtime,
    username: Username,
}

impl<T: Term> HasTerm for Yukicoder<T> {
    type Term = T;

    fn term(&mut self) -> &mut T {
        &mut self.term
    }
}

impl<T: Term> Service for Yukicoder<T> {
    type Stderr = T::Stderr;

    fn requirements(&mut self) -> (&mut T::Stderr, &mut HttpSession, &mut Runtime) {
        (self.term.stderr(), &mut self.session, &mut self.runtime)
    }
}

impl<T: Term> DownloadProgress for Yukicoder<T> {
    type Write = T::Stderr;

    fn requirements(&mut self) -> (&mut T::Stderr, &HttpSession, &mut Runtime) {
        (self.term.stderr(), &self.session, &mut self.runtime)
    }
}

impl<T: Term> ExtractZip for Yukicoder<T> {
    type Write = T::Stderr;

    fn out(&mut self) -> &mut T::Stderr {
        self.term.stderr()
    }
}

impl<T: Term> Yukicoder<T> {
    fn try_new(props: SessionProps, mut term: T) -> ServiceResult<Self> {
        let mut runtime = Runtime::new()?;
        let session = props.start_session(term.stderr(), &mut runtime)?;
        Ok(Self {
            login_retries: props.login_retries,
            term,
            session,
            runtime,
            username: Username::None,
        })
    }

    fn login(&mut self, assure: bool) -> ServiceResult<LoginOutcome> {
        self.fetch_username()?;
        if self.username.name().is_none() {
            let (mut first, mut retries) = (true, self.login_retries);
            loop {
                if first {
                    if !assure && !self.ask_yes_or_no("Login? ", true)? {
                        break;
                    }
                    writeln!(
                        self.stderr(),
                        "\nInput \"REVEL_SESSION\".\n\n\
                         Firefox: sqlite3 ~/path/to/cookies.sqlite 'SELECT value FROM moz_cookies \
                         WHERE baseDomain=\"yukicoder.me\" AND name=\"REVEL_SESSION\"'\n\
                         Chrome: chrome://settings/cookies/detail?site=yukicoder.me&search=cookie\n"
                    )?;
                    first = false;
                }
                let revel_session = self.prompt_password_stderr("REVEL_SESSION: ")?;
                if self.confirm_revel_session(revel_session)? {
                    break;
                }
                if retries == Some(0) {
                    return Err(ServiceErrorKind::LoginRetriesExceeded.into());
                }
                retries = retries.map(|n| n - 1);
                writeln!(self.stderr(), "Wrong \"REVEL_SESSION\".")?;
                self.stderr().flush()?;
            }
        }
        let username = self.username.clone();
        writeln!(self.stderr(), "Username: {}", username)?;
        self.stderr().flush()?;
        Ok(LoginOutcome {})
    }

    fn confirm_revel_session(&mut self, revel_session: String) -> ServiceResult<bool> {
        self.session.clear_cookies()?;
        let cookie = Cookie::new("REVEL_SESSION", revel_session);
        self.session.insert_cookie(cookie)?;
        self.fetch_username()?;
        Ok(self.username.name().is_some())
    }

    fn fetch_username(&mut self) -> ServiceResult<()> {
        self.username = self.get("/").recv_html()?.extract_username();
        Ok(())
    }

    fn download(
        &mut self,
        download_props: &DownloadProps<YukicoderContest>,
    ) -> ServiceResult<DownloadOutcome> {
        let DownloadProps {
            contest,
            problems,
            destinations,
            open_in_browser,
            only_scraped,
        } = download_props;
        self.login(false)?;
        let scrape =
            |document: &Document, problem: &str| -> ServiceResult<(TestSuite, SuiteFilePath)> {
                let suite = document.extract_samples()?;
                let path = destinations.expand(problem)?;
                Ok((suite, path))
            };
        let mut outcome = DownloadOutcome::new(contest);
        match (contest, problems.as_ref()) {
            (YukicoderContest::No, None) => {
                return Err(ServiceErrorKind::PleaseSpecifyProblems.into());
            }
            (YukicoderContest::No, Some(problems)) => {
                let (mut not_found, mut not_public) = (vec![], vec![]);
                for problem in problems {
                    let url = format!("/problems/no/{}", problem);
                    let res = self.get(&url).acceptable(&[200, 404]).send()?;
                    let status = res.status();
                    let document = res.document(&mut self.runtime)?;
                    let public = document
                        .find(selector!("#content").child(Text))
                        .next()
                        .map_or(true, |t| !t.text().contains("非表示"));
                    if status == StatusCode::NOT_FOUND {
                        not_found.push(problem);
                    } else if !public {
                        not_public.push(problem);
                    } else {
                        let (suite, path) = scrape(&document, problem)?;
                        let url = self.session.resolve_url(&url)?;
                        outcome.push_problem(problem.to_owned(), url, suite, path);
                    }
                }
                let stderr = self.stderr();
                if !not_found.is_empty() {
                    stderr.with_reset(|o| writeln!(o.fg(11)?, "Not found: {:?}", not_found))?;
                    stderr.flush()?;
                }
                if !not_public.is_empty() {
                    stderr.with_reset(|o| writeln!(o.fg(11)?, "Not public: {:?}", not_public))?;
                    stderr.flush()?;
                }
            }
            (YukicoderContest::Contest(contest), problems) => {
                let target_problems = self
                    .get(&format!("/contests/{}", contest))
                    .recv_html()?
                    .extract_problems()?;
                for (name, href) in target_problems {
                    if problems.is_none() || problems.as_ref().unwrap().contains(&name) {
                        let document = self.get(&href).recv_html()?;
                        let (suite, path) = scrape(&document, &name)?;
                        let url = self.session.resolve_url(&href)?;
                        outcome.push_problem(name, url, suite, path);
                    }
                }
            }
        }
        let nos = outcome
            .problems
            .iter()
            .map(|p| p.name.clone())
            .collect::<Vec<_>>();
        let solved_batch_nos = if *only_scraped {
            vec![]
        } else {
            self.filter_solved(&nos)?
                .into_iter()
                .filter(|no| {
                    outcome
                        .problems
                        .iter()
                        .any(|problem| match &problem.test_suite {
                            TestSuite::Batch(_) => problem.name == *no,
                            _ => false,
                        })
                })
                .collect()
        };

        let text_file_paths = if solved_batch_nos.is_empty() {
            vec![]
        } else {
            let urls = solved_batch_nos
                .iter()
                .map(|no| format!("https://yukicoder.me/problems/no/{}/testcase.zip", no))
                .collect::<Vec<_>>();
            self.download_progress(&urls, &solved_batch_nos, None)?
                .into_iter()
                .zip_eq(&solved_batch_nos)
                .map(|(zip, &no)| {
                    static ZIP_ENTRIES: Lazy<ZipEntries> = sync_lazy!(ZipEntries {
                        in_entry: Regex::new(r"\Atest_in/([a-z0-9_]+)\.txt\z").unwrap(),
                        in_match_group: 1,
                        in_crlf_to_lf: true,
                        out_entry: Regex::new(r"\Atest_out/([a-z0-9_]+)\.txt\z").unwrap(),
                        out_match_group: 1,
                        out_crlf_to_lf: true,
                        sortings: vec![ZipEntriesSorting::Dictionary, ZipEntriesSorting::Number],
                    });
                    let paths =
                        self.extract_zip(no, &zip, &destinations.text_file_dir(no)?, &ZIP_ENTRIES)?;
                    Ok((no, paths))
                })
                .collect::<ServiceResult<Vec<_>>>()?
        };
        for DownloadOutcomeProblem {
            name,
            test_suite,
            test_suite_path,
            ..
        } in &mut outcome.problems
        {
            for (no, text_file_paths) in &text_file_paths {
                if name == no {
                    *test_suite = match mem::replace(test_suite, TestSuite::Unsubmittable) {
                        TestSuite::Batch(suite) => {
                            suite.without_cases().paths(text_file_paths.clone()).into()
                        }
                        suite => suite,
                    };
                    break;
                }
            }
            test_suite.save(name, test_suite_path, self.stderr())?;
        }
        if *open_in_browser {
            for DownloadOutcomeProblem { url, .. } in &outcome.problems {
                self.open_in_browser(url.as_str())?;
            }
        }
        Ok(outcome)
    }

    fn submit(&mut self, props: &SubmitProps<YukicoderContest>) -> ServiceResult<SubmitOutcome> {
        let SubmitProps {
            contest,
            problem,
            lang_name,
            src_path,
            open_in_browser,
            skip_checking_if_accepted,
        } = props;

        let code = crate::fs::read_to_string(src_path)?;

        self.login(true)?;
        let url = self.get_submit_url(contest, problem)?;
        let no = {
            static NO: Lazy<Regex> =
                lazy_regex!(r"\A(https://yukicoder\.me)?/problems/no/(\d+)/submit\z");
            NO.captures(url.as_ref()).map(|caps| caps[2].to_owned())
        };
        if let Some(no) = no {
            if !(self.filter_solved(&[no])?.is_empty() || *skip_checking_if_accepted) {
                return Err(ServiceErrorKind::AlreadyAccepted.into());
            }
        }
        let document = self.get(url.as_ref()).recv_html()?;
        let lang_id = document
            .extract_langs()?
            .get(lang_name)
            .ok_or_else(|| ServiceErrorKind::NoSuchLang(lang_name.clone()))?
            .clone();
        writeln!(
            self.stderr(),
            "Submitting as {:?} (ID: {:?})",
            lang_name,
            lang_id,
        )?;
        let token = document.extract_csrf_token_from_submit_page()?;
        let form = reqwest::r#async::multipart::Form::new()
            .text("csrf_token", token)
            .text("lang", lang_id.clone())
            .text("source", code.clone());
        let url = document.extract_url_from_submit_page()?;
        let res = self.post(&url).send_multipart(form)?;
        let location = match res.headers().get(header::LOCATION) {
            None => None,
            Some(location) => Some(
                location
                    .to_str()
                    .with_context(|_| ServiceErrorKind::ReadHeader(header::LOCATION))?,
            ),
        };
        if let Some(&location) = location.as_ref() {
            if location.contains("/submissions/") {
                writeln!(self.stderr(), "Success: {:?}", location)?;
                self.stderr().flush()?;
                if *open_in_browser {
                    self.open_in_browser(location)?;
                }
                return Ok(SubmitOutcome {});
            }
        }
        Err(ServiceErrorKind::SubmissionRejected {
            lang_name: lang_name.clone(),
            lang_id,
            size: code.len(),
            status: res.status(),
            location: location.map(ToOwned::to_owned),
        }
        .into())
    }

    fn list_langs(
        &mut self,
        props: ListLangsProps<YukicoderContest>,
    ) -> ServiceResult<ListLangsOutcome> {
        let ListLangsProps { contest, problem } = props;
        let problem = problem.ok_or(ServiceErrorKind::PleaseSpecifyProblem)?;
        self.login(true)?;
        let url = self.get_submit_url(&contest, &problem)?;
        let langs = self.get(url.as_str()).recv_html()?.extract_langs()?;
        self.print_lang_list(&langs)?;
        Ok(ListLangsOutcome::new(url, langs))
    }

    fn filter_solved<'b>(
        &mut self,
        nos: &'b [impl 'b + AsRef<str>],
    ) -> ServiceResult<Vec<&'b str>> {
        #[derive(Deserialize)]
        #[serde(rename_all = "PascalCase")]
        struct Problem {
            no: u64,
        }

        if let Some(username) = self.username.name().map(ToOwned::to_owned) {
            let url = format!("/api/v1/solved/name/{}", username);
            let solved_nos = self
                .get(&url)
                .send()?
                .json::<Vec<Problem>>(&mut self.runtime)?
                .into_iter()
                .map(|problem| problem.no.to_string())
                .collect::<Vec<_>>();
            Ok(nos
                .iter()
                .map(AsRef::as_ref)
                .filter(|no1| solved_nos.iter().any(|no2| no1 == no2))
                .collect())
        } else {
            Ok(vec![])
        }
    }

    fn get_submit_url(&mut self, contest: &YukicoderContest, problem: &str) -> ServiceResult<Url> {
        let mut url = match contest {
            YukicoderContest::No => format!("https://yukicoder.me/problems/no/{}", problem),
            YukicoderContest::Contest(contest) => self
                .get(&format!("/contests/{}", contest))
                .recv_html()?
                .extract_problems()?
                .into_iter()
                .filter(|(name, _)| name.eq_ignore_ascii_case(problem))
                .map(|(_, href)| href)
                .next()
                .ok_or_else(|| ServiceErrorKind::NoSuchProblem(problem.to_owned()))?,
        };
        url += "/submit";
        url.parse::<Url>()
            .map_err(|e| e.context(ServiceErrorKind::ParseUrl(url)).into())
    }
}

enum YukicoderContest {
    No,
    Contest(String),
}

impl fmt::Display for YukicoderContest {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            YukicoderContest::No => write!(f, "no"),
            YukicoderContest::Contest(contest) => write!(f, "{}", contest),
        }
    }
}

impl Contest for YukicoderContest {
    fn slug(&self) -> Cow<str> {
        self.to_string().into()
    }
}

impl FromStr for YukicoderContest {
    type Err = Never;

    fn from_str(s: &str) -> std::result::Result<Self, Never> {
        Ok(if s.eq_ignore_ascii_case("no") {
            YukicoderContest::No
        } else {
            YukicoderContest::Contest(s.to_owned())
        })
    }
}

#[derive(Clone, Debug)]
enum Username {
    None,
    // /public/img/anony.png (for now)
    Yukicoder(String),
    // https://avatars2.githubusercontent.com/...
    Github(String),
    // ?
    ProbablyTwitter(String),
}

impl Username {
    fn name(&self) -> Option<&str> {
        match self {
            Username::None => None,
            Username::Yukicoder(s) | Username::Github(s) | Username::ProbablyTwitter(s) => Some(&s),
        }
    }
}

impl fmt::Display for Username {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Username::None => write!(f, "<not logged in>"),
            Username::Yukicoder(s) => write!(f, "{} (yukicoder)", s.trim()),
            Username::Github(s) => write!(f, "{} (GitHub)", s.trim()),
            Username::ProbablyTwitter(s) => write!(f, "{} (probably Twitter)", s.trim()),
        }
    }
}

trait Extract {
    fn extract_username(&self) -> Username;
    fn extract_samples(&self) -> ScrapeResult<TestSuite>;
    fn extract_problems(&self) -> ScrapeResult<Vec<(String, String)>>;
    fn extract_csrf_token_from_submit_page(&self) -> ScrapeResult<String>;
    fn extract_url_from_submit_page(&self) -> ScrapeResult<String>;
    fn extract_langs(&self) -> ScrapeResult<NonEmptyIndexMap<String, String>>;
}

impl Extract for Document {
    fn extract_username(&self) -> Username {
        let extract = || {
            let a = self.find(selector!("#usermenu > a")).next()?;
            let name = a.find(Text).next()?.text();
            let src = a.find(selector!("img")).next()?.attr("src")?;
            Some(if src == "/public/img/anony.png" {
                Username::Yukicoder(name)
            } else if src.starts_with("https://avatars2.githubusercontent.com") {
                Username::Github(name)
            } else {
                Username::ProbablyTwitter(name)
            })
        };
        extract().unwrap_or(Username::None)
    }

    fn extract_samples(&self) -> ScrapeResult<TestSuite> {
        #[derive(Clone, Copy, PartialEq)]
        enum ProblemKind {
            Regular,
            Special,
            Reactive,
        }

        let extract = || {
            static R: Lazy<Regex> = lazy_regex!(
                "\\A / 実行時間制限 : 1ケース (\\d)\\.(\\d{3})秒 / メモリ制限 : \\d+ MB / \
                 (通常|スペシャルジャッジ|リアクティブ)問題.*\n?.*\\z"
            );
            let text = self
                .find(selector!("#content > div").child(Text))
                .map(|text| text.text())
                .nth(1)?;
            let caps = R.captures(&text)?;
            let timelimit = {
                let s = caps[1].parse::<u64>().unwrap();
                let m = caps[2].parse::<u64>().unwrap();
                Duration::from_millis(1000 * s + m)
            };
            let kind = match &caps[3] {
                "通常" => ProblemKind::Regular,
                "スペシャルジャッジ" => ProblemKind::Special,
                "リアクティブ" => ProblemKind::Reactive,
                _ => return None,
            };
            match kind {
                ProblemKind::Regular | ProblemKind::Special => {
                    let mut samples = vec![];
                    for paragraph in self.find(selector!(
                        "#content > div.block > div.sample > div.paragraph",
                    )) {
                        let pres = paragraph
                            .find(selector!("pre").child(Text))
                            .collect::<Vec<_>>();
                        guard!(pres.len() == 2);
                        let input = pres[0].text();
                        let output = match kind {
                            ProblemKind::Regular => Some(pres[1].text()),
                            ProblemKind::Special => None,
                            ProblemKind::Reactive => unreachable!(),
                        };
                        samples.push((input, output));
                    }
                    let mut suite = BatchSuite::new(timelimit)
                        .sample_cases(samples.into_iter(), |i| format!("サンプル{}", i + 1));
                    if kind == ProblemKind::Special {
                        suite = suite.matching(testsuite::Match::Any);
                    }
                    Some(suite.into())
                }
                ProblemKind::Reactive => Some(InteractiveSuite::new(timelimit).into()),
            }
        };
        extract().ok_or_else(ScrapeError::new)
    }

    fn extract_problems(&self) -> ScrapeResult<Vec<(String, String)>> {
        let extract = || {
            let mut problems = vec![];
            for tr in self.find(selector!("#content > div.left > table.table > tbody > tr")) {
                let name = tr.find(selector!("td")).nth(0)?.text();
                let href = tr
                    .find(selector!("td"))
                    .nth(2)?
                    .find(selector!("a"))
                    .next()?
                    .attr("href")?
                    .to_owned();
                problems.push((name, href));
            }
            if problems.is_empty() {
                None
            } else {
                Some(problems)
            }
        };
        extract().ok_or_else(ScrapeError::new)
    }

    fn extract_csrf_token_from_submit_page(&self) -> ScrapeResult<String> {
        self.find(selector!("#submit_form > input[name=\"csrf_token\"]"))
            .find_map(|input| input.attr("value").map(ToOwned::to_owned))
            .ok_or_else(ScrapeError::new)
    }

    fn extract_url_from_submit_page(&self) -> ScrapeResult<String> {
        self.find(selector!("#submit_form"))
            .find_map(|form| form.attr("action").map(ToOwned::to_owned))
            .ok_or_else(ScrapeError::new)
    }

    fn extract_langs(&self) -> ScrapeResult<NonEmptyIndexMap<String, String>> {
        static WS: Lazy<Regex> = lazy_regex!(r"[\s\n]+");
        let names = self
            .find(selector!("#lang > option"))
            .map(|option| {
                let name = option.find(Text).next()?.as_text().unwrap();
                let name = WS.replace_all(name.trim(), " ").into_owned();
                let id = option.attr("value")?.to_owned();
                Some((name, id))
            })
            .map(|p| p.ok_or_else(ScrapeError::new))
            .collect::<ScrapeResult<IndexMap<_, _>>>()?;
        NonEmptyIndexMap::try_new(names).ok_or_else(ScrapeError::new)
    }
}

#[cfg(test)]
mod tests {
    use crate::errors::ServiceResult;
    use crate::service;
    use crate::service::yukicoder::Extract;

    use failure::Fallible;
    use pretty_assertions::assert_eq;
    use select::document::Document;

    use std::borrow::Borrow;
    use std::time::Duration;

    #[test]
    fn it_extracts_samples_from_problem1() -> Fallible<()> {
        test_extracting_samples("/problems/no/1", "cf65ae411bc8d32b75beb771905c9dc0")
    }

    #[test]
    fn it_extracts_samples_from_problem188() -> Fallible<()> {
        test_extracting_samples("/problems/no/188", "671c7191064f7703abcb5e06fad3f32e")
    }

    #[test]
    fn it_extracts_samples_from_problem192() -> Fallible<()> {
        test_extracting_samples("/problems/no/192", "f8ce3328c431737dcb748770abd9a09b")
    }

    #[test]
    fn it_extracts_samples_from_problem246() -> Fallible<()> {
        test_extracting_samples("/problems/no/246", "9debfd89a82271d763b717313363acda")
    }

    fn test_extracting_samples(rel_url: &str, expected_md5: &str) -> Fallible<()> {
        let document = get_html(rel_url)?;
        let suite = document.extract_samples()?;
        let actual_md5 = suite.md5()?;
        suite.assert_serialize_correctly()?;
        assert_eq!(format!("{:x}", actual_md5), expected_md5);
        Ok(())
    }

    #[test]
    fn it_extracts_problems_names_and_hrefs_from_yukicoder_open_2015_small() -> ServiceResult<()> {
        static EXPECTED: &[(&str, &str)] = &[
            ("A", "/problems/no/191"),
            ("B", "/problems/no/192"),
            ("C", "/problems/no/193"),
            ("D", "/problems/no/194"),
            ("E", "/problems/no/195"),
            ("F", "/problems/no/196"),
        ];
        let document = get_html("/contests/100")?;
        let problems = document.extract_problems()?;
        assert_eq!(own_pairs(EXPECTED), problems);
        Ok(())
    }

    fn own_pairs<O: Borrow<B>, B: ToOwned<Owned = O> + ?Sized>(pairs: &[(&B, &B)]) -> Vec<(O, O)> {
        pairs
            .iter()
            .map(|(l, r)| ((*l).to_owned(), (*r).to_owned()))
            .collect()
    }

    fn get_html(rel_url: &str) -> reqwest::Result<Document> {
        let client = service::reqwest_sync_client(Duration::from_secs(60))?;
        let url = format!("https://yukicoder.me{}", rel_url);
        let content = client.get(&url).send()?.text()?;
        Ok(Document::from(content.as_str()))
    }
}
