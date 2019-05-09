use crate::errors::{ScrapeError, ScrapeResult, ServiceErrorKind, ServiceResult};
use crate::service::download::DownloadProgress;
use crate::service::session::HttpSession;
use crate::service::{
    Contest, ExtractZip, LoginOutcome, RetrieveLangsOutcome, RetrieveLangsProps,
    RetrieveTestCasesOutcome, RetrieveTestCasesOutcomeProblem, RetrieveTestCasesProps, Service,
    SessionProps, SubmitOutcome, SubmitOutcomeLanguage, SubmitOutcomeResponse, SubmitProps,
    ZipEntries, ZipEntriesSorting,
};
use crate::terminal::{HasTermProps, Input};
use crate::testsuite::{self, BatchSuite, InteractiveSuite, SuiteFilePath, TestSuite};
use crate::util::collections::NonEmptyIndexMap;
use crate::util::str::CaseConversion;

use cookie::Cookie;
use failure::Fail as _;
use indexmap::IndexMap;
use itertools::Itertools as _;
use once_cell::sync::Lazy;
use once_cell::sync_lazy;
use regex::Regex;
use reqwest::StatusCode;
use scraper::Html;
use serde_derive::Deserialize;
use termcolor::WriteColor;
use tokio::runtime::Runtime;
use url::Url;

use std::borrow::Cow;
use std::convert::Infallible;
use std::str::FromStr;
use std::time::Duration;
use std::{fmt, mem};

pub(super) fn login(
    props: SessionProps,
    stdin: impl Input,
    stderr: impl WriteColor + HasTermProps,
) -> ServiceResult<LoginOutcome> {
    Yukicoder::try_new(props, stdin, stderr)?.login(true)
}

pub(super) fn retrieve_testcases(
    props: (SessionProps, RetrieveTestCasesProps<String>),
    stdin: impl Input,
    stderr: impl WriteColor + HasTermProps,
) -> ServiceResult<RetrieveTestCasesOutcome> {
    let (sess_props, retrieve_props) = props;
    let retrieve_props = retrieve_props
        .convert_problems(CaseConversion::Upper)
        .parse_contest()
        .unwrap();
    Yukicoder::try_new(sess_props, stdin, stderr)?.retrieve_testcases(&retrieve_props)
}

pub(super) fn retrieve_langs(
    props: (SessionProps, RetrieveLangsProps<String>),
    stdin: impl Input,
    stderr: impl WriteColor + HasTermProps,
) -> ServiceResult<RetrieveLangsOutcome> {
    let (sess_props, retrieve_props) = props;
    let retrieve_props = retrieve_props
        .convert_problem(CaseConversion::Upper)
        .parse_contest()
        .unwrap();
    Yukicoder::try_new(sess_props, stdin, stderr)?.retrieve_langs(retrieve_props)
}

pub(super) fn submit(
    props: (SessionProps, SubmitProps<String>),
    stdin: impl Input,
    stderr: impl WriteColor + HasTermProps,
) -> ServiceResult<SubmitOutcome> {
    let (sess_props, submit_props) = props;
    let submit_props = submit_props
        .convert_problem(CaseConversion::Upper)
        .parse_contest()
        .unwrap();
    Yukicoder::try_new(sess_props, stdin, stderr)?.submit(submit_props)
}

#[derive(Debug)]
struct Yukicoder<I: Input, E: WriteColor + HasTermProps> {
    login_retries: Option<u32>,
    stdin: I,
    stderr: E,
    session: HttpSession,
    runtime: Runtime,
    username: Username,
}

impl<I: Input, E: WriteColor + HasTermProps> Service for Yukicoder<I, E> {
    type Stdin = I;
    type Stderr = E;

    fn requirements(&mut self) -> (&mut I, &mut E, &mut HttpSession, &mut Runtime) {
        (
            &mut self.stdin,
            &mut self.stderr,
            &mut self.session,
            &mut self.runtime,
        )
    }
}

impl<I: Input, E: WriteColor + HasTermProps> DownloadProgress for Yukicoder<I, E> {
    type Write = E;

    fn requirements(&mut self) -> (&mut E, &HttpSession, &mut Runtime) {
        (&mut self.stderr, &self.session, &mut self.runtime)
    }
}

impl<I: Input, E: WriteColor + HasTermProps> ExtractZip for Yukicoder<I, E> {
    type Write = E;

    fn out(&mut self) -> &mut E {
        &mut self.stderr
    }
}

impl<I: Input, E: WriteColor + HasTermProps> Yukicoder<I, E> {
    fn try_new(props: SessionProps, stdin: I, mut stderr: E) -> ServiceResult<Self> {
        let mut runtime = Runtime::new()?;
        let session = props.start_session(&mut stderr, &mut runtime)?;
        Ok(Self {
            login_retries: props.login_retries,
            stdin,
            stderr,
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
                        self.stderr,
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
                writeln!(self.stderr, "Wrong \"REVEL_SESSION\".")?;
                self.stderr.flush()?;
            }
        }
        let username = self.username.clone();
        writeln!(self.stderr, "Username: {}", username)?;
        self.stderr.flush()?;
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

    fn retrieve_testcases(
        &mut self,
        props: &RetrieveTestCasesProps<YukicoderContest>,
    ) -> ServiceResult<RetrieveTestCasesOutcome> {
        let RetrieveTestCasesProps {
            contest,
            problems,
            destinations,
            open_in_browser,
            only_scraped,
        } = props;
        self.login(false)?;
        let scrape = |html: &Html, problem: &str| -> ServiceResult<(TestSuite, SuiteFilePath)> {
            let suite = html.extract_samples()?;
            let path = destinations.expand(problem)?;
            Ok((suite, path))
        };
        let mut outcome = RetrieveTestCasesOutcome::new(contest);
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
                    let html = res.html(&mut self.runtime)?;
                    let public = html
                        .select(selector!("#content"))
                        .flat_map(|r| r.text())
                        .next()
                        .map_or(true, |t| !t.contains("非表示"));
                    if status == StatusCode::NOT_FOUND {
                        not_found.push(problem);
                    } else if !public {
                        not_public.push(problem);
                    } else {
                        let (suite, path) = scrape(&html, problem)?;
                        let url = self.session.resolve_url(&url)?;
                        outcome.push_problem(problem.to_owned(), url, suite, path);
                    }
                }
                let stderr = &mut self.stderr;
                if !not_found.is_empty() {
                    stderr.set_color(color!(fg(Yellow), intense))?;
                    write!(stderr, "Not found: {:?}", not_found)?;;
                    stderr.reset()?;
                    writeln!(stderr)?;
                    stderr.flush()?;
                }
                if !not_public.is_empty() {
                    stderr.set_color(color!(fg(Yellow), intense))?;
                    write!(stderr, "Not public: {:?}", not_public)?;;
                    stderr.reset()?;
                    writeln!(stderr)?;
                    stderr.flush()?;
                }
            }
            (YukicoderContest::Contest(contest), problems) => {
                let target_problems = self
                    .get(&format!("/contests/{}", contest))
                    .recv_html()?
                    .extract_problems()?;
                for (slug, href) in target_problems {
                    if problems.is_none() || problems.as_ref().unwrap().contains(&slug) {
                        let html = self.get(&href).recv_html()?;
                        let (suite, path) = scrape(&html, &slug)?;
                        let url = self.session.resolve_url(&href)?;
                        outcome.push_problem(slug, url, suite, path);
                    }
                }
            }
        }
        let nos = outcome
            .problems
            .iter()
            .map(|p| p.slug.clone())
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
                            TestSuite::Batch(_) => problem.slug == *no,
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
        for RetrieveTestCasesOutcomeProblem {
            slug,
            test_suite,
            test_suite_path,
            ..
        } in &mut outcome.problems
        {
            for (no, text_file_paths) in &text_file_paths {
                if slug == no {
                    *test_suite = match mem::replace(test_suite, TestSuite::Unsubmittable) {
                        TestSuite::Batch(suite) => {
                            suite.without_cases().paths(text_file_paths.clone()).into()
                        }
                        suite => suite,
                    };
                    break;
                }
            }
            test_suite.save(test_suite_path)?;
        }
        if *open_in_browser {
            for RetrieveTestCasesOutcomeProblem { url, .. } in &outcome.problems {
                self.open_in_browser(url.as_str())?;
            }
        }
        Ok(outcome)
    }

    fn retrieve_langs(
        &mut self,
        props: RetrieveLangsProps<YukicoderContest>,
    ) -> ServiceResult<RetrieveLangsOutcome> {
        let RetrieveLangsProps { contest, problem } = props;
        let problem = problem.ok_or(ServiceErrorKind::PleaseSpecifyProblem)?;
        self.login(true)?;
        let url = self.get_submit_url(&contest, &problem)?;
        let langs = self.get(url.as_str()).recv_html()?.extract_langs()?;
        Ok(RetrieveLangsOutcome::new(url, langs))
    }

    fn submit(&mut self, props: SubmitProps<YukicoderContest>) -> ServiceResult<SubmitOutcome> {
        let SubmitProps {
            contest,
            problem,
            lang_name,
            src_path,
            open_in_browser,
            skip_checking_if_accepted,
        } = props;

        let code = crate::fs::read_to_string(&src_path)?;

        self.login(true)?;
        let url = self.get_submit_url(&contest, &problem)?;
        let no = {
            lazy_regex!(r"\A(https://yukicoder\.me)?/problems/no/(\d+)/submit\z")
                .captures(url.as_ref())
                .map(|caps| caps[2].to_owned())
        };
        if let Some(no) = no {
            if !(self.filter_solved(&[no])?.is_empty() || skip_checking_if_accepted) {
                return Err(ServiceErrorKind::AlreadyAccepted.into());
            }
        }
        let html = self.get(url.as_ref()).recv_html()?;
        let lang_id = html
            .extract_langs()?
            .get(&lang_name)
            .ok_or_else(|| ServiceErrorKind::NoSuchLang(lang_name.clone()))?
            .clone();
        let token = html.extract_csrf_token_from_submit_page()?;
        let form = reqwest::r#async::multipart::Form::new()
            .text("csrf_token", token)
            .text("lang", lang_id.clone())
            .text("source", code.clone());
        let url = html.extract_url_from_submit_page()?;
        let res = self.post(&url).send_multipart(form)?;
        let header_location = res.header_location()?.map(ToOwned::to_owned);
        let rejected = header_location
            .as_ref()
            .map_or(true, |l| !l.contains("/submissions/"));
        if let Some(header_location) = &header_location {
            if !rejected && open_in_browser {
                self.open_in_browser(header_location)?;
            }
        }
        Ok(SubmitOutcome {
            rejected,
            response: SubmitOutcomeResponse {
                status: res.status(),
                header_location,
            },
            language: SubmitOutcomeLanguage {
                name: lang_name,
                id: lang_id,
            },
            file: src_path,
            code,
        })
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

#[derive(Debug)]
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
    type Err = Infallible;

    fn from_str(s: &str) -> std::result::Result<Self, Infallible> {
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

impl Extract for Html {
    fn extract_username(&self) -> Username {
        let extract = || {
            let a = self.select(selector!("#usermenu > a")).next()?;
            let name = a.text().next()?.to_owned();
            let src = a.select(selector!("img")).next()?.value().attr("src")?;
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
            let text = self
                .select(selector!("#content > div"))
                .flat_map(|r| r.text())
                .nth(1)?;
            let caps = lazy_regex!(
                "\\A / 実行時間制限 : 1ケース (\\d)\\.(\\d{3})秒 / メモリ制限 : \\d+ MB / \
                 (通常|スペシャルジャッジ|リアクティブ)問題.*\n?.*\\z",
            )
            .captures(text)?;
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
                    for paragraph in self.select(selector!(
                        "#content > div.block > div.sample > div.paragraph",
                    )) {
                        let pres = paragraph
                            .select(selector!("pre"))
                            .flat_map(|r| r.text())
                            .collect::<Vec<_>>();
                        guard!(pres.len() == 2);
                        let input = pres[0].to_owned();
                        let output = match kind {
                            ProblemKind::Regular => Some(pres[1].to_owned()),
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
            for tr in self.select(selector!("#content > div.left > table.table > tbody > tr")) {
                let name = tr.select(selector!("td")).nth(0)?.text().next()?.to_owned();
                let href = tr
                    .select(selector!("td"))
                    .nth(2)?
                    .select(selector!("a"))
                    .next()?
                    .value()
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
        self.select(selector!("#submit_form > input[name=\"csrf_token\"]"))
            .find_map(|r| r.value().attr("value").map(ToOwned::to_owned))
            .ok_or_else(ScrapeError::new)
    }

    fn extract_url_from_submit_page(&self) -> ScrapeResult<String> {
        self.select(selector!("#submit_form"))
            .find_map(|r| r.value().attr("action").map(ToOwned::to_owned))
            .ok_or_else(ScrapeError::new)
    }

    fn extract_langs(&self) -> ScrapeResult<NonEmptyIndexMap<String, String>> {
        let names = self
            .select(selector!("#lang > option"))
            .map(|option| {
                let name = option.text().next()?;
                let name = lazy_regex!(r"[\s\n]+")
                    .replace_all(name.trim(), " ")
                    .into_owned();
                let id = option.value().attr("value")?.to_owned();
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
    use scraper::Html;

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
        let html = get_html(rel_url)?;
        let suite = html.extract_samples()?;
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
        let html = get_html("/contests/100")?;
        let problems = html.extract_problems()?;
        assert_eq!(problems, own_pairs(EXPECTED));
        Ok(())
    }

    fn own_pairs<O: Borrow<B>, B: ToOwned<Owned = O> + ?Sized>(pairs: &[(&B, &B)]) -> Vec<(O, O)> {
        pairs
            .iter()
            .map(|(l, r)| ((*l).to_owned(), (*r).to_owned()))
            .collect()
    }

    fn get_html(rel_url: &str) -> reqwest::Result<Html> {
        let client = service::reqwest_sync_client(Duration::from_secs(60))?;
        let url = format!("https://yukicoder.me{}", rel_url);
        let text = client.get(&url).send()?.text()?;
        Ok(Html::parse_document(&text))
    }
}
