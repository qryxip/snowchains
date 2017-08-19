use super::scraping_session::ScrapingSession;
use super::super::error::{ServiceError, ServiceErrorKind, ServiceResult};
use super::super::judge::Cases;
use html5ever;
use html5ever::rcdom::{Handle, NodeData, RcDom};
use html5ever::tendril::TendrilSink;
use regex::Regex;
use reqwest::StatusCode;
use rpassword;
use rprompt;
use std::default::Default;
use std::io::{self, Read};
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};
use term::{Attr, color};


pub fn login() -> ServiceResult<()> {
    fn verify(session: &mut ScrapingSession) -> bool {
        static URL: &'static str = "https://practice.contest.atcoder.jp/settings";
        session.http_get(URL).is_ok()
    }

    if let Ok(mut session) = ScrapingSession::from_cookie_file("atcoder") {
        if verify(&mut session) {
            return Ok(println!("Already signed in."));
        }
    }
    AtCoder::login_and_save().map(|_| ())
}


pub fn participate(contest_name: &str) -> ServiceResult<()> {
    AtCoder::load_or_login()?.participate(contest_name)
}


pub fn download(
    contest_name: &str,
    path_to_save: &Path,
    extension: &'static str,
) -> ServiceResult<()> {
    let mut atcoder = AtCoder::load_or_login()?;
    atcoder.download_all_tasks(contest_name, path_to_save, extension)
}


struct AtCoder(ScrapingSession);

impl Deref for AtCoder {
    type Target = ScrapingSession;
    fn deref(&self) -> &ScrapingSession {
        &self.0
    }
}

impl DerefMut for AtCoder {
    fn deref_mut(&mut self) -> &mut ScrapingSession {
        &mut self.0
    }
}

impl AtCoder {
    fn load_or_login() -> ServiceResult<Self> {
        fn verify(session: &mut ScrapingSession) -> bool {
            static URL: &'static str = "https://practice.contest.atcoder.jp/settings";
            session.http_get(URL).is_ok()
        }

        if let Ok(mut session) = ScrapingSession::from_cookie_file("atcoder") {
            if verify(&mut session) {
                return Ok(AtCoder(session));
            }
        }
        Self::login_and_save()
    }

    fn participate(&mut self, contest_name: &str) -> ServiceResult<()> {
        let url = format!(
            "https://{}.contest.atcoder.jp/participants/insert",
            contest_name
        );
        self.http_get_expecting(&url, StatusCode::Found).map(|_| ())
    }

    fn download_all_tasks(
        &mut self,
        contest_name: &str,
        path_to_save: &Path,
        extension: &'static str,
    ) -> ServiceResult<()> {
        let names_and_pathes = {
            let url = format!("http://{}.contest.atcoder.jp/assignments", contest_name);
            let mut response = self.http_get(&url)?;
            extract_names_and_pathes(&mut response)?
        };
        for (alphabet, path) in names_and_pathes {
            let url = format!("http://{}.contest.atcoder.jp{}", contest_name, path);
            let mut response = self.http_get(&url)?;
            match extract_cases(&mut response) {
                Ok(cases) => {
                    let mut pathbuf = PathBuf::from(path_to_save);
                    pathbuf.push(alphabet.to_lowercase());
                    pathbuf.set_extension(extension);
                    if let Err(e) = cases.save(&pathbuf) {
                        eprintln!("{:?}", e);
                        unimplemented!();
                    }
                    println!("Task {}: saved to {:?}", alphabet, pathbuf);
                }
                Err(ServiceError(ServiceErrorKind::ScrapingFailed, _)) => {
                    println!("Failed to scrape. Ignoring.");
                }
                Err(e) => return Err(e),
            }
        }
        Ok(self.save()?)
    }

    fn login_and_save() -> ServiceResult<Self> {
        #[derive(Serialize)]
        struct PostData {
            name: String,
            password: String,
        }

        fn post_data() -> io::Result<PostData> {
            let user_id = rprompt::prompt_reply_stderr("User ID: ")?;
            let password = rpassword::prompt_password_stderr("Password: ")?;
            Ok(PostData {
                name: user_id,
                password: password,
            })
        }

        static URL: &'static str = "https://practice.contest.atcoder.jp/login";
        let mut session = ScrapingSession::new();
        session.http_get(URL)?;
        while let Err(e) = session.http_post_urlencoded(URL, post_data()?, StatusCode::Found) {
            eprint_decorated!(Attr::Bold, Some(color::RED), "error: ");
            eprintln!("{:?}", e);
            println!("Failed to sign in. try again.")
        }
        let atcoder = AtCoder(session);
        atcoder.show_username();
        atcoder.save()?;
        Ok(atcoder)
    }

    fn show_username(&self) {
        let username = self.cookie_value("_user_name", "atcoder.jp")
            .unwrap_or_default();
        println!("Hello, {}.", username);
    }

    fn save(&self) -> io::Result<()> {
        self.save_cookie_to_file("atcoder")
    }
}


macro_rules! matched_tags {
    ($handle: ident, $tag_name: expr, $class_name: expr) => {
        $handle.children.borrow().iter()
            .filter(|child| is_certain_tag(&child.data, $tag_name, $class_name))
    }
}

macro_rules! find_certain_tag {
    ($handle: ident, $tag_name: expr, $class_name: expr) => {
        $handle.children.borrow().iter()
            .find(|child| is_certain_tag(&child.data, $tag_name, $class_name))
    }
}

macro_rules! find_by_id {
    ($handle: ident, $id: expr) => {
        $handle.children.borrow().iter().find(|child| is_certain_id(&child.data, $id))
    }
}


fn extract_names_and_pathes<R: Read>(html: &mut R) -> ServiceResult<Vec<(String, String)>> {
    fn extract(handle: &Handle) -> Option<(String, String)> {
        if let NodeData::Element {
            ref name,
            ref attrs,
            ..
        } = handle.data
        {
            if format!("{}", name.local) == "a" {
                for attr in attrs.borrow().iter() {
                    if format!("{}", attr.name.local) == "href" {
                        let url = format!("{}", attr.value);
                        if let Some(handle) = handle.children.borrow().iter().next() {
                            let ref node = handle.data;
                            if let NodeData::Text { ref contents } = *node {
                                let contents = contents
                                    .borrow()
                                    .chars()
                                    .flat_map(|c| c.escape_default())
                                    .collect();
                                return Some((contents, url));
                            }
                        }
                    }
                }
            }
        }
        None
    }

    let handle = html5ever::parse_document(RcDom::default(), Default::default())
        .from_utf8()
        .read_from(html)?
        .document;
    if let Some(handle) = find_certain_tag!(handle, "html", None) {
        if let Some(handle) = find_certain_tag!(handle, "body", None) {
            if let Some(handle) = find_by_id!(handle, "outer-inner") {
                if let Some(handle) = find_certain_tag!(handle, "table", None) {
                    if let Some(handle) = find_certain_tag!(handle, "tbody", None) {
                        let mut result = Vec::new();
                        for handle in matched_tags!(handle, "tr", None) {
                            if let Some(handle) = find_certain_tag!(handle, "td", None) {
                                if let Some(handle) = find_certain_tag!(handle, "a", None) {
                                    if let Some(name_and_url) = extract(handle) {
                                        result.push(name_and_url);
                                    }
                                }
                            }
                        }
                        return Ok(result);
                    }
                }
            }
        }
    }
    bail!(ServiceErrorKind::ScrapingFailed)
}


fn extract_cases<R: Read>(html: &mut R) -> ServiceResult<Cases> {
    fn extract_timelimit_as_millis(outer_inner_handle: &Handle) -> ServiceResult<u64> {
        let re_timelimit = Regex::new("\\D*([0-9]+)sec.*").unwrap();
        for handle in matched_tags!(outer_inner_handle, "p", None) {
            for handle in handle.children.borrow().iter() {
                if let NodeData::Text { ref contents } = handle.data {
                    if let Some(caps) = re_timelimit.captures(&contents.borrow()) {
                        return Ok(1000 * caps[1].parse::<u64>().unwrap());
                    }
                }
            }
        }
        bail!(ServiceErrorKind::ScrapingFailed)
    }

    fn extract_samples(span_handle: &Handle) -> Vec<(String, String)> {
        fn extract_from_section(section_handle: &Handle) -> Option<String> {
            if let Some(handle) = find_certain_tag!(section_handle, "pre", None) {
                for handle in handle.children.borrow().iter() {
                    if let NodeData::Text { ref contents } = handle.data {
                        return Some(format!("{}", *contents.borrow()));
                    }
                }
            }
            None
        }

        let re_input = Regex::new("^入力例 ([0-9]+)$").unwrap();
        let re_output = Regex::new("^出力例 ([0-9]+)$").unwrap();
        let (mut expected, mut input) = (Vec::new(), Vec::new());
        for handle in matched_tags!(span_handle, "div", Some("part")) {
            if let Some(handle) = find_certain_tag!(handle, "section", None) {
                if let Some(h3_handle) = find_certain_tag!(handle, "h3", None) {
                    if let Some(h3_contents_handle) = h3_handle.children.borrow().iter().next() {
                        if let NodeData::Text { ref contents } = h3_contents_handle.data {
                            if let Some(_) = re_input.captures(&contents.borrow()) {
                                if let Some(text) = extract_from_section(&handle) {
                                    input.push(text);
                                }
                            } else if let Some(_) = re_output.captures(&contents.borrow()) {
                                if let Some(text) = extract_from_section(&handle) {
                                    expected.push(text);
                                }
                            }
                        }
                    }
                }
            }
        }
        expected.into_iter().zip(input.into_iter()).collect()
    }

    let handle = html5ever::parse_document(RcDom::default(), Default::default())
        .from_utf8()
        .read_from(html)?
        .document;
    if let Some(child) = find_certain_tag!(handle, "html", None) {
        for child in child.children.borrow().iter() {
            if is_certain_tag(&child.data, "head", None) {
                if let Some(child) = find_certain_tag!(child, "title", None) {
                    for child in child.children.borrow().iter() {
                        if let NodeData::Text { ref contents } = child.data {
                            let re_404 = Regex::new("^.*404.*$").unwrap();
                            if re_404.is_match(&contents.borrow()) {
                                bail!(ServiceErrorKind::ScrapingFailed);
                            }
                        }
                    }
                }
            } else if is_certain_tag(&child.data, "body", None) {
                if let Some(child) = find_by_id!(child, "outer-inner") {
                    let timeout = extract_timelimit_as_millis(&child)?;
                    if let Some(child) = find_by_id!(child, "task-statement") {
                        if let Some(child) = find_certain_tag!(child, "span", Some("lang")) {
                            if let Some(child) = find_certain_tag!(child, "span", Some("lang-ja")) {
                                let result = extract_samples(&child);
                                return if result.is_empty() {
                                    bail!(ServiceErrorKind::ScrapingFailed)
                                } else {
                                    Ok(Cases::from_text(timeout, result))
                                };
                            }
                        }
                    }
                }
            }
        }
    }
    bail!(ServiceErrorKind::ScrapingFailed)
}


fn is_certain_tag(
    node_data: &NodeData,
    tag_name: &'static str,
    class_name: Option<&'static str>,
) -> bool {
    if let NodeData::Element {
        ref name,
        ref attrs,
        ..
    } = *node_data
    {
        if format!("{}", name.local) == tag_name {
            return match class_name {
                Some(class_name) => {
                    attrs.borrow().iter().any(|attr| {
                        format!("{}", attr.name.local) == "class" &&
                            format!("{}", attr.value) == class_name
                    })
                }
                None => true,
            };
        }
    }
    false
}


fn is_certain_id(node_data: &NodeData, id: &'static str) -> bool {
    if let NodeData::Element { name: _, ref attrs, .. } = *node_data {
        return attrs.borrow().iter().any(|attr| {
            format!("{}", attr.name.local) == "id" && format!("{}", attr.value) == id
        });
    }
    false
}
