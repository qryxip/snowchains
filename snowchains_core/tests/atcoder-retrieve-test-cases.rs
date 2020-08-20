use cookie_store::CookieStore;
use insta::{assert_debug_snapshot, assert_json_snapshot};
use reqwest::{Method, StatusCode};
use snowchains_core::web::{
    Atcoder, AtcoderRetrieveSampleTestCasesCredentials, AtcoderRetrieveTestCasesTargets,
    CookieStorage, RetrieveTestCases, StatusCodeColor,
};
use std::{
    fmt,
    io::{self, Read as _},
    time::Duration,
};
use url::Url;

#[test]
fn abc003_samples() -> anyhow::Result<()> {
    test("abc003", || unreachable!())
}

#[test]
fn abc007_samples() -> anyhow::Result<()> {
    test("abc007", || unreachable!())
}

#[test]
fn abc019_samples() -> anyhow::Result<()> {
    test("abc019", || unreachable!())
}

#[test]
fn agc028_samples() -> anyhow::Result<()> {
    test("agc028", || unreachable!())
}

#[test]
fn agc047_samples() -> anyhow::Result<()> {
    test("agc047", || unreachable!())
}

#[test]
fn arc019_samples() -> anyhow::Result<()> {
    test("arc019", || unreachable!())
}

#[test]
fn arc021_samples() -> anyhow::Result<()> {
    test("arc021", || unreachable!())
}

#[cfg(feature = "__test_with_credentials")]
#[test]
fn practice_samples() -> anyhow::Result<()> {
    use anyhow::Context as _;
    use std::env;

    fn username_and_password() -> anyhow::Result<(String, String)> {
        (|| {
            let username = env::var("ATCODER_USERNAME")?;
            let password = env::var("ATCODER_PASSWORD")?;
            Ok::<_, env::VarError>((username, password))
        })()
        .with_context(|| "`$ATCODER_USERNAME` and `$ATCODER_PASSWORD` are requied")
    }

    test("practice", username_and_password)
}

fn test(
    contest: &str,
    username_and_password: fn() -> anyhow::Result<(String, String)>,
) -> anyhow::Result<()> {
    const TIMEOUT: Option<Duration> = Some(Duration::from_secs(30));

    struct Shell<'a>(&'a mut Vec<Message>);

    impl snowchains_core::web::Shell for Shell<'_> {
        fn print_ansi(&mut self, message: &[u8]) -> io::Result<()> {
            fn from_utf8(bytes: impl AsRef<[u8]>) -> io::Result<String> {
                let mut ret = "".to_owned();
                bytes.as_ref().read_to_string(&mut ret)?;
                Ok(ret)
            }

            self.0.push(Message::PrintAnsi(from_utf8(message)?));
            Ok(())
        }

        fn warn<T: fmt::Display>(&mut self, message: T) -> io::Result<()> {
            self.0.push(Message::Warn(message.to_string()));
            Ok(())
        }

        fn on_request(&mut self, req: &reqwest::blocking::Request) -> io::Result<()> {
            self.0
                .push(Message::OnRequest(req.method().clone(), req.url().clone()));
            Ok(())
        }

        fn on_response(
            &mut self,
            res: &reqwest::blocking::Response,
            status_code_color: StatusCodeColor,
        ) -> std::io::Result<()> {
            self.0
                .push(Message::OnResponse(res.status(), status_code_color));
            Ok(())
        }
    }

    #[derive(Debug)]
    enum Message {
        PrintAnsi(String),
        Warn(String),
        OnRequest(Method, Url),
        OnResponse(StatusCode, StatusCodeColor),
    }

    let mut messages = vec![];

    let outcome = Atcoder::exec(RetrieveTestCases {
        targets: AtcoderRetrieveTestCasesTargets {
            contest: contest.to_owned(),
            problems: None,
        },
        credentials: AtcoderRetrieveSampleTestCasesCredentials {
            username_and_password: &mut { username_and_password },
        },
        full: None,
        cookie_storage: CookieStorage {
            cookie_store: CookieStore::default(),
            on_update: Box::new(|_| Ok(())),
        },
        timeout: TIMEOUT,
        shell: Shell(&mut messages),
    })?;

    assert_json_snapshot!(format!("{}_samples_outcome", contest), outcome);
    assert_debug_snapshot!(format!("{}_samples_messages", contest), messages);
    Ok(())
}
