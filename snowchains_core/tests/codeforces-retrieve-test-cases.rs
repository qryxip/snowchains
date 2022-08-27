#![cfg(feature = "__test_with_credentials")]

use cookie_store::CookieStore;
use insta::{assert_debug_snapshot, assert_json_snapshot};
use reqwest::{Method, StatusCode};
use snowchains_core::web::{
    Codeforces, CodeforcesRetrieveSampleTestCasesCredentials, CookieStorage, ProblemsInContest,
    RetrieveTestCases, StatusCodeColor,
};
use std::{
    fmt,
    io::{self, Read as _},
    time::Duration,
};
use url::Url;

#[test]
fn contest340() -> eyre::Result<()> {
    use eyre::Context as _;
    use std::env;

    fn username_and_password() -> eyre::Result<(String, String)> {
        (|| {
            let username = env::var("CODEFORCES_USERNAME")?;
            let password = env::var("CODEFORCES_PASSWORD")?;
            Ok::<_, env::VarError>((username, password))
        })()
        .with_context(|| "`$CODEFORCES_USERNAME` and `$CODEFORCES_PASSWORD` are requied")
    }

    test("340", username_and_password)
}

fn test(
    contest: &str,
    username_and_password: fn() -> eyre::Result<(String, String)>,
) -> eyre::Result<()> {
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

    let outcome = Codeforces::exec(RetrieveTestCases {
        targets: ProblemsInContest::Indexes {
            contest: contest.to_owned(),
            problems: None,
        },
        credentials: CodeforcesRetrieveSampleTestCasesCredentials {
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
