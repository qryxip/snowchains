use insta::{assert_debug_snapshot, assert_json_snapshot};
use maplit::btreeset;
use reqwest::{Method, StatusCode};
use snowchains_core::web::{
    RetrieveTestCases, StatusCodeColor, Yukicoder, YukicoderRetrieveTestCasesTargets,
};
use std::{
    fmt,
    io::{self, Read as _},
    time::Duration,
};
use url::Url;

#[test]
fn problem_no_1() -> eyre::Result<()> {
    test(Target::ProblemNo(1))
}

#[test]
fn contest_281_samples() -> eyre::Result<()> {
    test(Target::Contest(281))
}

fn test(target: Target) -> eyre::Result<()> {
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

    let outcome = Yukicoder::exec(RetrieveTestCases {
        targets: match target {
            Target::Contest(contest) => {
                YukicoderRetrieveTestCasesTargets::Contest(contest.to_string(), None)
            }
            Target::ProblemNo(problem_no) => {
                YukicoderRetrieveTestCasesTargets::ProblemNos(btreeset!(problem_no.to_string()))
            }
        },
        credentials: (),
        full: None,
        cookie_storage: (),
        timeout: TIMEOUT,
        shell: Shell(&mut messages),
    })?;

    let target = match target {
        Target::Contest(contest) => format!("contest_{}", contest),
        Target::ProblemNo(problem_no) => format!("problem_no_{}", problem_no),
    };

    assert_json_snapshot!(format!("{}_samples_outcome", target), outcome);
    assert_debug_snapshot!(format!("{}_samples_messages", target), messages);
    Ok(())
}

#[derive(Clone, Copy)]
enum Target {
    Contest(u64),
    ProblemNo(u64),
}
