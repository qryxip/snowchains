use crate::shell::Shell;
use serde::Serialize;
use snowchains_core::web::{
    Atcoder, AtcoderLoginCredentials, Codeforces, CodeforcesLoginCredentials, Login, PlatformKind,
};
use std::{
    cell::RefCell,
    io::{BufRead, Write},
};
use structopt::StructOpt;
use strum::VariantNames as _;
use termcolor::WriteColor;

#[derive(StructOpt, Debug)]
pub struct OptLogin {
    /// Prints the output as a JSON value
    #[structopt(long)]
    pub json: bool,

    /// Coloring
    #[structopt(
        long,
        possible_values(crate::ColorChoice::VARIANTS),
        default_value("auto")
    )]
    pub color: crate::ColorChoice,

    /// Target platform
    #[structopt(possible_values(&["atcoder", "codeforces"]))]
    pub service: PlatformKind,
}

#[derive(Clone, Copy, Debug, Serialize)]
struct Outcome {
    kind: snowchains_core::web::LoginOutcome,
}

impl Outcome {
    fn to_json(self) -> String {
        serde_json::to_string(&self).expect("should not fail")
    }
}

pub(crate) fn run(
    opt: OptLogin,
    ctx: crate::Context<impl BufRead, impl Write, impl WriteColor>,
) -> anyhow::Result<()> {
    let OptLogin {
        json,
        color: _,
        service,
    } = opt;

    let crate::Context {
        cwd: _,
        stdin,
        mut stdout,
        stderr,
        stdin_process_redirection: _,
        stdout_process_redirection: _,
        stderr_process_redirection: _,
        draw_progress: _,
    } = ctx;

    let cookie_storage = crate::web::cookie_storage::cookie_storage()?;

    let stderr = RefCell::new(stderr);
    let shell = Shell::new(&stderr, false);

    let timeout = Some(crate::web::SESSION_TIMEOUT);

    let outcome = match service {
        PlatformKind::Atcoder => {
            let credentials = AtcoderLoginCredentials {
                username_and_password: &mut crate::web::credentials::atcoder_username_and_password(
                    stdin, &stderr,
                ),
            };

            Atcoder::exec(Login {
                credentials,
                cookie_storage,
                timeout,
                shell,
            })
        }
        PlatformKind::Codeforces => {
            let credentials = CodeforcesLoginCredentials {
                username_and_password:
                    &mut crate::web::credentials::codeforces_username_and_password(stdin, &stderr),
            };

            Codeforces::exec(Login {
                credentials,
                cookie_storage,
                timeout,
                shell,
            })
        }
        PlatformKind::Yukicoder => unreachable!("should be filtered by `possible_values`"),
    }?;

    let message = if json {
        Outcome { kind: outcome }.to_json()
    } else {
        match outcome {
            snowchains_core::web::LoginOutcome::Success => "Successfully logged in.",
            snowchains_core::web::LoginOutcome::AlreadyLoggedIn => "Already logged in.",
        }
        .to_owned()
    };

    writeln!(stdout, "{}", message)?;
    stdout.flush().map_err(Into::into)
}
