use serde::Serialize;
use snowchains_core::web::{
    Atcoder, AtcoderLoginCredentials, Codeforces, CodeforcesLoginCredentials, CookieStorage, Login,
    PlatformKind,
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

    let crate::Context { cwd: _, mut shell } = ctx;

    let cookie_storage = CookieStorage::with_jsonl(crate::web::credentials::cookie_store_path()?)?;

    let timeout = Some(crate::web::SESSION_TIMEOUT);

    let outcome = match service {
        PlatformKind::Atcoder => {
            let shell = RefCell::new(&mut shell);

            let credentials = AtcoderLoginCredentials {
                username_and_password: &mut crate::web::credentials::atcoder_username_and_password(
                    &shell,
                ),
            };

            Atcoder::exec(Login {
                credentials,
                cookie_storage,
                timeout,
                shell: &shell,
            })
        }
        PlatformKind::Codeforces => {
            let shell = RefCell::new(&mut shell);

            let credentials = CodeforcesLoginCredentials {
                username_and_password:
                    &mut crate::web::credentials::codeforces_username_and_password(&shell),
            };

            Codeforces::exec(Login {
                credentials,
                cookie_storage,
                timeout,
                shell: &shell,
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

    writeln!(shell.stdout, "{}", message)?;
    shell.stdout.flush().map_err(Into::into)
}
