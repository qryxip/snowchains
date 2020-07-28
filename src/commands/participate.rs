use crate::shell::Shell;
use serde::Serialize;
use snowchains_core::web::{
    Atcoder, AtcoderParticipateCredentials, AtcoderParticipateTarget, CookieStorage, Participate,
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
pub struct OptParticipate {
    /// Prints the result as JSON
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
    #[structopt(possible_value("atcoder"))]
    pub service: PlatformKind,

    /// Contest ID
    pub contest: String,
}

#[derive(Clone, Copy, Debug, Serialize)]
struct Outcome {
    kind: snowchains_core::web::ParticipateOutcome,
}

impl Outcome {
    fn to_json(self) -> String {
        serde_json::to_string(&self).expect("should not fail")
    }

    fn message(self) -> &'static str {
        self.kind.message()
    }
}

pub(crate) fn run(
    opt: OptParticipate,
    ctx: crate::Context<impl BufRead, impl Write, impl WriteColor>,
) -> anyhow::Result<()> {
    let OptParticipate {
        json,
        color: _,
        service: _,
        contest,
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

    let cookie_storage = CookieStorage::with_jsonl(crate::web::credentials::cookie_store_path()?)?;
    let timeout = Some(crate::web::SESSION_TIMEOUT);

    let stderr = RefCell::new(stderr);

    let shell = Shell::new(&stderr, false);

    let kind = Atcoder::exec(Participate {
        target: AtcoderParticipateTarget { contest },
        credentials: AtcoderParticipateCredentials {
            username_and_password: &mut crate::web::credentials::atcoder_username_and_password(
                stdin, &stderr,
            ),
        },
        cookie_storage,
        timeout,
        shell,
    })?;

    let outcome = Outcome { kind };

    if json {
        writeln!(stdout, "{}", outcome.to_json())
    } else {
        writeln!(stdout, "{}", outcome.message())
    }?;
    stdout.flush()?;
    Ok(())
}
