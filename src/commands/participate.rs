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
) -> eyre::Result<()> {
    let OptParticipate {
        json,
        color: _,
        service: _,
        contest,
    } = opt;

    let crate::Context { cwd: _, mut shell } = ctx;

    let cookie_storage = CookieStorage::with_jsonl(crate::web::credentials::cookie_store_path()?)?;
    let timeout = Some(crate::web::SESSION_TIMEOUT);

    let kind = {
        let shell = RefCell::new(&mut shell);

        let target = AtcoderParticipateTarget { contest };

        let credentials = AtcoderParticipateCredentials {
            username_and_password: &mut crate::web::credentials::atcoder_username_and_password(
                &shell,
            ),
        };

        Atcoder::exec(Participate {
            target,
            credentials,
            cookie_storage,
            timeout,
            shell: &shell,
        })?
    };

    let outcome = Outcome { kind };

    if json {
        writeln!(shell.stdout, "{}", outcome.to_json())
    } else {
        writeln!(shell.stdout, "{}", outcome.message())
    }?;
    shell.stdout.flush()?;
    Ok(())
}
