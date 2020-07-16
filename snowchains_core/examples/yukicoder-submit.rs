use anyhow::Context as _;
use either::Either;
use snowchains_core::web::{StandardStreamShell, Submit, Yukicoder};
use std::{env, fs, path::PathBuf, str};
use structopt::StructOpt;
use strum::{EnumString, EnumVariantNames, VariantNames as _};
use termcolor::ColorChoice;

#[derive(StructOpt, Debug)]
struct Opt {
    #[structopt(short, long, value_name("HUMANTIME"))]
    timeout: Option<humantime::Duration>,

    #[structopt(
        long,
        value_name("VIA"),
        default_value("prompt"),
        possible_values(CredentialsVia::VARIANTS)
    )]
    credentials: CredentialsVia,

    #[structopt(long, value_name("CONTEST_ID"))]
    contest: Option<String>,

    problem_no_or_slug: String,

    language_id: String,

    file: PathBuf,
}

#[derive(EnumString, EnumVariantNames, Debug)]
#[strum(serialize_all = "kebab-case")]
enum CredentialsVia {
    Prompt,
    Env,
}

fn main() -> anyhow::Result<()> {
    let Opt {
        timeout,
        credentials,
        contest,
        problem_no_or_slug,
        language_id,
        file,
    } = Opt::from_args();

    let outcome = Yukicoder::exec(Submit {
        target: if let Some(contest) = contest {
            Either::Right((contest, problem_no_or_slug))
        } else {
            Either::Left(problem_no_or_slug)
        },
        language_id,
        code: fs::read_to_string(&file)
            .with_context(|| format!("Failed to read {}", file.display()))?,
        watch_submission: false,
        timeout: timeout.map(Into::into),
        cookie_store: (),
        shell: StandardStreamShell::new(if atty::is(atty::Stream::Stderr) {
            ColorChoice::Auto
        } else {
            ColorChoice::Never
        }),
        credentials: (|| match credentials {
            CredentialsVia::Prompt => {
                rpassword::read_password_from_tty(Some("yukicoder API Key: ")).map_err(Into::into)
            }
            CredentialsVia::Env => env::var("YUKICODER_API_KEY").map_err(Into::into),
        },),
    })?;

    dbg!(outcome);

    Ok(())
}