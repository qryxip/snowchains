use snowchains_core::web::{
    RetrieveFullTestCases, RetrieveTestCases, StandardStreamShell, Yukicoder,
    YukicoderRetrieveFullTestCasesCredentials, YukicoderRetrieveTestCasesTargets,
};
use std::{env, str};
use structopt::StructOpt;
use strum::{EnumString, EnumVariantNames, VariantNames as _};
use termcolor::ColorChoice;

#[derive(StructOpt, Debug)]
struct Opt {
    #[structopt(long)]
    full: bool,

    #[structopt(short, long, value_name("HUMANTIME"))]
    timeout: Option<humantime::Duration>,

    #[structopt(
        long,
        value_name("VIA"),
        default_value("prompt"),
        possible_values(CredentialsVia::VARIANTS)
    )]
    credentials: CredentialsVia,

    #[structopt(short, long, value_name("CONTEST_ID"))]
    contest: Option<String>,

    #[structopt(
        short,
        long,
        value_name("PROBLEM_NUMBERS_OR_SLUGS_IN_CONTEST"),
        required_unless("contest")
    )]
    problems: Option<Vec<String>>,
}

#[derive(EnumString, EnumVariantNames, Debug)]
#[strum(serialize_all = "kebab-case")]
enum CredentialsVia {
    Prompt,
    Env,
}

fn main() -> anyhow::Result<()> {
    let Opt {
        full,
        timeout,
        credentials,
        problems,
        contest,
    } = Opt::from_args();

    let outcome = Yukicoder::exec(RetrieveTestCases {
        targets: match (contest, problems) {
            (None, None) => unreachable!(),
            (None, Some(problem_nos)) => {
                YukicoderRetrieveTestCasesTargets::ProblemNos(problem_nos.into_iter().collect())
            }
            (Some(contest), problem_indexes) => YukicoderRetrieveTestCasesTargets::Contest(
                contest,
                problem_indexes.map(|ps| ps.into_iter().collect()),
            ),
        },
        credentials: (),
        full: if full {
            Some(RetrieveFullTestCases {
                credentials: YukicoderRetrieveFullTestCasesCredentials {
                    api_key: match credentials {
                        CredentialsVia::Prompt => {
                            rpassword::read_password_from_tty(Some("yukicoder API Key: "))?
                        }
                        CredentialsVia::Env => env::var("YUKICODER_API_KEY")?,
                    },
                },
            })
        } else {
            None
        },
        cookie_storage: (),
        timeout: timeout.map(Into::into),
        shell: StandardStreamShell::new(if atty::is(atty::Stream::Stderr) {
            ColorChoice::Auto
        } else {
            ColorChoice::Never
        }),
    })?;

    dbg!(outcome);

    Ok(())
}
