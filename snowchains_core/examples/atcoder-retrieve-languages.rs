use anyhow::{anyhow, Context as _};
use cookie_store::CookieStore;
use snowchains_core::web::{
    Atcoder, AtcoderRetrieveLanguagesCredentials, AtcoderRetrieveLanguagesTarget, CookieStorage,
    RetrieveLanguages, StandardStreamShell,
};
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

    #[structopt(long, value_name("PATH"))]
    cookies: Option<PathBuf>,

    #[structopt(short, long, requires("problem"))]
    contest: Option<String>,

    #[structopt(short, long, requires("contest"))]
    problem: Option<String>,
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
        cookies,
        contest,
        problem,
    } = Opt::from_args();

    let outcome = Atcoder::exec(RetrieveLanguages {
        target: AtcoderRetrieveLanguagesTarget {
            contest_and_problem: match (contest, problem) {
                (Some(contest), Some(problem)) => Some((contest, problem)),
                (Some(_), None) | (None, Some(_)) => unreachable!(),
                (None, None) => None,
            },
        },
        credentials: AtcoderRetrieveLanguagesCredentials {
            username_and_password: &mut || {
                let username_and_password = match credentials {
                    CredentialsVia::Prompt => (
                        rprompt::prompt_reply_stderr("Username: ")?,
                        rpassword::read_password_from_tty(Some("Password: "))?,
                    ),
                    CredentialsVia::Env => {
                        (env::var("ATCODER_USERNAME")?, env::var("ATCODER_PASSWORD")?)
                    }
                };
                Ok(username_and_password)
            },
        },
        cookie_storage: CookieStorage {
            cookie_store: CookieStore::default(),
            on_update: Box::new(move |cookie_store| -> _ {
                if let Some(cookies) = &cookies {
                    let mut content = vec![];

                    cookie_store
                        .save_json(&mut content)
                        .map_err(|e| anyhow!("{}", e))?;

                    fs::write(cookies, content)
                        .with_context(|| format!("Could not write `{}`", cookies.display()))?;
                }
                Ok(())
            }),
        },
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
