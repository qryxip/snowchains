use anyhow::{anyhow, Context as _};
use cookie_store::CookieStore;
use snowchains_core::web::{
    Codeforces, CodeforcesRetrieveSampleTestCasesCredentials, CodeforcesRetrieveTestCasesTargets,
    CookieStorage, RetrieveTestCases, StandardStreamShell,
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

    #[structopt(short, long)]
    problems: Option<Vec<String>>,

    contest: u64,
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
        problems,
        contest,
    } = Opt::from_args();

    let outcome = Codeforces::exec(RetrieveTestCases {
        targets: CodeforcesRetrieveTestCasesTargets {
            contest,
            problems: problems.map(|ps| ps.into_iter().collect()),
        },
        credentials: CodeforcesRetrieveSampleTestCasesCredentials {
            username_and_password: &mut || {
                let username_and_password = match credentials {
                    CredentialsVia::Prompt => (
                        rprompt::prompt_reply_stderr("Handle/Email: ")?,
                        rpassword::read_password_from_tty(Some("Password: "))?,
                    ),
                    CredentialsVia::Env => (
                        env::var("CODEFORCES_USERNAME")?,
                        env::var("CODEFORCES_PASSWORD")?,
                    ),
                };
                Ok(username_and_password)
            },
        },
        full: None,
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
