use anyhow::{anyhow, Context as _};
use cookie_store::CookieStore;
use snowchains_core::web::{
    Codeforces, CodeforcesSubmitCredentials, CodeforcesSubmitTarget, CookieStorage,
    StandardStreamShell, Submit,
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

    contest: u64,

    problem: String,

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
        cookies,
        contest,
        problem,
        language_id,
        file,
    } = Opt::from_args();

    let api_key = match credentials {
        CredentialsVia::Prompt => rprompt::prompt_reply_stderr("API Key: ")?,
        CredentialsVia::Env => env::var("CODEFORCES_API_KEY")?,
    };

    let api_secret = match credentials {
        CredentialsVia::Prompt => rpassword::read_password_from_tty(Some("API Secret: "))?,
        CredentialsVia::Env => env::var("CODEFORCES_API_SECRET")?,
    };

    let outcome = Codeforces::exec(Submit {
        target: CodeforcesSubmitTarget { contest, problem },
        credentials: CodeforcesSubmitCredentials {
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
            api_key,
            api_secret,
        },
        language_id,
        code: fs::read_to_string(&file)
            .with_context(|| format!("Failed to read {}", file.display()))?,
        watch_submission: false,
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
