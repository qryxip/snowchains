use anyhow::{anyhow, Context as _};
use cookie_store::CookieStore;
use snowchains_core::web::{
    Atcoder, AtcoderSubmitCredentials, AtcoderSubmitTarget, Cookies, StandardStreamShell, Submit,
};
use std::{env, fs, path::PathBuf, str};
use structopt::StructOpt;
use strum::{EnumString, EnumVariantNames, VariantNames as _};
use termcolor::ColorChoice;

#[derive(StructOpt, Debug)]
struct Opt {
    #[structopt(long)]
    no_watch: bool,

    #[structopt(short, long, value_name("HUMANTIME"))]
    timeout: Option<humantime::Duration>,

    #[structopt(
        long,
        value_name("VIA"),
        default_value("prompt"),
        possible_values(CredentialsVia::VARIANTS)
    )]
    credentials: CredentialsVia,

    contest: String,

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
        no_watch,
        timeout,
        credentials,
        contest,
        problem,
        language_id,
        file,
    } = Opt::from_args();

    let mut cookies_jsonl = vec![];

    let outcome = Atcoder::exec(Submit {
        target: AtcoderSubmitTarget { contest, problem },
        language_id,
        code: fs::read_to_string(&file)
            .with_context(|| format!("Failed to read {}", file.display()))?,
        watch_submission: !no_watch,
        timeout: timeout.map(Into::into),
        cookies: Cookies {
            cookie_store: CookieStore::default(),
            on_update_cookie_store: |cookie_store| -> _ {
                cookies_jsonl.clear();
                cookie_store
                    .save_json(&mut cookies_jsonl)
                    .map_err(|e| anyhow!("{}", e))?;
                Ok(())
            },
        },
        shell: StandardStreamShell::new(if atty::is(atty::Stream::Stderr) {
            ColorChoice::Auto
        } else {
            ColorChoice::Never
        }),
        credentials: AtcoderSubmitCredentials {
            username_and_password: || {
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
    })?;

    eprintln!();
    dbg!(outcome);
    eprintln!("\n{}", str::from_utf8(&cookies_jsonl)?);

    Ok(())
}
