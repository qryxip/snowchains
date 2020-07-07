use anyhow::anyhow;
use cookie_store::CookieStore;
use snowchains_core::web::{Atcoder, Participate, StandardStreamShell};
use std::{env, str};
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

    contest: String,
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
    } = Opt::from_args();

    let mut cookies_jsonl = vec![];

    let outcome = Atcoder::exec(Participate {
        target: contest,
        timeout: timeout.map(Into::into),
        cookie_store: (CookieStore::default(), |cookie_store: &CookieStore| -> _ {
            cookies_jsonl.clear();
            cookie_store
                .save_json(&mut cookies_jsonl)
                .map_err(|e| anyhow!("{}", e))?;
            Ok(())
        }),
        shell: StandardStreamShell::new(if atty::is(atty::Stream::Stderr) {
            ColorChoice::Auto
        } else {
            ColorChoice::Never
        }),
        credentials: (|| {
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
        },),
    })?;

    dbg!(outcome);
    eprintln!("\n{}", str::from_utf8(&cookies_jsonl)?);

    Ok(())
}
