use anyhow::anyhow;
use cookie_store::CookieStore;
use snowchains_core::web::{
    Atcoder, RetrieveFullTestCases, RetrieveSampleTestCases, StandardStreamShell,
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

    #[structopt(short, long, value_name("PROBLEMS"))]
    problems: Option<Vec<String>>,

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
        full,
        timeout,
        credentials,
        problems,
        contest,
    } = Opt::from_args();

    let mut cookies_jsonl = vec![];

    let targets = (contest, problems.as_deref());
    let timeout = timeout.map(Into::into);
    let cookie_store = (CookieStore::default(), |cookie_store: &CookieStore| {
        cookies_jsonl.clear();
        cookie_store
            .save_json(&mut cookies_jsonl)
            .map_err(|e| anyhow!("{}", e))?;
        Ok(())
    });
    let shell = StandardStreamShell::new(if atty::is(atty::Stream::Stderr) {
        ColorChoice::Auto
    } else {
        ColorChoice::Never
    });
    let username_and_password = || {
        let username_and_password = match credentials {
            CredentialsVia::Prompt => (
                rprompt::prompt_reply_stderr("Username: ")?,
                rpassword::read_password_from_tty(Some("Password: "))?,
            ),
            CredentialsVia::Env => (env::var("ATCODER_USERNAME")?, env::var("ATCODER_PASSWORD")?),
        };
        Ok(username_and_password)
    };

    if full {
        Atcoder::exec(RetrieveFullTestCases {
            targets,
            timeout,
            cookie_store,
            shell,
            credentials: (username_and_password, || match credentials {
                CredentialsVia::Prompt => {
                    rpassword::read_password_from_tty(Some("Dropbox access token: "))
                        .map_err(Into::into)
                }
                CredentialsVia::Env => env::var("DROPBOX_ACCESS_TOKEN").map_err(Into::into),
            }),
        })?;

        eprintln!("{}", str::from_utf8(&cookies_jsonl)?);
    } else {
        let outcome = Atcoder::exec(RetrieveSampleTestCases {
            targets,
            timeout,
            cookie_store,
            shell,
            credentials: (username_and_password,),
        })?;

        dbg!(outcome);
        eprintln!("\n{}", str::from_utf8(&cookies_jsonl)?);
    }

    Ok(())
}