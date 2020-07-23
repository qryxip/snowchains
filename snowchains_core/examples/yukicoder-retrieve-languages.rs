use snowchains_core::web::{RetrieveLanguages, StandardStreamShell, Yukicoder};
use std::str;
use structopt::StructOpt;
use strum::{EnumString, EnumVariantNames};
use termcolor::ColorChoice;

#[derive(StructOpt, Debug)]
struct Opt {
    #[structopt(short, long, value_name("HUMANTIME"))]
    timeout: Option<humantime::Duration>,
}

#[derive(EnumString, EnumVariantNames, Debug)]
#[strum(serialize_all = "kebab-case")]
enum CredentialsVia {
    Prompt,
    Env,
}

fn main() -> anyhow::Result<()> {
    let Opt { timeout } = Opt::from_args();

    let outcome = Yukicoder::exec(RetrieveLanguages {
        target: (),
        credentials: (),
        cookies: (),
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
