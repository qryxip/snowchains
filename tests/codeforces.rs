mod service;

use snowchains::app::{App, Opt};
use snowchains::service::ServiceKind;
use snowchains::terminal::{AnsiColorChoice, TermImpl};

use failure::Fallible;

#[test]
fn it_logins() -> Fallible<()> {
    fn login(mut app: App<TermImpl<&[u8], Vec<u8>, Vec<u8>>>) -> snowchains::Result<()> {
        app.run(Opt::Login {
            color_choice: AnsiColorChoice::Never,
            service: ServiceKind::Codeforces,
        })
    }

    let stdin = credentials_as_input()?;
    service::test_in_tempdir("it_logins", &stdin, login)
}

fn credentials_as_input() -> Fallible<String> {
    let username = service::env_var("CODEFORCES_USERNAME")?;
    let password = service::env_var("CODEFORCES_PASSWORD")?;
    let api_key = service::env_var("CODEFORCES_API_KEY")?;
    let api_secret = service::env_var("CODEFORCES_API_SECRET")?;
    Ok(format!(
        "{}\n{}\n{}\n{}\n",
        username, password, api_key, api_secret,
    ))
}
