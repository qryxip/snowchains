use crate::TtyOrPiped;
use anyhow::Context as _;
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
    io::{self, BufRead, Write},
    path::PathBuf,
};
use termcolor::WriteColor;

pub(crate) fn cookie_store_path() -> anyhow::Result<PathBuf> {
    let data_local_dir =
        dirs::data_local_dir().with_context(|| "Could not find the local date directory")?;
    Ok(data_local_dir.join("snowchains").join("cookies.jsonl"))
}

pub(crate) fn atcoder_username_and_password<'a, R: BufRead + 'a, W: WriteColor>(
    stdin: TtyOrPiped<R>,
    stderr: &'a RefCell<W>,
) -> impl FnMut() -> anyhow::Result<(String, String)> + 'a {
    username_and_password(stdin, stderr, "Username: ")
}

pub(crate) fn codeforces_username_and_password<'a, R: BufRead + 'a, W: WriteColor>(
    stdin: TtyOrPiped<R>,
    stderr: &'a RefCell<W>,
) -> impl FnMut() -> anyhow::Result<(String, String)> + 'a {
    username_and_password(stdin, stderr, "Handle/Email: ")
}

pub(crate) fn username_and_password<'a, R: BufRead + 'a, W: WriteColor>(
    mut stdin: TtyOrPiped<R>,
    stderr: &'a RefCell<W>,
    username_prompt: &'static str,
) -> impl FnMut() -> anyhow::Result<(String, String)> + 'a {
    move || -> _ {
        let mut stderr = stderr.borrow_mut();

        write!(stderr, "{}", username_prompt)?;
        stderr.flush()?;
        let username = stdin.read_reply()?;

        write!(stderr, "Password: ")?;
        stderr.flush()?;
        let password = stdin.read_password()?;

        Ok((username, password))
    }
}

pub(crate) fn dropbox_access_token() -> anyhow::Result<String> {
    let path = token_path("dropbox.json")?;

    let Dropbox { access_token } = crate::fs::read_json(&path)
        .with_context(|| format!("First, save the access token to `{}`", path.display()))?;

    return Ok(access_token);

    #[derive(Deserialize)]
    struct Dropbox {
        access_token: String,
    }
}

pub(crate) fn codeforces_api_key_and_secret(
    input: &mut TtyOrPiped<impl BufRead>,
    mut stderr: impl Write,
) -> anyhow::Result<(String, String)> {
    let path = token_path("codeforces.json")?;

    let Codeforces {
        api_key,
        api_secret,
    } = if path.exists() {
        crate::fs::read_json(path)?
    } else {
        let api_key = read_password(input, &mut stderr, "Codeforces `api_key`: ")?;
        let api_secret = read_password(input, stderr, "Codeforces `api_secret`: ")?;

        let pair = Codeforces {
            api_key,
            api_secret,
        };

        crate::fs::write_json(path, &pair, true)?;
        pair
    };

    return Ok((api_key, api_secret));

    #[derive(Deserialize, Serialize)]
    struct Codeforces {
        api_key: String,
        api_secret: String,
    }
}

pub(crate) fn yukicoder_api_key(
    input: &mut TtyOrPiped<impl BufRead>,
    stderr: impl Write,
) -> anyhow::Result<String> {
    let path = token_path("yukicoder.json")?;

    if path.exists() {
        crate::fs::read_json(path)
    } else {
        let api_key = read_password(input, stderr, "yukicoder API key: ")?;
        crate::fs::write_json(path, &api_key, true)?;
        Ok(api_key)
    }
}

fn token_path(file_name: &str) -> anyhow::Result<PathBuf> {
    let data_local_dir =
        dirs::data_local_dir().with_context(|| "Could not find the local data directory")?;

    Ok(data_local_dir
        .join("snowchains")
        .join("tokens")
        .join(file_name))
}

fn read_password(
    input: &mut TtyOrPiped<impl BufRead>,
    mut stderr: impl Write,
    prompt: &str,
) -> io::Result<String> {
    write!(stderr, "{}", prompt)?;
    stderr.flush()?;
    input.read_password()
}
