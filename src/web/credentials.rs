use anyhow::Context as _;
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
    io::{BufRead, Write},
    path::PathBuf,
};

pub(crate) fn cookie_store_path() -> anyhow::Result<PathBuf> {
    let data_local_dir =
        dirs::data_local_dir().with_context(|| "Could not find the local date directory")?;
    Ok(data_local_dir.join("snowchains").join("cookies.jsonl"))
}

pub(crate) fn atcoder_username_and_password<'a, R: BufRead, W1, W2: Write>(
    shell: &'a RefCell<&'a mut crate::shell::Shell<R, W1, W2>>,
) -> impl FnMut() -> anyhow::Result<(String, String)> + 'a {
    username_and_password(shell, "Username: ")
}

pub(crate) fn codeforces_username_and_password<'a, R: BufRead, W1, W2: Write>(
    shell: &'a RefCell<&'a mut crate::shell::Shell<R, W1, W2>>,
) -> impl FnMut() -> anyhow::Result<(String, String)> + 'a {
    username_and_password(shell, "Handle/Email: ")
}

pub(crate) fn username_and_password<'a, R: BufRead, W1, W2: Write>(
    shell: &'a RefCell<&'a mut crate::shell::Shell<R, W1, W2>>,
    username_prompt: &'static str,
) -> impl FnMut() -> anyhow::Result<(String, String)> + 'a {
    move || -> _ {
        let mut shell = shell.borrow_mut();
        let username = shell.read_reply(username_prompt)?;
        let password = shell.read_password("Password: ")?;
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
    shell: &mut crate::shell::Shell<impl BufRead, impl Sized, impl Write>,
) -> anyhow::Result<(String, String)> {
    let path = token_path("codeforces.json")?;

    let Codeforces {
        api_key,
        api_secret,
    } = if path.exists() {
        crate::fs::read_json(path)?
    } else {
        let api_key = shell.read_password("Codeforces `api_key`: ")?;
        let api_secret = shell.read_password("Codeforces `api_secret`: ")?;

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
    shell: &mut crate::shell::Shell<impl BufRead, impl Sized, impl Write>,
) -> anyhow::Result<String> {
    let path = token_path("yukicoder.json")?;

    if path.exists() {
        crate::fs::read_json(path)
    } else {
        let api_key = shell.read_password("yukicoder API key: ")?;
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
