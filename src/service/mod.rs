pub mod atcoder;
mod scraping_session;

use super::error::{ServiceResult, ServiceResultExt};
use rpassword;
use rprompt;


fn read_username_and_password(username_prompt: &'static str) -> ServiceResult<(String, String)> {
    let username = rprompt::prompt_reply_stderr(&format!("{}: ", username_prompt))
        .chain_err(|| {
            format!(
                "Failed to read the {}. Use winpty if you are using mintty on Windows",
                username_prompt.to_lowercase()
            )
        })?;
    let password = rpassword::prompt_password_stderr("Password: ").chain_err(
        || "Failed to read the password. Use winpty if you are using mintty on Windows",
    )?;
    Ok((username, password))
}
