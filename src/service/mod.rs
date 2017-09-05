pub mod atcoder;
pub mod atcoder_beta;
mod scraping_session;

use super::error::{ServiceErrorKind, ServiceResult, ServiceResultExt};
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


fn halt_on_failure<T>(o: Option<T>, f: for<'a> fn(&'a T) -> bool) -> ServiceResult<T> {
    if let Some(x) = o {
        if !f(&x) {
            return Ok(x);
        }
    }
    bail!(ServiceErrorKind::ScrapingFailed);
}
