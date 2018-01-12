pub mod atcoder;
pub mod atcoder_beta;
pub mod hackerrank;
mod session;

use errors::{ServiceError, ServiceErrorKind, ServiceResult};
use util;

use {rpassword, rprompt};
use regex::Regex;
use term::color;

use std::ffi::OsStr;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::Path;

/// Reads username and password from stdin, showing the prompts on stderr.
///
/// If fails to read a password because of OS error 6 or 32, askes a password again without hiding
/// the input.
fn read_username_and_password(username_prompt: &'static str) -> io::Result<(String, String)> {
    let errno_brokenpipe = if cfg!(target_os = "windows") { 6 } else { 32 };
    let username = rprompt::prompt_reply_stderr(username_prompt)?;
    let password =
        rpassword::prompt_password_stderr("Password: ").or_else(|e| match e.raw_os_error() {
            Some(n) if n == errno_brokenpipe => {
                eprintln_bold!(Some(color::BRIGHT_MAGENTA), "FALLBACK (os error {})", n);
                rprompt::prompt_reply_stderr("Password (not hidden): ")
            }
            _ => Err(e),
        })?;
    Ok((username, password))
}

/// Gets the value `x` if `Some(x) = o` and `!f(x)`.
///
/// # Errors
///
/// Returns `Err` if the above condition is not satisfied.
fn quit_on_failure<T>(o: Option<T>, f: for<'a> fn(&'a T) -> bool) -> ServiceResult<T> {
    if let Some(x) = o {
        if !f(&x) {
            return Ok(x);
        }
    }
    bail!(ServiceErrorKind::ScrapingFailed);
}

/// Reads a source code from `path`, replacing the main class name with `class_name` if the source
/// code is Java or Scala.
fn replace_class_name_if_necessary(path: &Path, class_name: &'static str) -> ServiceResult<String> {
    let replace = move |file: File, regex: &Regex, stem: &OsStr| -> io::Result<Option<String>> {
        let code = BufReader::new(file);
        let mut processed = vec![];
        let mut replaced_p = false;
        for line in code.lines() {
            let line = line?;
            if !replaced_p {
                if let Some(caps) = regex.captures(&line) {
                    if OsStr::new(&caps[2]) == stem {
                        processed.push(format!("{}{}{}", &caps[1], class_name, &caps[3]));
                        replaced_p = true;
                        continue;
                    }
                }
            }
            processed.push(line);
        }

        Ok(if replaced_p {
            info!(
                "The main class name was successfully replaced with {:?}",
                class_name
            );
            Some(processed.join("\n"))
        } else {
            None
        })
    };

    lazy_static! {
        static ref JAVA_CLASS: Regex =
            Regex::new(r"^(\s*public\s+class\s+)([a-zA-Z_\$][a-zA-Z0-9_\$]*)(.*)$").unwrap();
        static ref SCALA_CLASS: Regex =
            Regex::new(r"^(\s*object\s+)([a-zA-Z_\$][a-zA-Z0-9_\$]*)(.*)$").unwrap();
    }
    let file = util::open_file(path)?;
    let stem = path.file_stem().unwrap_or_default();
    let extension = path.extension();
    let e = || ServiceError::from(ServiceErrorKind::ReplacingClassNameFailure(path.to_owned()));
    if extension == Some(OsStr::new("java")) {
        replace(file, &JAVA_CLASS, stem)?.ok_or_else(e)
    } else if extension == Some(OsStr::new("scala")) {
        replace(file, &SCALA_CLASS, stem)?.ok_or_else(e)
    } else {
        Ok(util::string_from_read(file)?)
    }
}
