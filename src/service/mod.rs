pub mod atcoder;
pub mod atcoder_beta;
pub mod hackerrank;
mod session;

use error::{ServiceError, ServiceErrorKind, ServiceResult};
use util;

use regex::Regex;
use rpassword;
use rprompt;
use std::ffi::OsStr;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::Path;
use term::{Attr, color};


/// Reads username and password from stdin, showing the prompts on stderr.
///
/// The password is not hidden if `rpassword::prompt_password_stderr` fails.
fn read_username_and_password(username_prompt: &'static str) -> io::Result<(String, String)> {
    let username = rprompt::prompt_reply_stderr(username_prompt)?;
    let password = rpassword::prompt_password_stderr("Password: ").or_else(
        |_| {
            eprintln_decorated!(Attr::Bold, Some(color::BRIGHT_MAGENTA), "FALLBACK");
            rprompt::prompt_reply_stderr("Password (not hidden): ")
        },
    )?;
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
        let mut replaced = vec![];
        let mut is_replaced = false;
        for line in code.lines() {
            let line = line?;
            if !is_replaced {
                if let Some(caps) = regex.captures(&line) {
                    if OsStr::new(&caps[2]) == stem {
                        replaced.push(format!("{}{}{}", &caps[1], class_name, &caps[3]));
                        is_replaced = true;
                        continue;
                    }
                }
            }
            replaced.push(line);
        }

        Ok(if is_replaced {
            info!(
                "The main class name was successfully replaced with {:?}",
                class_name
            );
            Some(replaced.join("\n"))
        } else {
            None
        })
    };

    lazy_static! {
        static ref RE_JAVA_CLASS: Regex =
            Regex::new(r"^(\s*public\s+class\s+)([a-zA-Z_\$][a-zA-Z0-9_\$]*)(.*)$").unwrap();
        static ref RE_SCALA_CLASS: Regex =
            Regex::new(r"^(\s*object\s+)([a-zA-Z_\$][a-zA-Z0-9_\$]*)(.*)$").unwrap();
    }
    let file = util::open_file(path)?;
    let stem = path.file_stem().unwrap_or_default();
    let extension = path.extension();
    let e = || ServiceError::from(ServiceErrorKind::ReplacingClassNameFailure(path.to_owned()));
    if extension == Some(OsStr::new("java")) {
        replace(file, &RE_JAVA_CLASS, stem)?.ok_or_else(e)
    } else if extension == Some(OsStr::new("scala")) {
        replace(file, &RE_SCALA_CLASS, stem)?.ok_or_else(e)
    } else {
        Ok(util::string_from_read(file)?)
    }
}
