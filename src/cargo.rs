use super::error::{JudgeErrorKind, JudgeResult};
use super::judge;
use super::testcase::Cases;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;


pub fn judge(cases: &str, target: &str, args: &[&str]) -> JudgeResult<()> {
    cargo_build_release()?;
    let cases = Cases::load(&cases_path(cases)?)?;
    judge::judge_all(cases, target_path(target)?.as_path(), args)
}


pub fn cases_path(problem: &str) -> JudgeResult<PathBuf> {
    let mut path = crate_root()?;
    path.push(problem);
    Ok(path)
}


fn cargo_build_release() -> JudgeResult<()> {
    if Command::new("cargo")
        .args(&["build", "--release"])
        .spawn()?
        .wait()?
        .success()
    {
        Ok(())
    } else {
        bail!(JudgeErrorKind::BuildFailed)
    }
}


fn target_path(problem: &str) -> JudgeResult<PathBuf> {
    let mut path = crate_root()?;
    path.push("target");
    path.push("release");
    path.push(problem);
    Ok(path)
}


fn crate_root() -> JudgeResult<PathBuf> {
    fn find_cargo_toml<P: AsRef<Path>>(dir: P) -> JudgeResult<bool> {
        // Doesn't care if "Cargo.toml" is a directory because `cargo build` fails if such a
        // directory exists.
        for entry in fs::read_dir(dir)? {
            if &entry?.file_name() == "Cargo.toml" {
                return Ok(true);
            }
        }
        Ok(false)
    }

    let mut dir = env::current_dir()?;
    loop {
        if find_cargo_toml(&dir)? {
            return Ok(dir);
        } else if !dir.pop() {
            bail!(JudgeErrorKind::ProjectNotFound);
        }
    }
}
