use errors::{FileIoError, FileIoErrorKind, FileIoResult, FileIoResultExt};

use std::{env, str};
use std::borrow::Cow;
use std::fs::{self, File};
use std::io::{self, Read};
use std::path::{Path, PathBuf};

/// Opens a file in read only mode.
pub fn open_file(path: &Path) -> FileIoResult<File> {
    File::open(path).chain_err(|| FileIoErrorKind::OpenInReadOnly(path.to_owned()))
}

/// Opens a file in write only mode creating its parent directory.
pub fn create_file_and_dirs(path: &Path) -> FileIoResult<File> {
    if let Some(dir) = path.parent() {
        if !dir.exists() {
            fs::create_dir_all(dir)?;
        }
    }
    File::create(path).chain_err(|| FileIoErrorKind::OpenInWriteOnly(path.to_owned()))
}

/// Returns a `String` read from `read`.
pub fn string_from_read<R: Read>(mut read: R, capacity: usize) -> io::Result<String> {
    let mut buf = String::with_capacity(capacity);
    read.read_to_string(&mut buf)?;
    Ok(buf)
}

/// Reads a file content into a string.
pub fn string_from_file_path(path: &Path) -> FileIoResult<String> {
    let file = open_file(path)?;
    let len = file.metadata()?.len() as usize;
    string_from_read(file, len).map_err(Into::into)
}

/// Prints `s` ignoring a trailing newline if it exists.
pub fn eprintln_trimming_trailing_newline(s: &str) {
    eprintln!("{}", trim_trailing_newline(s));
}

pub(self) fn trim_trailing_newline(s: &str) -> Cow<str> {
    if s.ends_with("\r\n") {
        let n = s.len();
        String::from_utf8_lossy(&s.as_bytes()[..n - 2])
    } else if s.ends_with('\n') {
        let n = s.len();
        String::from_utf8_lossy(&s.as_bytes()[..n - 1])
    } else {
        s.into()
    }
}

/// Returns `~/<names>` as `io::Result`.
///
/// # Errors
///
/// Returns `Err` IFF a home directory not found.
pub fn path_under_home(names: &[&str]) -> FileIoResult<PathBuf> {
    let home_dir =
        env::home_dir().ok_or_else::<FileIoError, _>(|| FileIoErrorKind::HomeDirNotFound.into())?;
    Ok(names.iter().fold(home_dir, |mut path, name| {
        path.push(name);
        path
    }))
}

pub fn expand_path<'a, B: Into<Option<&'a Path>>>(path: &str, base: B) -> FileIoResult<PathBuf> {
    fn expand(path: &str, base: Option<&Path>) -> FileIoResult<PathBuf> {
        let mut chars = path.chars();
        let (c1, c2) = (chars.next(), chars.next());
        if c1 == Some('~') {
            return if [Some('/'), Some('\\'), None].contains(&c2) {
                path_under_home(&[&chars.collect::<String>()]).and_then(|path| {
                    if path.is_absolute() {
                        Ok(path)
                    } else {
                        let mut r = env::current_dir()?;
                        r.push(&path);
                        Ok(r)
                    }
                })
            } else {
                bail!(FileIoErrorKind::UnsupportedUseOfTilde);
            };
        }
        let path = PathBuf::from(path);
        Ok(if path.is_absolute() {
            path
        } else {
            let mut pathbuf = base.map(PathBuf::from).unwrap_or_default();
            pathbuf.push(path);
            pathbuf
        })
    }

    expand(path, base.into()).chain_err(|| FileIoErrorKind::Expand(path.to_owned()))
}

pub fn cfg_windows() -> bool {
    cfg!(windows)
}

pub fn is_default<T: Default + PartialEq>(x: &T) -> bool {
    x == &T::default()
}

pub trait OkAsRefOr {
    type Item;
    /// Get the value `Ok(&x)` if `Some(ref x) = self`.
    ///
    /// # Errors
    ///
    /// Returns `Err(e)` if `self` is `None`.
    fn ok_as_ref_or<E>(&self, e: E) -> Result<&Self::Item, E>;
}

impl<T> OkAsRefOr for Option<T> {
    type Item = T;

    fn ok_as_ref_or<E>(&self, e: E) -> Result<&T, E> {
        match *self {
            Some(ref x) => Ok(x),
            None => Err(e),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::path::Path;

    #[test]
    fn it_trims_a_trailing_newline() {
        assert_eq!("aaa", super::trim_trailing_newline("aaa\r\n"));
        assert_eq!("bbb", super::trim_trailing_newline("bbb\n"));
        assert_eq!("ccc", super::trim_trailing_newline("ccc"));
        assert_eq!("ddd\r", super::trim_trailing_newline("ddd\r"));
    }

    #[test]
    fn it_expands_a_path() {
        assert_eq!(env::home_dir(), super::expand_path("~/", None).ok());
        super::expand_path("~root/", None).unwrap_err();
        assert_eq!(
            Path::new("/a/b/c/d"),
            super::expand_path("c/d", Path::new("/a/b")).unwrap()
        )
    }
}
