use errors::{FileIoError, FileIoErrorKind, FileIoResult, FileIoResultExt};

use std::{self, env};
use std::fs::{File, ReadDir};
use std::io::Write as _Write;
use std::path::{Path, PathBuf};

/// Calls `std::fs::create_dir_all` chaining a `FileIoError`.
pub(crate) fn create_dir_all(dir: &Path) -> FileIoResult<()> {
    std::fs::create_dir_all(dir).chain_err(|| FileIoErrorKind::CreateDirAll(dir.to_owned()))
}

/// Calls `std::fs::read_dir` chaining a `FileIoError`.
pub(crate) fn read_dir(dir: &Path) -> FileIoResult<ReadDir> {
    std::fs::read_dir(dir).chain_err(|| FileIoErrorKind::ReadDir(dir.to_owned()))
}

/// Calls `std::fs::create_dir_all` and `std::fs::write` chaining
/// `FileIoError`s.
pub(crate) fn write(path: &Path, contents: &[u8]) -> FileIoResult<()> {
    create_file_and_dirs(path)?
        .write_all(contents)
        .chain_err(|| FileIoErrorKind::Write(path.to_owned()))
}

/// Opens a file in read only mode.
pub fn open(path: &Path) -> FileIoResult<File> {
    File::open(path).chain_err(|| FileIoErrorKind::OpenInReadOnly(path.to_owned()))
}

/// Opens a file in write only mode creating its parent directory.
pub fn create_file_and_dirs(path: &Path) -> FileIoResult<File> {
    if let Some(dir) = path.parent() {
        if !dir.exists() {
            create_dir_all(dir)?;
        }
    }
    File::create(path).chain_err(|| FileIoErrorKind::OpenInWriteOnly(path.to_owned()))
}

/// Reads a file content into a string.
pub fn string_from_path(path: &Path) -> FileIoResult<String> {
    let file = open(path)?;
    let len = file.metadata().map(|m| m.len() as usize).unwrap_or(0);
    super::string_from_read(file, len).map_err(Into::into)
}

/// Returns `~/<names>` as `io::Result`.
///
/// # Errors
///
/// Returns `Err` IFF a home directory not found.
pub fn join_from_home(names: &[&str]) -> FileIoResult<PathBuf> {
    let home_dir =
        env::home_dir().ok_or_else::<FileIoError, _>(|| FileIoErrorKind::HomeDirNotFound.into())?;
    Ok(names.iter().fold(home_dir, |mut path, name| {
        path.push(name);
        path
    }))
}
