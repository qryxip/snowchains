use errors::{FileIoErrorKind, FileIoResult, FileIoResultExt};

use fs2::FileExt as _FileExt;

use std;
use std::fs::{File, OpenOptions, ReadDir};
use std::io::Write as _Write;
use std::path::Path;

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
pub fn write(path: &Path, contents: &[u8]) -> FileIoResult<()> {
    create_file_and_dirs(path)?
        .write_all(contents)
        .chain_err(|| FileIoErrorKind::Write(path.to_owned()))
}

/// Calls `std::fs::read_to_string` chaining a `FileIoError`.
pub(crate) fn read_to_string(path: &Path) -> FileIoResult<String> {
    std::fs::read_to_string(path).chain_err(|| FileIoErrorKind::Read(path.to_owned()))
}

/// Opens a file in read only mode.
pub(crate) fn open(path: &Path) -> FileIoResult<File> {
    File::open(path).chain_err(|| FileIoErrorKind::OpenInReadOnly(path.to_owned()))
}

/// Opens a file in write only mode creating its parent directory.
pub(crate) fn create_file_and_dirs(path: &Path) -> FileIoResult<File> {
    if let Some(dir) = path.parent() {
        if !dir.exists() {
            create_dir_all(dir)?;
        }
    }
    File::create(path).chain_err(|| FileIoErrorKind::OpenInWriteOnly(path.to_owned()))
}

pub(crate) fn create_and_lock(path: &Path) -> FileIoResult<File> {
    if let Some(dir) = path.parent() {
        create_dir_all(dir)?;
    }
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(path)
        .chain_err(|| FileIoErrorKind::OpenInReadWrite(path.to_owned()))?;
    file.try_lock_exclusive()
        .chain_err(|| FileIoErrorKind::Lock(path.to_owned()))?;
    Ok(file)
}
