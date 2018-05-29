use errors::{FileIoError, FileIoErrorKind, FileIoResult};

use fs2::FileExt as _FileExt;

use std;
use std::fs::{File, OpenOptions, ReadDir};
use std::io::Write as _Write;
use std::path::Path;

pub(crate) fn create_dir_all(dir: &Path) -> FileIoResult<()> {
    std::fs::create_dir_all(dir)
        .map_err(|e| FileIoError::chaining(FileIoErrorKind::CreateDirAll, dir, e))
}

pub(crate) fn read_dir(dir: &Path) -> FileIoResult<ReadDir> {
    std::fs::read_dir(dir).map_err(|e| FileIoError::chaining(FileIoErrorKind::ReadDir, dir, e))
}

pub fn write(path: &Path, contents: &[u8]) -> FileIoResult<()> {
    create_file_and_dirs(path)?
        .write_all(contents)
        .map_err(|e| FileIoError::chaining(FileIoErrorKind::Write, path, e))
}

pub(crate) fn read_to_string(path: &Path) -> FileIoResult<String> {
    std::fs::read_to_string(path).map_err(|e| FileIoError::chaining(FileIoErrorKind::Read, path, e))
}

/// Opens a file in read only mode.
pub(crate) fn open(path: &Path) -> FileIoResult<File> {
    File::open(path).map_err(|e| FileIoError::chaining(FileIoErrorKind::OpenInReadOnly, path, e))
}

/// Opens a file in write only mode creating its parent directory.
pub(crate) fn create_file_and_dirs(path: &Path) -> FileIoResult<File> {
    if let Some(dir) = path.parent() {
        if !dir.exists() {
            create_dir_all(dir)?;
        }
    }
    File::create(path).map_err(|e| FileIoError::chaining(FileIoErrorKind::OpenInWriteOnly, path, e))
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
        .map_err(|e| FileIoError::chaining(FileIoErrorKind::OpenInReadWrite, path, e))?;
    file.try_lock_exclusive()
        .map_err(|e| FileIoError::chaining(FileIoErrorKind::Lock, path, e))?;
    Ok(file)
}
