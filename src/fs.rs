use crate::errors::{FileIoError, FileIoErrorKind, FileIoResult};
use crate::path::{AbsPath, AbsPathBuf};

use fs2::FileExt as _FileExt;

use std::fs::{File, OpenOptions};
use std::io::Write as _Write;

pub(crate) fn create_dir_all(dir: &AbsPath) -> FileIoResult<()> {
    std::fs::create_dir_all(dir)
        .map_err(|err| FileIoError::new(FileIoErrorKind::CreateDirAll, dir).with(err))
}

pub(crate) fn write(path: &AbsPath, contents: &[u8]) -> FileIoResult<()> {
    create_file_and_dirs(path)?
        .write_all(contents)
        .map_err(|err| FileIoError::new(FileIoErrorKind::Write, path).with(err))
}

pub(crate) fn read_to_string(path: &AbsPath) -> FileIoResult<String> {
    std::fs::read_to_string(path)
        .map_err(|err| FileIoError::new(FileIoErrorKind::Read, path).with(err))
}

/// Opens a file in read only mode.
pub(crate) fn open(path: &AbsPath) -> FileIoResult<File> {
    File::open(path)
        .map_err(|err| FileIoError::new(FileIoErrorKind::OpenInReadOnly, path).with(err))
}

/// Opens a file in write only mode creating its parent directory.
pub(crate) fn create_file_and_dirs(path: &AbsPath) -> FileIoResult<File> {
    if let Some(dir) = path.parent() {
        if !dir.exists() {
            create_dir_all(&dir)?;
        }
    }
    File::create(path)
        .map_err(|err| FileIoError::new(FileIoErrorKind::OpenInWriteOnly, path).with(err))
}

pub(crate) fn create_and_lock(path: &AbsPath) -> FileIoResult<File> {
    if let Some(parent) = path.parent() {
        create_dir_all(&parent)?;
    }
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(path)
        .map_err(|err| FileIoError::new(FileIoErrorKind::OpenInReadWrite, path).with(err))?;
    file.try_lock_exclusive()
        .map_err(|err| FileIoError::new(FileIoErrorKind::Lock, path).with(err))?;
    Ok(file)
}

pub(crate) fn find_filepath(start: &AbsPath, filename: &'static str) -> FileIoResult<AbsPathBuf> {
    let mut dir = start.to_owned();
    loop {
        let path = dir.join(filename);
        if std::fs::metadata(&path).is_ok() {
            break Ok(path);
        }
        if !dir.pop() {
            break Err(FileIoError::new(FileIoErrorKind::Search(filename), start));
        }
    }
}
