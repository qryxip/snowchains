use crate::errors::{FileErrorKind, FileResult, StdError};
use crate::path::{AbsPath, AbsPathBuf};

use failure::{Fail as _, Fallible, ResultExt as _};
use fs2::FileExt as _;
use rand::distributions::Alphanumeric;
use rand::Rng as _;
use serde::de::DeserializeOwned;
use serde::Serialize;

use std::ffi::OsStr;
use std::fs::{File, OpenOptions};
use std::io::{self, Seek as _, SeekFrom, Write as _};
use std::path::{Path, PathBuf};
use std::{env, mem};

pub(crate) fn create_dir_all(dir: &AbsPath) -> FileResult<()> {
    std::fs::create_dir_all(dir)
        .map_err(|e| e.context(FileErrorKind::CreateDir(dir.to_owned())).into())
}

pub(crate) fn write(path: &AbsPath, contents: &[u8]) -> FileResult<()> {
    create_file_and_dirs(path)?
        .write_all(contents)
        .map_err(|e| e.context(FileErrorKind::Write(path.to_owned())).into())
}

pub(crate) fn write_json_pretty(
    path: &AbsPath,
    json: &(impl Serialize + ?Sized),
) -> FileResult<()> {
    let json = serde_json::to_string_pretty(json)
        .with_context(|_| FileErrorKind::Write(path.to_owned()))?;
    write(path, json.as_bytes())
}

pub(crate) fn read_to_string(path: &AbsPath) -> FileResult<String> {
    std::fs::read_to_string(path)
        .map_err(|e| e.context(FileErrorKind::Read(path.to_owned())).into())
}

pub(crate) fn read_json<T: DeserializeOwned>(path: &AbsPath) -> FileResult<T> {
    File::open(path)
        .map_err(failure::Error::from)
        .and_then(|f| serde_json::from_reader(f).map_err(|e| StdError::from(e).into()))
        .map_err(|e| e.context(FileErrorKind::Read(path.to_owned())).into())
}

pub(crate) fn read_toml<T: DeserializeOwned>(path: &AbsPath) -> FileResult<T> {
    std::fs::read_to_string(path)
        .map_err(failure::Error::from)
        .and_then(|s| toml::from_str(&s).map_err(|e| StdError::from(e).into()))
        .map_err(|e| e.context(FileErrorKind::Read(path.to_owned())).into())
}

pub(crate) fn read_yaml<T: DeserializeOwned>(path: &AbsPath) -> FileResult<T> {
    std::fs::read_to_string(path)
        .map_err(failure::Error::from)
        .and_then(|s| serde_yaml::from_str(&s).map_err(|e| StdError::from(e).into()))
        .map_err(|e| e.context(FileErrorKind::Read(path.to_owned())).into())
}

pub(crate) fn create_file_and_dirs(path: &AbsPath) -> FileResult<File> {
    if let Some(dir) = path.parent() {
        if !dir.exists() {
            create_dir_all(&dir)?;
        }
    }
    File::create(path).map_err(|e| e.context(FileErrorKind::OpenWo(path.to_owned())).into())
}

pub(crate) fn find_path(filename: &'static str, start: &AbsPath) -> FileResult<AbsPathBuf> {
    let mut dir = start.to_owned();
    loop {
        let path = dir.join(filename);
        if std::fs::metadata(&path).is_ok() {
            break Ok(path);
        }
        if !dir.pop() {
            break Err(FileErrorKind::Find {
                filename,
                start: start.to_owned(),
            }
            .into());
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Serialize)]
pub(crate) struct TempFile(PathBuf);

impl TempFile {
    pub(crate) fn create(ext: &OsStr, contents: &str) -> io::Result<Self> {
        let dir = dunce::canonicalize(env::temp_dir())?;

        let mut stem = "snowchains_".to_owned();
        let mut rng = rand::thread_rng();
        (0..8).for_each(|_| stem.push(rng.sample(Alphanumeric)));

        let path = dir.join(stem).with_extension(ext);
        std::fs::write(&path, contents)?;
        Ok(Self(path))
    }

    pub(crate) fn path(&self) -> &Path {
        &self.0
    }
}

impl Drop for TempFile {
    fn drop(&mut self) {
        // TODO: Remove the file manually.
        let _ = std::fs::remove_file(&self.0);
    }
}

#[derive(Debug)]
pub(crate) enum LazyLockedFile {
    Null,
    Uninited(AbsPathBuf),
    Inited(LockedFile),
}

impl LazyLockedFile {
    pub(crate) fn exists(&self) -> bool {
        match self {
            LazyLockedFile::Null => false,
            LazyLockedFile::Uninited(path) => path.exists(),
            LazyLockedFile::Inited(_) => true,
        }
    }

    pub(crate) fn get_or_init(&mut self) -> FileResult<&mut LockedFile> {
        *self = match mem::take(self) {
            LazyLockedFile::Null => LazyLockedFile::Null,
            LazyLockedFile::Uninited(path) => LazyLockedFile::Inited(LockedFile::try_new(&path)?),
            LazyLockedFile::Inited(file) => LazyLockedFile::Inited(file),
        };
        match self {
            LazyLockedFile::Null => Err(io::Error::from(io::ErrorKind::InvalidInput).into()),
            LazyLockedFile::Uninited(_) => unreachable!(),
            LazyLockedFile::Inited(file) => Ok(file),
        }
    }
}

impl Default for LazyLockedFile {
    fn default() -> Self {
        LazyLockedFile::Null
    }
}

#[derive(Debug)]
pub(crate) struct LockedFile {
    inner: File,
    path: AbsPathBuf,
}

impl LockedFile {
    pub(crate) fn try_new(path: &AbsPath) -> FileResult<Self> {
        if let Some(parent) = path.parent() {
            create_dir_all(&parent)?;
        }
        let inner = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(path)
            .with_context(|_| FileErrorKind::OpenRw(path.to_owned()))?;
        inner
            .try_lock_exclusive()
            .map_err(|e| StdError::from(e).context(FileErrorKind::Lock(path.to_owned())))?;
        Ok(Self {
            inner,
            path: path.to_owned(),
        })
    }

    pub(crate) fn is_empty(&self) -> io::Result<bool> {
        self.inner.metadata().map(|m| m.len() == 0)
    }

    pub(crate) fn json<T: DeserializeOwned>(&mut self) -> FileResult<T> {
        fn json<T: DeserializeOwned>(file: &mut File) -> Fallible<T> {
            file.seek(SeekFrom::Start(0))?;
            serde_json::from_reader(file).map_err(|e| StdError::from(e).into())
        }

        json(&mut self.inner).map_err(|e| e.context(FileErrorKind::Read(self.path.clone())).into())
    }

    pub(crate) fn write_json<T: Serialize>(&mut self, value: &T) -> FileResult<()> {
        fn write_json<T: Serialize>(file: &mut File, value: &T) -> Fallible<()> {
            file.seek(SeekFrom::Start(0))?;
            file.set_len(0)?;
            serde_json::to_writer(file, value).map_err(|e| StdError::from(e).into())
        }

        write_json(&mut self.inner, value)
            .map_err(|e| e.context(FileErrorKind::Write(self.path.clone())).into())
    }
}
