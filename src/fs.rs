use crate::path::{AbsPath, AbsPathBuf};

use fs2::FileExt as _FileExt;
use serde::de::DeserializeOwned;
use serde::Serialize;
use zip::ZipArchive;

use std::fmt;
use std::fs::{File, OpenOptions};
use std::io::{self, Seek as _Seek, SeekFrom, Write as _Write};

pub(crate) fn create_dir_all(dir: &AbsPath) -> io::Result<()> {
    std::fs::create_dir_all(dir).io_context(|| format!("Failed to create {}", dir.display()))
}

pub(crate) fn write(path: &AbsPath, contents: &[u8]) -> io::Result<()> {
    create_file_and_dirs(path)?
        .write_all(contents)
        .io_context(|| format!("Failed to write to {}", path.display()))
}

pub(crate) fn read_to_string(path: &AbsPath) -> io::Result<String> {
    std::fs::read_to_string(path).io_context(|| format!("Failed to read {}", path.display()))
}

pub(crate) fn read_json<T: DeserializeOwned>(path: &AbsPath) -> io::Result<T> {
    File::open(path)
        .and_then(|f| serde_json::from_reader(f).map_err(Into::into))
        .io_context(|| format!("Failed to read {}", path.display()))
}

pub(crate) fn read_toml<T: DeserializeOwned>(path: &AbsPath) -> io::Result<T> {
    std::fs::read_to_string(path)
        .and_then(|s| toml::from_str(&s).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e)))
        .io_context(|| format!("Failed to read {}", path.display()))
}

pub(crate) fn read_yaml<T: DeserializeOwned>(path: &AbsPath) -> io::Result<T> {
    std::fs::read_to_string(path)
        .and_then(|s| {
            serde_yaml::from_str(&s).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
        }).io_context(|| format!("Failed to read {}", path.display()))
}

pub(crate) fn open_zip(path: &AbsPath) -> io::Result<ZipArchive<File>> {
    File::open(path)
        .and_then(|f| ZipArchive::new(f).map_err(Into::into))
        .io_context(|| format!("Failed to read {}", path.display()))
}

pub(crate) fn create_file_and_dirs(path: &AbsPath) -> io::Result<File> {
    if let Some(dir) = path.parent() {
        if !dir.exists() {
            create_dir_all(&dir)?;
        }
    }
    File::create(path).io_context(|| {
        format!(
            "An IO error occurred while opening/creating {} in write-only mode",
            path.display()
        )
    })
}

pub(crate) fn find_filepath(start: &AbsPath, filename: &'static str) -> io::Result<AbsPathBuf> {
    let mut dir = start.to_owned();
    loop {
        let path = dir.join(filename);
        if std::fs::metadata(&path).is_ok() {
            break Ok(path);
        }
        if !dir.pop() {
            break Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!(
                    "Could not find {:?} in {} or any parent directory",
                    filename,
                    start.display()
                ),
            ));
        }
    }
}

#[derive(Debug)]
pub(crate) struct LockedFile {
    inner: File,
    path: AbsPathBuf,
}

impl LockedFile {
    pub(crate) fn try_new(path: &AbsPath) -> io::Result<Self> {
        if let Some(parent) = path.parent() {
            create_dir_all(&parent)?;
        }
        let inner = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(path)
            .io_context(|| {
                format!(
                    "An IO error occurred while opening {} in read/write mode",
                    path.display()
                )
            })?;
        inner
            .try_lock_exclusive()
            .io_context(|| format!("Failed to lock {}", path.display()))?;
        Ok(Self {
            inner,
            path: path.to_owned(),
        })
    }

    pub(crate) fn is_empty(&self) -> io::Result<bool> {
        self.inner.metadata().map(|m| m.len() == 0)
    }

    pub(crate) fn bincode<T: DeserializeOwned>(&mut self) -> io::Result<T> {
        fn bincode<T: DeserializeOwned>(file: &mut File) -> io::Result<T> {
            file.seek(SeekFrom::Start(0))?;
            bincode::deserialize_from(file).map_err(|e| match *e {
                bincode::ErrorKind::Io(e) => e,
                bincode::ErrorKind::InvalidUtf8Encoding(e) => {
                    io::Error::new(io::ErrorKind::InvalidData, e)
                }
                e => io::Error::new(io::ErrorKind::InvalidData, e),
            })
        }

        bincode(&mut self.inner).io_context(|| format!("Failed to read {}", self.path.display()))
    }

    pub(crate) fn write_bincode<T: Serialize>(&mut self, value: &T) -> io::Result<()> {
        fn write_bincode<T: Serialize>(file: &mut File, value: &T) -> io::Result<()> {
            file.seek(SeekFrom::Start(0))?;
            file.set_len(0)?;
            bincode::serialize_into(file, value).map_err(|e| match *e {
                bincode::ErrorKind::Io(e) => e,
                e => io::Error::new(io::ErrorKind::Other, e),
            })
        }

        write_bincode(&mut self.inner, value)
            .io_context(|| format!("Failed to write to {}", self.path.display()))
    }
}

trait IoContext {
    fn io_context<E: fmt::Display + fmt::Debug + Send + Sync + 'static, F: FnOnce() -> E>(
        self,
        f: F,
    ) -> Self;
}

impl<T> IoContext for io::Result<T> {
    fn io_context<E: fmt::Display + fmt::Debug + Send + Sync + 'static, F: FnOnce() -> E>(
        self,
        f: F,
    ) -> Self {
        #[derive(Debug)]
        struct IoContext<E> {
            ctx: E,
            cause: io::Error,
        }

        impl<E: fmt::Display> fmt::Display for IoContext<E> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.ctx.fmt(f)
            }
        }

        impl<E: fmt::Display + fmt::Debug> std::error::Error for IoContext<E> {
            fn description(&self) -> &str {
                "IoContext"
            }

            fn cause(&self) -> Option<&dyn std::error::Error> {
                Some(&self.cause)
            }
        }

        self.map_err(|cause| io::Error::new(cause.kind(), IoContext { ctx: f(), cause }))
    }
}
