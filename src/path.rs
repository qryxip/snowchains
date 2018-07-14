use errors::{FileIoError, FileIoErrorKind, FileIoResult};

use dirs;

use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::ops::Deref;
use std::path::{Path, PathBuf};

pub(crate) type AbsPath<'a> = &'a AbsPathBuf;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct AbsPathBuf(PathBuf);

impl AbsPathBuf {
    pub fn new_or_panic(abs_path: impl Into<PathBuf>) -> Self {
        let abs_path = abs_path.into();
        if abs_path.is_relative() {
            panic!("the argument was relative: {:?}", abs_path);
        }
        AbsPathBuf(abs_path)
    }

    pub(crate) fn pop(&mut self) -> bool {
        self.0.pop()
    }

    pub(crate) fn with_extension(&self, extension: impl AsRef<OsStr>) -> Self {
        AbsPathBuf(self.0.with_extension(extension))
    }

    pub(crate) fn parent(&self) -> Option<Self> {
        self.0.parent().map(ToOwned::to_owned).map(AbsPathBuf)
    }

    pub(crate) fn join(&self, path: impl AsRef<Path>) -> Self {
        let path = path.as_ref();
        if path.is_absolute() {
            AbsPathBuf(remove_dots(path.into()))
        } else {
            AbsPathBuf(concat_removing_dots(self.0.clone(), path.iter()))
        }
    }

    pub(crate) fn join_expanding_tilde(&self, path: impl Into<OsString>) -> FileIoResult<Self> {
        let path = PathBuf::from(path.into());
        if path.is_absolute() {
            Ok(AbsPathBuf(remove_dots(Cow::from(path))))
        } else if path.iter().next() == Some(OsStr::new("~")) {
            let home = dirs::home_dir()
                .ok_or_else(|| FileIoError::new(FileIoErrorKind::HomeDirNotFound, path.clone()))?;
            Ok(AbsPathBuf(concat_removing_dots(home, path.iter().skip(1))))
        } else {
            if let Some(h) = path.iter().next() {
                if h.to_string_lossy().starts_with('~') {
                    return Err(FileIoError::new(
                        FileIoErrorKind::UnsupportedUseOfTilde,
                        path.clone(),
                    ));
                }
            }
            Ok(self.join(&path))
        }
    }
}

fn concat_removing_dots<'a>(base: PathBuf, rest: impl Iterator<Item = &'a OsStr>) -> PathBuf {
    let mut r = base;
    for s in rest {
        if s == ".." {
            r.pop();
        } else if s != "." {
            r.push(s);
        }
    }
    r
}

fn remove_dots(path: Cow<Path>) -> PathBuf {
    if path.iter().any(|s| s == "." || s == "..") {
        let base = PathBuf::from(OsString::with_capacity(path.as_os_str().len()));
        concat_removing_dots(base, path.iter())
    } else {
        path.into_owned()
    }
}

impl<'a> Into<AbsPathBuf> for AbsPath<'a> {
    fn into(self) -> AbsPathBuf {
        self.clone()
    }
}

impl<'a> Into<PathBuf> for AbsPath<'a> {
    fn into(self) -> PathBuf {
        self.0.clone()
    }
}

impl Into<PathBuf> for AbsPathBuf {
    fn into(self) -> PathBuf {
        self.0
    }
}

impl Default for AbsPathBuf {
    fn default() -> Self {
        if cfg!(target_os = "windows") {
            AbsPathBuf(PathBuf::from(r"C:\"))
        } else {
            AbsPathBuf(PathBuf::from("/"))
        }
    }
}

impl fmt::Debug for AbsPathBuf {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl Deref for AbsPathBuf {
    type Target = Path;

    fn deref(&self) -> &Path {
        &self.0
    }
}

impl AsRef<Path> for AbsPathBuf {
    fn as_ref(&self) -> &Path {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use path::AbsPathBuf;

    use dirs;

    use std::path::Path;

    #[cfg(not(windows))]
    #[test]
    fn it_removes_dots() {
        let base = AbsPathBuf::new_or_panic("/foo");
        assert_eq!(Path::new("/foo"), base.join("").0);
        assert_eq!(Path::new("/foo"), base.join(".").0);
        assert_eq!(Path::new("/foo/bar"), base.join("bar").0);
        assert_eq!(Path::new("/foo/bar"), base.join("./bar").0);
        assert_eq!(Path::new("/bar"), base.join("../bar").0);
    }

    #[cfg(windows)]
    #[test]
    fn it_removes_dots() {
        let base = Path::new(r"C:\foo");
        assert_eq!(Path::new(r"C:\foo"), base.join("").0);
        assert_eq!(Path::new(r"C:\foo"), base.join(".").0);
        assert_eq!(Path::new(r"C:\foo\bar"), base.join("bar").0);
        assert_eq!(Path::new(r"C:\foo\bar"), base.join(r".\bar").0);
        assert_eq!(Path::new(r"C:\bar"), base.join(r"..\bar").0);
    }

    #[test]
    fn it_expands_tilde() {
        let home = dirs::home_dir().unwrap();
        let expected = home.join("foo");
        let actual = AbsPathBuf::default()
            .join_expanding_tilde("~/foo")
            .unwrap()
            .0;
        assert_eq!(expected, actual);
    }
}
