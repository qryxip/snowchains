use std::borrow::{Borrow, Cow};
use std::ffi::{OsStr, OsString};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::{env, fmt, io};

#[repr(transparent)]
pub struct AbsPath {
    inner: Path,
}

impl AbsPath {
    fn unchecked(path: &Path) -> &Self {
        unsafe { &*(path as *const Path as *const Self) }
    }

    pub(crate) fn parent(&self) -> Option<&Self> {
        self.inner.parent().map(Self::unchecked)
    }

    pub(crate) fn join(&self, path: impl AsRef<Path>) -> AbsPathBuf {
        AbsPathBuf {
            inner: self.inner.join(path),
        }
    }

    pub(crate) fn join_canonicalizing_lossy(&self, path: impl AsRef<Path>) -> AbsPathBuf {
        let r = AbsPathBuf {
            inner: self.inner.join(path),
        };
        match r.canonicalize_lossy() {
            Cow::Borrowed(_) => r,
            Cow::Owned(r) => r,
        }
    }

    pub(crate) fn join_expanding_tilde(&self, path: impl AsRef<OsStr>) -> io::Result<AbsPathBuf> {
        let path = Path::new(path.as_ref());
        let new = if path.is_absolute() {
            path.to_owned()
        } else if path.iter().next() == Some(OsStr::new("~")) {
            let home = dirs::home_dir().ok_or_else(|| {
                io::Error::new(io::ErrorKind::NotFound, "Home directory not found")
            })?;
            path.iter().skip(1).fold(home, |mut r, p| {
                r.push(p);
                r
            })
        } else if path.to_string_lossy().starts_with('~') {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Unsupported use of '~': {}", path.display()),
            ));
        } else {
            self.inner.join(path)
        };
        Ok(AbsPathBuf { inner: new }.canonicalize_lossy().into_owned())
    }

    #[cfg(not(windows))]
    fn canonicalize_lossy(&self) -> Cow<Self> {
        match self.inner.canonicalize() {
            Ok(inner) => AbsPathBuf { inner }.into(),
            Err(_) => self.remove_single_dots(),
        }
    }

    #[cfg(windows)]
    fn canonicalize_lossy(&self) -> Cow<Self> {
        let mut inner = PathBuf::new();
        for s in self.inner.iter() {
            if s == OsStr::new("..") {
                inner.pop();
            } else if s != OsStr::new(".") {
                inner.push(s);
            }
            if let Ok(link) = inner.read_link() {
                inner = link;
            }
            if inner.metadata().is_err() {
                return self.remove_single_dots();
            }
        }
        AbsPathBuf { inner }.into()
    }

    fn remove_single_dots(&self) -> Cow<Self> {
        if self.inner.iter().any(|s| s == OsStr::new(".")) {
            let inner = self.inner.iter().filter(|&s| s != OsStr::new(".")).fold(
                PathBuf::new(),
                |mut r, s| {
                    r.push(s);
                    r
                },
            );
            AbsPathBuf { inner }.into()
        } else {
            self.into()
        }
    }
}

impl ToOwned for AbsPath {
    type Owned = AbsPathBuf;

    fn to_owned(&self) -> AbsPathBuf {
        AbsPathBuf {
            inner: self.inner.to_owned(),
        }
    }
}

impl Deref for AbsPath {
    type Target = Path;

    fn deref(&self) -> &Path {
        &self.inner
    }
}

impl AsRef<Path> for AbsPath {
    fn as_ref(&self) -> &Path {
        &self.inner
    }
}

impl AsRef<OsStr> for AbsPath {
    fn as_ref(&self) -> &OsStr {
        self.inner.as_ref()
    }
}

impl<'a> Into<Cow<'a, AbsPath>> for &'a AbsPath {
    fn into(self) -> Cow<'a, AbsPath> {
        Cow::Borrowed(self)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct AbsPathBuf {
    inner: PathBuf,
}

impl AbsPathBuf {
    pub fn cwd() -> io::Result<Self> {
        #[derive(Debug)]
        struct GetcwdError(io::Error);

        impl fmt::Display for GetcwdError {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "Failed to get the current directory")
            }
        }

        impl std::error::Error for GetcwdError {
            fn description(&self) -> &str {
                "failed to getcwd"
            }

            fn cause(&self) -> Option<&dyn std::error::Error> {
                Some(&self.0)
            }
        }

        env::current_dir()
            .map(|inner| Self { inner })
            .map_err(|e| io::Error::new(e.kind(), GetcwdError(e)))
    }

    /// # Panics
    ///
    /// Panics if `path` is relative.
    pub fn new_or_panic(path: impl Into<PathBuf>) -> Self {
        let inner = path.into();
        if inner.is_relative() {
            panic!("called `AbsPathBuf::new_or_panic` on a relative path");
        }
        Self { inner }
    }

    pub(crate) fn pop(&mut self) -> bool {
        self.inner.pop()
    }
}

impl Default for AbsPathBuf {
    #[cfg(not(windows))]
    fn default() -> Self {
        Self {
            inner: PathBuf::from("/"),
        }
    }

    #[cfg(windows)]
    fn default() -> Self {
        Self {
            inner: PathBuf::from("C:\\"),
        }
    }
}

impl Borrow<AbsPath> for AbsPathBuf {
    fn borrow(&self) -> &AbsPath {
        AbsPath::unchecked(&self.inner)
    }
}

impl Deref for AbsPathBuf {
    type Target = AbsPath;

    fn deref(&self) -> &AbsPath {
        AbsPath::unchecked(&self.inner)
    }
}

impl AsRef<Path> for AbsPathBuf {
    fn as_ref(&self) -> &Path {
        &self.inner
    }
}

impl<'a> Into<Cow<'a, AbsPath>> for AbsPathBuf {
    fn into(self) -> Cow<'a, AbsPath> {
        Cow::Owned(self)
    }
}

impl Into<OsString> for AbsPathBuf {
    fn into(self) -> OsString {
        self.inner.into()
    }
}

#[cfg(test)]
mod tests {
    use super::{AbsPath, AbsPathBuf};

    use dirs;

    use std::path::Path;

    #[cfg(not(windows))]
    #[test]
    fn it_removes_dots() {
        let base = AbsPath::unchecked(Path::new("/foo"));
        assert_eq!(Path::new("/foo"), base.join("").inner);
        assert_eq!(Path::new("/foo"), base.join(".").inner);
        assert_eq!(Path::new("/foo/bar"), base.join("bar").inner);
        assert_eq!(Path::new("/foo/bar"), base.join("./bar").inner);
        assert_eq!(Path::new("/foo/../bar"), base.join("../bar").inner);
    }

    #[cfg(windows)]
    #[test]
    fn it_removes_dots() {
        let base = AbsPath::unchecked(Path::new(r"C:\foo"));
        assert_eq!(Path::new(r"C:\foo"), base.join("").inner);
        assert_eq!(Path::new(r"C:\foo"), base.join(".").inner);
        assert_eq!(Path::new(r"C:\foo\bar"), base.join("bar").inner);
        assert_eq!(Path::new(r"C:\foo\bar"), base.join(r".\bar").inner);
        assert_eq!(Path::new(r"C:\foo\..\bar"), base.join(r"..\bar").inner);
    }

    #[test]
    fn it_expands_tilde() {
        let home = dirs::home_dir().unwrap();
        let expected = home.join("foo");
        let actual = AbsPathBuf::default()
            .join_expanding_tilde("~/foo")
            .unwrap()
            .inner;
        assert_eq!(expected, actual);
    }
}
