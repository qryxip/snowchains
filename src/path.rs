use derivative::Derivative;
use ref_cast::RefCast;
use serde::{Serialize, Serializer};

use std::borrow::{Borrow, Cow};
use std::ffi::{OsStr, OsString};
use std::ops::Deref;
use std::path::{Component, Path, PathBuf};
use std::{env, io};

#[derive(PartialEq, Derivative, RefCast)]
#[derivative(Debug = "transparent")]
#[repr(transparent)]
pub struct AbsPath {
    inner: Path,
}

impl AbsPath {
    fn unchecked(path: &Path) -> &Self {
        Self::ref_cast(path)
    }

    pub(crate) fn parent(&self) -> Option<&Self> {
        self.inner.parent().map(Self::unchecked)
    }

    pub(crate) fn join(&self, path: impl AsRef<Path>) -> AbsPathBuf {
        AbsPathBuf {
            inner: self.inner.join(path),
        }
    }

    pub(crate) fn with_extension(&self, extension: impl AsRef<OsStr>) -> AbsPathBuf {
        AbsPathBuf {
            inner: self.inner.with_extension(extension),
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

    pub(crate) fn join_expanding_user(&self, path: impl AsRef<OsStr>) -> io::Result<AbsPathBuf> {
        let path = Path::new(path.as_ref());
        let inner = if path.is_absolute() {
            path.to_owned()
        } else if let Some(Component::Normal(first)) = path.components().next() {
            if first == "~" {
                let home = dirs::home_dir().ok_or_else(|| {
                    io::Error::new(io::ErrorKind::NotFound, "Home directory not found")
                })?;
                path.components().skip(1).fold(home, |mut r, c| {
                    r.push(c);
                    r
                })
            } else if first.to_string_lossy().starts_with('~') {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("Unsupported use of '~': {}", path.display()),
                ));
            } else {
                self.inner.join(path)
            }
        } else {
            self.inner.join(path)
        };
        Ok(AbsPathBuf { inner }.canonicalize_lossy().into_owned())
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
        use if_chain::if_chain;

        use std::path::Prefix;

        // `dunce` does not strip a UNC prefix when:
        // - the prefix is not `VerbatimDisk`
        // - the whole path is not a valid Unicode
        let inner = if_chain! {
            if let Ok(inner) = dunce::canonicalize(&self.inner);
            if let Some(Component::Prefix(prefix)) = inner.components().next();
            if let Prefix::Disk(_) = prefix.kind();
            then {
                inner
            } else {
                let mut inner = PathBuf::new();
                for component in self.inner.components() {
                    if component == Component::ParentDir {
                        inner.pop();
                    } else if component != Component::CurDir {
                        inner.push(component);
                    }
                    if let Ok(link) = inner.read_link() {
                        inner = link;
                    }
                    if inner.metadata().is_err() {
                        return self.remove_single_dots();
                    }
                }
                inner
            }
        };
        AbsPathBuf { inner }.into()
    }

    fn remove_single_dots(&self) -> Cow<Self> {
        if self.inner.components().any(|c| c == Component::CurDir) {
            let inner = self
                .inner
                .components()
                .filter(|&c| c != Component::CurDir)
                .fold(PathBuf::new(), |mut r, c| {
                    r.push(c);
                    r
                });
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

impl Serialize for AbsPath {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        self.deref().serialize(serializer)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Derivative)]
#[derivative(Debug = "transparent")]
pub struct AbsPathBuf {
    inner: PathBuf,
}

impl AbsPathBuf {
    pub fn cwd() -> io::Result<Self> {
        #[derive(derive_more::Display, Debug)]
        #[display(fmt = "Failed to get the current directory")]
        struct GetcwdError(io::Error);

        impl std::error::Error for GetcwdError {
            fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
                Some(&self.0)
            }
        }

        env::current_dir()
            .and_then(|inner| {
                if inner.is_absolute() {
                    Ok(Self { inner })
                } else {
                    Err(io::ErrorKind::Other.into())
                }
            })
            .map_err(|e| io::Error::new(e.kind(), GetcwdError(e)))
    }

    pub fn try_new(path: impl Into<PathBuf>) -> Option<Self> {
        let inner = path.into();
        guard!(inner.is_absolute());
        Some(Self { inner })
    }

    pub(crate) fn pop(&mut self) -> bool {
        self.inner.pop()
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

impl AsRef<OsStr> for AbsPathBuf {
    fn as_ref(&self) -> &OsStr {
        self.inner.as_ref()
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

impl Into<PathBuf> for AbsPathBuf {
    fn into(self) -> PathBuf {
        self.inner
    }
}

#[cfg(test)]
impl PartialEq<PathBuf> for AbsPathBuf {
    fn eq(&self, other: &PathBuf) -> bool {
        self.inner == *other
    }
}

impl Serialize for AbsPathBuf {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        AsRef::<Path>::as_ref(self).serialize(serializer)
    }
}

#[cfg(test)]
mod tests {
    use super::{AbsPath, AbsPathBuf};

    use dirs;
    use pretty_assertions::assert_eq;

    use std::io;
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
    fn it_expands_user() -> io::Result<()> {
        let home = dirs::home_dir().unwrap();
        assert_eq!(
            if cfg!(windows) {
                AbsPathBuf::try_new("C:\\")
            } else {
                AbsPathBuf::try_new("/")
            }
            .unwrap()
            .join_expanding_user("~/foo")?
            .inner,
            home.join("foo"),
        );
        Ok(())
    }
}
