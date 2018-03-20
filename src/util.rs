use std::{env, str};
use std::fs::{self, File};
use std::io::{self, Read};
use std::path::{Path, PathBuf};

/// Calls `File::open(path)` and if the result is `Err`, replace the error with
/// new one which message contains `path`.
pub fn open_file(path: &Path) -> io::Result<File> {
    File::open(path).map_err(|e| {
        let message = match e.kind() {
            io::ErrorKind::NotFound => format!("No such file: {:?}", path),
            _ => format!("An IO error occured while opening {:?}: {}", path, e),
        };
        io::Error::new(e.kind(), message)
    })
}

/// Calls `fs::create_dir_all` and `File::create`.
pub fn create_file_and_dirs(path: &Path) -> io::Result<File> {
    if let Some(dir) = path.parent() {
        fs::create_dir_all(dir)?;
    }
    File::create(path).map_err(|e| {
        let message = format!(
            "An IO error occured while opening/creating {:?}: {}",
            path, e
        );
        io::Error::new(e.kind(), message)
    })
}

/// Returns a `String` read from `read`.
pub fn string_from_read<R: Read>(mut read: R, capacity: usize) -> io::Result<String> {
    let mut buf = String::with_capacity(capacity);
    read.read_to_string(&mut buf)?;
    Ok(buf)
}

/// Equals to `string_from_read(open_file(path)?)`.
pub fn string_from_file_path(path: &Path) -> io::Result<String> {
    let file = open_file(path)?;
    let len = file.metadata()?.len() as usize;
    string_from_read(file, len)
}

/// Prints `s` ignoring a trailing newline if it exists.
pub fn eprintln_trimming_trailing_newline(s: &str) {
    if s.ends_with('\n') {
        eprint_and_flush!("{}", s);
    } else {
        eprintln!("{}", s);
    }
}

/// Returns `~/<names>` as `io::Result`.
///
/// # Errors
///
/// Returns `Err` if a home directory not found.
pub fn path_under_home(names: &[&str]) -> io::Result<PathBuf> {
    let home_dir = env::home_dir()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "Home directory not found"))?;
    Ok(names.iter().fold(home_dir, |mut path, name| {
        path.push(name);
        path
    }))
}

pub fn expand_path<'a, B: Into<Option<&'a Path>>>(path: &str, base: B) -> io::Result<PathBuf> {
    fn canonicalize_if_unix<P: AsRef<Path>>(path: P) -> io::Result<PathBuf> {
        if cfg!(unix) {
            Path::new(path.as_ref()).canonicalize()
        } else {
            Ok(PathBuf::from(path.as_ref())) // https://github.com/rust-lang/rust/issues/42869
        }
    }

    let mut chars = path.chars();
    let (c1, c2) = (chars.next(), chars.next());
    if c1 == Some('~') {
        return if [Some('/'), Some('\\'), None].contains(&c2) {
            path_under_home(&[&chars.collect::<String>()])
                .and_then(|path| canonicalize_if_unix(&path))
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                format!("Unsupported use of \"~\": {}", path),
            ))
        };
    }
    let path = canonicalize_if_unix(path)?;
    Ok(if path.is_absolute() {
        path
    } else {
        let mut pathbuf = base.into().map(PathBuf::from).unwrap_or_default();
        pathbuf.push(path);
        pathbuf
    })
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

pub trait Camelize {
    /// Converts `self` to CamlCase.
    fn camelize(&self) -> String;
}

impl Camelize for str {
    fn camelize(&self) -> String {
        let mut s = String::with_capacity(self.len());
        let mut p = true;
        self.chars().for_each(|c| match c.to_uppercase().next() {
            Some('-') | Some('_') => {
                p = true;
            }
            Some(c) if p => {
                p = false;
                s.push(c)
            }
            _ => s.push(c),
        });
        s
    }
}

#[cfg(test)]
mod tests {
    use util::{self, Camelize};

    use std::env;
    use std::path::Path;

    #[test]
    fn it_expands_a_path() {
        assert_eq!(env::home_dir(), util::expand_path("~/", None).ok());
        util::expand_path("~root/", None).unwrap_err();
        assert_eq!(
            Path::new("/a/b/c/d"),
            util::expand_path("c/d", Path::new("/a/b")).unwrap()
        )
    }

    #[test]
    fn it_camelizes_a_string() {
        assert_eq!("Foo", "foo".camelize());
        assert_eq!("FooBar", "foo-bar".camelize());
        assert_eq!("FooBarBaz", "foo-bar-baz".camelize());
        assert_eq!("Foo bar", "foo bar".camelize());
    }
}
