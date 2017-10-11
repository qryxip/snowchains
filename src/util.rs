use std::env;
use std::fs::{self, File};
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::str;


/// Calls `File::open(path)` and if the result is `Err`, replace the error with new one which
/// message contains `path`.
pub fn open_file(path: &Path) -> io::Result<File> {
    File::open(path).map_err(|e| if e.kind() == io::ErrorKind::NotFound {
        let message = format!("No such file: {:?}", path);
        io::Error::new(io::ErrorKind::NotFound, message)
    } else {
        let message = format!("An IO error occured while opening {:?}: {}", path, e);
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
            path,
            e
        );
        io::Error::new(e.kind(), message)
    })
}


/// Returns a `String` read from `read`.
pub fn string_from_read<R: Read>(read: R) -> io::Result<String> {
    let mut read = read;
    let mut buf = String::new();
    read.read_to_string(&mut buf)?;
    Ok(buf)
}


/// Equals to `string_from_read(open_file(path)?)`.
pub fn string_from_file_path(path: &Path) -> io::Result<String> {
    string_from_read(open_file(path)?)
}


/// Prints the given string ignoring the last newline if it exists.
pub fn eprintln_trimming_last_newline(s: &str) {
    if s.chars().last() == Some('\n') {
        eprint_and_flush!("{}", s);
    } else {
        eprintln!("{}", s);
    }
}


/// Returns the path the current user's home directory as `io::Result`.
pub fn home_dir_as_io_result() -> io::Result<PathBuf> {
    env::home_dir().ok_or(io::Error::new(
        io::ErrorKind::Other,
        "Home directory not found",
    ))
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


pub trait UnwrapAsRefMut {
    type Item;
    /// Gets the value `&mut x` if `Some(ref mut x) = self`.
    ///
    /// # Panics
    ///
    /// Panics if `self` is `None`.
    fn unwrap_as_ref_mut(&mut self) -> &mut Self::Item;
}

impl<T> UnwrapAsRefMut for Option<T> {
    type Item = T;

    fn unwrap_as_ref_mut(&mut self) -> &mut T {
        match *self {
            Some(ref mut x) => x,
            None => {
                panic!(
                    "called `<Option as UnwrapAsRefMut>::unwrap_as_ref_mut` \
                     on a `None` value"
                )
            }
        }
    }
}


pub trait ToCamlCase {
    /// Converts `self` to CamlCase.
    fn to_caml_case(&self) -> String;
}

impl ToCamlCase for str {
    fn to_caml_case(&self) -> String {
        let mut s = String::new();
        let mut p = true;
        self.chars().foreach(|c| match c.to_uppercase().next() {
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


pub trait Foreach {
    type Item;
    /// A substitution of `Iterator::for_each`, which is unstable until Rust 1.21.
    fn foreach<F: FnMut(Self::Item)>(self, f: F);
}

impl<T, I: Iterator<Item = T>> Foreach for I {
    type Item = T;

    fn foreach<F: FnMut(T)>(self, mut f: F) {
        self.fold((), move |(), x| f(x));
    }
}
