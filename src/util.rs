use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::path::{Path, PathBuf};


/// Returns a `String` read from the file of given path.
pub fn string_from_file_path(path: &Path) -> io::Result<String> {
    match File::open(path) {
        Ok(file) => string_from_read(file),
        Err(ref e) if e.kind() == io::ErrorKind::NotFound => Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("No such file: {:?}", path),
        )),
        Err(e) => Err(e),
    }
}


/// Returns a `String` read from given source.
pub fn string_from_read<R: Read>(read: R) -> io::Result<String> {
    let mut read = read;
    let mut buf = String::new();
    read.read_to_string(&mut buf)?;
    Ok(buf)
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
    /// Get the value `&x` if `Some(ref x) = self`.
    ///
    /// # Panics
    ///
    /// Panics if `self` is `None`.
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


pub trait CapitalizeFirst {
    /// Capitalizes the first letter.
    fn capitalize_first(&self) -> String;
}

impl CapitalizeFirst for str {
    fn capitalize_first(&self) -> String {
        let mut chars = self.chars();
        chars
            .next()
            .map(|c| format!("{}{}", c.to_uppercase(), chars.as_str()))
            .unwrap_or_default()
    }
}
