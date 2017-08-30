use std::env;
use std::io::{self, Read};
use std::path::PathBuf;


pub fn string_from_read<R: Read>(r: R) -> io::Result<String> {
    let mut r = r;
    let mut buf = String::new();
    r.read_to_string(&mut buf)?;
    Ok(buf)
}


pub fn eprintln_trimming_last_newline(s: &str) {
    if s.chars().last() == Some('\n') {
        eprint_and_flush!("{}", s);
    } else {
        eprintln!("{}", s);
    }
}


pub fn home_dir_as_io_result() -> io::Result<PathBuf> {
    env::home_dir().ok_or(io::Error::new(
        io::ErrorKind::Other,
        "Home directory not found",
    ))
}


pub trait OkAsRefOr {
    type Item;
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
