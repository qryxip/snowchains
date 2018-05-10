use std::path::{Path, PathBuf};

pub(crate) fn remove_dots(path: &Path) -> PathBuf {
    let mut r = PathBuf::new();
    path.iter().for_each(|p| match p {
        p if p == "." => {}
        p if p == ".." => {
            r.pop();
        }
        p => r.push(p),
    });
    r
}
