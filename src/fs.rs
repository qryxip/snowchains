use anyhow::Context as _;
use std::path::Path;

pub(crate) fn write(
    path: impl AsRef<Path>,
    contents: impl AsRef<[u8]>,
    create_dir_all: bool,
) -> anyhow::Result<()> {
    if create_dir_all {
        if let Some(parent) = path.as_ref().parent() {
            self::create_dir_all(parent)?;
        }
    }

    std::fs::write(&path, contents)
        .with_context(|| format!("Could not write `{}`", path.as_ref().display()))
}

fn create_dir_all(path: impl AsRef<Path>) -> anyhow::Result<()> {
    std::fs::create_dir_all(&path)
        .with_context(|| format!("Could not create `{}`", path.as_ref().display()))
}
