use anyhow::Context as _;
use serde::{de::DeserializeOwned, Serialize};
use std::{fs::Metadata, path::Path};

pub(crate) fn metadata(path: impl AsRef<Path>) -> anyhow::Result<Metadata> {
    let path = path.as_ref();
    std::fs::metadata(path)
        .with_context(|| format!("Could not get the metadata of `{}`", path.display()))
}

pub(crate) fn read_json<T: DeserializeOwned, P: AsRef<Path>>(path: P) -> anyhow::Result<T> {
    let path = path.as_ref();
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Could not read `{}`", path.display()))?;
    serde_json::from_str(&content)
        .with_context(|| format!("Could not parse the JSON at `{}`", path.display()))
}

pub(crate) fn read_yaml<T: DeserializeOwned, P: AsRef<Path>>(path: P) -> anyhow::Result<T> {
    let path = path.as_ref();
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("Could not read `{}`", path.display()))?;
    serde_yaml::from_str(&content)
        .with_context(|| format!("Could not parse the YAML at `{}`", path.display()))
}

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

pub(crate) fn write_json(
    path: impl AsRef<Path>,
    value: impl Serialize,
    create_dir_all: bool,
) -> anyhow::Result<()> {
    write(path, serde_json::to_string(&value)?, create_dir_all)
}

pub(crate) fn create_dir_all(path: impl AsRef<Path>) -> anyhow::Result<()> {
    std::fs::create_dir_all(&path)
        .with_context(|| format!("Could not create `{}`", path.as_ref().display()))
}
