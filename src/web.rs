use anyhow::{anyhow, Context as _};
use cookie_store::CookieStore;
use fs2::FileExt as _;
use std::{
    fs::{self, File},
    io::BufReader,
    io::{Seek as _, SeekFrom},
    path::{Path, PathBuf},
    sync::Mutex,
    time::Duration,
};

pub(crate) const SESSION_TIMEOUT: Duration = Duration::from_secs(30);

#[derive(Debug)]
pub(crate) struct LazyLockedFile {
    path: PathBuf,
    file: Mutex<Option<File>>,
}

impl LazyLockedFile {
    pub(crate) fn new(path: &Path) -> Self {
        Self {
            path: path.to_owned(),
            file: Mutex::new(None),
        }
    }

    pub(crate) fn path(&self) -> &Path {
        &self.path
    }

    fn overwrite(&self, f: impl FnOnce(&mut File) -> anyhow::Result<()>) -> anyhow::Result<()> {
        let Self { path, file } = self;

        let mut file = file.lock().unwrap();

        let new_file = if file.is_none() {
            if let Some(parent) = path.parent() {
                if !parent.exists() {
                    fs::create_dir_all(parent)
                        .with_context(|| format!("Could not create `{}`", parent.display()))?;
                }
            }

            let new_file = File::create(&path)
                .with_context(|| format!("Could not open `{}`", path.display()))?;

            new_file
                .try_lock_exclusive()
                .with_context(|| format!("Could not lock `{}`", path.display()))?;

            Some(new_file)
        } else {
            None
        };

        let file = file.get_or_insert_with(|| new_file.unwrap());

        file.seek(SeekFrom::Start(0))
            .and_then(|_| file.set_len(0))
            .map_err(Into::into)
            .and_then(|()| f(file))
            .and_then(|()| file.sync_data().map_err(Into::into))
            .with_context(|| format!("Could not write `{}`", path.display()))
    }
}

pub(crate) fn cookies_path() -> anyhow::Result<PathBuf> {
    let data_local_dir =
        dirs::data_local_dir().with_context(|| "Could not find the local date directory")?;
    Ok(data_local_dir.join("snowchains").join("cookies.jsonl"))
}

pub(crate) fn load_cookie_store(path: &Path) -> anyhow::Result<CookieStore> {
    if path.exists() {
        File::open(path)
            .map_err(anyhow::Error::from)
            .and_then(|h| CookieStore::load_json(BufReader::new(h)).map_err(|e| anyhow!("{}", e)))
            .with_context(|| format!("Could not load cookies from `{}`", path.display()))
    } else {
        Ok(CookieStore::default())
    }
}

pub(crate) fn save_cookie_store(
    cookie_store: &CookieStore,
    file: &LazyLockedFile,
) -> anyhow::Result<()> {
    file.overwrite(|file| {
        cookie_store.save_json(file).map_err(|e| anyhow!("{}", e))?;
        Ok(())
    })
}
