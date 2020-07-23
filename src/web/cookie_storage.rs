use anyhow::{anyhow, Context as _};
use cookie_store::CookieStore;
use fs2::FileExt as _;
use snowchains_core::web::CookieStorage;
use std::{
    fs::{self, File},
    io::{BufReader, Seek as _, SeekFrom},
    path::{Path, PathBuf},
    sync::Mutex,
};

pub(crate) fn cookie_storage() -> anyhow::Result<CookieStorage> {
    let path = dirs::data_local_dir()
        .with_context(|| "Could not find the local date directory")?
        .join("snowchains")
        .join("cookies.jsonl");

    let cookie_store = if path.exists() {
        File::open(&path)
            .map_err(anyhow::Error::from)
            .and_then(|h| CookieStore::load_json(BufReader::new(h)).map_err(|e| anyhow!("{}", e)))
            .with_context(|| format!("Could not load cookies from `{}`", path.display()))?
    } else {
        CookieStore::default()
    };

    let file = LazyLockedFile::new(&path);

    let on_update = Box::new(move |cookie_store: &CookieStore| -> _ {
        file.overwrite(|file| {
            cookie_store.save_json(file).map_err(|e| anyhow!("{}", e))?;
            Ok(())
        })
    });

    Ok(CookieStorage {
        cookie_store,
        on_update,
    })
}

#[derive(Debug)]
struct LazyLockedFile {
    path: PathBuf,
    file: Mutex<Option<File>>,
}

impl LazyLockedFile {
    fn new(path: &Path) -> Self {
        Self {
            path: path.to_owned(),
            file: Mutex::new(None),
        }
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
