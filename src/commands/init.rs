use crate::config::{self, Mode};
use anyhow::{bail, Context as _};
use snowchains_core::web::PlatformVariant;
use std::{fs, path::PathBuf};
use structopt::StructOpt;
use strum::VariantNames as _;
use termcolor::WriteColor;

#[derive(StructOpt, Debug)]
pub struct OptInit {
    /// Overwrites the existing config
    #[structopt(short, long)]
    pub force: bool,

    /// Coloring
    #[structopt(
        long,
        possible_values(crate::ColorChoice::VARIANTS),
        default_value("auto")
    )]
    pub color: crate::ColorChoice,

    /// Directory to create a `snowchains.dhall`
    #[structopt(default_value("."))]
    pub directory: PathBuf,
}

pub(crate) fn run<R: Sized, W1: Sized, W2: WriteColor>(
    opt: OptInit,
    ctx: crate::Context<R, W1, W2>,
) -> anyhow::Result<()> {
    let OptInit {
        force,
        color: _,
        directory,
    } = opt;

    let crate::Context {
        cwd,
        stdin: _,
        stdout: _,
        mut stderr,
        draw_progress: _,
    } = ctx;

    let path = cwd
        .join(directory.strip_prefix(".").unwrap_or(&directory))
        .join("snowchains.dhall");

    if !force && path.exists() {
        bail!(
            "`{}` exists. Enable to `--force` to overwrite",
            path.display(),
        );
    }

    fs::write(
        &path,
        include_str!("../../resources/config/default-config.dhall"),
    )
    .with_context(|| format!("Could not write `{}`", path.display()))?;

    writeln!(stderr, "Wrote `{}`", path.display())?;
    stderr.flush()?;

    let t = std::time::Instant::now();
    let lang = config::load_language(
        &path,
        Some(&path),
        Some(PlatformVariant::Atcoder),
        Some("practice"),
        Some("a"),
        Some("cpp"),
        Mode::Debug,
    )?;
    let t = std::time::Instant::now() - t;
    dbg!(t);
    dbg!(lang);

    Ok(())
}
