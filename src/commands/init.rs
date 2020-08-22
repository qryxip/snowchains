use anyhow::{bail, Context as _};
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

pub(crate) fn run(
    opt: OptInit,
    ctx: crate::Context<impl Sized, impl Sized, impl WriteColor>,
) -> anyhow::Result<()> {
    let OptInit {
        force,
        color: _,
        directory,
    } = opt;

    let crate::Context {
        cwd,
        shell: crate::shell::Shell { mut stderr, .. },
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

    Ok(())
}

#[cfg(test)]
mod tests {
    use rusty_fork::rusty_fork_test;

    rusty_fork_test! {
        #[cfg(not(debug_assertions))]
        #[test]
        fn resolve_default_config_dhall() {
            // https://docs.rs/dhall/0.6.0/src/dhall/semantics/resolve/cache.rs.html#15-35

            use std::env;

            let cache_dir = tempfile::Builder::new()
                .prefix("snowchains-tests-")
                .tempdir()
                .unwrap();

            env::set_var("XDG_CACHE_HOME", cache_dir.path());

            dhall::semantics::parse::parse_str(include_str!(
                "../../resources/config/default-config.dhall",
            ))
            .unwrap()
            .resolve()
            .unwrap();

            cache_dir.close().unwrap();
        }
    }
}
