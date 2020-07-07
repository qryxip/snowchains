use anyhow::{bail, Context as _};
use indicatif::ProgressDrawTarget;
use maplit::btreemap;
use snowchains_core::{judge::CommandExpression, testsuite::TestSuite};
use std::{env, ffi::OsString, fs, path::PathBuf};
use structopt::StructOpt;
use termcolor::{BufferedStandardStream, ColorChoice};

#[derive(StructOpt, Debug)]
struct Opt {
    file: PathBuf,

    #[structopt(parse(from_os_str), raw(true), required(true))]
    args: Vec<OsString>,
}

fn main() -> anyhow::Result<()> {
    let Opt { file, args } = Opt::from_args();

    let cwd = env::current_dir().with_context(|| "Could not get the current directory")?;
    let file = cwd.join(file.strip_prefix(".").unwrap_or(&file));

    let test_suite =
        fs::read(&file).with_context(|| format!("Could not read `{}`", file.display()))?;

    let test_suite = serde_yaml::from_slice(&test_suite)
        .with_context(|| format!("Could not parse the test file at `{}`", file.display()))?;

    let test_suite = match test_suite {
        TestSuite::Batch(test_suite) => test_suite,
        _ => bail!("Only `Batch` is supported"),
    };

    let test_cases = test_suite.load_test_cases(file.parent().expect("should have file name"))?;

    let outcome = snowchains_core::judge::judge(
        ProgressDrawTarget::stderr(),
        &CommandExpression {
            program: args[0].clone(),
            args: args[1..].to_owned(),
            cwd,
            env: btreemap!(),
        },
        &test_cases,
    )?;

    let stdout = BufferedStandardStream::stdout(if atty::is(atty::Stream::Stdout) {
        ColorChoice::Auto
    } else {
        ColorChoice::Never
    });

    eprintln!();
    outcome.print_pretty(stdout, None)?;

    Ok(())
}
