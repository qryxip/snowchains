use crate::config;
use anyhow::bail;
use indicatif::ProgressDrawTarget;
use itertools::Itertools as _;
use maplit::btreemap;
use snowchains_core::{judge::CommandExpression, testsuite::TestSuite, web::PlatformVariant};
use std::{
    ffi::OsStr,
    io::Write as _,
    iter, mem,
    ops::Deref,
    path::{Path, PathBuf},
    process::Stdio,
};
use termcolor::{Color, WriteColor};

#[derive(Debug)]
pub(crate) struct Args<W1, W2> {
    pub(crate) stdout: W1,
    pub(crate) stderr: W2,
    pub(crate) stdin_process_redirection: fn() -> Stdio,
    pub(crate) stdout_process_redirection: fn() -> Stdio,
    pub(crate) stderr_process_redirection: fn() -> Stdio,
    pub(crate) draw_progress: bool,
    pub(crate) base_dir: PathBuf,
    pub(crate) service: PlatformVariant,
    pub(crate) contest: Option<String>,
    pub(crate) problem: String,
    pub(crate) src: String,
    pub(crate) transpile: Option<config::Compile>,
    pub(crate) compile: Option<config::Compile>,
    pub(crate) run: config::Command,
}

pub(crate) fn judge(args: Args<impl WriteColor, impl WriteColor>) -> anyhow::Result<()> {
    let Args {
        stdout,
        mut stderr,
        stdin_process_redirection,
        stdout_process_redirection,
        stderr_process_redirection,
        draw_progress,
        base_dir,
        service,
        contest,
        problem,
        src,
        transpile,
        compile,
        run,
    } = args;

    let test_suite_dir = base_dir
        .join(".snowchains")
        .join("tests")
        .join(service.to_kebab_case_str())
        .join(contest.as_deref().unwrap_or(""));
    let test_suite_path = test_suite_dir.join(problem).with_extension("yml");

    let test_cases = match crate::fs::read_yaml(&test_suite_path)? {
        TestSuite::Batch(test_sutie) => test_sutie.load_test_cases(&test_suite_dir)?,
        _ => todo!("currently only `Batch` is supported"),
    };

    let redirections = (
        stdin_process_redirection,
        stdout_process_redirection,
        stderr_process_redirection,
    );

    let mut newline = false;

    for (action, msg) in &[(transpile, "Transpiling..."), (compile, "Compiling...")] {
        if let Some(action) = action {
            if mem::replace(&mut newline, true) {
                writeln!(stderr)?;
            }

            build(&mut stderr, &base_dir, &src, action, redirections, msg)?;
        }
    }

    if mem::replace(&mut newline, true) {
        writeln!(stderr)?;
    }
    stderr.set_color(color_spec!(bold))?;
    write!(stderr, "Running the tests...")?;
    stderr.reset()?;
    writeln!(stderr)?;
    stderr.flush()?;

    let (cmd, tempfile) = match run {
        config::Command::Args(args) => {
            let cmd = CommandExpression {
                program: args.get(0).cloned().unwrap_or_default().into(),
                args: args.into_iter().skip(1).map(Into::into).collect(),
                cwd: base_dir,
                env: btreemap!(),
            };

            (cmd, None)
        }
        config::Command::Script(config::Script {
            program,
            extension,
            content,
        }) => {
            let mut tempfile = tempfile::Builder::new()
                .prefix("snowchains-test")
                .suffix(&format!(".{}", extension))
                .tempfile()?;

            tempfile.write_all(content.as_ref())?;

            let cmd = CommandExpression {
                program: program.into(),
                args: vec![tempfile.path().into()],
                cwd: base_dir,
                env: btreemap!(),
            };

            (cmd, Some(tempfile))
        }
    };

    stderr.set_color(color_spec!(bold, fg(Color::Magenta)))?;
    write!(stderr, "Test file:")?;
    stderr.reset()?;
    writeln!(stderr, " {}", test_suite_path.display())?;

    stderr.set_color(color_spec!(bold, fg(Color::Magenta)))?;
    write!(stderr, "Command:")?;
    stderr.reset()?;
    writeln!(stderr, " {}", shell_escape_args(&cmd.program, &cmd.args))?;

    stderr.set_color(color_spec!(bold, fg(Color::Magenta)))?;
    write!(stderr, "Working Directory:")?;
    stderr.reset()?;
    writeln!(stderr, " {}", cmd.cwd.display())?;

    stderr.flush()?;

    let outcome = snowchains_core::judge::judge(
        if draw_progress {
            ProgressDrawTarget::stderr()
        } else {
            ProgressDrawTarget::hidden()
        },
        &cmd,
        &test_cases,
    )?;

    if let Some(tempfile) = tempfile {
        tempfile.close()?;
    }

    writeln!(stderr)?;
    stderr.flush()?;
    outcome.print_pretty(stdout, Some(64 * 1024))?;
    Ok(())
}

pub(crate) fn transpile(
    stderr: impl WriteColor,
    base_dir: &Path,
    src: &str,
    transpile: &config::Compile,
    stdin_process_redirection: fn() -> Stdio,
    stdout_process_redirection: fn() -> Stdio,
    stderr_process_redirection: fn() -> Stdio,
) -> anyhow::Result<()> {
    build(
        stderr,
        base_dir,
        src,
        transpile,
        (
            stdin_process_redirection,
            stdout_process_redirection,
            stderr_process_redirection,
        ),
        "Transpiling...",
    )
}

#[allow(clippy::type_complexity)]
fn build(
    mut stderr: impl WriteColor,
    base_dir: &Path,
    src: &str,
    build_action: &config::Compile,
    redirections: (fn() -> Stdio, fn() -> Stdio, fn() -> Stdio),
    msg: &'static str,
) -> anyhow::Result<()> {
    let src_modified = {
        let src = Path::new(&src);
        let src = base_dir.join(src.strip_prefix(".").unwrap_or(src));
        crate::fs::metadata(src)?.modified()?
    };

    let config::Compile { command, output } = build_action;

    let output = Path::new(&output);
    let output = base_dir.join(output.strip_prefix(".").unwrap_or(output));

    let (stdin_process_redirection, stdout_process_redirection, stderr_process_redirection) =
        redirections;

    if output.exists() && crate::fs::metadata(&output)?.modified()? > src_modified {
        writeln!(stderr, "{} is up to date.", output.display())?;
        stderr.flush()?;
    } else {
        stderr.set_color(color_spec!(bold))?;
        write!(stderr, "{}", msg)?;
        stderr.reset()?;
        writeln!(stderr)?;
        stderr.flush()?;

        if let Some(parent) = output.parent() {
            if !parent.exists() {
                crate::fs::create_dir_all(parent)?;

                write!(stderr, "Created ")?;
                stderr.set_color(color_spec!(fg(Color::Cyan)))?;
                write!(stderr, "{}", parent.display())?;
                stderr.reset()?;
                writeln!(stderr)?;
                stderr.flush()?;
            }
        }

        match command {
            config::Command::Args(args) => run_command(
                args.get(0).map(Deref::deref).unwrap_or(""),
                args.iter().skip(1),
                &base_dir,
                stdin_process_redirection(),
                stdout_process_redirection(),
                stderr_process_redirection(),
                &mut stderr,
            )?,
            config::Command::Script(config::Script {
                program,
                extension,
                content,
            }) => {
                let mut tempfile = tempfile::Builder::new()
                    .prefix("snowchains-test")
                    .suffix(&format!(".{}", extension))
                    .tempfile()?;

                tempfile.write_all(content.as_ref())?;

                run_command(
                    program,
                    &[tempfile.path()],
                    &base_dir,
                    stdin_process_redirection(),
                    stdout_process_redirection(),
                    stderr_process_redirection(),
                    &mut stderr,
                )?;

                tempfile.close()?;
            }
        }
    }

    Ok(())
}

fn run_command<S1: AsRef<OsStr>, S2: AsRef<OsStr>, I: IntoIterator<Item = S2>, W: WriteColor>(
    program: S1,
    args: I,
    base_dir: &Path,
    stdin_process_redirection: Stdio,
    stdout_process_redirection: Stdio,
    stderr_process_redirection: Stdio,
    mut stderr: W,
) -> anyhow::Result<()> {
    let program = program.as_ref();

    let args = args
        .into_iter()
        .map(|s| s.as_ref().to_owned())
        .collect::<Vec<_>>();

    let shell_escaped = shell_escape_args(program, &args);

    stderr.set_color(color_spec!(bold, fg(Color::Magenta)))?;
    write!(stderr, "Command:")?;
    stderr.reset()?;
    writeln!(stderr, " {}", shell_escaped)?;

    stderr.set_color(color_spec!(bold, fg(Color::Magenta)))?;
    write!(stderr, "Working Directory:")?;
    stderr.reset()?;
    writeln!(stderr, " {}", base_dir.display())?;

    stderr.flush()?;

    let status = std::process::Command::new(program)
        .args(&args)
        .current_dir(base_dir)
        .stdin(stdin_process_redirection)
        .stdout(stdout_process_redirection)
        .stderr(stderr_process_redirection)
        .status()?;

    if !status.success() {
        bail!(
            "{} {}",
            shell_escaped,
            if let Some(code) = status.code() {
                format!("exited with code {}", code)
            } else {
                "was terminated by signal".to_owned()
            },
        );
    }

    Ok(())
}

fn shell_escape_args(program: impl AsRef<OsStr>, args: &[impl AsRef<OsStr>]) -> String {
    format!(
        "`{}`",
        iter::once(program.as_ref())
            .chain(args.iter().map(AsRef::as_ref))
            .map(|s| shell_escape::unix::escape(s.to_string_lossy()))
            .format(" "),
    )
}
