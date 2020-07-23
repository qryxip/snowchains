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
use structopt::StructOpt;
use strum::VariantNames as _;
use termcolor::{Color, WriteColor};

#[derive(StructOpt, Debug)]
pub struct OptTest {
    /// Build in `Release` mode
    #[structopt(long)]
    pub release: bool,

    ///// Prints the output as a JSON value
    //#[structopt(long)]
    //pub json: bool,
    /// Path to `snowchains.dhall`
    #[structopt(long)]
    pub config: Option<PathBuf>,

    /// Coloring
    #[structopt(
        long,
        possible_values(crate::ColorChoice::VARIANTS),
        default_value("auto")
    )]
    pub color: crate::ColorChoice,

    /// Platform
    #[structopt(
        short,
        long,
        value_name("SERVICE"),
        possible_values(PlatformVariant::KEBAB_CASE_VARIANTS)
    )]
    pub service: Option<PlatformVariant>,

    /// Contest ID
    #[structopt(short, long, value_name("STRING"))]
    pub contest: Option<String>,

    /// Language name
    #[structopt(short, long, value_name("STRING"))]
    pub language: Option<String>,

    /// Problem index (e.g. "a", "b", "c")
    pub problem: Option<String>,
}

pub(crate) fn run(
    opt: OptTest,
    ctx: crate::Context<impl Sized, impl WriteColor, impl WriteColor>,
) -> anyhow::Result<()> {
    let OptTest {
        release,
        //json,
        config,
        color: _,
        service,
        contest,
        language,
        problem,
    } = opt;

    let crate::Context {
        cwd,
        stdin: _,
        stdout,
        mut stderr,
        stdin_process_redirection,
        stdout_process_redirection,
        stderr_process_redirection,
        draw_progress,
    } = ctx;

    let (
        config::Target {
            service,
            contest,
            problem,
            ..
        },
        config::Language {
            src,
            transpile,
            compile,
            run,
            languageId: _,
        },
        base_dir,
    ) = config::target_and_language(
        &cwd,
        config.as_deref(),
        service,
        contest.as_deref(),
        problem.as_deref(),
        language.as_deref(),
        if release {
            config::Mode::Release
        } else {
            config::Mode::Debug
        },
    )?;

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

    let src_modified = {
        let src = Path::new(&src);
        let src = base_dir.join(src.strip_prefix(".").unwrap_or(src));
        crate::fs::metadata(src)?.modified()?
    };

    let mut newline = false;

    for (compile, msg) in &[(transpile, "Transpiling..."), (compile, "Compiling...")] {
        if let Some(config::Compile { command, output }) = compile {
            if mem::replace(&mut newline, true) {
                writeln!(stderr)?;
            }

            let output = Path::new(&output);
            let output = base_dir.join(output.strip_prefix(".").unwrap_or(output));

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
