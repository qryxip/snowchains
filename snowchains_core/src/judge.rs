use crate::testsuite::{BatchTestCase, ExpectedOutput};
use anyhow::Context as _;
use indicatif::{MultiProgress, ProgressBar, ProgressDrawTarget, ProgressStyle};
use std::{
    collections::BTreeMap,
    ffi::OsString,
    io,
    path::PathBuf,
    process::{ExitStatus, Output, Stdio},
    sync::Arc,
    time::{Duration, Instant},
};
use termcolor::{Color, ColorSpec, WriteColor};
use tokio::io::AsyncWriteExt as _;
use unicode_width::UnicodeWidthStr;

#[derive(Debug, Clone)]
pub struct JudgeOutcome {
    pub verdicts: Vec<Verdict>,
}

impl JudgeOutcome {
    pub fn print_pretty<W: WriteColor>(
        &self,
        mut wtr: W,
        display_limit: Option<usize>,
    ) -> io::Result<()> {
        for (i, verdict) in self.verdicts.iter().enumerate() {
            if i > 0 {
                writeln!(wtr)?;
            }

            write!(
                wtr,
                "{}/{} ({}) ",
                i + 1,
                self.verdicts.len(),
                verdict.test_case_name().unwrap_or(""),
            )?;

            wtr.set_color(&color_spec(verdict.summary_color(), true))?;
            writeln!(wtr, "{}", verdict.summary())?;
            wtr.reset()?;

            let mut write_text = |header: &str,
                                  text: &str,
                                  skip_if_empty: bool,
                                  highlight_numbers: bool|
             -> io::Result<()> {
                if text.is_empty() && skip_if_empty {
                    return Ok(());
                }

                wtr.set_color(&color_spec(Color::Magenta, true))?;
                writeln!(wtr, "{}", header)?;
                wtr.reset()?;

                if text.is_empty() {
                    wtr.set_color(&color_spec(Color::Yellow, true))?;
                    writeln!(wtr, "EMPTY")?;
                    return wtr.reset();
                }

                if matches!(display_limit, Some(l) if l < text.len()) {
                    wtr.set_color(&color_spec(Color::Yellow, true))?;
                    writeln!(wtr, "{} B", text.len())?;
                    return wtr.reset();
                }

                for token in parse_to_tokens(text, highlight_numbers) {
                    match token {
                        Token::SpcLf(s) | Token::Plain(s) => wtr.write_all(s.as_ref())?,
                        Token::Cr(n) => {
                            wtr.set_color(&color_spec(Color::Yellow, false))?;
                            (0..n).try_for_each(|_| wtr.write_all(b"\\r"))?;
                            wtr.reset()?;
                        }
                        Token::Tab(n) => {
                            wtr.set_color(&color_spec(Color::Yellow, false))?;
                            (0..n).try_for_each(|_| wtr.write_all(b"\\t"))?;
                            wtr.reset()?;
                        }
                        Token::OtherWhitespaceControl(s) => {
                            wtr.set_color(&color_spec(Color::Yellow, false))?;
                            write!(wtr, "{}", s.escape_unicode())?;
                            wtr.reset()?;
                        }
                        Token::HighlightedNumber(s) => {
                            wtr.set_color(&color_spec(Color::Cyan, false))?;
                            wtr.write_all(s.as_ref())?;
                            wtr.reset()?;
                        }
                    }
                }

                if !text.ends_with('\n') {
                    wtr.set_color(&color_spec(Color::Yellow, false))?;
                    writeln!(wtr, "⏎")?;
                    wtr.reset()?;
                }

                Ok(())
            };

            write_text("stdin:", verdict.stdin(), false, false)?;
            if let Some(expected) = verdict.expected().text() {
                write_text("expected:", expected, false, verdict.expected().is_float())?;
            }
            if let Some(stdout) = verdict.stdout() {
                write_text("actual:", stdout, false, verdict.expected().is_float())?;
            }
            if let Some(stderr) = verdict.stderr() {
                write_text("stderr:", stderr, true, verdict.expected().is_float())?;
            }
        }

        return wtr.flush();

        #[derive(Debug)]
        enum Token<'a> {
            SpcLf(&'a str),
            Cr(usize),
            Tab(usize),
            OtherWhitespaceControl(&'a str),
            HighlightedNumber(&'a str),
            Plain(&'a str),
        }

        fn parse_to_tokens(text: &str, highlight_numbers: bool) -> Vec<Token<'_>> {
            use nom::branch::alt;
            use nom::bytes::complete::take_while1;
            use nom::character::complete::char;
            use nom::combinator::recognize;
            use nom::multi::{many0, many1};
            use nom::number::complete::recognize_float;
            use nom::IResult;

            let (_, tokens) = many0(alt((
                spc_lf,
                cr,
                tab,
                other_whitespace_control,
                highlighted_number_or_plain(highlight_numbers),
            )))(text)
            .unwrap();

            return tokens;

            fn spc_lf(input: &str) -> IResult<&str, Token<'_>> {
                let (rest, target) = take_while1(|c| [' ', '\n'].contains(&c))(input)?;
                Ok((rest, Token::SpcLf(target)))
            }

            fn cr(input: &str) -> IResult<&str, Token<'_>> {
                let (rest, target) = recognize(many1(char('\r')))(input)?;
                Ok((rest, Token::Cr(target.len())))
            }

            fn tab(input: &str) -> IResult<&str, Token<'_>> {
                let (rest, target) = recognize(many1(char('\t')))(input)?;
                Ok((rest, Token::Tab(target.len())))
            }

            fn other_whitespace_control(input: &str) -> IResult<&str, Token<'_>> {
                let (rest, target) =
                    take_while1(|c: char| c.is_whitespace() || c.is_control())(input)?;
                Ok((rest, Token::OtherWhitespaceControl(target)))
            }

            fn highlighted_number_or_plain(
                highlight_numbers: bool,
            ) -> fn(&str) -> IResult<&str, Token<'_>> {
                return if highlight_numbers {
                    |input| highlighted_number(input).or_else(|_| plain(input))
                } else {
                    plain
                };

                fn highlighted_number(input: &str) -> IResult<&str, Token<'_>> {
                    let (rest, target) = recognize_float(input)?;
                    Ok((rest, Token::HighlightedNumber(target)))
                }

                fn plain(input: &str) -> IResult<&str, Token<'_>> {
                    let (rest, target) =
                        take_while1(|c: char| !(c.is_whitespace() || c.is_control()))(input)?;
                    Ok((rest, Token::Plain(target)))
                }
            }
        }

        fn color_spec(fg: Color, bold: bool) -> ColorSpec {
            let mut spec = ColorSpec::new();
            spec.set_reset(false).set_bold(bold).set_fg(Some(fg));
            spec
        }
    }
}

#[derive(Debug, Clone)]
pub enum Verdict {
    Accepted {
        test_case_name: Option<String>,
        elapsed: Duration,
        stdin: Arc<str>,
        stdout: Arc<str>,
        stderr: Arc<str>,
        expected: ExpectedOutput,
    },
    WrongAnswer {
        test_case_name: Option<String>,
        elapsed: Duration,
        stdin: Arc<str>,
        stdout: Arc<str>,
        stderr: Arc<str>,
        expected: ExpectedOutput,
    },
    RuntimeError {
        test_case_name: Option<String>,
        elapsed: Duration,
        stdin: Arc<str>,
        stdout: Arc<str>,
        stderr: Arc<str>,
        expected: ExpectedOutput,
        status: ExitStatus,
    },
    TimelimitExceeded {
        test_case_name: Option<String>,
        timelimit: Duration,
        stdin: Arc<str>,
        expected: ExpectedOutput,
    },
}

impl Verdict {
    fn test_case_name(&self) -> Option<&str> {
        match self {
            Verdict::Accepted { test_case_name, .. }
            | Verdict::WrongAnswer { test_case_name, .. }
            | Verdict::RuntimeError { test_case_name, .. }
            | Verdict::TimelimitExceeded { test_case_name, .. } => test_case_name.as_deref(),
        }
    }

    fn stdin(&self) -> &str {
        match self {
            Verdict::Accepted { stdin, .. }
            | Verdict::WrongAnswer { stdin, .. }
            | Verdict::RuntimeError { stdin, .. }
            | Verdict::TimelimitExceeded { stdin, .. } => stdin,
        }
    }

    fn stdout(&self) -> Option<&str> {
        match self {
            Verdict::Accepted { stdout, .. }
            | Verdict::WrongAnswer { stdout, .. }
            | Verdict::RuntimeError { stdout, .. } => Some(stdout),
            Verdict::TimelimitExceeded { .. } => None,
        }
    }

    fn stderr(&self) -> Option<&str> {
        match self {
            Verdict::Accepted { stderr, .. }
            | Verdict::WrongAnswer { stderr, .. }
            | Verdict::RuntimeError { stderr, .. } => Some(stderr),
            Verdict::TimelimitExceeded { .. } => None,
        }
    }

    fn expected(&self) -> &ExpectedOutput {
        match self {
            Verdict::Accepted { expected, .. }
            | Verdict::WrongAnswer { expected, .. }
            | Verdict::RuntimeError { expected, .. }
            | Verdict::TimelimitExceeded { expected, .. } => expected,
        }
    }

    fn summary(&self) -> String {
        match self {
            Self::Accepted { elapsed, .. } => format!("Accepted ({} ms)", elapsed.as_millis()),
            Self::TimelimitExceeded { timelimit, .. } => {
                format!("Timelimit Exceeded ({} ms)", timelimit.as_millis())
            }
            Self::WrongAnswer { elapsed, .. } => {
                format!("Wrong Answer ({} ms)", elapsed.as_millis())
            }
            Self::RuntimeError {
                elapsed, status, ..
            } => format!("Runtime Error ({} ms, {})", elapsed.as_millis(), status),
        }
    }

    fn summary_color(&self) -> Color {
        match self {
            Self::Accepted { .. } => Color::Green,
            Self::TimelimitExceeded { .. } => Color::Red,
            Self::WrongAnswer { .. } | Self::RuntimeError { .. } => Color::Yellow,
        }
    }

    fn summary_style(&self) -> &'static str {
        match self {
            Self::Accepted { .. } => ".bold.green",
            Self::TimelimitExceeded { .. } => ".bold.red",
            Self::WrongAnswer { .. } | Self::RuntimeError { .. } => ".bold.yellow",
        }
    }
}

pub struct CommandExpression {
    pub program: OsString,
    pub args: Vec<OsString>,
    pub cwd: PathBuf,
    pub env: BTreeMap<OsString, OsString>,
}

impl CommandExpression {
    fn build(&self) -> tokio::process::Command {
        let mut cmd = tokio::process::Command::new(&self.program);

        cmd.args(&self.args)
            .current_dir(&self.cwd)
            .envs(&self.env)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        cmd
    }
}

pub fn judge(
    draw_target: ProgressDrawTarget,
    cmd: &CommandExpression,
    test_cases: &[BatchTestCase],
) -> anyhow::Result<JudgeOutcome> {
    let num_test_cases = test_cases.len();

    let name_width = test_cases
        .iter()
        .flat_map(|BatchTestCase { name, .. }| name.as_deref())
        .map(UnicodeWidthStr::width)
        .max()
        .unwrap_or(0);

    let mp = MultiProgress::with_draw_target(draw_target);

    let mut rt = tokio::runtime::Builder::new()
        .enable_io()
        .enable_time()
        .basic_scheduler()
        .threaded_scheduler()
        .build()?;

    let mut targets = vec![];

    for (i, test_case) in test_cases.iter().enumerate() {
        let pb = mp.add(ProgressBar::new_spinner());

        pb.set_style(progress_style("{prefix}{spinner} {msg:bold}"));

        pb.set_prefix(&format!(
            "{}/{} {} ",
            align_right(&(i + 1).to_string(), num_test_cases.to_string().len()),
            num_test_cases,
            align_left(
                &format!("({})", test_case.name.as_deref().unwrap_or("")),
                name_width,
            ),
        ));

        pb.set_message("Judging...");
        pb.enable_steady_tick(50);

        targets.push((cmd.build(), test_case.clone(), pb));
    }

    let outcome = rt.spawn(async move {
        let num_targets = targets.len();

        let (mut tx, mut rx) = tokio::sync::mpsc::channel(num_cpus::get());
        for _ in 0..num_cpus::get() {
            tx.send(()).await?;
        }

        let mut results = vec![];

        for (i, (mut cmd, test_case, pb)) in targets.into_iter().enumerate() {
            rx.recv().await;

            let mut tx = tx.clone();

            results.push(tokio::task::spawn(async move {
                let finish_pb = |verdict: &Verdict| {
                    tokio::task::block_in_place(|| {
                        pb.set_style(progress_style(&format!(
                            "{{prefix}}{{msg:{}}}",
                            verdict.summary_style(),
                        )));
                        pb.finish_with_message(&verdict.summary());
                    });
                };

                let test_case_name = test_case.name.clone();
                let timelimit = test_case.timelimit;
                let stdin = test_case.input.clone();
                let expected = test_case.output.clone();

                let started = Instant::now();

                let mut child = cmd.spawn()?;

                child
                    .stdin
                    .as_mut()
                    .expect("should be `piped`")
                    .write_all((*stdin).as_ref())
                    .await?;

                child.stdin.take();

                let output = child.wait_with_output();
                let output = if let Some(timelimit) = timelimit {
                    let timeout = timelimit + Duration::from_millis(100);
                    if let Ok(output) = tokio::time::timeout(timeout, output).await {
                        output?
                    } else {
                        let verdict = Verdict::TimelimitExceeded {
                            test_case_name,
                            timelimit,
                            stdin,
                            expected,
                        };
                        finish_pb(&verdict);
                        return Ok((i, verdict));
                    }
                } else {
                    output.await?
                };

                let elapsed = Instant::now() - started;

                let Output {
                    status,
                    stdout,
                    stderr,
                } = output;

                let stdout = String::from_utf8(stdout).with_context(|| "Non-UTF8 output")?;
                let stdout = Arc::from(stdout);

                let stderr = String::from_utf8(stderr).with_context(|| "Non-UTF8 output")?;
                let stderr = Arc::from(stderr);

                let verdict = if matches!(timelimit, Some(t) if t < elapsed) {
                    Verdict::TimelimitExceeded {
                        test_case_name,
                        timelimit: timelimit.unwrap(),
                        stdin,
                        expected,
                    }
                } else if !status.success() {
                    Verdict::RuntimeError {
                        test_case_name,
                        elapsed,
                        stdin,
                        stdout,
                        stderr,
                        expected,
                        status,
                    }
                } else if !test_case.output.accepts(&stdout) {
                    Verdict::WrongAnswer {
                        test_case_name,
                        elapsed,
                        stdin,
                        stdout,
                        stderr,
                        expected,
                    }
                } else {
                    Verdict::Accepted {
                        test_case_name,
                        elapsed,
                        stdin,
                        stdout,
                        stderr,
                        expected,
                    }
                };

                finish_pb(&verdict);

                tx.send(()).await?;

                Ok::<_, anyhow::Error>((i, verdict))
            }));
        }

        let mut verdicts = vec![None; num_targets];
        for result in results {
            let (i, element) = result.await??;
            verdicts[i] = Some(element);
        }
        let verdicts = verdicts.into_iter().map(Option::unwrap).collect();

        Ok::<_, anyhow::Error>(JudgeOutcome { verdicts })
    });

    mp.join()?;

    return rt.block_on(outcome)?;

    fn progress_style(template: impl AsRef<str>) -> ProgressStyle {
        ProgressStyle::default_spinner().template(template.as_ref())
    }

    fn align_left(s: &str, n: usize) -> String {
        let spaces = n.saturating_sub(s.width());
        s.chars().chain(itertools::repeat_n(' ', spaces)).collect()
    }

    fn align_right(s: &str, n: usize) -> String {
        let spaces = n.saturating_sub(s.width());
        itertools::repeat_n(' ', spaces).chain(s.chars()).collect()
    }
}
