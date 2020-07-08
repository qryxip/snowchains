use anyhow::Context as _;
use std::{
    env,
    io::{self, Write as _},
    process,
};
use structopt::StructOpt as _;
use termcolor::{BufferedStandardStream, Color, ColorSpec, WriteColor as _};

fn main() {
    let opt = snowchains::Opt::from_args();

    let color = opt.color();

    let stdin = io::stdin();
    let stdin = stdin.lock();
    let stdout = BufferedStandardStream::stdout(termcolor_color(color, atty::Stream::Stdout));
    let mut stderr = BufferedStandardStream::stderr(termcolor_color(color, atty::Stream::Stdout));

    let result = (|| -> _ {
        let ctx = snowchains::Context {
            cwd: env::current_dir().with_context(|| "Failed to get the current directory")?,
            stdin,
            stdout,
            stderr: &mut stderr,
            draw_progress: true,
        };

        snowchains::run(opt, ctx)
    })();

    if let Err(err) = result {
        for (i, s) in format!("{:?}", err).splitn(2, "Caused by:\n").enumerate() {
            let _ = stderr.set_color(
                ColorSpec::new()
                    .set_reset(false)
                    .set_bold(true)
                    .set_fg(Some(Color::Red)),
            );

            if i == 0 {
                let _ = stderr.write_all(b"Error:");
                let _ = stderr.reset();
                let _ = stderr.write_all(b" ");
            } else {
                let _ = stderr.write_all(b"Caused by:");
                let _ = stderr.reset();
                let _ = stderr.write_all(b"\n");
            }

            let _ = stderr.write_all(s.as_ref());
        }

        let _ = stderr.write_all(b"\n");
        let _ = stderr.flush();

        process::exit(1);
    }
}

fn termcolor_color(color: snowchains::ColorChoice, stream: atty::Stream) -> termcolor::ColorChoice {
    if atty::is(stream) {
        match color {
            snowchains::ColorChoice::Auto => termcolor::ColorChoice::Auto,
            snowchains::ColorChoice::Always => termcolor::ColorChoice::Always,
            snowchains::ColorChoice::Never => termcolor::ColorChoice::Never,
        }
    } else {
        termcolor::ColorChoice::Never
    }
}
