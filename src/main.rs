use std::{env, io::Write as _, process};
use structopt::clap;
use termcolor::{BufferedStandardStream, Color, ColorChoice, ColorSpec, WriteColor as _};

fn main() {
    if let Err(err) = snowchains::run(env::args_os()) {
        if let Some(err) = err.downcast_ref::<clap::Error>() {
            err.exit();
        }

        // same as `clap`
        let mut stderr = BufferedStandardStream::stderr(if atty::is(atty::Stream::Stderr) {
            ColorChoice::Auto
        } else {
            ColorChoice::Never
        });

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
