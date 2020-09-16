use anyhow::Context as _;
use snowchains_core::color_spec;
use std::{
    env,
    io::{self, Write as _},
    process,
};
use termcolor::{Color, WriteColor as _};

fn main() {
    let opt = snowchains::Opt::from_args_with_workaround_for_clap_issue_1538();
    let color = opt.color();

    run_with_large_stack(|| {
        let stdin = io::stdin();

        let snowchains::shell::Shell {
            stdin,
            stdout,
            mut stderr,
            stderr_tty,
            stdin_process_redirection,
            stdout_process_redirection,
            stderr_process_redirection,
        } = snowchains::shell::Shell::new(&stdin, color);

        let result = (|| -> _ {
            let ctx = snowchains::Context {
                cwd: env::current_dir().with_context(|| "Failed to get the current directory")?,
                shell: snowchains::shell::Shell {
                    stdin,
                    stdout,
                    stderr: &mut stderr,
                    stderr_tty,
                    stdin_process_redirection,
                    stdout_process_redirection,
                    stderr_process_redirection,
                },
            };

            snowchains::run(opt, ctx)
        })();

        if let Err(err) = result {
            for (i, s) in format!("{:?}", err).splitn(2, "Caused by:\n").enumerate() {
                let _ = stderr.set_color(color_spec!(Bold, Fg(Color::Red)));

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
    });
}

fn run_with_large_stack(f: impl FnOnce() + Send) {
    crossbeam_utils::thread::scope(|scope| {
        scope
            .builder()
            .name("with-stack-size".to_owned())
            .stack_size(snowchains::STACK_SIZE)
            .spawn(|_| f())
            .unwrap()
            .join()
            .unwrap()
    })
    .unwrap();
}
