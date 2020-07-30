use anyhow::Context as _;
use cookie_store::CookieStore;
use indicatif::ProgressDrawTarget;
use snowchains_core::{
    color_spec,
    web::{
        Codeforces, CodeforcesSubmitCredentials, CodeforcesSubmitTarget, CookieStorage,
        StatusCodeColor, Submit,
    },
};
use std::{
    env, fmt, fs,
    io::{self, Write as _},
    path::PathBuf,
};
use structopt::StructOpt;
use strum::{EnumString, EnumVariantNames, VariantNames as _};
use termcolor::{BufferedStandardStream, Color, WriteColor as _};

#[derive(StructOpt, Debug)]
struct Opt {
    #[structopt(short, long, value_name("HUMANTIME"))]
    timeout: Option<humantime::Duration>,

    #[structopt(
        long,
        value_name("VIA"),
        default_value("prompt"),
        possible_values(CredentialsVia::VARIANTS)
    )]
    credentials: CredentialsVia,

    contest: u64,

    problem: String,

    language_id: String,

    file: PathBuf,
}

#[derive(EnumString, EnumVariantNames, Debug, Clone, Copy)]
#[strum(serialize_all = "kebab-case")]
enum CredentialsVia {
    Prompt,
    Env,
}

fn main() -> anyhow::Result<()> {
    let Opt {
        timeout,
        credentials,
        contest,
        problem,
        language_id,
        file,
    } = Opt::from_args();

    let api_key = match credentials {
        CredentialsVia::Prompt => rprompt::prompt_reply_stderr("API Key: ")?,
        CredentialsVia::Env => env::var("CODEFORCES_API_KEY")?,
    };

    let api_secret = match credentials {
        CredentialsVia::Prompt => rpassword::read_password_from_tty(Some("API Secret: "))?,
        CredentialsVia::Env => env::var("CODEFORCES_API_SECRET")?,
    };

    let outcome = Codeforces::exec(Submit {
        target: CodeforcesSubmitTarget {
            contest: contest.to_string(),
            problem,
        },
        credentials: CodeforcesSubmitCredentials {
            username_and_password: &mut username_and_password(credentials),
            api_key,
            api_secret,
        },
        language_id,
        code: fs::read_to_string(&file)
            .with_context(|| format!("Failed to read {}", file.display()))?,
        watch_submission: false,
        cookie_storage: CookieStorage {
            cookie_store: CookieStore::default(),
            on_update: Box::new(|_| Ok(())),
        },
        timeout: timeout.map(Into::into),
        shell: Shell::new(),
    })?;

    dbg!(outcome);

    Ok(())
}

fn username_and_password(via: CredentialsVia) -> impl FnMut() -> anyhow::Result<(String, String)> {
    move || {
        let username_and_password = match via {
            CredentialsVia::Prompt => (
                rprompt::prompt_reply_stderr("Handle/Email: ")?,
                rpassword::read_password_from_tty(Some("Password: "))?,
            ),
            CredentialsVia::Env => (
                env::var("CODEFORCES_USERNAME")?,
                env::var("CODEFORCES_PASSWORD")?,
            ),
        };
        Ok(username_and_password)
    }
}

struct Shell(BufferedStandardStream);

impl Shell {
    fn new() -> Self {
        Self(BufferedStandardStream::stderr(
            if atty::is(atty::Stream::Stderr) {
                termcolor::ColorChoice::Auto
            } else {
                termcolor::ColorChoice::Never
            },
        ))
    }
}

impl snowchains_core::web::Shell for Shell {
    fn progress_draw_target(&self) -> ProgressDrawTarget {
        if self.0.supports_color() {
            ProgressDrawTarget::stderr()
        } else {
            ProgressDrawTarget::hidden()
        }
    }

    fn print_ansi(&mut self, message: &[u8]) -> io::Result<()> {
        fwdansi::write_ansi(&mut self.0, message)
    }

    fn warn<T: fmt::Display>(&mut self, message: T) -> io::Result<()> {
        self.0.set_color(color_spec!(Bold, Fg(Color::Yellow)))?;
        write!(self.0, "warning:")?;
        self.0.reset()?;

        writeln!(self.0, " {}", message)?;

        self.0.flush()
    }

    fn on_request(&mut self, req: &reqwest::blocking::Request) -> io::Result<()> {
        self.0.set_color(color_spec!(Bold))?;
        write!(self.0, "{}", req.method())?;
        self.0.reset()?;

        write!(self.0, " ")?;

        self.0.set_color(color_spec!(Fg(Color::Cyan)))?;
        write!(self.0, "{}", req.url())?;
        self.0.reset()?;

        write!(self.0, " ... ")?;

        self.0.flush()
    }

    fn on_response(
        &mut self,
        res: &reqwest::blocking::Response,
        status_code_color: StatusCodeColor,
    ) -> io::Result<()> {
        let fg = match status_code_color {
            StatusCodeColor::Ok => Some(Color::Green),
            StatusCodeColor::Warn => Some(Color::Yellow),
            StatusCodeColor::Error => Some(Color::Red),
            StatusCodeColor::Unknown => None,
        };

        self.0.set_color(color_spec!(Bold).set_fg(fg))?;
        write!(self.0, "{}", res.status())?;
        self.0.reset()?;

        writeln!(self.0)?;

        self.0.flush()
    }
}
