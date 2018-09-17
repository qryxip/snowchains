use command::{CompilationCommand, JudgingCommand};
use errors::{ExpandTemplateError, ExpandTemplateErrorContext, ExpandTemplateResult};
use path::{AbsPath, AbsPathBuf};

use combine::Parser;
use failure::ResultExt as _ResultExt;
use heck::{
    CamelCase as _CamelCase, KebabCase as _KebabCase, MixedCase as _MixedCase,
    ShoutySnakeCase as _ShoutySnakeCase, SnakeCase as _SnakeCase, TitleCase as _TitleCase,
};
use serde::de::DeserializeOwned;
use serde::{self, Deserialize, Deserializer, Serialize, Serializer};

use std::borrow::Borrow;
use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::hash::Hash;
use std::str::FromStr;
use std::{self, env, fmt};

#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Default, Serialize, Deserialize)]
pub(crate) struct TemplateBuilder<T: Target>(T::Inner);

#[cfg(test)]
impl<T: Target> FromStr for TemplateBuilder<T>
where
    T::Inner: FromStr<Err = ParseTemplateError>,
{
    type Err = ParseTemplateError;

    fn from_str(s: &str) -> ParseTemplateResult<Self> {
        T::Inner::from_str(s).map(TemplateBuilder)
    }
}

impl TemplateBuilder<String> {
    pub fn build(&self) -> Template<String> {
        Template {
            inner: self.0.clone(),
            base_dir: (),
            command_additional: (),
            strings: hashmap!(),
        }
    }
}

impl TemplateBuilder<AbsPathBuf> {
    pub fn build(&self, base_dir: AbsPath) -> Template<AbsPathBuf> {
        Template {
            inner: self.0.clone(),
            base_dir: base_dir.to_owned(),
            command_additional: (),
            strings: hashmap!(),
        }
    }
}

impl TemplateBuilder<CompilationCommand> {
    pub fn build(
        &self,
        base_dir: AbsPath,
        shell: &[TemplateBuilder<OsString>],
        wd: &TemplateBuilder<AbsPathBuf>,
        src: &TemplateBuilder<AbsPathBuf>,
        bin: &TemplateBuilder<AbsPathBuf>,
    ) -> Template<CompilationCommand> {
        let command_additional = CommandAdditionalInfo {
            shell: shell.iter().map(|t| t.0.clone()).collect(),
            working_dir: wd.0.clone(),
            src: src.0.clone(),
            bin: Some(bin.0.clone()),
        };
        Template {
            inner: self.0.clone(),
            base_dir: base_dir.to_owned(),
            command_additional,
            strings: hashmap!(),
        }
    }
}

impl TemplateBuilder<JudgingCommand> {
    pub fn build(
        &self,
        base_dir: AbsPath,
        shell: &[TemplateBuilder<OsString>],
        wd: &TemplateBuilder<AbsPathBuf>,
        src: &TemplateBuilder<AbsPathBuf>,
        bin: Option<&TemplateBuilder<AbsPathBuf>>,
    ) -> Template<JudgingCommand> {
        let command_additional = CommandAdditionalInfo {
            shell: shell.iter().map(|t| t.0.clone()).collect(),
            working_dir: wd.0.clone(),
            src: src.0.clone(),
            bin: bin.map(|bin| bin.0.clone()),
        };
        Template {
            inner: self.0.clone(),
            base_dir: base_dir.to_owned(),
            command_additional,
            strings: hashmap!(),
        }
    }
}

#[derive(Clone)]
pub(crate) struct Template<T: Target> {
    inner: T::Inner,
    base_dir: T::BaseDir,
    command_additional: T::CommandAdditional,
    strings: HashMap<String, String>,
}

impl<T: Target> Template<T> {
    pub fn insert_string(mut self, key: impl AsRef<str>, value: impl AsRef<str>) -> Self {
        self.strings
            .insert(key.as_ref().to_owned(), value.as_ref().to_owned());
        self
    }

    pub fn insert_strings<K: AsRef<str>, V: AsRef<str>, I: IntoIterator<Item = (K, V)>>(
        mut self,
        strings: I,
    ) -> Self {
        for (k, v) in strings {
            self.strings
                .insert(k.as_ref().to_owned(), v.as_ref().to_owned());
        }
        self
    }

    pub fn strings(self, mut strings: HashMap<String, String>) -> Self {
        for (k, v) in self.strings {
            strings.insert(k, v);
        }
        Self { strings, ..self }
    }
}

impl Template<String> {
    pub fn expand(&self, problem: &str) -> ExpandTemplateResult<String> {
        self.inner.expand_as_string(problem, &self.strings)
    }
}

impl Template<AbsPathBuf> {
    pub fn expand(&self, problem: &str) -> ExpandTemplateResult<AbsPathBuf> {
        self.inner
            .expand_as_path(problem, &self.base_dir, &self.strings)
    }
}

impl Template<CompilationCommand> {
    pub fn expand(&self, problem: &str) -> ExpandTemplateResult<CompilationCommand> {
        let (args, wd, src, bin) = self.expand_as_command(problem)?;
        let bin = bin.unwrap();
        Ok(CompilationCommand::new(&args, wd, src, bin))
    }
}

impl Template<JudgingCommand> {
    pub fn expand(&self, problem: &str) -> ExpandTemplateResult<JudgingCommand> {
        let (args, wd, _, _) = self.expand_as_command(problem)?;
        Ok(JudgingCommand::new(&args, wd))
    }
}

impl<
        T: Target<
            Inner = CommandTemplateInner,
            BaseDir = AbsPathBuf,
            CommandAdditional = CommandAdditionalInfo,
        >,
    > Template<T>
{
    fn expand_as_command(
        &self,
        problem: &str,
    ) -> ExpandTemplateResult<(Vec<OsString>, AbsPathBuf, AbsPathBuf, Option<AbsPathBuf>)> {
        let CommandAdditionalInfo {
            working_dir,
            src,
            bin,
            ..
        } = &self.command_additional;
        let wd = working_dir.expand_as_path(problem, &self.base_dir, &self.strings)?;
        let src = src.expand_as_path(problem, &self.base_dir, &self.strings)?;
        let bin = match bin {
            None => None,
            Some(bin) => Some(bin.expand_as_path(problem, &self.base_dir, &self.strings)?),
        };
        let args = {
            let mut os_strings = hashmap!("src" => src.as_os_str());
            if let Some(bin) = bin.as_ref() {
                os_strings.insert("bin", bin.as_os_str());
            }
            match &self.inner {
                CommandTemplateInner::Args(args) => Cow::Borrowed(args),
                CommandTemplateInner::Shell(arg) => {
                    let mut args = self.command_additional.shell.to_vec();
                    args.push(arg.clone());
                    Cow::Owned(args)
                }
            }.iter()
            .map(|arg| arg.expand_as_os_string(problem, &self.strings, &os_strings))
            .collect::<ExpandTemplateResult<Vec<_>>>()?
        };
        Ok((args, wd, src, bin))
    }
}

pub(crate) trait Target {
    type Inner: Clone + Serialize + DeserializeOwned;
    type BaseDir;
    type CommandAdditional;
}

impl Target for String {
    type Inner = Tokens;
    type BaseDir = ();
    type CommandAdditional = ();
}

impl Target for OsString {
    type Inner = Tokens;
    type BaseDir = ();
    type CommandAdditional = ();
}

impl Target for AbsPathBuf {
    type Inner = Tokens;
    type BaseDir = AbsPathBuf;
    type CommandAdditional = ();
}

impl Target for CompilationCommand {
    type Inner = CommandTemplateInner;
    type BaseDir = AbsPathBuf;
    type CommandAdditional = CommandAdditionalInfo;
}

impl Target for JudgingCommand {
    type Inner = CommandTemplateInner;
    type BaseDir = AbsPathBuf;
    type CommandAdditional = CommandAdditionalInfo;
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone)]
enum Token {
    Text(String),
    Var(String),
    Problem(String),
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Default)]
pub struct Tokens(Vec<Token>);

impl FromStr for Tokens {
    type Err = ParseTemplateError;

    fn from_str(input: &str) -> ParseTemplateResult<Self> {
        use combine::char::{alpha_num, char, letter, spaces, string};
        use combine::{choice, eof, many, many1, satisfy, try};

        fn escape<'a>(
            from: &'static str,
            to: &'static str,
        ) -> impl Parser<Input = &'a str, Output = Token> {
            string(from).map(move |_| Token::Text(to.to_owned()))
        }

        let plain = many1(satisfy(|c| !['$', '{', '}'].contains(&c))).map(Token::Text);
        let problem = char('{')
            .with(spaces())
            .with(many(letter()))
            .skip(spaces().and(char('}')))
            .map(Token::Problem);
        let var = char('$')
            .with(choice((
                alpha_num().and(many(alpha_num().or(char('_')))),
                char('_').and(many1(alpha_num().or(char('_')))),
                char('*').and(many(satisfy(|_| false))),
            ))).map(|(h, t): (_, String)| Token::Var(format!("{}{}", h, t)));
        many(choice((
            plain,
            try(escape("$$", "$")),
            try(escape("{{", "{")),
            try(escape("}}", "}")),
            problem,
            var,
        ))).skip(eof())
        .parse(input)
        .map(|(tokens, _)| Tokens(tokens))
        .map_err(|_| ParseTemplateError {
            input: input.to_owned(),
        })
    }
}

impl fmt::Display for Tokens {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for token in &self.0 {
            match token {
                Token::Text(s) => for c in s.chars() {
                    if ['$', '{', '}'].contains(&c) {
                        write!(f, "{}{}", c, c)
                    } else {
                        write!(f, "{}", c)
                    }?;
                },
                Token::Problem(s) => write!(f, "{{{}}}", s)?,
                Token::Var(s) => write!(f, "${}", s)?,
            }
        }
        Ok(())
    }
}

impl fmt::Debug for Tokens {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let n = self.0.len();
        if n != 1 {
            write!(f, "(")?;
        }
        for (i, t) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, " ++ ")?;
            }
            match t {
                Token::Text(s) => write!(f, "{:?}", s),
                Token::Var(s) => write!(f, "${}", s),
                Token::Problem(s) => write!(f, "{{{}}}", s),
            }?
        }
        if n != 1 {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl Serialize for Tokens {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        serializer.collect_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Tokens {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        Self::from_str(&s).map_err(serde::de::Error::custom)
    }
}

impl Tokens {
    fn expand_as_string(
        &self,
        problem: &str,
        strings: &HashMap<impl Borrow<str> + Eq + Hash, impl AsRef<str>>,
    ) -> ExpandTemplateResult<String> {
        self.expand_with_context(
            || {
                let mut r = "".to_owned();
                for token in &self.0 {
                    match token {
                        Token::Text(s) => r += s,
                        Token::Var(k) => {
                            let value = match strings.get(k.as_str()) {
                                Some(v) => Cow::Borrowed(v.as_ref()),
                                None => env::var(k).map(Cow::Owned).map_err(|e| match e {
                                    env::VarError::NotPresent => {
                                        ExpandTemplateError::EnvVarNotPresent(k.to_owned())
                                    }
                                    env::VarError::NotUnicode(v) => {
                                        ExpandTemplateError::EnvVarNotUnicode(k.to_owned(), v)
                                    }
                                })?,
                            };
                            r += &value;
                        }
                        Token::Problem(s) => r += &apply_specifier(problem, s)?,
                    }
                }
                Ok(r)
            },
            || ExpandTemplateErrorContext::Str {
                tokens: self.clone(),
                problem: problem.to_owned(),
            },
        )
    }

    fn expand_as_os_string(
        &self,
        problem: &str,
        strings: &HashMap<impl Borrow<str> + Eq + Hash, impl AsRef<str>>,
        os_strings: &HashMap<&'static str, &OsStr>,
    ) -> ExpandTemplateResult<OsString> {
        self.expand_with_context(
            || {
                let mut r = OsString::new();
                for token in &self.0 {
                    match token {
                        Token::Text(s) => r.push(s),
                        Token::Var(k) => {
                            let value = strings
                                .get(k.as_str())
                                .map(|v| Cow::Borrowed(OsStr::new(v.as_ref())));
                            let value = value
                                .or_else(|| os_strings.get(k.as_str()).map(|&v| Cow::Borrowed(v)));
                            let value = value.or_else(|| env::var_os(k).map(Cow::Owned));
                            let value = value.ok_or_else(|| {
                                ExpandTemplateError::EnvVarNotPresent(k.to_owned())
                            })?;
                            r.push(&value);
                        }
                        Token::Problem(s) => r.push(apply_specifier(problem, s)?.as_ref()),
                    }
                }
                Ok(r)
            },
            || ExpandTemplateErrorContext::OsStr {
                tokens: self.clone(),
                problem: problem.to_owned(),
            },
        )
    }

    fn expand_as_path(
        &self,
        problem: &str,
        base_dir: AbsPath,
        strings: &HashMap<impl Borrow<str> + Eq + Hash, impl AsRef<str>>,
    ) -> ExpandTemplateResult<AbsPathBuf> {
        self.expand_with_context(
            || {
                self.expand_as_os_string(problem, strings, &hashmap!())
                    .and_then(|s| base_dir.join_expanding_tilde(&s).map_err(Into::into))
            },
            || ExpandTemplateErrorContext::Path {
                tokens: self.clone(),
                problem: problem.to_owned(),
                base_dir: base_dir.to_owned(),
            },
        )
    }

    fn expand_with_context<T: Target>(
        &self,
        f1: impl FnOnce() -> ExpandTemplateResult<T>,
        f2: impl FnOnce() -> ExpandTemplateErrorContext,
    ) -> ExpandTemplateResult<T> {
        f1().with_context(|_| f2()).map_err(Into::into)
    }
}

fn apply_specifier<'a>(problem: &'a str, specifier: &str) -> ExpandTemplateResult<Cow<'a, str>> {
    use std::borrow::Cow::{Borrowed, Owned};
    match specifier {
        s if s.eq_ignore_ascii_case("") => Ok(Borrowed(problem)),
        s if s.eq_ignore_ascii_case("lower") => Ok(Owned(problem.to_lowercase())),
        s if s.eq_ignore_ascii_case("upper") => Ok(Owned(problem.to_uppercase())),
        s if s.eq_ignore_ascii_case("kebab") => Ok(Owned(problem.to_kebab_case())),
        s if s.eq_ignore_ascii_case("snake") => Ok(Owned(problem.to_snake_case())),
        s if s.eq_ignore_ascii_case("screaming") => Ok(Owned(problem.to_shouty_snake_case())),
        s if s.eq_ignore_ascii_case("mixed") => Ok(Owned(problem.to_mixed_case())),
        s if s.eq_ignore_ascii_case("pascal") => Ok(Owned(problem.to_camel_case())),
        s if s.eq_ignore_ascii_case("title") => Ok(Owned(problem.to_title_case())),
        s => Err(ExpandTemplateError::UnknownSpecifier(s.to_owned())),
    }
}

#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum CommandTemplateInner {
    Shell(Tokens),
    Args(Vec<Tokens>),
}

pub(crate) struct CommandAdditionalInfo {
    shell: Vec<Tokens>,
    working_dir: Tokens,
    src: Tokens,
    bin: Option<Tokens>,
}

type ParseTemplateResult<T> = std::result::Result<T, ParseTemplateError>;

#[derive(Debug, PartialEq)]
pub struct ParseTemplateError {
    input: String,
}

impl fmt::Display for ParseTemplateError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Failed to parse {:?}", self.input)
    }
}

#[cfg(test)]
mod tests {
    use super::TemplateBuilder;

    use command::{CompilationCommand, JudgingCommand};
    use path::AbsPathBuf;

    use {dirs, env_logger, serde_json};

    use std::env;
    use std::ffi::{OsStr, OsString};
    use std::path::Path;
    use std::sync::atomic::{self, AtomicBool};

    macro_rules! test {
        ($input:expr => !) => {
            ::std::panic::catch_unwind(move || process_input($input)).unwrap_err()
        };
        ($input:expr => $expected:expr) => {
            assert_eq!(process_expected($expected), process_input($input))
        };
    }

    #[test]
    fn it_serializes_and_deserializes_templates() {
        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct S {
            a: TemplateBuilder<String>,
            b: TemplateBuilder<OsString>,
            c: TemplateBuilder<AbsPathBuf>,
            d: TemplateBuilder<CompilationCommand>,
            e: TemplateBuilder<JudgingCommand>,
        }

        static JSON: &str = r#"{
  "a": "target is {}",
  "b": "target is {}",
  "c": "rs/{kebab}.rs",
  "d": [
    "rustc",
    "+stable",
    "-o",
    "$bin",
    "$src"
  ],
  "e": "$bin"
}"#;

        let _ = env_logger::try_init();
        let s1 = serde_json::from_str::<S>(JSON).unwrap();
        let re_serialized = serde_json::to_string_pretty(&s1).unwrap();
        let s2 = serde_json::from_str::<S>(&re_serialized).unwrap();
        assert_eq!(JSON, re_serialized);
        assert_eq!(s1, s2);
    }

    #[test]
    fn it_expands_string_templates() {
        fn process_input(input: &str) -> String {
            let template = input.parse::<TemplateBuilder<String>>().unwrap();
            template.build().expand("problem name").unwrap()
        }

        fn process_expected(expected: &str) -> &str {
            expected
        }

        let _ = env_logger::try_init();
        set_env_vars();
        test!(""                   => "");
        test!("text"               => "text");
        test!("{}"                 => "problem name");
        test!("{lower}"            => "problem name");
        test!("{UPPER}"            => "PROBLEM NAME");
        test!("{kebab}"            => "problem-name");
        test!("{snake}"            => "problem_name");
        test!("{SCREAMING}"        => "PROBLEM_NAME");
        test!("{mixed}"            => "problemName");
        test!("{Pascal}"           => "ProblemName");
        test!("{Title}"            => "Problem Name");
        test!("$ENVVAR"            => "<value of ENVVAR>");
        test!("$_ENVVAR"           => "<value of _ENVVAR>");
        test!("$__"                => "<value of __>");
        test!("$${{}}"             => "${}");
        test!("{InvalidSpecifier}" => !);
        test!("$NONEXISTING"       => !);
        test!("$---"               => !);
        test!("{"                  => !);
        test!("}"                  => !);
        test!("$"                  => !);
    }

    #[test]
    fn it_expands_path_templates() {
        fn process_input(input: &str) -> AbsPathBuf {
            let template = input.parse::<TemplateBuilder<AbsPathBuf>>().unwrap();
            template.build(&base_dir()).expand("problem-name").unwrap()
        }

        fn process_expected(expected: impl AsRef<OsStr>) -> AbsPathBuf {
            let expected = Path::new(expected.as_ref());
            if expected.is_absolute() {
                AbsPathBuf::new_or_panic(expected)
            } else {
                base_dir().join(expected)
            }
        }

        let _ = env_logger::try_init();
        set_env_vars();
        if cfg!(windows) {
            test!(r"C:\absolute" => r"C:\absolute");
        } else {
            test!("/absolute" => "/absolute");
        }
        test!(""                        => "./");
        test!("."                       => "./");
        test!("relative"                => "./relative");
        test!("./relative"              => "./relative");
        test!("cpp/{snake}.cpp"         => "./cpp/problem_name.cpp");
        test!("cs/{Pascal}/{Pascal}.cs" => "./cs/ProblemName/ProblemName.cs");
        test!("$ENVVAR"                 => "./<value of ENVVAR>");
        {
            fn process_input(input: &str) -> AbsPathBuf {
                let template = input.parse::<TemplateBuilder<AbsPathBuf>>().unwrap();
                template
                    .build(&base_dir())
                    .strings(hashmap!(
                        "service".to_owned() => "Service".to_owned(),
                        "contest".to_owned() => "Contest".to_owned()
                    )).expand("")
                    .unwrap()
            }
            test!("snowchains/$service/$contest" => "snowchains/Service/Contest");
        }
        test!("~"         => dirs::home_dir().unwrap());
        test!("~/foo/bar" => dirs::home_dir().unwrap().join("foo/bar"));
        test!("~root"     => !);
    }

    fn set_env_vars() {
        static NOT_YET: AtomicBool = AtomicBool::new(true);
        if NOT_YET.swap(false, atomic::Ordering::Relaxed) {
            env::set_var("ENVVAR", "<value of ENVVAR>");
            env::set_var("_ENVVAR", "<value of _ENVVAR>");
            env::set_var("__", "<value of __>");
            env::set_var("---", "");
            env::remove_var("NONEXISTING");
        }
    }

    fn base_dir() -> AbsPathBuf {
        AbsPathBuf::new_or_panic(if cfg!(windows) {
            r"C:\basedir"
        } else {
            "/basedir"
        })
    }
}
