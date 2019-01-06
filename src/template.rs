use crate::command::{
    CompilationCommand, HookCommand, HookCommands, JudgingCommand, TranspilationCommand,
};
use crate::errors::{ExpandTemplateErrorKind, ExpandTemplateResult, StdError};
use crate::path::{AbsPath, AbsPathBuf};
use crate::service::ServiceName;
use crate::testsuite::SuiteFileExtension;
use crate::util::collections::SingleKeyValue;

use combine::Parser;
use failure::{Fail, ResultExt};
use heck::{CamelCase, KebabCase, MixedCase, ShoutySnakeCase, SnakeCase, TitleCase};
use maplit::hashmap;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_derive::{Deserialize, Serialize};

use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::hash::Hash;
use std::str::FromStr;
use std::sync::Arc;
use std::{env, fmt, iter};

#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Serialize, Deserialize)]
pub(crate) struct TemplateBuilder<T: Target>(T::Inner);

impl<T: Target> Default for TemplateBuilder<T>
where
    T::Inner: Default,
{
    fn default() -> Self {
        TemplateBuilder(T::Inner::default())
    }
}

impl<T: Target> Clone for TemplateBuilder<T>
where
    T::Inner: Clone,
{
    fn clone(&self) -> Self {
        TemplateBuilder(self.0.clone())
    }
}

impl<T: Target> FromStr for TemplateBuilder<T>
where
    T::Inner: FromStr<Err = ParseTemplateError>,
{
    type Err = ParseTemplateError;

    fn from_str(s: &str) -> ParseTemplateResult<Self> {
        T::Inner::from_str(s).map(TemplateBuilder)
    }
}

impl<T: Target> TemplateBuilder<T> {
    pub(crate) fn build(&self, requirements: T::Requirements) -> Template<T> {
        Template {
            inner: self.0.clone(),
            requirements,
            strings: hashmap!(),
        }
    }
}

pub(crate) struct Template<T: Target> {
    inner: T::Inner,
    requirements: T::Requirements,
    strings: HashMap<String, String>,
}

impl<T: Target> Template<T> {
    pub(crate) fn service(mut self, value: ServiceName) -> Self {
        for &key in &["service", "SNOWCHAINS_SERVICE"] {
            self.strings.insert(key.to_owned(), value.to_string());
        }
        self
    }

    pub(crate) fn contest(mut self, value: &str) -> Self {
        for &key in &["contest", "SNOWCHAINS_CONTEST"] {
            self.strings.insert(key.to_owned(), value.to_owned());
        }
        self
    }

    pub(crate) fn extension(mut self, value: SuiteFileExtension) -> Self {
        for &key in &["extension", "SNOWCHAINS_EXTENSION"] {
            self.strings.insert(key.to_owned(), value.to_string());
        }
        self
    }

    pub(crate) fn envs(mut self, envs: Option<&HashMap<String, String>>) -> Self {
        if let Some(envs) = envs {
            self.strings
                .extend(envs.iter().map(|(k, v)| (k.clone(), v.clone())));
        }
        self
    }
}

#[cfg(test)]
impl Template<OsString> {
    pub(crate) fn expand(&self, problem: &str) -> ExpandTemplateResult<OsString> {
        let empty = HashMap::<_, &'static OsStr>::new();
        self.inner
            .expand_as_os_string(problem, &self.strings, &empty)
    }
}

impl Template<AbsPathBuf> {
    pub(crate) fn expand(&self, problem: &str) -> ExpandTemplateResult<AbsPathBuf> {
        self.inner
            .expand_as_path(problem, &self.requirements, &self.strings)
    }
}

impl Template<HookCommands> {
    pub(crate) fn expand(&self, problem: &str) -> ExpandTemplateResult<HookCommands> {
        if self.inner.is_empty() {
            return Ok(iter::empty().collect());
        }
        let HookCommandsRequirements {
            base_dir,
            shell,
            result,
        } = &self.requirements;
        let strings = {
            let mut strings = self.strings.clone();
            let json = match result.as_ref() {
                Ok(json) => Ok(json.clone()),
                Err(err) => Err(failure::err_msg(err.to_string())
                    .context(ExpandTemplateErrorKind::SerializeJson)),
            }?;
            strings.insert("result".into(), json.clone());
            strings.insert("SNOWCHAINS_RESULT".into(), json);
            strings
        };
        self.inner
            .iter()
            .map(|inner| {
                let mut args = vec![];
                let empty = HashMap::<&str, &OsStr>::new();
                match inner {
                    CommandTemplateInner::Args(ss) => {
                        for s in ss {
                            args.push(s.expand_as_os_string(problem, &strings, &empty)?);
                        }
                    }
                    CommandTemplateInner::Shell(SingleKeyValue { key, value }) => {
                        let os_strings = hashmap!("command" =>  OsString::from(value.clone()));
                        let shell = shell
                            .get(key)
                            .ok_or_else(|| ExpandTemplateErrorKind::NoSuchShell(key.to_owned()))?;
                        for TemplateBuilder(s) in shell {
                            args.push(s.expand_as_os_string(problem, &strings, &os_strings)?);
                        }
                    }
                }
                let envs = setup_env_vars(problem, &strings, &empty);
                Ok(HookCommand::new(args, base_dir.clone(), envs))
            })
            .collect()
    }
}

impl Template<TranspilationCommand> {
    pub(crate) fn expand(&self, problem: &str) -> ExpandTemplateResult<TranspilationCommand> {
        let TranspilationCommandRequirements {
            base_dir,
            shell,
            working_dir: TemplateBuilder(working_dir),
            src: TemplateBuilder(src),
            transpiled: TemplateBuilder(transpiled),
        } = &self.requirements;
        let strings = &self.strings;
        let wd = working_dir.expand_as_path(problem, base_dir, strings)?;
        let src = src.expand_as_path(problem, base_dir, strings)?;
        let transpiled = transpiled.expand_as_path(problem, base_dir, strings)?;
        let os_strings = hashmap!(
            "src"            => src.as_os_str(),
            "SNOWCHAINS_SRC" => src.as_os_str(),
            "transpiled"     => transpiled.as_os_str(),
            "SNOWCHAINS_BIN" => transpiled.as_os_str(),
        );
        let mut args = vec![];
        match &self.inner {
            CommandTemplateInner::Args(ss) => {
                for s in ss {
                    args.push(s.expand_as_os_string(problem, strings, &os_strings)?);
                }
            }
            CommandTemplateInner::Shell(SingleKeyValue { key, value }) => {
                let mut os_strings = os_strings.clone();
                os_strings.insert("command", value.as_ref());
                let shell = shell
                    .get(key)
                    .ok_or_else(|| ExpandTemplateErrorKind::NoSuchShell(key.to_owned()))?;
                for TemplateBuilder(s) in shell {
                    args.push(s.expand_as_os_string(problem, strings, &os_strings)?);
                }
            }
        }
        let envs = setup_env_vars(problem, strings, &os_strings);
        Ok(TranspilationCommand::new(args, wd, src, transpiled, envs))
    }
}

impl Template<CompilationCommand> {
    pub(crate) fn expand(&self, problem: &str) -> ExpandTemplateResult<CompilationCommand> {
        let CompilationCommandRequirements {
            base_dir,
            shell,
            working_dir: TemplateBuilder(working_dir),
            src: TemplateBuilder(src),
            transpiled,
            bin: TemplateBuilder(bin),
        } = &self.requirements;
        let strings = &self.strings;
        let wd = working_dir.expand_as_path(problem, base_dir, strings)?;
        let src = src.expand_as_path(problem, base_dir, strings)?;
        let bin = bin.expand_as_path(problem, base_dir, strings)?;
        let mut os_strings = hashmap!(
            "src"            => src.as_os_str().to_owned(),
            "SNOWCHAINS_SRC" => src.as_os_str().to_owned(),
            "bin"            => bin.as_os_str().to_owned(),
            "SNOWCHAINS_BIN" => bin.as_os_str().to_owned(),
        );
        if let Some(TemplateBuilder(transpiled)) = transpiled {
            let transpiled = transpiled.expand_as_path(problem, base_dir, strings)?;
            for &key in &["transpiled", "SNOWCHAINS_TRANSPILED"] {
                os_strings.insert(key, transpiled.as_os_str().to_owned());
            }
        }
        let mut args = vec![];
        match &self.inner {
            CommandTemplateInner::Args(ss) => {
                for s in ss {
                    args.push(s.expand_as_os_string(problem, strings, &os_strings)?);
                }
            }
            CommandTemplateInner::Shell(SingleKeyValue { key, value }) => {
                let mut os_strings = os_strings.clone();
                for &key in &["command", "SNOWCHAINS_COMMAND"] {
                    os_strings.insert(key, value.as_str().into());
                }
                let shell = shell
                    .get(key)
                    .ok_or_else(|| ExpandTemplateErrorKind::NoSuchShell(key.to_owned()))?;
                for TemplateBuilder(s) in shell {
                    args.push(s.expand_as_os_string(problem, strings, &os_strings)?);
                }
            }
        }
        let envs = setup_env_vars(problem, strings, &os_strings);
        Ok(CompilationCommand::new(args, wd, src, bin, envs))
    }
}

impl Template<JudgingCommand> {
    pub(crate) fn expand(&self, problem: &str) -> ExpandTemplateResult<JudgingCommand> {
        let JudgingCommandRequirements {
            base_dir,
            shell,
            working_dir: TemplateBuilder(working_dir),
            src: TemplateBuilder(src),
            transpiled,
            bin,
            crlf_to_lf,
        } = &self.requirements;
        let strings = &self.strings;
        let wd = working_dir.expand_as_path(problem, base_dir, strings)?;
        let src = src.expand_as_path(problem, base_dir, strings)?;
        let mut os_strings = hashmap!(
            "src"            => src.as_os_str().to_owned(),
            "SNOWCHAINS_SRC" => src.as_os_str().to_owned(),
        );
        if let Some(TemplateBuilder(bin)) = bin.as_ref() {
            let bin = bin.expand_as_path(problem, base_dir, strings)?;
            for &key in &["bin", "SNOWCHAINS_BIN"] {
                os_strings.insert(key, bin.as_os_str().to_owned());
            }
        }
        if let Some(TemplateBuilder(transpiled)) = transpiled.as_ref() {
            let transpiled = transpiled.expand_as_path(problem, base_dir, strings)?;
            for &key in &["transpiled", "SNOWCHAINS_TRANSPILED"] {
                os_strings.insert(key, transpiled.as_os_str().to_owned());
            }
        }
        let mut args = vec![];
        match &self.inner {
            CommandTemplateInner::Args(ss) => {
                for s in ss {
                    args.push(s.expand_as_os_string(problem, strings, &os_strings)?);
                }
            }
            CommandTemplateInner::Shell(SingleKeyValue { key, value }) => {
                let mut os_strings = os_strings.clone();
                for &key in &["command", "SNOWCHAINS_COMMAND"] {
                    os_strings.insert(key, value.as_str().into());
                }
                let shell = shell
                    .get(key)
                    .ok_or_else(|| ExpandTemplateErrorKind::NoSuchShell(key.to_owned()))?;
                for TemplateBuilder(s) in shell {
                    args.push(s.expand_as_os_string(problem, strings, &os_strings)?);
                }
            }
        }
        let envs = setup_env_vars(problem, strings, &os_strings);
        Ok(JudgingCommand::new(args, wd, *crlf_to_lf, envs))
    }
}

impl<T: Target> Clone for Template<T>
where
    T::Inner: Clone,
    T::Requirements: Clone,
{
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            requirements: self.requirements.clone(),
            strings: self.strings.clone(),
        }
    }
}

pub(crate) trait Target {
    type Inner: Clone + Serialize + DeserializeOwned;
    type Requirements;
}

impl Target for OsString {
    type Inner = Tokens;
    type Requirements = ();
}

impl Target for AbsPathBuf {
    type Inner = Tokens;
    type Requirements = AbsPathBuf;
}

impl Target for HookCommands {
    type Inner = Vec<CommandTemplateInner>;
    type Requirements = HookCommandsRequirements;
}

impl Target for TranspilationCommand {
    type Inner = CommandTemplateInner;
    type Requirements = TranspilationCommandRequirements;
}

impl Target for CompilationCommand {
    type Inner = CommandTemplateInner;
    type Requirements = CompilationCommandRequirements;
}

impl Target for JudgingCommand {
    type Inner = CommandTemplateInner;
    type Requirements = JudgingCommandRequirements;
}

#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum CommandTemplateInner {
    Args(Vec<Tokens>),
    Shell(SingleKeyValue<String, String>),
}

#[derive(Clone)]
pub(crate) struct HookCommandsRequirements {
    pub(crate) base_dir: AbsPathBuf,
    pub(crate) shell: HashMap<String, Vec<TemplateBuilder<OsString>>>,
    pub(crate) result: Arc<serde_json::Result<String>>,
}

#[derive(Clone)]
pub(crate) struct TranspilationCommandRequirements {
    pub(crate) base_dir: AbsPathBuf,
    pub(crate) shell: HashMap<String, Vec<TemplateBuilder<OsString>>>,
    pub(crate) working_dir: TemplateBuilder<AbsPathBuf>,
    pub(crate) src: TemplateBuilder<AbsPathBuf>,
    pub(crate) transpiled: TemplateBuilder<AbsPathBuf>,
}

#[derive(Clone)]
pub(crate) struct CompilationCommandRequirements {
    pub(crate) base_dir: AbsPathBuf,
    pub(crate) shell: HashMap<String, Vec<TemplateBuilder<OsString>>>,
    pub(crate) working_dir: TemplateBuilder<AbsPathBuf>,
    pub(crate) src: TemplateBuilder<AbsPathBuf>,
    pub(crate) transpiled: Option<TemplateBuilder<AbsPathBuf>>,
    pub(crate) bin: TemplateBuilder<AbsPathBuf>,
}

#[derive(Clone)]
pub(crate) struct JudgingCommandRequirements {
    pub(crate) base_dir: AbsPathBuf,
    pub(crate) shell: HashMap<String, Vec<TemplateBuilder<OsString>>>,
    pub(crate) working_dir: TemplateBuilder<AbsPathBuf>,
    pub(crate) src: TemplateBuilder<AbsPathBuf>,
    pub(crate) transpiled: Option<TemplateBuilder<AbsPathBuf>>,
    pub(crate) bin: Option<TemplateBuilder<AbsPathBuf>>,
    pub(crate) crlf_to_lf: bool,
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
        use combine::{attempt, choice, eof, many, many1, satisfy};

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
            )))
            .map(|(h, t): (_, String)| Token::Var(format!("{}{}", h, t)));
        many(choice((
            plain,
            attempt(escape("$$", "$")),
            attempt(escape("{{", "{")),
            attempt(escape("}}", "}")),
            problem,
            var,
        )))
        .skip(eof())
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
                Token::Text(s) => {
                    for c in s.chars() {
                        if ['$', '{', '}'].contains(&c) {
                            write!(f, "{}{}", c, c)
                        } else {
                            write!(f, "{}", c)
                        }?;
                    }
                }
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
    fn expand_as_os_string(
        &self,
        problem: &str,
        strings: &HashMap<impl Borrow<str> + Eq + Hash, impl AsRef<str>>,
        os_strings: &HashMap<&'static str, impl AsRef<OsStr>>,
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
                            let value = value.or_else(|| {
                                os_strings
                                    .get(k.as_str())
                                    .map(|v| Cow::Borrowed(v.as_ref()))
                            });
                            let value = value.or_else(|| env::var_os(k).map(Cow::Owned));
                            let value = value.ok_or_else(|| {
                                ExpandTemplateErrorKind::EnvVarNotPresent(k.to_owned())
                            })?;
                            r.push(&value);
                        }
                        Token::Problem(s) => r.push(apply_specifier(problem, s)?.as_ref()),
                    }
                }
                Ok(r)
            },
            || ExpandTemplateErrorKind::OsStr {
                tokens: self.clone(),
                problem: problem.to_owned(),
            },
        )
    }

    fn expand_as_path(
        &self,
        problem: &str,
        base_dir: &AbsPath,
        strings: &HashMap<impl Borrow<str> + Eq + Hash, impl AsRef<str>>,
    ) -> ExpandTemplateResult<AbsPathBuf> {
        self.expand_as_os_string(problem, strings, &HashMap::<_, &'static OsStr>::new())
            .and_then(|s| {
                base_dir.join_expanding_tilde(&s).map_err(|e| {
                    StdError::from(e)
                        .context(ExpandTemplateErrorKind::Path {
                            tokens: self.clone(),
                            problem: problem.to_owned(),
                            base_dir: base_dir.to_owned(),
                        })
                        .into()
                })
            })
    }

    fn expand_with_context<T: Target>(
        &self,
        f1: impl FnOnce() -> ExpandTemplateResult<T>,
        f2: impl FnOnce() -> ExpandTemplateErrorKind,
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
        s => Err(ExpandTemplateErrorKind::UnknownSpecifier(s.to_owned()).into()),
    }
}

fn setup_env_vars(
    problem: &str,
    strings: &HashMap<impl Borrow<str> + Eq + Hash, impl AsRef<str>>,
    os_strings: &HashMap<&'static str, impl AsRef<OsStr>>,
) -> HashMap<OsString, OsString> {
    let mut ret = hashmap!(
        "SNOWCHAINS_PROBLEM".into()           => problem.into(),
        "SNOWCHAINS_PROBLEM_LOWER".into()     => problem.to_lowercase().into(),
        "SNOWCHAINS_PROBLEM_UPPER".into()     => problem.to_uppercase().into(),
        "SNOWCHAINS_PROBLEM_KEBAB".into()     => problem.to_kebab_case().into(),
        "SNOWCHAINS_PROBLEM_SNAKE".into()     => problem.to_snake_case().into(),
        "SNOWCHAINS_PROBLEM_SCREAMING".into() => problem.to_shouty_snake_case().into(),
        "SNOWCHAINS_PROBLEM_MIXED".into()     => problem.to_mixed_case().into(),
        "SNOWCHAINS_PROBLEM_PASCAL".into()    => problem.to_camel_case().into(),
        "SNOWCHAINS_PROBLEM_TITLE".into()     => problem.to_title_case().into(),
    );
    for (name, value) in strings {
        ret.insert(name.borrow().into(), value.as_ref().into());
    }
    for (name, value) in os_strings {
        ret.insert(name.into(), value.into());
        ret.insert(format!("SNOWCHAINS_{}", name).into(), value.into());
    }
    ret
}

type ParseTemplateResult<T> = std::result::Result<T, ParseTemplateError>;

#[derive(Debug, PartialEq, derive_more::Display)]
#[display(fmt = "Failed to parse {:?}", input)]
pub struct ParseTemplateError {
    input: String,
}

#[cfg(test)]
mod tests {
    use super::TemplateBuilder;

    use crate::command::{CompilationCommand, JudgingCommand};
    use crate::path::AbsPathBuf;
    use crate::service::ServiceName;

    use serde_derive::{Deserialize, Serialize};

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
            a: TemplateBuilder<OsString>,
            b: TemplateBuilder<AbsPathBuf>,
            c: TemplateBuilder<CompilationCommand>,
            d: TemplateBuilder<JudgingCommand>,
        }

        static JSON: &str = r#"{
  "a": "target is {}",
  "b": "rs/{kebab}.rs",
  "c": [
    "rustc",
    "+stable",
    "-o",
    "$bin",
    "$src"
  ],
  "d": {
    "bash": "$bin"
  }
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
        fn process_input(input: &str) -> OsString {
            let template = input.parse::<TemplateBuilder<OsString>>().unwrap();
            template.build(()).expand("problem name").unwrap()
        }

        fn process_expected(expected: &str) -> &OsStr {
            OsStr::new(expected)
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
            template.build(base_dir()).expand("problem-name").unwrap()
        }

        fn process_expected(expected: impl AsRef<OsStr>) -> AbsPathBuf {
            let expected = Path::new(expected.as_ref());
            if expected.is_absolute() {
                AbsPathBuf::try_new(expected).unwrap()
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
                    .build(base_dir())
                    .service(ServiceName::Atcoder)
                    .contest("arc100")
                    .expand("")
                    .unwrap()
            }
            test!("snowchains/$service/$contest" => "snowchains/atcoder/arc100");
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

    #[cfg(not(windows))]
    fn base_dir() -> AbsPathBuf {
        AbsPathBuf::try_new("/basedir").unwrap()
    }

    #[cfg(windows)]
    fn base_dir() -> AbsPathBuf {
        AbsPathBuf::try_new(r"C:\basedir").unwrap()
    }
}
