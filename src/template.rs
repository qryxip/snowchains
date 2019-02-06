use crate::command::{
    CompilationCommand, HookCommand, HookCommands, JudgingCommand, TranspilationCommand,
};
use crate::errors::{ExpandTemplateErrorKind, ExpandTemplateResult, StdError};
use crate::path::{AbsPath, AbsPathBuf};
use crate::service::ServiceName;
use crate::testsuite::SuiteFileExtension;
use crate::util::collections::SingleKeyValue;
use crate::util::std_unstable::Transpose_;

use combine::Parser;
use derive_new::new;
use failure::{Backtrace, Fail, ResultExt};
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
            extension: None,
            envs: hashmap!(),
        }
    }
}

#[cfg(test)]
impl TemplateBuilder<JudgingCommand> {
    pub(crate) fn dummy() -> Self {
        TemplateBuilder(CommandTemplateInner::Args(vec![Tokens(vec![
            Token::Plain("executable".to_owned()),
        ])]))
    }
}

pub(crate) struct Template<T: Target> {
    inner: T::Inner,
    requirements: T::Requirements,
    extension: Option<SuiteFileExtension>,
    envs: HashMap<String, String>,
}

impl<T: Target> Template<T> {
    pub(crate) fn extension(mut self, value: SuiteFileExtension) -> Self {
        self.extension = Some(value);
        self
    }

    pub(crate) fn envs(mut self, envs: Option<&HashMap<String, String>>) -> Self {
        if let Some(envs) = envs {
            self.envs
                .extend(envs.iter().map(|(k, v)| (k.clone(), v.clone())));
        }
        self
    }
}

#[cfg(test)]
impl Template<OsString> {
    pub(crate) fn expand(
        &self,
        vars: &HashMap<&'static str, &'static str>,
        envs: &HashMap<&'static str, &'static str>,
    ) -> ExpandTemplateResult<OsString> {
        self.inner.expand_as_os_string(vars, envs)
    }
}

impl Template<AbsPathBuf> {
    pub(crate) fn expand(&self, problem: Option<&str>) -> ExpandTemplateResult<AbsPathBuf> {
        let AbsPathBufRequirements {
            base_dir,
            service,
            contest,
        } = &self.requirements;
        let envs = {
            let mut envs = setup_env_vars(
                problem,
                Some(&self.envs),
                Some(*service),
                Some(contest),
                None,
                None,
                None,
            );
            envs.extend(self.envs.iter().map(|(k, v)| (k.clone(), v.clone().into())));
            envs
        };
        let vars = {
            let mut vars = HashMap::<_, OsString>::new();
            if let Some(problem) = problem {
                vars.insert("problem", problem.into());
                vars.insert("problem_lower", problem.to_lowercase().into());
                vars.insert("problem_upper", problem.to_uppercase().into());
                vars.insert("problem_kebab", problem.to_kebab_case().into());
                vars.insert("problem_snake", problem.to_snake_case().into());
                vars.insert("problem_screaming", problem.to_shouty_snake_case().into());
                vars.insert("problem_mixed", problem.to_mixed_case().into());
                vars.insert("problem_pascal", problem.to_camel_case().into());
                vars.insert("problem_title", problem.to_title_case().into());
            }
            vars.insert("service", service.to_string().into());
            vars.insert("contest", contest.clone().into());
            if let Some(extension) = self.extension {
                vars.insert("extension", extension.to_string().into());
            }
            vars
        };
        self.inner.expand_as_path(base_dir, &vars, &envs)
    }
}

impl Template<HookCommands> {
    pub(crate) fn expand(&self) -> ExpandTemplateResult<HookCommands> {
        if self.inner.is_empty() {
            return Ok(iter::empty().collect());
        }
        let HookCommandsRequirements {
            base_dir,
            shell,
            result,
        } = &self.requirements;
        let (mut vars, envs) = {
            let json = match result.as_ref() {
                Ok(json) => Ok(json.clone()),
                Err(err) => Err(failure::err_msg(err.to_string())
                    .context(ExpandTemplateErrorKind::SerializeJson)),
            }?;
            let vars = hashmap!("result" => OsString::from(&json));
            let mut envs = self
                .envs
                .iter()
                .map(|(k, v)| (k.clone(), OsString::from(v.clone())))
                .collect::<HashMap<_, _>>();
            envs.insert("SNOWCHAINS_RESULT".to_owned(), OsString::from(json));
            (vars, envs)
        };
        self.inner
            .iter()
            .map(|inner| {
                let mut args = vec![];
                match inner {
                    CommandTemplateInner::Args(ss) => {
                        for s in ss {
                            args.push(s.expand_as_os_string(&vars, &envs)?);
                        }
                    }
                    CommandTemplateInner::Shell(SingleKeyValue { key, value }) => {
                        vars.entry("command")
                            .or_insert_with(|| value.clone().into());
                        let shell = shell
                            .get(key)
                            .ok_or_else(|| ExpandTemplateErrorKind::NoSuchShell(key.to_owned()))?;
                        for TemplateBuilder(s) in shell {
                            args.push(s.expand_as_os_string(&vars, &envs)?);
                        }
                    }
                }
                Ok(HookCommand::new(args, base_dir.clone(), envs.clone()))
            })
            .collect()
    }
}

impl Template<TranspilationCommand> {
    pub(crate) fn expand(&self, problem: &str) -> ExpandTemplateResult<TranspilationCommand> {
        let TranspilationCommandRequirements {
            base_dir,
            service,
            contest,
            shell,
            working_dir: TemplateBuilder(working_dir),
            src: TemplateBuilder(src),
            transpiled,
        } = &self.requirements;

        let mut vars = hashmap!(
            "problem"           => OsString::from(problem),
            "problem_lower"     => OsString::from(problem.to_lowercase()),
            "problem_upper"     => OsString::from(problem.to_uppercase()),
            "problem_kebab"     => OsString::from(problem.to_kebab_case()),
            "problem_snake"     => OsString::from(problem.to_snake_case()),
            "problem_screaming" => OsString::from(problem.to_shouty_snake_case()),
            "problem_mixed"     => OsString::from(problem.to_mixed_case()),
            "problem_pascal"    => OsString::from(problem.to_camel_case()),
            "problem_title"     => OsString::from(problem.to_title_case()),
            "service"           => OsString::from(service.to_string()),
            "contest"           => OsString::from(contest.clone()),
        );

        let mut envs = hashmap!(
            "SNOWCHAINS_PROBLEM".to_owned()       => OsString::from(problem),
            "SNOWCHAINS_PROBLEM_LOWER".into()     => problem.to_lowercase().into(),
            "SNOWCHAINS_PROBLEM_UPPER".into()     => problem.to_uppercase().into(),
            "SNOWCHAINS_PROBLEM_KEBAB".into()     => problem.to_kebab_case().into(),
            "SNOWCHAINS_PROBLEM_SNAKE".into()     => problem.to_snake_case().into(),
            "SNOWCHAINS_PROBLEM_SCREAMING".into() => problem.to_shouty_snake_case().into(),
            "SNOWCHAINS_PROBLEM_MIXED".into()     => problem.to_mixed_case().into(),
            "SNOWCHAINS_PROBLEM_PASCAL".into()    => problem.to_camel_case().into(),
            "SNOWCHAINS_PROBLEM_TITLE".into()     => problem.to_title_case().into(),
            "SNOWCHAINS_SERVICE".into()           => service.to_string().into(),
            "SNOWCHAINS_CONTEST".into()           => contest.clone().into(),
        );
        envs.extend(self.envs.iter().map(|(k, v)| (k.clone(), v.clone().into())));

        let wd = working_dir.expand_as_path(base_dir, &vars, &envs)?;
        let src = src.expand_as_path(base_dir, &vars, &envs)?;
        vars.insert("src", src.clone().into());
        envs.insert("SNOWCHAINS_SRC".into(), src.clone().into());
        let transpiled = transpiled
            .as_ref()
            .map(|TemplateBuilder(transpiled)| transpiled.expand_as_path(base_dir, &vars, &envs))
            .transpose_()?;
        if let Some(transpiled) = &transpiled {
            vars.insert("transpiled", transpiled.clone().into());
            envs.insert("SNOWCHAINS_TRANSPILED".into(), transpiled.clone().into());
        }

        let mut args = vec![];
        match &self.inner {
            CommandTemplateInner::Args(ss) => {
                for s in ss {
                    args.push(s.expand_as_os_string(&vars, &envs)?);
                }
            }
            CommandTemplateInner::Shell(SingleKeyValue { key, value }) => {
                vars.entry("command")
                    .or_insert_with(|| value.clone().into());
                let shell = shell
                    .get(key)
                    .ok_or_else(|| ExpandTemplateErrorKind::NoSuchShell(key.to_owned()))?;
                for TemplateBuilder(s) in shell {
                    args.push(s.expand_as_os_string(&vars, &envs)?);
                }
            }
        }
        Ok(TranspilationCommand::new(args, wd, src, transpiled, envs))
    }
}

impl Template<CompilationCommand> {
    pub(crate) fn expand(&self, problem: &str) -> ExpandTemplateResult<CompilationCommand> {
        let CompilationCommandRequirements {
            base_dir,
            service,
            contest,
            shell,
            working_dir: TemplateBuilder(working_dir),
            src: TemplateBuilder(src),
            transpiled,
            bin,
        } = &self.requirements;

        let mut vars = hashmap!(
            "problem"           => OsString::from(problem),
            "problem_lower"     => OsString::from(problem.to_lowercase()),
            "problem_upper"     => OsString::from(problem.to_uppercase()),
            "problem_kebab"     => OsString::from(problem.to_kebab_case()),
            "problem_snake"     => OsString::from(problem.to_snake_case()),
            "problem_screaming" => OsString::from(problem.to_shouty_snake_case()),
            "problem_mixed"     => OsString::from(problem.to_mixed_case()),
            "problem_pascal"    => OsString::from(problem.to_camel_case()),
            "problem_title"     => OsString::from(problem.to_title_case()),
            "service"           => OsString::from(service.to_string()),
            "contest"           => OsString::from(contest.clone()),
        );

        let mut envs = hashmap!(
            "SNOWCHAINS_PROBLEM".to_owned()       => OsString::from(problem),
            "SNOWCHAINS_PROBLEM_LOWER".into()     => problem.to_lowercase().into(),
            "SNOWCHAINS_PROBLEM_UPPER".into()     => problem.to_uppercase().into(),
            "SNOWCHAINS_PROBLEM_KEBAB".into()     => problem.to_kebab_case().into(),
            "SNOWCHAINS_PROBLEM_SNAKE".into()     => problem.to_snake_case().into(),
            "SNOWCHAINS_PROBLEM_SCREAMING".into() => problem.to_shouty_snake_case().into(),
            "SNOWCHAINS_PROBLEM_MIXED".into()     => problem.to_mixed_case().into(),
            "SNOWCHAINS_PROBLEM_PASCAL".into()    => problem.to_camel_case().into(),
            "SNOWCHAINS_PROBLEM_TITLE".into()     => problem.to_title_case().into(),
            "SNOWCHAINS_SERVICE".into()           => service.to_string().into(),
            "SNOWCHAINS_CONTEST".into()           => contest.clone().into(),
        );
        envs.extend(self.envs.iter().map(|(k, v)| (k.clone(), v.clone().into())));

        let wd = working_dir.expand_as_path(base_dir, &vars, &envs)?;
        let src = src.expand_as_path(base_dir, &vars, &envs)?;
        vars.insert("src", src.clone().into());
        envs.insert("SNOWCHAINS_SRC".into(), src.clone().into());
        let transpiled = transpiled
            .as_ref()
            .map(|TemplateBuilder(transpiled)| transpiled.expand_as_path(base_dir, &vars, &envs))
            .transpose_()?;
        if let Some(transpiled) = &transpiled {
            vars.insert("transpiled", transpiled.clone().into());
            envs.insert("SNOWCHAINS_TRANSPILED".into(), transpiled.clone().into());
        }

        let bin = bin
            .as_ref()
            .map(|TemplateBuilder(bin)| bin.expand_as_path(base_dir, &vars, &envs))
            .transpose_()?;
        if let Some(bin) = &bin {
            vars.insert("bin", bin.clone().into());
            envs.insert("SNOWCHAINS_BIN".into(), bin.clone().into());
        }

        let mut args = vec![];
        match &self.inner {
            CommandTemplateInner::Args(ss) => {
                for s in ss {
                    args.push(s.expand_as_os_string(&vars, &envs)?);
                }
            }
            CommandTemplateInner::Shell(SingleKeyValue { key, value }) => {
                vars.entry("command")
                    .or_insert_with(|| value.clone().into());
                let shell = shell
                    .get(key)
                    .ok_or_else(|| ExpandTemplateErrorKind::NoSuchShell(key.to_owned()))?;
                for TemplateBuilder(s) in shell {
                    args.push(s.expand_as_os_string(&vars, &envs)?);
                }
            }
        }
        Ok(CompilationCommand::new(args, wd, src, bin, envs))
    }
}

impl Template<JudgingCommand> {
    pub(crate) fn expand(&self, problem: &str) -> ExpandTemplateResult<JudgingCommand> {
        let JudgingCommandRequirements {
            base_dir,
            service,
            contest,
            shell,
            working_dir: TemplateBuilder(working_dir),
            src: TemplateBuilder(src),
            transpiled,
            bin,
            crlf_to_lf,
        } = &self.requirements;

        let mut vars = hashmap!(
            "problem"           => OsString::from(problem),
            "problem_lower"     => OsString::from(problem.to_lowercase()),
            "problem_upper"     => OsString::from(problem.to_uppercase()),
            "problem_kebab"     => OsString::from(problem.to_kebab_case()),
            "problem_snake"     => OsString::from(problem.to_snake_case()),
            "problem_screaming" => OsString::from(problem.to_shouty_snake_case()),
            "problem_mixed"     => OsString::from(problem.to_mixed_case()),
            "problem_pascal"    => OsString::from(problem.to_camel_case()),
            "problem_title"     => OsString::from(problem.to_title_case()),
            "service"           => OsString::from(service.to_string()),
            "contest"           => OsString::from(contest.clone()),
        );

        let mut envs = hashmap!(
            "SNOWCHAINS_PROBLEM".to_owned()       => OsString::from(problem),
            "SNOWCHAINS_PROBLEM_LOWER".into()     => problem.to_lowercase().into(),
            "SNOWCHAINS_PROBLEM_UPPER".into()     => problem.to_uppercase().into(),
            "SNOWCHAINS_PROBLEM_KEBAB".into()     => problem.to_kebab_case().into(),
            "SNOWCHAINS_PROBLEM_SNAKE".into()     => problem.to_snake_case().into(),
            "SNOWCHAINS_PROBLEM_SCREAMING".into() => problem.to_shouty_snake_case().into(),
            "SNOWCHAINS_PROBLEM_MIXED".into()     => problem.to_mixed_case().into(),
            "SNOWCHAINS_PROBLEM_PASCAL".into()    => problem.to_camel_case().into(),
            "SNOWCHAINS_PROBLEM_TITLE".into()     => problem.to_title_case().into(),
            "SNOWCHAINS_SERVICE".into()           => service.to_string().into(),
            "SNOWCHAINS_CONTEST".into()           => contest.clone().into(),
        );
        envs.extend(self.envs.iter().map(|(k, v)| (k.clone(), v.clone().into())));

        let wd = working_dir.expand_as_path(base_dir, &vars, &envs)?;
        let src = src.expand_as_path(base_dir, &vars, &envs)?;
        vars.insert("src", src.clone().into());
        envs.insert("SNOWCHAINS_SRC".into(), src.clone().into());
        let transpiled = transpiled
            .as_ref()
            .map(|TemplateBuilder(transpiled)| transpiled.expand_as_path(base_dir, &vars, &envs))
            .transpose_()?;
        if let Some(transpiled) = &transpiled {
            vars.insert("transpiled", transpiled.clone().into());
            envs.insert("SNOWCHAINS_TRANSPILED".into(), transpiled.clone().into());
        }
        let bin = bin
            .as_ref()
            .map(|TemplateBuilder(bin)| bin.expand_as_path(base_dir, &vars, &envs))
            .transpose_()?;
        if let Some(bin) = &bin {
            vars.insert("bin", bin.clone().into());
            envs.insert("SNOWCHAINS_BIN".into(), bin.clone().into());
        }

        let mut args = vec![];
        match &self.inner {
            CommandTemplateInner::Args(ss) => {
                for s in ss {
                    args.push(s.expand_as_os_string(&vars, &envs)?);
                }
            }
            CommandTemplateInner::Shell(SingleKeyValue { key, value }) => {
                vars.entry("command")
                    .or_insert_with(|| value.clone().into());
                let shell = shell
                    .get(key)
                    .ok_or_else(|| ExpandTemplateErrorKind::NoSuchShell(key.to_owned()))?;
                for TemplateBuilder(s) in shell {
                    args.push(s.expand_as_os_string(&vars, &envs)?);
                }
            }
        }
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
            extension: self.extension,
            envs: self.envs.clone(),
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
    type Requirements = AbsPathBufRequirements;
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
pub(crate) struct AbsPathBufRequirements {
    pub(crate) base_dir: AbsPathBuf,
    pub(crate) service: ServiceName,
    pub(crate) contest: String,
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
    pub(crate) service: ServiceName,
    pub(crate) contest: String,
    pub(crate) shell: HashMap<String, Vec<TemplateBuilder<OsString>>>,
    pub(crate) working_dir: TemplateBuilder<AbsPathBuf>,
    pub(crate) src: TemplateBuilder<AbsPathBuf>,
    pub(crate) transpiled: Option<TemplateBuilder<AbsPathBuf>>,
}

#[derive(Clone)]
pub(crate) struct CompilationCommandRequirements {
    pub(crate) base_dir: AbsPathBuf,
    pub(crate) service: ServiceName,
    pub(crate) contest: String,
    pub(crate) shell: HashMap<String, Vec<TemplateBuilder<OsString>>>,
    pub(crate) working_dir: TemplateBuilder<AbsPathBuf>,
    pub(crate) src: TemplateBuilder<AbsPathBuf>,
    pub(crate) transpiled: Option<TemplateBuilder<AbsPathBuf>>,
    pub(crate) bin: Option<TemplateBuilder<AbsPathBuf>>,
}

#[derive(Clone)]
pub(crate) struct JudgingCommandRequirements {
    pub(crate) base_dir: AbsPathBuf,
    pub(crate) service: ServiceName,
    pub(crate) contest: String,
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
    Plain(String),
    Var(String),
    EnvVar(String),
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Default)]
pub struct Tokens(Vec<Token>);

impl FromStr for Tokens {
    type Err = ParseTemplateError;

    fn from_str(input: &str) -> ParseTemplateResult<Self> {
        use combine::char::{char, spaces, string};
        use combine::{attempt, choice, eof, many, many1, satisfy};

        fn escape<'a>(
            from: &'static str,
            to: &'static str,
        ) -> impl Parser<Input = &'a str, Output = Token> {
            string(from).map(move |_| Token::Plain(to.to_owned()))
        }

        fn identifier<'a>() -> impl Parser<Input = &'a str, Output = String> {
            many1(satisfy(|c| match c {
                'a'..='z' | 'A'..='Z' | '_' => true,
                _ => false,
            }))
        }

        let plain = many1(satisfy(|c| !['$', '{', '}'].contains(&c))).map(Token::Plain);
        let envvar = string("${")
            .with(spaces().and(string("env:")).and(spaces()))
            .with(identifier())
            .skip(spaces().and(char('}')))
            .map(Token::EnvVar);
        let var = string("${")
            .with(spaces())
            .with(identifier())
            .skip(spaces().and(char('}')))
            .map(Token::Var);

        many(choice((
            plain,
            attempt(escape("$$", "$")),
            attempt(escape("{{", "{")),
            attempt(escape("}}", "}")),
            attempt(envvar),
            var,
        )))
        .skip(eof())
        .parse(input)
        .map(|(tokens, _)| Tokens(tokens))
        .map_err(|_| ParseTemplateError::new(input.to_owned()))
    }
}

impl fmt::Display for Tokens {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for token in &self.0 {
            match token {
                Token::Plain(s) => {
                    for c in s.chars() {
                        if ['$', '{', '}'].contains(&c) {
                            write!(f, "{}{}", c, c)
                        } else {
                            write!(f, "{}", c)
                        }?;
                    }
                }
                Token::Var(s) => write!(f, "${{{}}}", s)?,
                Token::EnvVar(s) => write!(f, "${{env:{}}}", s)?,
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
                Token::Plain(s) => write!(f, "{:?}", s),
                Token::Var(s) => write!(f, "${{{}}}", s),
                Token::EnvVar(s) => write!(f, "${{env:{}}}", s),
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
        vars: &HashMap<&'static str, impl AsRef<OsStr>>,
        envs: &HashMap<impl Borrow<str> + Eq + Hash, impl AsRef<OsStr>>,
    ) -> ExpandTemplateResult<OsString> {
        let expand_as_os_string = || -> ExpandTemplateResult<_> {
            let mut r = OsString::new();
            for token in &self.0 {
                match token {
                    Token::Plain(s) => r.push(s),
                    Token::Var(k) => {
                        let value = vars
                            .get(k.as_str())
                            .ok_or_else(|| ExpandTemplateErrorKind::UndefinedVar(k.clone()))?;
                        r.push(&value);
                    }
                    Token::EnvVar(k) => {
                        let value = envs
                            .get(k.as_str())
                            .map(|v| Cow::Borrowed(v.as_ref()))
                            .or_else(|| env::var_os(k).map(Cow::Owned))
                            .ok_or_else(|| ExpandTemplateErrorKind::EnvVarNotPresent(k.clone()))?;
                        r.push(&value);
                    }
                }
            }
            Ok(r)
        };

        expand_as_os_string()
            .with_context(|_| ExpandTemplateErrorKind::OsStr {
                tokens: self.clone(),
            })
            .map_err(Into::into)
    }

    fn expand_as_path(
        &self,
        base_dir: &AbsPath,
        vars: &HashMap<&'static str, impl AsRef<OsStr>>,
        envs: &HashMap<impl Borrow<str> + Eq + Hash, impl AsRef<OsStr>>,
    ) -> ExpandTemplateResult<AbsPathBuf> {
        self.expand_as_os_string(vars, envs).and_then(|s| {
            base_dir.join_expanding_tilde(&s).map_err(|e| {
                StdError::from(e)
                    .context(ExpandTemplateErrorKind::Path {
                        tokens: self.clone(),
                        base_dir: base_dir.to_owned(),
                    })
                    .into()
            })
        })
    }
}

fn setup_env_vars(
    problem: Option<&str>,
    utf8_envs: Option<&HashMap<String, String>>,
    service: Option<ServiceName>,
    contest: Option<&str>,
    src: Option<&AbsPath>,
    transpiled: Option<&AbsPath>,
    bin: Option<&AbsPath>,
) -> HashMap<String, OsString> {
    let mut ret = problem
        .map::<HashMap<String, OsString>, _>(|problem| {
            hashmap!(
                "SNOWCHAINS_PROBLEM".into()           => problem.into(),
                "SNOWCHAINS_PROBLEM_LOWER".into()     => problem.to_lowercase().into(),
                "SNOWCHAINS_PROBLEM_UPPER".into()     => problem.to_uppercase().into(),
                "SNOWCHAINS_PROBLEM_KEBAB".into()     => problem.to_kebab_case().into(),
                "SNOWCHAINS_PROBLEM_SNAKE".into()     => problem.to_snake_case().into(),
                "SNOWCHAINS_PROBLEM_SCREAMING".into() => problem.to_shouty_snake_case().into(),
                "SNOWCHAINS_PROBLEM_MIXED".into()     => problem.to_mixed_case().into(),
                "SNOWCHAINS_PROBLEM_PASCAL".into()    => problem.to_camel_case().into(),
                "SNOWCHAINS_PROBLEM_TITLE".into()     => problem.to_title_case().into(),
            )
        })
        .unwrap_or_default();
    if let Some(utf8_envs) = utf8_envs {
        for (name, value) in utf8_envs {
            ret.insert(name.as_str().into(), value.as_str().into());
        }
    }
    if let Some(service) = service {
        ret.insert("SNOWCHAINS_SERVICE".into(), service.to_string().into());
    }
    if let Some(contest) = contest {
        ret.insert("SNOWCHAINS_CONTEST".into(), contest.into());
    }
    if let Some(src) = src {
        ret.insert("SNOWCHAINS_SRC".into(), src.into());
    }
    if let Some(transpiled) = transpiled {
        ret.insert("SNOWCHAINS_TRANSPILED".into(), transpiled.into());
    }
    if let Some(bin) = bin {
        ret.insert("SNOWCHAINS_BIN".into(), bin.into());
    }
    ret
}

type ParseTemplateResult<T> = std::result::Result<T, ParseTemplateError>;

#[derive(Debug, derive_more::Display, Fail, new)]
#[display(fmt = "Failed to parse {:?}", input)]
pub struct ParseTemplateError {
    input: String,
    #[new(default)]
    #[fail(backtrace)]
    backtrace: Backtrace,
}

#[cfg(test)]
mod tests {
    use super::{AbsPathBufRequirements, TemplateBuilder};

    use crate::command::{CompilationCommand, JudgingCommand};
    use crate::path::AbsPathBuf;
    use crate::service::ServiceName;

    use failure::Fallible;
    use maplit::hashmap;
    use serde_derive::{Deserialize, Serialize};

    use std::ffi::{OsStr, OsString};
    use std::path::Path;

    macro_rules! test {
        ($input:expr => !) => {
            process_input($input).unwrap_err()
        };
        ($input:expr => $expected:expr) => {
            assert_eq!(process_input($input)?, process_expected($expected))
        };
    }

    #[test]
    fn it_serializes_and_deserializes_templates() -> Fallible<()> {
        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct S {
            a: TemplateBuilder<OsString>,
            b: TemplateBuilder<AbsPathBuf>,
            c: TemplateBuilder<CompilationCommand>,
            d: TemplateBuilder<JudgingCommand>,
        }

        static JSON: &str = r##"{
  "a": "${command}",
  "b": "rs/${problem_kebab}.rs",
  "c": [
    "rustc",
    "+${env:RUST_VERSION}",
    "-o",
    "${bin}",
    "${src}"
  ],
  "d": {
    "bash": "#"
  }
}"##;

        let _ = env_logger::try_init();
        let s1 = serde_json::from_str::<S>(JSON)?;
        let re_serialized = serde_json::to_string_pretty(&s1)?;
        let s2 = serde_json::from_str::<S>(&re_serialized)?;
        assert_eq!(JSON, re_serialized);
        assert_eq!(s1, s2);
        Ok(())
    }

    #[test]
    fn it_expands_os_string_templates() -> Fallible<()> {
        fn process_input(input: &str) -> Fallible<OsString> {
            let template = input.parse::<TemplateBuilder<OsString>>()?;
            let vars = hashmap!("var" => "<value of var>");
            let envs = hashmap!("ENVVAR" => "<value of ENVVAR>");
            template.build(()).expand(&vars, &envs).map_err(Into::into)
        }

        fn process_expected(expected: &str) -> &OsStr {
            OsStr::new(expected)
        }

        let _ = env_logger::try_init();
        test!(""                     => "");
        test!("text"                 => "text");
        test!("${var}"               => "<value of var>");
        test!("${env:ENVVAR}"        => "<value of ENVVAR>");
        test!("$${{}}"               => "${}");
        test!("{InvalidSpecifier}"   => !);
        test!("$NONEXISTING"         => !);
        test!("{var}"                => !);
        test!("{env:ENVVAR}"         => !);
        test!("${-}"                 => !);
        test!("{"                    => !);
        test!("}"                    => !);
        test!("$"                    => !);
        Ok(())
    }

    #[test]
    fn it_expands_path_templates() -> Fallible<()> {
        fn process_input(input: &str) -> Fallible<AbsPathBuf> {
            let template = input.parse::<TemplateBuilder<AbsPathBuf>>()?;
            template
                .build(AbsPathBufRequirements {
                    base_dir: base_dir(),
                    service: ServiceName::Atcoder,
                    contest: "arc100".to_owned(),
                })
                .expand(Some("problem-name"))
                .map_err(Into::into)
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
        if cfg!(windows) {
            test!(r"C:\absolute" => r"C:\absolute");
        } else {
            test!("/absolute" => "/absolute");
        }
        test!(""                                          => "./");
        test!("."                                         => "./");
        test!("relative"                                  => "./relative");
        test!("./relative"                                => "./relative");
        test!("cpp/${problem_snake}.cpp"                  => "./cpp/problem_name.cpp");
        test!("cs/${problem_pascal}/${problem_pascal}.cs" => "./cs/ProblemName/ProblemName.cs");
        test!("${service}/${contest}"                     => "./atcoder/arc100");
        {
            fn process_input(input: &str) -> Fallible<AbsPathBuf> {
                let template = input.parse::<TemplateBuilder<AbsPathBuf>>()?;
                template
                    .build(AbsPathBufRequirements {
                        base_dir: base_dir(),
                        service: ServiceName::Atcoder,
                        contest: "arc100".to_owned(),
                    })
                    .expand(None)
                    .map_err(Into::into)
            }
            test!("snowchains/${service}/${contest}" => "snowchains/atcoder/arc100");
        }
        test!("~"         => dirs::home_dir().unwrap());
        test!("~/foo/bar" => dirs::home_dir().unwrap().join("foo/bar"));
        test!("~root"     => !);
        Ok(())
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
