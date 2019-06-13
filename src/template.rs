use crate::command::{
    CompilationCommand, HookCommand, HookCommands, JudgingCommand, TranspilationCommand,
};
use crate::config;
use crate::errors::{ExpandTemplateErrorKind, ExpandTemplateResult, StdError};
use crate::fs::TempFile;
use crate::path::{AbsPath, AbsPathBuf};
use crate::service::ServiceKind;
use crate::testsuite::SuiteFileExtension;
use crate::util::collections::SingleKeyValue;
use crate::util::combine::ParseFieldError;
use crate::util::str::CaseConversion;

use snowchains_proc_macros::{ArgEnum, DeserializeAsString, SerializeAsString};

use failure::{Fail, ResultExt as _};
use heck::{CamelCase as _, KebabCase as _, MixedCase as _, SnakeCase as _};
use indexmap::IndexMap;
use maplit::hashmap;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};

use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::hash::Hash;
use std::str::FromStr;
use std::sync::Arc;
use std::{env, fmt, iter};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug, Serialize, Deserialize)]
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
    T::Inner: FromStr<Err = ParseFieldError<String>>,
{
    type Err = ParseFieldError<String>;

    fn from_str(s: &str) -> std::result::Result<Self, ParseFieldError<String>> {
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
    /// `bash` is in the `$PATH` by default on almost every platform including Windows.
    pub(crate) fn bash() -> Self {
        TemplateBuilder(CommandTemplateInner::Args(vec![
            Tokens(vec![Token::Plain("bash".to_owned())]),
            Tokens(vec![Token::Plain("-c".to_owned())]),
            Tokens(vec![]),
        ]))
    }
}

#[derive(Debug)]
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

    pub(crate) fn envs(mut self, envs: HashMap<String, String>) -> Self {
        self.envs.extend(envs);
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
            mode,
        } = &self.requirements;
        let envs = setup_env_vars(problem, &self.envs, *service, contest, *mode);
        let vars = {
            let mut vars = hashmap!(
                "service" => OsString::from(service.to_string()),
                "contest" => OsString::from(contest),
            );
            vars.extend(mode.map(|m| ("mode", m.to_string().into())));
            vars.extend(problem.map(|p| ("problem", p.into())));
            vars.extend(self.extension.map(|e| ("extension", e.to_string().into())));
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
            snowchains_result,
        } = &self.requirements;
        let (mut vars, envs) = {
            let vars = hashmap!("result" => OsString::from(&**snowchains_result));
            let mut envs = self
                .envs
                .iter()
                .map(|(k, v)| (k.clone(), OsString::from(v.clone())))
                .collect::<HashMap<_, _>>();
            envs.insert(
                "SNOWCHAINS_RESULT".to_owned(),
                OsString::from(&**snowchains_result),
            );
            (vars, envs)
        };
        self.inner
            .iter()
            .map(|inner| {
                let (mut args, mut temp_file) = (vec![], None);
                match inner {
                    CommandTemplateInner::Args(ss) => {
                        for s in ss {
                            args.push(s.expand_as_os_string(&vars, &envs)?);
                        }
                    }
                    CommandTemplateInner::Shell(SingleKeyValue { key, value }) => {
                        let shell = shell
                            .get(key)
                            .ok_or_else(|| ExpandTemplateErrorKind::NoSuchShell(key.to_owned()))?;
                        match shell {
                            config::Shell::Args(sh_args) => {
                                vars.entry("command")
                                    .or_insert_with(|| value.clone().into());
                                for TemplateBuilder(s) in sh_args {
                                    args.push(s.expand_as_os_string(&vars, &envs)?);
                                }
                            }
                            config::Shell::File { runner, extension } => {
                                let ext = extension.0.expand_as_os_string(&vars, &envs)?;
                                let temp = TempFile::create(&ext, value)
                                    .with_context(|_| ExpandTemplateErrorKind::CreateTempFile)?;
                                args.push(runner.0.expand_as_os_string(&vars, &envs)?);
                                args.push(temp.path().into());
                                temp_file = Some(temp);
                            }
                        }
                    }
                }
                HookCommand::try_new(args, base_dir.clone(), envs.clone(), temp_file)
                    .map_err(|(s, e)| e.context(ExpandTemplateErrorKind::Which(s)).into())
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
            mode,
            shell,
            working_dir: TemplateBuilder(working_dir),
            src: TemplateBuilder(src),
            transpiled,
        } = &self.requirements;

        let mut vars = setup_template_vars(Some(problem), *service, contest, *mode);
        let mut envs = setup_env_vars(Some(problem), &self.envs, *service, contest, Some(*mode));

        let wd = working_dir.expand_as_path(base_dir, &vars, &envs)?;
        let src = src.expand_as_path(base_dir, &vars, &envs)?;
        vars.insert("src", src.clone().into());
        envs.insert("SNOWCHAINS_SRC".into(), src.clone().into());
        let transpiled = transpiled
            .as_ref()
            .map(|TemplateBuilder(transpiled)| transpiled.expand_as_path(base_dir, &vars, &envs))
            .transpose()?;
        if let Some(transpiled) = &transpiled {
            vars.insert("transpiled", transpiled.clone().into());
            envs.insert("SNOWCHAINS_TRANSPILED".into(), transpiled.clone().into());
        }

        let (mut args, mut temp_file) = (vec![], None);
        match &self.inner {
            CommandTemplateInner::Args(ss) => {
                for s in ss {
                    args.push(s.expand_as_os_string(&vars, &envs)?);
                }
            }
            CommandTemplateInner::Shell(SingleKeyValue { key, value }) => {
                let shell = shell
                    .get(key)
                    .ok_or_else(|| ExpandTemplateErrorKind::NoSuchShell(key.to_owned()))?;
                match shell {
                    config::Shell::Args(sh_args) => {
                        vars.entry("command")
                            .or_insert_with(|| value.clone().into());
                        for TemplateBuilder(s) in sh_args {
                            args.push(s.expand_as_os_string(&vars, &envs)?);
                        }
                    }
                    config::Shell::File { runner, extension } => {
                        let ext = extension.0.expand_as_os_string(&vars, &envs)?;
                        let temp = TempFile::create(&ext, value)
                            .with_context(|_| ExpandTemplateErrorKind::CreateTempFile)?;
                        args.push(runner.0.expand_as_os_string(&vars, &envs)?);
                        args.push(temp.path().into());
                        temp_file = Some(temp);
                    }
                }
            }
        }
        TranspilationCommand::try_new(args, wd, src, transpiled, envs, temp_file)
            .map_err(|(s, e)| e.context(ExpandTemplateErrorKind::Which(s)).into())
    }
}

impl Template<CompilationCommand> {
    pub(crate) fn expand(&self, problem: &str) -> ExpandTemplateResult<CompilationCommand> {
        let CompilationCommandRequirements {
            base_dir,
            service,
            contest,
            mode,
            shell,
            working_dir: TemplateBuilder(working_dir),
            src: TemplateBuilder(src),
            transpiled,
            bin,
        } = &self.requirements;

        let mut vars = setup_template_vars(Some(problem), *service, contest, *mode);
        let mut envs = setup_env_vars(Some(problem), &self.envs, *service, contest, Some(*mode));

        let wd = working_dir.expand_as_path(base_dir, &vars, &envs)?;
        let src = src.expand_as_path(base_dir, &vars, &envs)?;
        vars.insert("src", src.clone().into());
        envs.insert("SNOWCHAINS_SRC".into(), src.clone().into());
        let transpiled = transpiled
            .as_ref()
            .map(|TemplateBuilder(transpiled)| transpiled.expand_as_path(base_dir, &vars, &envs))
            .transpose()?;
        if let Some(transpiled) = &transpiled {
            vars.insert("transpiled", transpiled.clone().into());
            envs.insert("SNOWCHAINS_TRANSPILED".into(), transpiled.clone().into());
        }

        let bin = bin
            .as_ref()
            .map(|TemplateBuilder(bin)| bin.expand_as_path(base_dir, &vars, &envs))
            .transpose()?;
        if let Some(bin) = &bin {
            vars.insert("bin", bin.clone().into());
            envs.insert("SNOWCHAINS_BIN".into(), bin.clone().into());
        }

        let (mut args, mut temp_file) = (vec![], None);
        match &self.inner {
            CommandTemplateInner::Args(ss) => {
                for s in ss {
                    args.push(s.expand_as_os_string(&vars, &envs)?);
                }
            }
            CommandTemplateInner::Shell(SingleKeyValue { key, value }) => {
                let shell = shell
                    .get(key)
                    .ok_or_else(|| ExpandTemplateErrorKind::NoSuchShell(key.to_owned()))?;
                match shell {
                    config::Shell::Args(sh_args) => {
                        vars.entry("command")
                            .or_insert_with(|| value.clone().into());
                        for TemplateBuilder(s) in sh_args {
                            args.push(s.expand_as_os_string(&vars, &envs)?);
                        }
                    }
                    config::Shell::File { runner, extension } => {
                        let ext = extension.0.expand_as_os_string(&vars, &envs)?;
                        let temp = TempFile::create(&ext, value)
                            .with_context(|_| ExpandTemplateErrorKind::CreateTempFile)?;
                        args.push(runner.0.expand_as_os_string(&vars, &envs)?);
                        args.push(temp.path().into());
                        temp_file = Some(temp);
                    }
                }
            }
        }
        CompilationCommand::try_new(args, wd, src, bin, envs, temp_file)
            .map_err(|(s, e)| e.context(ExpandTemplateErrorKind::Which(s)).into())
    }
}

impl Template<JudgingCommand> {
    pub(crate) fn expand(&self, problem: &str) -> ExpandTemplateResult<JudgingCommand> {
        let JudgingCommandRequirements {
            base_dir,
            service,
            contest,
            mode,
            shell,
            working_dir: TemplateBuilder(working_dir),
            src: TemplateBuilder(src),
            transpiled,
            bin,
            crlf_to_lf,
        } = &self.requirements;

        let mut vars = setup_template_vars(Some(problem), *service, contest, *mode);
        let mut envs = setup_env_vars(Some(problem), &self.envs, *service, contest, Some(*mode));

        let wd = working_dir.expand_as_path(base_dir, &vars, &envs)?;
        let src = src.expand_as_path(base_dir, &vars, &envs)?;
        vars.insert("src", src.clone().into());
        envs.insert("SNOWCHAINS_SRC".into(), src.clone().into());
        let transpiled = transpiled
            .as_ref()
            .map(|TemplateBuilder(transpiled)| transpiled.expand_as_path(base_dir, &vars, &envs))
            .transpose()?;
        if let Some(transpiled) = &transpiled {
            vars.insert("transpiled", transpiled.clone().into());
            envs.insert("SNOWCHAINS_TRANSPILED".into(), transpiled.clone().into());
        }
        let bin = bin
            .as_ref()
            .map(|TemplateBuilder(bin)| bin.expand_as_path(base_dir, &vars, &envs))
            .transpose()?;
        if let Some(bin) = &bin {
            vars.insert("bin", bin.clone().into());
            envs.insert("SNOWCHAINS_BIN".into(), bin.clone().into());
        }

        let (mut args, mut temp_file) = (vec![], None);
        match &self.inner {
            CommandTemplateInner::Args(ss) => {
                for s in ss {
                    args.push(s.expand_as_os_string(&vars, &envs)?);
                }
            }
            CommandTemplateInner::Shell(SingleKeyValue { key, value }) => {
                let shell = shell
                    .get(key)
                    .ok_or_else(|| ExpandTemplateErrorKind::NoSuchShell(key.to_owned()))?;
                match shell {
                    config::Shell::Args(sh_args) => {
                        vars.entry("command")
                            .or_insert_with(|| value.clone().into());
                        for TemplateBuilder(s) in sh_args {
                            args.push(s.expand_as_os_string(&vars, &envs)?);
                        }
                    }
                    config::Shell::File { runner, extension } => {
                        let ext = extension.0.expand_as_os_string(&vars, &envs)?;
                        let temp = TempFile::create(&ext, value)
                            .with_context(|_| ExpandTemplateErrorKind::CreateTempFile)?;
                        args.push(runner.0.expand_as_os_string(&vars, &envs)?);
                        args.push(temp.path().into());
                        temp_file = Some(temp);
                    }
                }
            }
        }

        JudgingCommand::try_new(args, wd, *crlf_to_lf, envs, temp_file)
            .map_err(|(s, e)| e.context(ExpandTemplateErrorKind::Which(s)).into())
    }
}

fn setup_template_vars(
    problem: Option<&str>,
    service: ServiceKind,
    contest: &str,
    mode: config::Mode,
) -> HashMap<&'static str, OsString> {
    let mut ret = hashmap!(
        "service" => OsString::from(service.to_string()),
        "contest" => OsString::from(contest),
        "mode" => OsString::from(mode.to_string()),
    );
    ret.extend(problem.map(|p| ("problem", p.into())));
    ret
}

fn setup_env_vars(
    problem: Option<&str>,
    utf8_envs: &HashMap<String, String>,
    service: ServiceKind,
    contest: &str,
    mode: Option<config::Mode>,
) -> HashMap<String, OsString> {
    let mut ret = problem
        .map::<HashMap<String, OsString>, _>(|problem| {
            hashmap!(
                "SNOWCHAINS_PROBLEM".into()             => problem.into(),
                "SNOWCHAINS_PROBLEM_LOWER_CASE".into()  => problem.to_lowercase().into(),
                "SNOWCHAINS_PROBLEM_UPPER_CASE".into()  => problem.to_uppercase().into(),
                "SNOWCHAINS_PROBLEM_KEBAB_CASE".into()  => problem.to_kebab_case().into(),
                "SNOWCHAINS_PROBLEM_SNAKE_CASE".into()  => problem.to_snake_case().into(),
                "SNOWCHAINS_PROBLEM_MIXED_CASE".into()  => problem.to_mixed_case().into(),
                "SNOWCHAINS_PROBLEM_PASCAL_CASE".into() => problem.to_camel_case().into(),
                "SNOWCHAINS_SERVICE".into()             => service.to_string().into(),
                "SNOWCHAINS_CONTEST".into()             => contest.into(),
                "SNOWCHAINS_CONTEST_LOWER_CASE".into()  => contest.to_lowercase().into(),
                "SNOWCHAINS_CONTEST_UPPER_CASE".into()  => contest.to_uppercase().into(),
                "SNOWCHAINS_CONTEST_KEBAB_CASE".into()  => contest.to_kebab_case().into(),
                "SNOWCHAINS_CONTEST_SNAKE_CASE".into()  => contest.to_snake_case().into(),
                "SNOWCHAINS_CONTEST_MIXED_CASE".into()  => contest.to_mixed_case().into(),
                "SNOWCHAINS_CONTEST_PASCAL_CASE".into() => contest.to_camel_case().into(),
            )
        })
        .unwrap_or_default();
    ret.extend(mode.map(|m| ("SNOWCHAINS_MODE".into(), m.to_string().into())));
    for (name, value) in utf8_envs {
        ret.insert(name.as_str().into(), value.as_str().into());
    }
    ret
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

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum CommandTemplateInner {
    Args(Vec<Tokens>),
    Shell(SingleKeyValue<String, String>),
}

#[derive(Debug, Clone)]
pub(crate) struct AbsPathBufRequirements {
    pub(crate) base_dir: AbsPathBuf,
    pub(crate) service: ServiceKind,
    pub(crate) contest: String,
    pub(crate) mode: Option<config::Mode>,
}

#[derive(Debug, Clone)]
pub(crate) struct HookCommandsRequirements {
    pub(crate) base_dir: AbsPathBuf,
    pub(crate) shell: IndexMap<String, config::Shell>,
    pub(crate) snowchains_result: Arc<String>,
}

#[derive(Debug, Clone)]
pub(crate) struct TranspilationCommandRequirements {
    pub(crate) base_dir: AbsPathBuf,
    pub(crate) service: ServiceKind,
    pub(crate) contest: String,
    pub(crate) mode: config::Mode,
    pub(crate) shell: IndexMap<String, config::Shell>,
    pub(crate) working_dir: TemplateBuilder<AbsPathBuf>,
    pub(crate) src: TemplateBuilder<AbsPathBuf>,
    pub(crate) transpiled: Option<TemplateBuilder<AbsPathBuf>>,
}

#[derive(Debug, Clone)]
pub(crate) struct CompilationCommandRequirements {
    pub(crate) base_dir: AbsPathBuf,
    pub(crate) service: ServiceKind,
    pub(crate) contest: String,
    pub(crate) mode: config::Mode,
    pub(crate) shell: IndexMap<String, config::Shell>,
    pub(crate) working_dir: TemplateBuilder<AbsPathBuf>,
    pub(crate) src: TemplateBuilder<AbsPathBuf>,
    pub(crate) transpiled: Option<TemplateBuilder<AbsPathBuf>>,
    pub(crate) bin: Option<TemplateBuilder<AbsPathBuf>>,
}

#[derive(Debug, Clone)]
pub(crate) struct JudgingCommandRequirements {
    pub(crate) base_dir: AbsPathBuf,
    pub(crate) service: ServiceKind,
    pub(crate) contest: String,
    pub(crate) mode: config::Mode,
    pub(crate) shell: IndexMap<String, config::Shell>,
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
    Expr(Expr),
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone)]
enum Expr {
    Var(String),
    NamespacedVar(String, String),
    App(String, Box<Self>),
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, Default, SerializeAsString, DeserializeAsString)]
pub struct Tokens(Vec<Token>);

impl FromStr for Tokens {
    type Err = ParseFieldError<String>;

    fn from_str(input: &str) -> std::result::Result<Self, ParseFieldError<String>> {
        use combine::char::{char, spaces, string};
        use combine::parser::choice::or;
        use combine::parser::function::parser;
        use combine::stream::state::{IndexPositioner, State};
        use combine::{choice, easy, eof, many, many1, satisfy, Parser};

        static GRAMMER: &str =
            r#"Template      ::= ( Plain | LeftCurly | RightCurly | DollarOrExpr )*
Plain         ::= [^${}]+
LeftCurly     ::= '{{'
RightCurly    ::= '}}'
DollarOrExpr  ::= '$' ( Dollar | '{' Expr '}' )
Dollar        ::= '$'
Expr          ::= Spaces ( Var | NamespacedVar | App ) Spaces
Var           ::= Identifier
NamespacedVar ::= Identifier ':' Identifier
App           ::= Identifier '(' Expr ')'
Identifier    ::= ( [a-zA-Z0-9] | '_' )+
Spaces        ::= ? White_Space character ?+
"#;

        fn parse_expr<'a>(
            input: &mut easy::Stream<State<&'a str, IndexPositioner>>,
        ) -> combine::ParseResult<Expr, easy::Stream<State<&'a str, IndexPositioner>>> {
            enum Right {
                Comma(String),
                Arg(Expr),
                None,
            }

            spaces()
                .with(identifier())
                .and(choice((
                    char(':')
                        .with(identifier())
                        .skip(spaces())
                        .map(Right::Comma),
                    char('(')
                        .with(parser(parse_expr))
                        .skip(char(')'))
                        .skip(spaces())
                        .map(Right::Arg),
                    spaces().map(|_| Right::None),
                )))
                .map(|(left, right)| match right {
                    Right::Comma(right) => Expr::NamespacedVar(left, right),
                    Right::Arg(right) => Expr::App(left, Box::new(right)),
                    Right::None => Expr::Var(left),
                })
                .parse_stream(input)
        }

        fn identifier<'a>(
        ) -> impl Parser<Input = easy::Stream<State<&'a str, IndexPositioner>>, Output = String>
        {
            many1(
                satisfy(|c| match c {
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
                    _ => false,
                })
                .expected("[a-zA-Z0-9] | '_'"),
            )
        }

        let plain =
            many1(satisfy(|c| !['$', '{', '}'].contains(&c)).expected("[^${}]")).map(Token::Plain);

        let left_curly = string("{{").map(|_| Token::Plain('{'.to_string()));

        let right_curly = string("}}").map(|_| Token::Plain('}'.to_string()));

        let dollar_or_expr = char('$').with(or(
            char('$').map(|_| Token::Plain("$".to_owned())),
            char('{')
                .with(parser(parse_expr))
                .skip(char('}'))
                .map(Token::Expr),
        ));

        many(choice((plain, left_curly, right_curly, dollar_or_expr)))
            .skip(eof())
            .easy_parse(State::with_positioner(input, IndexPositioner::new()))
            .map(|(tokens, _)| Tokens(tokens))
            .map_err(|e| ParseFieldError::new(input, e, GRAMMER).into_owned())
    }
}

impl fmt::Display for Tokens {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn fmt_expr(expr: &Expr, f: &mut fmt::Formatter) -> fmt::Result {
            match expr {
                Expr::Var(x) => write!(f, "{}", x),
                Expr::NamespacedVar(s, x) => write!(f, "{}:{}", s, x),
                Expr::App(fun, e) => {
                    write!(f, "{}(", fun)?;
                    fmt_expr(e, f)?;
                    write!(f, ")")
                }
            }
        }

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
                Token::Expr(e) => {
                    write!(f, "${{")?;
                    fmt_expr(e, f)?;
                    write!(f, "}}")?;
                }
            }
        }
        Ok(())
    }
}

impl fmt::Debug for Tokens {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn fmt_expr(expr: &Expr, f: &mut fmt::Formatter) -> fmt::Result {
            match expr {
                Expr::Var(x) => write!(f, "{}", x),
                Expr::NamespacedVar(s, x) => write!(f, "{}:{}", s, x),
                Expr::App(fun, e) => {
                    write!(f, "{}(", fun)?;
                    fmt_expr(e, f)?;
                    write!(f, ")")
                }
            }
        }

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
                Token::Expr(e) => {
                    write!(f, "${{")?;
                    fmt_expr(e, f)?;
                    write!(f, "}}")
                }
            }?
        }
        if n != 1 {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl Tokens {
    fn expand_as_os_string(
        &self,
        vars: &HashMap<&'static str, impl AsRef<OsStr>>,
        envs: &HashMap<impl Borrow<str> + Eq + Hash, impl AsRef<OsStr>>,
    ) -> ExpandTemplateResult<OsString> {
        fn eval_as_os_str<'a>(
            expr: &'a Expr,
            vars: &'a HashMap<&'static str, impl AsRef<OsStr>>,
            envs: &'a HashMap<impl Borrow<str> + Eq + Hash, impl AsRef<OsStr>>,
        ) -> ExpandTemplateResult<Cow<'a, OsStr>> {
            match expr {
                Expr::Var(name) => var_value(name, vars).map(Cow::Borrowed),
                Expr::NamespacedVar(namespace, name) => {
                    guard_namespace(namespace)?;
                    env_var_value(name, envs)
                }
                Expr::App(f, x) => {
                    let f = parse_fun(f)?;
                    let x = eval_as_str(x, vars, envs)?;
                    Ok(Cow::Owned(f.apply(&x).into()))
                }
            }
        }

        fn eval_as_str<'a>(
            expr: &'a Expr,
            vars: &'a HashMap<&'static str, impl AsRef<OsStr>>,
            envs: &'a HashMap<impl Borrow<str> + Eq + Hash, impl AsRef<OsStr>>,
        ) -> ExpandTemplateResult<Cow<'a, str>> {
            match expr {
                Expr::Var(name) => {
                    let value = var_value(name, vars)?;
                    let value = value.to_str().ok_or_else(|| {
                        ExpandTemplateErrorKind::NonUtf8Var(name.clone(), value.to_owned())
                    })?;
                    Ok(Cow::Borrowed(value))
                }
                Expr::NamespacedVar(namespace, name) => {
                    guard_namespace(namespace)?;
                    let value = match env_var_value(name, envs)? {
                        Cow::Owned(value) => Cow::Owned(value.into_string().map_err(|value| {
                            ExpandTemplateErrorKind::NonUtf8EnvVar(name.clone(), value)
                        })?),
                        Cow::Borrowed(value) => Cow::Borrowed(value.to_str().ok_or_else(|| {
                            ExpandTemplateErrorKind::NonUtf8EnvVar(name.clone(), value.to_owned())
                        })?),
                    };
                    Ok(value)
                }
                Expr::App(f, x) => {
                    let f = parse_fun(f)?;
                    let x = eval_as_str(x, vars, envs)?;
                    Ok(Cow::Owned(f.apply(&x)))
                }
            }
        }

        fn var_value<'a>(
            name: &str,
            vars: &'a HashMap<&'static str, impl AsRef<OsStr>>,
        ) -> ExpandTemplateResult<&'a OsStr> {
            vars.get(name)
                .ok_or_else(|| ExpandTemplateErrorKind::UndefinedVar(name.to_owned()).into())
                .map(AsRef::as_ref)
        }

        fn env_var_value<'a>(
            name: &str,
            envs: &'a HashMap<impl Borrow<str> + Eq + Hash, impl AsRef<OsStr>>,
        ) -> ExpandTemplateResult<Cow<'a, OsStr>> {
            envs.get(name)
                .map(|v| Cow::Borrowed(v.as_ref()))
                .or_else(|| env::var_os(name).map(Cow::Owned))
                .ok_or_else(|| ExpandTemplateErrorKind::EnvVarNotPresent(name.to_owned()).into())
        }

        fn guard_namespace(namespace: &str) -> ExpandTemplateResult<()> {
            match namespace {
                "env" => Ok(()),
                namespace => {
                    Err(ExpandTemplateErrorKind::UndefinedNamespace(namespace.to_owned()).into())
                }
            }
        }

        fn parse_fun(name: &str) -> ExpandTemplateResult<CaseConversion> {
            #[allow(clippy::enum_variant_names)]
            #[derive(ArgEnum)]
            #[arg_enum(case = "sensitive", rename_all = "snake_case")]
            enum Fun {
                LowerCase,
                UpperCase,
                SnakeCase,
                KebabCase,
                MixedCase,
                PascalCase,
            }

            let fun = name
                .parse::<Fun>()
                .map_err(failure::err_msg)
                .with_context(|_| ExpandTemplateErrorKind::UndefinedFun(name.to_owned()))?;

            Ok(match fun {
                Fun::LowerCase => CaseConversion::Lower,
                Fun::UpperCase => CaseConversion::Upper,
                Fun::SnakeCase => CaseConversion::Snake,
                Fun::KebabCase => CaseConversion::Kebab,
                Fun::MixedCase => CaseConversion::Mixed,
                Fun::PascalCase => CaseConversion::Pascal,
            })
        }

        let expand_as_os_string = || -> ExpandTemplateResult<_> {
            let mut r = OsString::new();
            for token in &self.0 {
                match token {
                    Token::Plain(s) => r.push(s),
                    Token::Expr(e) => r.push(eval_as_os_str(e, vars, envs)?),
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
            base_dir.join_expanding_user(&s).map_err(|e| {
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

#[cfg(test)]
mod tests {
    use super::{AbsPathBufRequirements, TemplateBuilder};

    use crate::command::{CompilationCommand, JudgingCommand};
    use crate::path::AbsPathBuf;
    use crate::service::ServiceKind;

    use difference::assert_diff;
    use failure::Fallible;
    use maplit::hashmap;
    use pretty_assertions::assert_eq;
    use serde::{Deserialize, Serialize};

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
  "b": "rs/${kebab_case(problem)}.rs",
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

        let s1 = serde_json::from_str::<S>(JSON)?;
        let re_serialized = serde_json::to_string_pretty(&s1)?;
        let s2 = serde_json::from_str::<S>(&re_serialized)?;
        assert_diff!(JSON, &re_serialized, "\n", 0);
        assert_eq!(s1, s2);
        Ok(())
    }

    #[test]
    fn it_expands_os_string_templates() -> Fallible<()> {
        fn process_input(input: &str) -> Fallible<OsString> {
            let template = input.parse::<TemplateBuilder<OsString>>()?;
            let vars = hashmap!("var" => "foo");
            let envs = hashmap!("ENVVAR" => "bar");
            template.build(()).expand(&vars, &envs).map_err(Into::into)
        }

        fn process_expected(expected: &str) -> &OsStr {
            OsStr::new(expected)
        }

        test!(""                          => "");
        test!("text"                      => "text");
        test!("${var}"                    => "foo");
        test!("${env:ENVVAR}"             => "bar");
        test!("${upper_case(var)}"        => "FOO");
        test!("${upper_case(env:ENVVAR)}" => "BAR");
        test!("$${{}}"                    => "${}");
        test!("{InvalidSpecifier}"        => !);
        test!("$NONEXISTING"              => !);
        test!("{var}"                     => !);
        test!("{env:ENVVAR}"              => !);
        test!("${-}"                      => !);
        test!("{"                         => !);
        test!("}"                         => !);
        test!("$"                         => !);
        Ok(())
    }

    #[test]
    fn it_expands_path_templates() -> Fallible<()> {
        fn process_input(input: &'static str) -> Fallible<AbsPathBuf> {
            let template = input.parse::<TemplateBuilder<AbsPathBuf>>()?;
            template
                .build(AbsPathBufRequirements {
                    base_dir: base_dir(),
                    service: ServiceKind::Atcoder,
                    contest: "arc100".to_owned(),
                    mode: None,
                })
                .expand(Some("ProblemName"))
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

        if cfg!(windows) {
            test!(r"C:\absolute" => r"C:\absolute");
        } else {
            test!("/absolute" => "/absolute");
        }
        test!(""                                  => "./");
        test!("."                                 => "./");
        test!("relative"                          => "./relative");
        test!("./relative"                        => "./relative");
        test!("cpp/${kebab_case(problem)}.cpp"    => "./cpp/problem-name.cpp");
        test!("${service}/${snake_case(contest)}" => "./atcoder/arc100");
        {
            fn process_input(input: &str) -> Fallible<AbsPathBuf> {
                let template = input.parse::<TemplateBuilder<AbsPathBuf>>()?;
                template
                    .build(AbsPathBufRequirements {
                        base_dir: base_dir(),
                        service: ServiceKind::Atcoder,
                        contest: "arc100".to_owned(),
                        mode: None,
                    })
                    .expand(None)
                    .map_err(Into::into)
            }
            test!("snowchains/${service}/${snake_case(contest)}" => "snowchains/atcoder/arc100");
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
