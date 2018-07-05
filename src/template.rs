use command::{CompilationCommand, JudgingCommand};
use errors::{TemplateExpandError, TemplateExpandErrorContext, TemplateExpandResult};
use path::{AbsPath, AbsPathBuf};

use combine::Parser;
use failure::ResultExt as _ResultExt;
use heck::{
    CamelCase as _CamelCase, KebabCase as _KebabCase, MixedCase as _MixedCase,
    ShoutySnakeCase as _ShoutySnakeCase, SnakeCase as _SnakeCase, TitleCase as _TitleCase,
};
use serde::{self, Deserialize, Deserializer, Serialize, Serializer};

use std::borrow::Borrow;
use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::OsString;
use std::hash::Hash;
use std::marker::PhantomData;
use std::path::PathBuf;
use std::str::FromStr;
use std::{self, env, fmt};

#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Serialize, Deserialize)]
pub(crate) struct StringTemplate(Template);

impl StringTemplate {
    #[cfg(test)]
    pub fn from_static_str(s: &'static str) -> Self {
        StringTemplate(Template::from_str(s).unwrap())
    }

    pub fn embed_strings<'a, K: 'a + Borrow<str> + Eq + Hash, V: 'a + Borrow<str> + Eq + Hash>(
        &self,
        strings: impl Into<Option<&'a HashMap<K, V>>>,
    ) -> Self {
        StringTemplate(self.0.embed_strings(strings))
    }

    pub fn expand(&self, problem: &str) -> TemplateExpandResult<String> {
        self.0.expand_as_string_or_panic(problem)
    }
}

pub(crate) trait BaseDirOption {}

#[cfg_attr(test, derive(PartialEq, Debug))]
#[derive(Clone, Copy)]
pub(crate) struct BaseDirSome<'a>(AbsPath<'a>);

impl<'a> BaseDirOption for BaseDirSome<'a> {}

#[cfg_attr(test, derive(PartialEq, Debug))]
#[derive(Clone, Copy)]
pub(crate) struct BaseDirNone;

impl BaseDirOption for BaseDirNone {}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub(crate) struct PathTemplate<B: BaseDirOption> {
    inner: Template,
    base_dir: B,
}

impl PathTemplate<BaseDirNone> {
    fn new(inner: Template) -> Self {
        Self {
            inner,
            base_dir: BaseDirNone,
        }
    }

    pub(crate) fn base_dir<'a>(&self, base_dir: AbsPath<'a>) -> PathTemplate<BaseDirSome<'a>> {
        PathTemplate {
            inner: self.inner.clone(),
            base_dir: BaseDirSome(base_dir),
        }
    }
}

impl<'a> PathTemplate<BaseDirSome<'a>> {
    pub fn embed_strings<'b, K: 'b + Borrow<str> + Eq + Hash, V: 'b + Borrow<str> + Eq + Hash>(
        &self,
        strings: impl Into<Option<&'b HashMap<K, V>>>,
    ) -> Self {
        PathTemplate {
            inner: self.inner.embed_strings(strings),
            base_dir: BaseDirSome(self.base_dir.0),
        }
    }

    pub fn expand(&self, problem: &str) -> TemplateExpandResult<AbsPathBuf> {
        self.inner.expand_as_path(&self.base_dir.0, problem)
    }

    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            base_dir: self.base_dir,
        }
    }
}

impl Default for PathTemplate<BaseDirNone> {
    fn default() -> Self {
        PathTemplate::new(Template(vec![Token::Text("".to_owned())]))
    }
}

impl Serialize for PathTemplate<BaseDirNone> {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        self.inner.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for PathTemplate<BaseDirNone> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        Template::deserialize(deserializer).map(Self::new)
    }
}

#[cfg_attr(test, derive(Debug, PartialEq))]
pub(crate) struct CommandTemplate<C> {
    inner: CommandTemplateInner,
    phantom: PhantomData<fn() -> C>,
}

impl<C> CommandTemplate<C> {
    fn new(inner: CommandTemplateInner) -> Self {
        Self {
            inner,
            phantom: PhantomData,
        }
    }

    pub fn embed_strings<'b, K: 'b + Borrow<str> + Eq + Hash, V: 'b + Borrow<str> + Eq + Hash>(
        &self,
        strings: impl Into<Option<&'b HashMap<K, V>>>,
    ) -> Self {
        use template::CommandTemplateInner::{Args, Shell};
        let strings = strings.into();
        let inner = match &self.inner {
            Shell(t) => Shell(t.embed_strings(strings)),
            Args(ts) => Args(ts.iter().map(|t| t.embed_strings(strings)).collect()),
        };
        Self::new(inner)
    }

    fn normalize(&self, shell: &[StringTemplate]) -> Vec<Template> {
        match &self.inner {
            CommandTemplateInner::Shell(t) => {
                let mut r = shell.iter().map(|arg| arg.0.clone()).collect::<Vec<_>>();
                r.push(t.clone());
                r
            }
            CommandTemplateInner::Args(ts) => ts.clone(),
        }
    }
}

impl CommandTemplate<CompilationCommand> {
    pub fn as_compilation<'a>(
        &self,
        shell: &[StringTemplate],
        wd: PathTemplate<BaseDirSome<'a>>,
        src: PathTemplate<BaseDirSome<'a>>,
        bin: PathTemplate<BaseDirSome<'a>>,
    ) -> CompilationTemplate<'a> {
        let inner = self
            .normalize(shell)
            .iter()
            .map(|t| t.embed_path_templates(&[("src", &src), ("bin", &bin)]))
            .collect();
        CompilationTemplate {
            inner,
            wd,
            src,
            bin,
        }
    }
}

impl CommandTemplate<JudgingCommand> {
    pub fn as_judge<'a, 'b>(
        &self,
        shell: &[StringTemplate],
        wd: PathTemplate<BaseDirSome<'a>>,
        src: &PathTemplate<BaseDirSome<'b>>,
        bin: Option<&PathTemplate<BaseDirSome<'b>>>,
    ) -> JudgeTemplate<'a> {
        let inner = self
            .normalize(shell)
            .iter()
            .map(|t| match bin {
                Some(bin) => t.embed_path_templates(&[("src", &src), ("bin", bin)]),
                None => t.embed_path_templates(&[("src", &src)]),
            })
            .collect();
        JudgeTemplate { inner, wd }
    }
}

impl<C> Serialize for CommandTemplate<C> {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        self.inner.serialize(serializer)
    }
}

impl<'de, C> Deserialize<'de> for CommandTemplate<C> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        CommandTemplateInner::deserialize(deserializer).map(Self::new)
    }
}

#[cfg_attr(test, derive(Debug, PartialEq))]
#[derive(Clone, Serialize, Deserialize)]
#[serde(untagged)]
enum CommandTemplateInner {
    Shell(Template),
    Args(Vec<Template>),
}

pub(crate) struct CompilationTemplate<'a> {
    inner: Vec<Template>,
    wd: PathTemplate<BaseDirSome<'a>>,
    src: PathTemplate<BaseDirSome<'a>>,
    bin: PathTemplate<BaseDirSome<'a>>,
}

impl<'a> CompilationTemplate<'a> {
    pub fn expand(&self, problem: &str) -> TemplateExpandResult<CompilationCommand> {
        let args = self
            .inner
            .iter()
            .map(|t| t.expand_as_os_string(problem))
            .collect::<TemplateExpandResult<Vec<_>>>()?;
        let wd = self.wd.expand(problem)?;
        let src = self.src.expand(problem)?;
        let bin = self.bin.expand(problem)?;
        Ok(CompilationCommand::new(&args, wd, src, bin))
    }
}

pub(crate) struct JudgeTemplate<'a> {
    inner: Vec<Template>,
    wd: PathTemplate<BaseDirSome<'a>>,
}

impl<'a> JudgeTemplate<'a> {
    pub fn embed_strings<'b, K: 'b + Borrow<str> + Eq + Hash, V: 'b + Borrow<str> + Eq + Hash>(
        &self,
        strings: impl Into<Option<&'b HashMap<K, V>>>,
    ) -> Self {
        let strings = strings.into();
        Self {
            inner: self
                .inner
                .iter()
                .map(|t| t.embed_strings(strings))
                .collect(),
            wd: self.wd.clone(),
        }
    }

    pub fn expand(&self, problem: &str) -> TemplateExpandResult<JudgingCommand> {
        let args = self
            .inner
            .iter()
            .map(|t| t.expand_as_os_string(problem))
            .collect::<TemplateExpandResult<Vec<_>>>()?;
        let wd = self.wd.expand(problem)?;
        Ok(JudgingCommand::new(&args, wd))
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone)]
struct Template(Vec<Token>);

impl Template {
    fn embed_strings<'a, K: 'a + Borrow<str> + Eq + Hash, V: 'a + Borrow<str> + Eq + Hash>(
        &self,
        strings: impl Into<Option<&'a HashMap<K, V>>>,
    ) -> Self {
        match strings.into() {
            None => self.clone(),
            Some(strings) => {
                let mut new = vec![];
                for token in &self.0 {
                    if let Token::Var(varname) = token {
                        if let Some(value) = strings.get(varname) {
                            new.push(Token::Text(value.borrow().to_owned()));
                            continue;
                        }
                    }
                    new.push(token.clone());
                }
                Template(new)
            }
        }
    }

    fn embed_path_templates(
        &self,
        templates: &[(&'static str, &PathTemplate<BaseDirSome>)],
    ) -> Self {
        let mut new = vec![];
        'l: for token in &self.0 {
            if let Token::Var(varname) = token {
                for &(name, template) in templates {
                    if name == varname {
                        new.push(Token::ExternPath(
                            template.inner.clone(),
                            template.base_dir.0.to_owned(),
                            name,
                        ));
                        continue 'l;
                    }
                }
            }
            new.push(token.clone());
        }
        Template(new)
    }

    fn expand_as_os_string(&self, problem: &str) -> TemplateExpandResult<OsString> {
        self.expand_with_context(problem, "a non UTF-8 string", || {
            let mut r = OsString::new();
            for token in &self.0 {
                match token.expand(problem, true)? {
                    Plain::Str(s) => r.push(s.as_ref()),
                    Plain::OsStr(s) => r.push(s),
                    Plain::Path(p) => r.push(p.as_ref()),
                }
            }
            Ok(r)
        })
    }

    fn expand_as_string_or_panic(&self, problem: &str) -> TemplateExpandResult<String> {
        self.expand_with_context(problem, "a UTF-8 string", || {
            let mut r = "".to_owned();
            for token in &self.0 {
                match token.expand(problem, false)? {
                    Plain::Str(s) => r += s.as_ref(),
                    _ => unreachable!(),
                }
            }
            Ok(r)
        })
    }

    fn expand_as_path(&self, base: AbsPath, problem: &str) -> TemplateExpandResult<AbsPathBuf> {
        self.expand_with_context(problem, "a path", || {
            let path = PathBuf::from(self.expand_as_os_string(problem)?);
            base.join_expanding_tilde(path).map_err(Into::into)
        })
    }

    fn expand_with_context<T>(
        &self,
        problem: &str,
        ty: &'static str,
        f: impl FnOnce() -> TemplateExpandResult<T>,
    ) -> TemplateExpandResult<T> {
        f().with_context(|_| TemplateExpandErrorContext::new(&self, problem, ty))
            .map_err(Into::into)
    }
}

impl<'a> fmt::Debug for Template {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, t) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, " ++ ")?;
            }
            match t {
                Token::ExternPath(t, b, s) => write!(f, "${}({}, {:?})", s, b.display(), t),
                Token::Text(s) => write!(f, "{:?}", s),
                Token::Var(s) => write!(f, "${}", s),
                Token::Problem(s) => write!(f, "{{{}}}", s),
            }?
        }
        Ok(())
    }
}

impl<'a> fmt::Display for Template {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for t in &self.0 {
            match t {
                Token::ExternPath(..) => unreachable!(),
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

impl FromStr for Template {
    type Err = TemplateParseError;

    fn from_str(input: &str) -> TemplateParseResult<Self> {
        use combine::char::{alpha_num, char, letter, spaces, string};
        use combine::{choice, eof, many, many1, satisfy, try};
        let plain = many1(satisfy(|c| !['$', '{', '}'].contains(&c))).map(Token::Text);
        let escaped =
            |f: &'static str, t: &'static str| string(f).map(move |_| Token::Text(t.to_owned()));
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
            try(escaped("$$", "$")),
            try(escaped("{{", "{")),
            try(escaped("}}", "}")),
            problem,
            var,
        ))).skip(eof())
            .parse(input)
            .map(|(ts, _)| Template(ts))
            .map_err(|_| TemplateParseError {
                input: input.to_owned(),
            })
    }
}

impl Serialize for Template {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        serializer.collect_str(&self)
    }
}

impl<'de> Deserialize<'de> for Template {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        Self::from_str(&s).map_err(serde::de::Error::custom)
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone)]
enum Token {
    ExternPath(Template, AbsPathBuf, &'static str),
    Text(String),
    Var(String),
    Problem(String),
}

impl Token {
    fn expand<'a>(
        &'a self,
        problem: &'a str,
        allow_non_utf8_envvar: bool,
    ) -> TemplateExpandResult<Plain<'a>> {
        match self {
            Token::ExternPath(t, b, _) => t.expand_as_path(&b, problem).map(Plain::Path),
            Token::Text(s) => Ok(Plain::Str(Cow::Borrowed(s))),
            Token::Var(k) if allow_non_utf8_envvar => Plain::from_env_var_os(k),
            Token::Var(k) => Plain::from_env_var(k),
            Token::Problem(s) => Plain::from_problem(problem, s),
        }
    }
}

enum Plain<'a> {
    Str(Cow<'a, str>),
    OsStr(OsString),
    Path(AbsPathBuf),
}

impl<'a> Plain<'a> {
    fn from_problem(problem: &'a str, specifier: &str) -> TemplateExpandResult<Self> {
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
            s => Err(TemplateExpandError::UnknownSpecifier(s.to_owned())),
        }.map(Plain::Str)
    }

    fn from_env_var(name: &str) -> TemplateExpandResult<Self> {
        env::var(name)
            .map(|v| Plain::Str(Cow::Owned(v)))
            .map_err(|e| {
                use errors::TemplateExpandError::{EnvVarNotPresent, EnvVarNotUnicode};
                use std::env::VarError::{NotPresent, NotUnicode};
                match e {
                    NotPresent => EnvVarNotPresent(name.to_owned()),
                    NotUnicode(v) => EnvVarNotUnicode(name.to_owned(), v),
                }
            })
    }

    fn from_env_var_os(name: &str) -> TemplateExpandResult<Self> {
        env::var_os(name)
            .map(Plain::OsStr)
            .ok_or_else(|| TemplateExpandError::EnvVarNotPresent(name.to_owned()))
    }
}

type TemplateParseResult<T> = std::result::Result<T, TemplateParseError>;

#[derive(Debug, PartialEq)]
struct TemplateParseError {
    input: String,
}

impl fmt::Display for TemplateParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Failed to parse {:?}", self.input)
    }
}

#[cfg(test)]
mod tests {
    use command::CompilationCommand;
    use path::AbsPathBuf;
    use template::{
        BaseDirNone, CommandTemplate, CommandTemplateInner, PathTemplate, StringTemplate, Template,
    };

    use serde_json;

    use std::ffi::OsStr;
    use std::path::Path;
    use std::{env, panic};

    macro_rules! test {
        ($input:expr => !) => {
            panic::catch_unwind(move || process_input($input)).unwrap_err()
        };
        ($input:expr => $expected:expr) => {
            assert_eq!(process_expected($expected), process_input($input))
        };
    }

    #[test]
    fn it_serializes_and_deserializes_templates() {
        #[derive(Debug, PartialEq, Serialize, Deserialize)]
        struct S {
            a: StringTemplate,
            b: PathTemplate<BaseDirNone>,
            c: CommandTemplate<CompilationCommand>,
            d: CommandTemplate<CompilationCommand>,
        }

        fn template(input: &str) -> Template {
            input.parse().unwrap()
        }

        let s1 = S {
            a: StringTemplate(template("{}/{kebab}/$VAR$VAR$${{}}")),
            b: PathTemplate::new(template("{}{ snake }")),
            c: CommandTemplate::new(CommandTemplateInner::Shell(template("yes > /dev/null"))),
            d: CommandTemplate::new(CommandTemplateInner::Args(vec![
                template("sh"),
                template("-c"),
                template("yes > /dev/null"),
            ])),
        };
        let s2 = serde_json::to_string(&s1).unwrap();
        let s2 = serde_json::from_str::<S>(&s2).unwrap();
        assert_eq!(s1, s2);
    }

    #[test]
    fn it_expands_a_string_template() {
        fn process_input(input: &str) -> String {
            StringTemplate(input.parse().unwrap())
                .expand("problem name")
                .unwrap()
        }

        fn process_expected(expected: &str) -> &str {
            expected
        }

        env::set_var("ENVVAR", "<value of ENVVAR>");
        env::set_var("_ENVVAR", "<value of _ENVVAR>");
        env::set_var("__", "<value of __>");
        env::set_var("---", "");
        env::remove_var("NONEXISTING");
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
    fn it_expands_a_path_template() {
        fn process_input(input: &str) -> AbsPathBuf {
            PathTemplate::new(input.parse().unwrap())
                .base_dir(&base_dir())
                .expand("problem-name")
                .unwrap()
        }

        fn process_expected<S: AsRef<OsStr>>(expected: S) -> AbsPathBuf {
            let expected = Path::new(expected.as_ref());
            if expected.is_absolute() {
                AbsPathBuf::new_or_panic(expected)
            } else {
                base_dir().join(expected)
            }
        }

        env::set_var("ENVVAR", "<value of ENVVAR>");
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
                PathTemplate::new(input.parse().unwrap())
                    .base_dir(&base_dir())
                    .embed_strings(&hashmap!["service" => "Service", "contest" => "Contest"])
                    .expand("")
                    .unwrap()
            }
            test!("snowchains/$service/$contest" => "snowchains/Service/Contest");
        }
        test!("~"         => env::home_dir().unwrap());
        test!("~/foo/bar" => env::home_dir().unwrap().join("foo/bar"));
        test!("~root"     => !);
    }

    fn base_dir() -> AbsPathBuf {
        AbsPathBuf::new_or_panic(if cfg!(windows) {
            r"C:\basedir"
        } else {
            "/basedir"
        })
    }
}
