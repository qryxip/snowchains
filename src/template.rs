use errors::{FileIoResult, TemplateError, TemplateErrorKind, TemplateResult};
use util;

use heck::{CamelCase as _CameCase, KebabCase as _KebabCase, MixedCase as _MixedCase,
           ShoutySnakeCase as _ShoutySnakeCase, SnakeCase as _SnakeCase, TitleCase as _TitleCase};

use std::{self, env};
use std::borrow::Borrow;
use std::borrow::Cow;
use std::collections::HashMap;
use std::hash::Hash;
use std::iter::FromIterator;
use std::path::{Path, PathBuf};

pub struct PathTemplate<'a> {
    template: Template,
    base_dir: &'a Path,
}

impl<'a> PathTemplate<'a> {
    pub fn expand(&self, target: &str) -> FileIoResult<PathBuf> {
        let formatted = self.template.format(target);
        util::expand_path(&formatted, self.base_dir)
    }
}

pub(crate) struct Template(Vec<TemplateToken>);

impl Template {
    pub(crate) fn format(&self, target: &str) -> String {
        self.0.iter().fold("".to_owned(), |mut r, t| {
            match *t {
                TemplateToken::Plain(ref s) => r += s,
                TemplateToken::Target => r += target,
                TemplateToken::TargetLower => r += &target.to_lowercase(),
                TemplateToken::TargetUpper => r += &target.to_uppercase(),
                TemplateToken::TargetKebab => r += &target.to_kebab_case(),
                TemplateToken::TargetLowerSnake => r += &target.to_snake_case(),
                TemplateToken::TargetUpperSnake => r += &target.to_shouty_snake_case(),
                TemplateToken::TargetLowerCamel => r += &target.to_mixed_case(),
                TemplateToken::TargetUpperCamel => r += &target.to_camel_case(),
                TemplateToken::TargetTitle => r += &target.to_title_case(),
            }
            r
        })
    }

    pub(crate) fn with_base_dir(self, base_dir: &Path) -> PathTemplate {
        PathTemplate {
            template: self,
            base_dir,
        }
    }
}

enum TemplateToken {
    Plain(String),
    Target,
    TargetLower,
    TargetUpper,
    TargetKebab,
    TargetLowerSnake,
    TargetUpperSnake,
    TargetLowerCamel,
    TargetUpperCamel,
    TargetTitle,
}

#[derive(Serialize, Deserialize)]
pub(crate) struct TemplateString(Cow<'static, str>);

impl TemplateString {
    pub(crate) fn new<S: Into<Cow<'static, str>>>(s: S) -> Self {
        TemplateString(s.into())
    }

    pub(crate) fn resolve_as_path<
        'a,
        K: 'a + Borrow<str> + Eq + Hash,
        V: 'a + Borrow<str> + Eq + Hash,
        M: Into<Option<&'a HashMap<K, V>>>,
    >(
        &self,
        base_dir: &Path,
        target: &str,
        variables: M,
    ) -> TemplateResult<PathBuf> {
        let resolved = self.embed_vars(variables)?
            .with_base_dir(base_dir)
            .expand(target)?;
        Ok(resolved)
    }

    pub(crate) fn format<
        'a,
        K: 'a + Borrow<str> + Eq + Hash,
        V: 'a + Borrow<str> + Eq + Hash,
        M: Into<Option<&'a HashMap<K, V>>>,
    >(
        &self,
        target: &str,
        variables: M,
    ) -> TemplateResult<String> {
        let transformed = self.embed_vars(variables)?;
        Ok(transformed.format(target))
    }

    pub(crate) fn as_path_template<
        'a,
        'b,
        K: 'b + Borrow<str> + Eq + Hash,
        V: 'b + Borrow<str> + Eq + Hash,
        M: Into<Option<&'b HashMap<K, V>>>,
    >(
        &self,
        base_dir: &'a Path,
        variables: M,
    ) -> TemplateResult<PathTemplate<'a>> {
        let template = self.embed_vars(variables)?;
        Ok(PathTemplate { template, base_dir })
    }

    pub(crate) fn embed_vars<
        'a,
        K: 'a + Borrow<str> + Eq + Hash,
        V: 'a + Borrow<str> + Eq + Hash,
        M: Into<Option<&'a HashMap<K, V>>>,
    >(
        &self,
        variables: M,
    ) -> TemplateResult<Template> {
        enum Token {
            Text(String),
            Var(String),
            Target(String),
        }

        impl Token {
            fn transform<K_: Borrow<str> + Eq + Hash, V_: Borrow<str> + Eq + Hash>(
                self,
                whole: &str,
                variables: Option<&HashMap<K_, V_>>,
            ) -> TemplateResult<TemplateToken> {
                match self {
                    Token::Text(s) => Ok(TemplateToken::Plain(s)),
                    Token::Var(s) => {
                        let env_var = |s: String| -> TemplateResult<TemplateToken> {
                            match env::var(s.as_str()) {
                                Ok(v) => Ok(TemplateToken::Plain(v)),
                                Err(env::VarError::NotPresent) => {
                                    let vars = variables
                                        .map(|m| {
                                            m.keys()
                                                .map(|v| v.borrow().to_owned())
                                                .collect::<Vec<_>>()
                                        })
                                        .unwrap_or_default();
                                    bail!(TemplateErrorKind::NoSuchVariable(
                                        whole.to_owned(),
                                        s,
                                        vars
                                    ))
                                }
                                Err(env::VarError::NotUnicode(_)) => {
                                    bail!(TemplateErrorKind::NonUtf8EnvVar(s.clone()))
                                }
                            }
                        };
                        match variables {
                            None => env_var(s),
                            Some(variables) => match variables.get(s.as_str()) {
                                None => env_var(s),
                                Some(v) => Ok(TemplateToken::Plain(v.borrow().to_owned())),
                            },
                        }
                    }
                    Token::Target(ref s) => match s.trim().to_lowercase() {
                        ref s if s.is_empty() => Ok(TemplateToken::Target),
                        ref s if s == "lower" => Ok(TemplateToken::TargetLower),
                        ref s if s == "upper" => Ok(TemplateToken::TargetUpper),
                        ref s if s == "kebab" => Ok(TemplateToken::TargetKebab),
                        ref s if s == "snake" => Ok(TemplateToken::TargetLowerSnake),
                        ref s if s == "screaming" => Ok(TemplateToken::TargetUpperSnake),
                        ref s if s == "mixed" => Ok(TemplateToken::TargetLowerCamel),
                        ref s if s == "pascal" => Ok(TemplateToken::TargetUpperCamel),
                        ref s if s == "title" => Ok(TemplateToken::TargetTitle),
                        s => {
                            static EXPECTED_KWS: &[&str] = &[
                                "lower",
                                "upper",
                                "kebab",
                                "snake",
                                "screaming",
                                "mixed",
                                "pascal",
                                "title",
                            ];
                            let whole = whole.to_owned();
                            bail!(TemplateErrorKind::NoSuchSpecifier(whole, s, EXPECTED_KWS))
                        }
                    },
                }
            }
        }

        enum State {
            Plain(String),
            Dollar(String),
            Brace(String),
        }

        impl State {
            fn push(mut self, c: char) -> Self {
                match self {
                    State::Plain(ref mut s)
                    | State::Dollar(ref mut s)
                    | State::Brace(ref mut s) => s.push(c),
                }
                self
            }

            fn plain(self, chars: Vec<char>, tokens: &mut Vec<Token>) -> Self {
                self.close(State::Plain(String::from_iter(chars)), tokens)
            }

            fn var(self, tokens: &mut Vec<Token>) -> Self {
                self.close(State::Dollar("".to_owned()), tokens)
            }

            fn brace(self, tokens: &mut Vec<Token>) -> Self {
                self.close(State::Brace("".to_owned()), tokens)
            }

            fn close(self, next: Self, tokens: &mut Vec<Token>) -> Self {
                match self {
                    State::Plain(ref s) if s.is_empty() => {}
                    State::Plain(s) => tokens.push(Token::Text(s)),
                    State::Dollar(s) => tokens.push(Token::Var(s)),
                    State::Brace(s) => tokens.push(Token::Target(s)),
                }
                next
            }

            fn end(self, whole: &str, tokens: &mut Vec<Token>) -> TemplateResult<()> {
                match self {
                    State::Plain(s) => {
                        tokens.push(Token::Text(s));
                        Ok(())
                    }
                    State::Dollar(s) => {
                        tokens.push(Token::Var(s));
                        Ok(())
                    }
                    State::Brace(_) => bail!(TemplateErrorKind::Syntax(whole.to_owned())),
                }
            }
        }

        #[cfg_attr(feature = "cargo-clippy", allow(match_same_arms))]
        fn tokenize(template: &str) -> TemplateResult<Vec<Token>> {
            let syntax_error =
                || TemplateError::from(TemplateErrorKind::Syntax(template.to_owned()));
            let mut state = State::Plain("".to_owned());
            let mut tokens = vec![];
            for c in template.chars() {
                state = match (c, state) {
                    ('$', state @ State::Plain(_)) => state.var(&mut tokens),
                    ('{', state @ State::Plain(_)) => state.brace(&mut tokens),
                    ('}', State::Plain(_)) => return Err(syntax_error()),
                    (c, state @ State::Plain(_)) => state.push(c),
                    ('$', state @ State::Dollar(_)) => state.var(&mut tokens),
                    ('{', state @ State::Dollar(_)) => state.brace(&mut tokens),
                    ('}', State::Dollar(_)) => return Err(syntax_error()),
                    (' ', state @ State::Dollar(_)) => state.plain(vec![' '], &mut tokens),
                    ('/', state @ State::Dollar(_)) => state.plain(vec!['/'], &mut tokens),
                    ('\\', state @ State::Dollar(_)) => state.plain(vec!['\\'], &mut tokens),
                    (c, state @ State::Dollar(_)) => state.push(c),
                    ('{', State::Brace(_)) => return Err(syntax_error()),
                    ('}', state @ State::Brace(_)) => state.plain(vec![], &mut tokens),
                    (c, state @ State::Brace(_)) => state.push(c),
                }
            }
            state.end(template, &mut tokens)?;
            Ok(tokens)
        }

        let variables = variables.into();
        if let Some(ref variables) = variables {
            for k in variables.keys().map(Borrow::borrow) {
                if ['$', '/', '\\', '{', '}', ' ']
                    .iter()
                    .any(|c| k.contains(*c))
                {
                    bail!(TemplateErrorKind::InvalidVariable(k.to_owned()));
                }
            }
        }
        let tokens = tokenize(&self.0)?
            .into_iter()
            .map(|t| t.transform(&self.0, variables))
            .collect::<std::result::Result<Vec<TemplateToken>, _>>()?;
        Ok(Template(tokens))
    }
}

#[cfg(test)]
mod tests {
    use template::TemplateString;

    use std::collections::HashMap;

    #[test]
    fn it_parses_a_template() {
        let empty = None::<&HashMap<&'static str, &'static str>>;

        let template = TemplateString::new("cc/{}.cc");
        assert_eq!("cc/a.cc", template.format("a", empty).unwrap());
        let template = TemplateString::new("cs/{Pascal}/{Pascal}.cs");
        assert_eq!("cs/A/A.cs", template.format("a", empty).unwrap());
        let template = TemplateString::new("gcc -o $bin $src");
        let vars = hashmap!("src" => "SRC", "bin" => "BIN");
        assert_eq!("gcc -o BIN SRC", template.format("", &vars).unwrap());
        let template = TemplateString::new("{ PaScAl }/{pascal}/{lower}");
        assert_eq!("A/A/a", template.format("a", empty).unwrap());
        let template = TemplateString::new("{snake}/{kebab}");
        assert_eq!("foo_bar/foo-bar", template.format("FooBar", empty).unwrap());
        let template = TemplateString::new("$foo/$bar/$baz");
        let vars = hashmap!("foo" => "FOO", "bar" => "BAR", "baz" => "BAZ");
        assert_eq!("FOO/BAR/BAZ", template.format("", &vars).unwrap());
        let template = TemplateString::new("$$$");
        let vars = hashmap!("" => "AAA");
        assert_eq!("AAAAAAAAA", template.format("", &vars).unwrap());

        let template = TemplateString::new("{}/{{}}");
        template.format("", empty).unwrap_err();
        let template = TemplateString::new("{}/{");
        template.format("", empty).is_err();
        let template = TemplateString::new("{}/}");
        template.format("", empty).is_err();
        let template = TemplateString::new("}/{}");
        template.format("", empty).is_err();
        let template = TemplateString::new("{}/{invalid}/{}");
        template.format("", empty).is_err();
        let template = TemplateString::new("$nonexisting");
        template.format("", empty).is_err();

        #[cfg(unix)]
        {
            use errors::{TemplateError, TemplateErrorKind};

            use std::env;
            use std::ffi::OsStr;
            use std::os::unix::ffi::OsStrExt;

            env::set_var("A", "あ");
            env::set_var("B", OsStr::from_bytes(b"\xc3\x28"));
            env::remove_var("C");
            let empty = None::<&HashMap<&'static str, &'static str>>;
            let template = TemplateString::new("$A/$A/{}");
            assert_eq!("あ/あ/a", template.format("a", empty).unwrap());
            match TemplateString::new("$B").format("", empty).unwrap_err() {
                TemplateError(TemplateErrorKind::NonUtf8EnvVar(k), _) => assert_eq!("B", k),
                e => panic!("{}", e),
            }
            match TemplateString::new("$C").format("", empty).unwrap_err() {
                TemplateError(TemplateErrorKind::NoSuchVariable(w, k, expected), _) => {
                    assert_eq!("$C", w);
                    assert_eq!("C", k);
                    assert!(expected.is_empty());
                }
                e => panic!("{}", e),
            }
        }
    }
}
