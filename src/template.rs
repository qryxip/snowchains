use errors::{TemplateError, TemplateResult};
use util::{self, Camelize};

use regex::Regex;

use std::{self, env, io};
use std::borrow::Cow;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::path::{Path, PathBuf};

pub struct PathTemplate<'a> {
    template: Template,
    base_dir: &'a Path,
}

impl<'a> PathTemplate<'a> {
    pub fn format(&self, target: &str) -> io::Result<PathBuf> {
        let formatted = self.template.format(target);
        util::expand_path(&formatted, self.base_dir)
    }
}

pub struct Template(Vec<TemplateToken>);

impl Template {
    pub fn format(&self, target: &str) -> String {
        self.0.iter().fold("".to_owned(), |mut r, t| {
            match *t {
                TemplateToken::Plain(ref s) => r += s,
                TemplateToken::Target => r += target,
                TemplateToken::TargetCamelized => r += &target.camelize(),
            }
            r
        })
    }

    pub fn with_base_dir(self, base_dir: &Path) -> PathTemplate {
        PathTemplate {
            template: self,
            base_dir,
        }
    }
}

enum TemplateToken {
    Plain(String),
    Target,
    TargetCamelized,
}

#[derive(Serialize, Deserialize)]
pub struct TemplateString(Cow<'static, str>);

impl TemplateString {
    pub fn new<S: Into<Cow<'static, str>>>(s: S) -> Self {
        TemplateString(s.into())
    }

    pub fn resolve_as_path(
        &self,
        base_dir: &Path,
        target: &str,
        variables: &HashMap<&str, &str>,
    ) -> TemplateResult<PathBuf> {
        let resolved = self.embed_vars(variables)?
            .with_base_dir(base_dir)
            .format(target)?;
        Ok(resolved)
    }

    pub fn format(&self, target: &str, variables: &HashMap<&str, &str>) -> TemplateResult<String> {
        let transformed = self.embed_vars(variables)?;
        Ok(transformed.format(target))
    }

    pub fn embed_vars(&self, variables: &HashMap<&str, &str>) -> TemplateResult<Template> {
        enum Token {
            Text(String),
            Var(String),
            Target(String),
        }

        impl Token {
            fn transform(
                self,
                whole: &str,
                variables: &HashMap<&str, &str>,
            ) -> TemplateResult<TemplateToken> {
                match self {
                    Token::Text(s) => Ok(TemplateToken::Plain(s)),
                    Token::Var(s) => match variables.get(s.as_str()) {
                        Some(v) => Ok(TemplateToken::Plain((*v).to_owned())),
                        None => match env::var(s.as_str()) {
                            Ok(v) => Ok(TemplateToken::Plain(v)),
                            Err(env::VarError::NotPresent) => {
                                let vars = variables.keys().map(|v| (*v).to_owned()).collect();
                                Err(TemplateError::NoSuchVariable(whole.to_owned(), s, vars))
                            }
                            Err(env::VarError::NotUnicode(_)) => {
                                Err(TemplateError::NonUtf8EnvVar(s.clone()))
                            }
                        },
                    },
                    Token::Target(ref s) => {
                        let s = trim_lr(s);
                        if s == "" {
                            Ok(TemplateToken::Target)
                        } else if ["c", "C"].contains(&s.as_str()) {
                            Ok(TemplateToken::TargetCamelized)
                        } else {
                            let whole = whole.to_owned();
                            static EXPECTED_KWS: &'static [&'static str] = &["c", "C"];
                            Err(TemplateError::NoSuchSpecifier(whole, s, EXPECTED_KWS))
                        }
                    }
                }
            }
        }

        fn trim_lr(s: &str) -> String {
            lazy_static! {
                static ref CENTOR: Regex = Regex::new(r"^\s*(\S*)\s*$").unwrap();
            }
            match CENTOR.captures(s) {
                Some(cap) => cap[1].to_owned(),
                None => s.to_owned(),
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
                    State::Brace(_) => Err(TemplateError::Syntax(whole.to_owned())),
                }
            }
        }

        #[cfg_attr(feature = "cargo-clippy", allow(match_same_arms))]
        fn tokenize(template: &str) -> TemplateResult<Vec<Token>> {
            let syntax_error = || TemplateError::Syntax(template.to_owned());
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

        for &k in variables.keys() {
            if ['$', '/', '\\', '{', '}', ' ']
                .iter()
                .any(|c| k.contains(*c))
            {
                return Err(TemplateError::InvalidVariable(k.to_owned()));
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
    use super::TemplateString;

    use std::collections::HashMap;
    use std::env;
    use std::iter::FromIterator;

    #[test]
    fn it_parses_a_template() {
        let template = TemplateString::new("cc/{}.cc");
        let vars = HashMap::new();
        assert_eq!("cc/a.cc", template.format("a", &vars).unwrap());
        let template = TemplateString::new("cs/{C}/{C}.cs");
        let vars = HashMap::new();
        assert_eq!("cs/A/A.cs", template.format("a", &vars).unwrap());
        let template = TemplateString::new("gcc -o $bin $src");
        let vars = HashMap::from_iter(vec![("src", "SRC"), ("bin", "BIN")]);
        assert_eq!("gcc -o BIN SRC", template.format("", &vars).unwrap());
        let template = TemplateString::new("{ c }/{c}/{C}");
        let vars = HashMap::new();
        assert_eq!("Name/Name/Name", template.format("name", &vars).unwrap());
        let template = TemplateString::new("$foo/$bar/$baz");
        let vars = HashMap::from_iter(vec![("foo", "FOO"), ("bar", "BAR"), ("baz", "BAZ")]);
        assert_eq!("FOO/BAR/BAZ", template.format("", &vars).unwrap());
        let template = TemplateString::new("$$$");
        let vars = HashMap::from_iter(vec![("", "AAA")]);
        assert_eq!("AAAAAAAAA", template.format("", &vars).unwrap());

        let template = TemplateString::new("{}/{{}}");
        assert!(template.format("", &HashMap::new()).is_err());
        let template = TemplateString::new("{}/{");
        assert!(template.format("", &HashMap::new()).is_err());
        let template = TemplateString::new("{}/}");
        assert!(template.format("", &HashMap::new()).is_err());
        let template = TemplateString::new("}/{}");
        assert!(template.format("", &HashMap::new()).is_err());
        let template = TemplateString::new("{}/{aaa C}/{}");
        assert!(template.format("", &HashMap::new()).is_err());
        let template = TemplateString::new("$unexistingkeyword");
        assert!(template.format("", &HashMap::new()).is_err());

        #[cfg(unix)]
        {
            use errors::TemplateError;

            use std::ffi::OsStr;
            use std::os::unix::ffi::OsStrExt;

            env::set_var("A", "あ");
            env::set_var("B", OsStr::from_bytes(b"\xc3\x28"));
            env::remove_var("C");
            let template = TemplateString::new("$A/$A/{}");
            assert_eq!("あ/あ/a", template.format("a", &HashMap::new()).unwrap());
            match TemplateString::new("$B").format("", &HashMap::new()) {
                Err(TemplateError::NonUtf8EnvVar(k)) => assert_eq!("B", k),
                _ => panic!(),
            }
            match TemplateString::new("$C").format("", &HashMap::new()) {
                Err(TemplateError::NoSuchVariable(w, k, expected)) => {
                    assert_eq!("$C", w);
                    assert_eq!("C", k);
                    assert!(expected.is_empty());
                }
                _ => panic!(),
            }
        }
    }
}
