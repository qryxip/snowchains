use errors::{CodeReplaceErrorKind, CodeReplaceResult};
use template::StringTemplate;

use regex::Regex;
use serde::{self, Deserialize, Deserializer, Serialize, Serializer};

use std;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

pub struct CodeReplacer {
    regex: Regex,
    regex_group: usize,
    local: StringTemplate,
    submit: StringTemplate,
    once: bool,
}

impl CodeReplacer {
    pub fn embed_strings<
        'a,
        M: Into<Option<&'a HashMap<K, V>>>,
        K: 'a + Borrow<str> + Eq + Hash,
        V: 'a + Borrow<str> + Eq + Hash,
    >(
        &self,
        strings: M,
    ) -> Self {
        let strings = strings.into();
        let local = self.local.embed_strings(strings);
        let submit = self.submit.embed_strings(strings);
        Self {
            regex: self.regex.clone(),
            regex_group: self.regex_group,
            local,
            submit,
            once: self.once,
        }
    }

    pub fn replace_as_submission(&self, target: &str, code: &str) -> CodeReplaceResult<String> {
        let (from, to) = (self.local.expand(target)?, self.submit.expand(target)?);
        self.replace(code, &from, &to)
    }

    pub fn replace_from_submission(&self, target: &str, code: &str) -> CodeReplaceResult<String> {
        let (from, to) = (self.submit.expand(target)?, self.local.expand(target)?);
        self.replace(code, &from, &to)
    }

    fn replace(&self, code: &str, from: &str, to: &str) -> CodeReplaceResult<String> {
        let mut replaced_p = false;
        let mut replaced_lines = vec![];
        for line in code.lines() {
            if !(self.once && replaced_p) {
                if let Some(caps) = self.regex.captures(line) {
                    if let Some(m) = caps.get(self.regex_group) {
                        if m.as_str() == from {
                            let mut r = line.as_bytes()[..m.start()].to_owned();
                            r.extend(to.as_bytes());
                            r.extend(&line.as_bytes()[m.end()..]);
                            replaced_lines.push(String::from_utf8(r)?);
                            replaced_p = true;
                        }
                        continue;
                    } else {
                        bail!(CodeReplaceErrorKind::RegexGroupOutOfBounds(
                            self.regex_group
                        ));
                    }
                }
            }
            replaced_lines.push(line.to_owned());
        }
        if !replaced_p {
            bail!(CodeReplaceErrorKind::NoMatch(
                self.regex.as_str().to_owned()
            ));
        }
        let mut r = replaced_lines.join("\n");
        if code.ends_with('\n') {
            r.push('\n');
        }
        Ok(r)
    }
}

impl Serialize for CodeReplacer {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        #[derive(Serialize)]
        struct Prop<'a> {
            regex: String,
            regex_group: usize,
            local: &'a StringTemplate,
            submit: &'a StringTemplate,
            once: bool,
        }

        Prop {
            regex: format!("/{}/", self.regex),
            regex_group: self.regex_group,
            local: &self.local,
            submit: &self.submit,
            once: self.once,
        }.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for CodeReplacer {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        #[derive(Deserialize)]
        struct Prop {
            regex: String,
            regex_group: usize,
            local: StringTemplate,
            submit: StringTemplate,
            once: bool,
        }
        let prop = Prop::deserialize(deserializer)?;
        let regex = if prop.regex.starts_with('/') && prop.regex.ends_with('/') {
            let n = prop.regex.len();
            String::from_utf8_lossy(&prop.regex.as_bytes()[1..n - 1])
        } else {
            prop.regex.as_str().into()
        };
        let regex = Regex::new(&regex).map_err(serde::de::Error::custom)?;
        Ok(Self {
            regex,
            regex_group: prop.regex_group,
            local: prop.local,
            submit: prop.submit,
            once: prop.once,
        })
    }
}

#[cfg(test)]
mod tests {
    use errors::{CodeReplaceError, CodeReplaceErrorKind};
    use replacer::CodeReplacer;
    use template::StringTemplate;

    use regex::Regex;

    #[test]
    fn it_replaces_a_scala_code() {
        static CODE: &str = r#"import java.util.Scanner

object A {
  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val (a, b, c, s) = (sc.nextInt(), sc.nextInt(), sc.nextInt(), sc.next())
    printf("%d %s\n", a + b + c, s)
  }
}

object Foo {}
"#;
        static EXPECTED: &str = r#"import java.util.Scanner

object Main {
  def main(args: Array[String]): Unit = {
    val sc = new Scanner(System.in)
    val (a, b, c, s) = (sc.nextInt(), sc.nextInt(), sc.nextInt(), sc.next())
    printf("%d %s\n", a + b + c, s)
  }
}

object Foo {}
"#;

        fn code_replacer(regex_group: usize) -> CodeReplacer {
            CodeReplacer {
                regex: Regex::new(r"^object\s+([A-Z][a-zA-Z0-9_]*).*$").unwrap(),
                regex_group,
                local: StringTemplate::from_static_str("{Camel}"),
                submit: StringTemplate::from_static_str("Main"),
                once: true,
            }
        }

        let replaced = code_replacer(1).replace(CODE, "A", "Main").unwrap();
        assert_eq!(EXPECTED, replaced);
        match code_replacer(2).replace(CODE, "", "").unwrap_err() {
            CodeReplaceError(CodeReplaceErrorKind::RegexGroupOutOfBounds(2), _) => {}
            e => panic!("{}", e),
        }
    }
}
