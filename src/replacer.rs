use errors::{CodeReplaceErrorKind, CodeReplaceResult};
use template::StringTemplate;
use util;

use regex::Regex;

use std::str;
use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Serialize, Deserialize)]
pub(crate) struct CodeReplacer {
    #[serde(serialize_with = "util::serde::serialize_regex",
            deserialize_with = "util::serde::deserialize_regex")]
    regex: Regex,
    match_group: usize,
    local: StringTemplate,
    submit: StringTemplate,
    all_matched: bool,
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
            match_group: self.match_group,
            local,
            submit,
            all_matched: self.all_matched,
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
            if self.all_matched || !replaced_p {
                if let Some(caps) = self.regex.captures(line) {
                    if let Some(m) = caps.get(self.match_group) {
                        if m.as_str() == from {
                            let mut r = line.as_bytes()[..m.start()].to_owned();
                            r.extend(to.as_bytes());
                            r.extend(&line.as_bytes()[m.end()..]);
                            replaced_lines.push(Cow::from(String::from_utf8(r)?));
                            replaced_p = true;
                            continue;
                        }
                    } else {
                        bail!(CodeReplaceErrorKind::RegexGroupOutOfBounds(
                            self.match_group
                        ));
                    }
                }
            }
            replaced_lines.push(Cow::from(line));
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

        fn code_replacer(match_group: usize) -> CodeReplacer {
            CodeReplacer {
                regex: Regex::new(r"^object\s+([A-Z][a-zA-Z0-9_]*).*$").unwrap(),
                match_group,
                local: StringTemplate::from_static_str("{Camel}"),
                submit: StringTemplate::from_static_str("Main"),
                all_matched: false,
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
