use errors::{CodeReplaceError, CodeReplaceResult};
use template::{Template, TemplateBuilder};
use yaml;

use regex::Regex;
use serde_derive::{Deserialize, Serialize};

use std::borrow::Cow;
use std::collections::HashMap;
use std::hash::Hash;
use std::str;

#[derive(Serialize, Deserialize)]
pub(crate) struct CodeReplacerConf {
    #[serde(
        serialize_with = "yaml::serialize_regex",
        deserialize_with = "yaml::deserialize_regex"
    )]
    regex: Regex,
    match_group: usize,
    local: TemplateBuilder<String>,
    submit: TemplateBuilder<String>,
    all_matched: bool,
}

impl CodeReplacerConf {
    pub fn build<'a, K: 'a + AsRef<str> + Eq + Hash, V: 'a + AsRef<str> + Eq + Hash>(
        &self,
        strings: impl Into<Option<&'a HashMap<K, V>>>,
    ) -> CodeReplacer {
        let (mut local, mut submit) = (self.local.build(), self.submit.build());
        if let Some(strings) = strings.into() {
            local = local.insert_strings(strings);
            submit = submit.insert_strings(strings);
        }
        CodeReplacer {
            regex: self.regex.clone(),
            match_group: self.match_group,
            local,
            submit,
            all_matched: self.all_matched,
        }
    }
}

pub(crate) struct CodeReplacer {
    regex: Regex,
    match_group: usize,
    local: Template<String>,
    submit: Template<String>,
    all_matched: bool,
}

impl CodeReplacer {
    pub fn replace_from_local_to_submission(
        &self,
        problem: &str,
        code: &str,
    ) -> CodeReplaceResult<String> {
        let (from, to) = (self.local.expand(problem)?, self.submit.expand(problem)?);
        self.replace(code, &from, &to)
    }

    pub fn replace_from_submission_to_local(
        &self,
        problem: &str,
        code: &str,
    ) -> CodeReplaceResult<String> {
        let (from, to) = (self.submit.expand(problem)?, self.local.expand(problem)?);
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
                        return Err(CodeReplaceError::RegexGroupOutOfBounds(self.match_group));
                    }
                }
            }
            replaced_lines.push(Cow::from(line));
        }
        if !replaced_p {
            return Err(CodeReplaceError::NoMatch(self.regex.as_str().to_owned()));
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
    use errors::CodeReplaceError;
    use replacer::CodeReplacer;
    use template::TemplateBuilder;

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
            let local = "{Camel}".parse::<TemplateBuilder<String>>().unwrap();
            let submit = "Main".parse::<TemplateBuilder<String>>().unwrap();
            CodeReplacer {
                regex: Regex::new(r"^object\s+([A-Z][a-zA-Z0-9_]*).*$").unwrap(),
                match_group,
                local: local.build(),
                submit: submit.build(),
                all_matched: false,
            }
        }

        let replaced = code_replacer(1).replace(CODE, "A", "Main").unwrap();
        assert_eq!(EXPECTED, replaced);
        match code_replacer(2).replace(CODE, "", "").unwrap_err() {
            CodeReplaceError::RegexGroupOutOfBounds(2) => {}
            e => panic!("{}", e),
        }
    }
}
