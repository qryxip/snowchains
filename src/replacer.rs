use crate::errors::{ReplaceCodeErrorKind, ReplaceCodeResult};
use crate::template::{Template, TemplateBuilder};
use crate::yaml;

use failure::ResultExt as _ResultExt;
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
    ) -> ReplaceCodeResult<String> {
        let from = self
            .local
            .expand(problem)
            .context(ReplaceCodeErrorKind::ExpandTemplate("local"))?;
        let to = self
            .submit
            .expand(problem)
            .context(ReplaceCodeErrorKind::ExpandTemplate("submit"))?;
        self.replace(code, &from, &to)
    }

    pub fn replace_from_submission_to_local(
        &self,
        problem: &str,
        code: &str,
    ) -> ReplaceCodeResult<String> {
        let from = self
            .submit
            .expand(problem)
            .context(ReplaceCodeErrorKind::ExpandTemplate("submit"))?;
        let to = self
            .local
            .expand(problem)
            .context(ReplaceCodeErrorKind::ExpandTemplate("local"))?;
        self.replace(code, &from, &to)
    }

    fn replace(&self, code: &str, from: &str, to: &str) -> ReplaceCodeResult<String> {
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
                            let r =
                                String::from_utf8(r).context(ReplaceCodeErrorKind::InvalidUtf8)?;
                            replaced_lines.push(Cow::from(r));
                            replaced_p = true;
                            continue;
                        }
                    } else {
                        return Err(
                            ReplaceCodeErrorKind::RegexGroupOutOfBounds(self.match_group).into(),
                        );
                    }
                }
            }
            replaced_lines.push(Cow::from(line));
        }
        if !replaced_p {
            return Err(ReplaceCodeErrorKind::NoMatch(self.regex.as_str().to_owned()).into());
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
    use crate::errors::ReplaceCodeErrorKind;
    use crate::replacer::CodeReplacer;
    use crate::template::TemplateBuilder;

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
        match code_replacer(2).replace(CODE, "", "").unwrap_err().kind() {
            ReplaceCodeErrorKind::RegexGroupOutOfBounds(2) => {}
            e => panic!("{}", e),
        }
    }
}
