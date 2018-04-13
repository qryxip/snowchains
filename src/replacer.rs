use errors::{CodeReplaceErrorKind, CodeReplaceResult};
use template::Template;

use regex::Regex;

use std::borrow::Cow;

pub struct CodeReplacer {
    regex: Regex,
    regex_group: usize,
    local: Template,
    atcoder: Cow<'static, str>,
    once: bool,
}

impl CodeReplacer {
    pub(crate) fn new(
        regex: Regex,
        regex_group: usize,
        local: Template,
        atcoder: Cow<'static, str>,
        once: bool,
    ) -> Self {
        Self {
            regex,
            regex_group,
            local,
            atcoder,
            once,
        }
    }

    pub fn replace_as_atcoder_submission(
        &self,
        target: &str,
        code: &str,
    ) -> CodeReplaceResult<String> {
        self.replace(code, &self.local.format(target), &self.atcoder)
    }

    pub fn replace_from_atcoder_submission(
        &self,
        target: &str,
        code: &str,
    ) -> CodeReplaceResult<String> {
        self.replace(code, &self.atcoder, &self.local.format(target))
    }

    pub(self) fn replace(&self, code: &str, from: &str, to: &str) -> CodeReplaceResult<String> {
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

#[cfg(test)]
mod tests {
    use errors::{CodeReplaceError, CodeReplaceErrorKind};
    use replacer::CodeReplacer;
    use template::TemplateString;

    use regex::Regex;

    use std::collections::HashMap;

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
            CodeReplacer::new(
                Regex::new(r"^object\s+([A-Z][a-zA-Z0-9_]*).*$").unwrap(),
                regex_group,
                TemplateString::new("{C}")
                    .embed_vars(&HashMap::new())
                    .unwrap(),
                "Main".into(),
                true,
            )
        }

        let replaced = code_replacer(1).replace(CODE, "A", "Main").unwrap();
        assert_eq!(EXPECTED, replaced);
        match code_replacer(2).replace(CODE, "", "").unwrap_err() {
            CodeReplaceError(CodeReplaceErrorKind::RegexGroupOutOfBounds(2), _) => {}
            e => panic!("{}", e),
        }
    }
}
