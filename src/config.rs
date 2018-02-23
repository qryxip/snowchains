use command::{CompilationCommand, JudgingCommand};
use errors::{ConfigError, ConfigErrorKind, ConfigResult, PathFormatError, PathFormatResult};
use testsuite::{SuiteFileExtension, SuiteFilePaths};
use util::{self, Camelize};

use regex::Regex;
use serde_yaml;

use std::{cmp, env, fmt, fs};
use std::borrow::Cow;
use std::collections::HashMap;
use std::io::{self, Write};
use std::iter::FromIterator;
use std::path::{Path, PathBuf};
use std::str::FromStr;

/// Creates `snowchains.yml` in `dir`.
pub fn create_config_file(lang_name: &str, dir: &str) -> ConfigResult<()> {
    let config = format!(
        r#"---
service: "atcoderbeta"
contest: "chokudai_s001"
testsuites: "snowchains/$service/$contest/"
extension_on_downloading: "yml"
extensions_on_judging: ["json", "toml", "yaml", "yml"]
default_lang: {default_lang}

languages:
  - name: "c++"
    src: "cc/{{}}.cc"
    bin: "cc/build/{{}}{exe}"
    compile: "g++ -std=c++14 -O2 -o $bin $src"
    run: "$bin"
    compilation_working_dir: "cc/"
    runtime_working_dir: "cc/"
    atcoder_lang_id: 3003
  - name: "rust"
    src: "rust/src/bin/{{}}.rs"
    bin: "rust/target/release/{{}}{exe}"
    compile: "rustc -O -o $bin $src"
    run: "$bin"
    compilation_working_dir: "rust/"
    runtime_working_dir: "rust/"
    atcoder_lang_id: 3504
  - name: "haskell"
    src: "haskell/src/{{C}}.hs"
    bin: "haskell/target/{{C}}{exe}"
    compile: "stack ghc -- -O2 -o $bin $src"
    run: "$bin"
    compilation_working_dir: "haskell/"
    runtime_working_dir: "haskell/"
    atcoder_lang_id: 3014
  - name: "python3"
    src: "python/{{}}.py"
    bin: ~
    compile: ~
    run: "python3 $src"
    compilation_working_dir: ""
    runtime_working_dir: "python/"
    atcoder_lang_id: 3023
  - name: "java"
    src: "java/src/main/java/{{C}}.java"
    bin: "java/build/classes/java/main/{{C}}.class"
    compile: "javac -d ./build/classes/java/main/ $src"
    run: "java -classpath ./build/classes/java/main/{{C}}"
    compilation_working_dir: "java/"
    runtime_working_dir: "java/"
    atcoder_lang_id: 3016
  - name: "scala"
    src: "scala/src/main/scala/{{C}}.scala"
    bin: "scala/target/scala-2.12/classes/{{C}}.class"
    compile: "scalac -optimise -d ./target/scala-2.12/classes/ $src"
    run: "scala -classpath ./target/scala-2.12/classes/ {{C}}"
    compilation_working_dir: "scala/"
    runtime_working_dir: "scala/"
    atcoder_lang_id: 3025
{csharp}
"#,
        default_lang = format!("{:?}", lang_name),
        exe = if cfg!(target_os = "windows") {
            ".exe"
        } else {
            ""
        },
        csharp = if cfg!(target_os = "windows") {
            r#"  - name: "c#"
    src: "csharp/{C}/{C}.cs"
    bin: "csharp/{C}/bin/Release/{C}.exe"
    compile: "csc /o+ /r:System.Numerics /out:$bin $src"
    run: "$bin"
    compilation_working_dir: "csharp/"
    runtime_working_dir: "csharp/"
    atcoder_lang_id: 3006"#
        } else {
            r#" - name: "c#"
    src: "csharp/{C}/{C}.cs"
    bin: "csharp/{C}/bin/Release/{C}.exe"
    compile: "mcs -o+ -r:System.Numerics -out:$bin $src"
    run: "mono $bin"
    compilation_working_dir: "csharp/"
    runtime_working_dir: "csharp/"
    atcoder_lang_id: 3006"#
        }
    );

    let mut path = PathBuf::from(dir);
    path.push("snowchains.yml");
    Ok(util::create_file_and_dirs(&path)?.write_all(config.as_bytes())?)
}

/// Changes <service> and <contest>.
pub fn switch(service: ServiceName, contest: &str) -> ConfigResult<()> {
    fn str_from_opt<T: fmt::Display>(x: &Option<T>) -> Cow<'static, str> {
        match *x {
            Some(ref x) => Cow::Owned(format!("{:?}", x.to_string())),
            None => Cow::Borrowed("None"),
        }
    }

    fn print_change(n: usize, prev: &Cow<'static, str>, new: &str) {
        print!("{}", prev);
        for _ in 0..n - prev.len() {
            print!(" ");
        }
        println!(" -> {:?}", new);
    }

    let (_, path) = find_base()?;
    let text = util::string_from_file_path(&path)?;
    println!("Loaded {}", path.display());
    let (replaced, prev_service, prev_contest) = {
        let mut replaced = "".to_owned();
        let (mut prev_service, mut prev_contest) = (None, None);
        for line in text.lines() {
            lazy_static! {
                static ref SERVICE: Regex = Regex::new(r"^service\s*:\s*(\S.*)$").unwrap();
                static ref CONTEST: Regex = Regex::new(r"^contest\s*:\s*(\S.*)$").unwrap();
            }
            if let Some(caps) = SERVICE.captures(line) {
                prev_service = Some(caps[1].to_owned());
                replaced += &format!("service: {:?}", service.to_string());
            } else if let Some(caps) = CONTEST.captures(line) {
                prev_contest = Some(caps[1].to_owned());
                replaced += &format!("contest: {:?}", contest);
            } else {
                replaced += line;
            }
            replaced.push('\n');
        }
        if prev_service.is_some() && prev_contest.is_some()
            && serde_yaml::from_str::<Config>(&replaced).is_ok()
        {
            let (prev_service, prev_contest) = (prev_service.unwrap(), prev_contest.unwrap());
            (replaced, Cow::Owned(prev_service), Cow::Owned(prev_contest))
        } else {
            let mut config = serde_yaml::from_str::<Config>(&text)?;
            let prev_service = str_from_opt(&config.service);
            let prev_contest = str_from_opt(&config.contest);
            config.service = Some(service);
            config.contest = Some(contest.to_owned());
            (serde_yaml::to_string(&config)?, prev_service, prev_contest)
        }
    };
    let n = cmp::max(prev_service.len(), prev_contest.len());
    print_change(n, &prev_service, &service.to_string().as_str());
    print_change(n, &prev_contest, &contest);
    let mut file = util::create_file_and_dirs(&path)?;
    file.write_all(replaced.as_bytes())?;
    Ok(println!("Saved."))
}

/// Config data.
#[derive(Serialize, Deserialize)]
pub struct Config {
    service: Option<ServiceName>,
    contest: Option<String>,
    #[serde(default = "PathFormat::default_testsuites")]
    testsuites: PathFormat,
    #[serde(default)]
    extension_on_downloading: SuiteFileExtension,
    #[serde(default = "default_extensions")]
    extensions_on_judging: Vec<SuiteFileExtension>,
    default_lang: String,
    languages: Vec<LangProperty>,
    #[serde(skip)]
    base_dir: PathBuf,
}

impl Config {
    /// Loads and deserializes from the nearest `snowchains.yml`
    pub fn load_from_file() -> ConfigResult<Self> {
        let (base, path) = find_base()?;
        let mut config = serde_yaml::from_str::<Self>(&util::string_from_file_path(&path)?)?;
        config.base_dir = base;
        println!("Loaded {}", path.display());
        Ok(config)
    }

    /// Gets `service`.
    pub fn service_name(&self) -> ConfigResult<ServiceName> {
        match self.service.clone() {
            Some(service) => Ok(service),
            None => bail!(ConfigErrorKind::PropertyNotSet("service")),
        }
    }

    /// Gets `contest`.
    pub fn contest_name(&self) -> ConfigResult<String> {
        match self.contest.clone() {
            Some(contest) => Ok(contest),
            None => bail!(ConfigErrorKind::PropertyNotSet("contest")),
        }
    }

    /// Gets the attribute `extension_on_downloading`.
    pub fn get_extension_on_downloading(&self) -> SuiteFileExtension {
        self.extension_on_downloading
    }

    /// Gets the absolute path of the test suite files directory
    pub fn suite_dir(&self) -> ConfigResult<PathBuf> {
        let service = self.service.map(|s| s.to_string()).unwrap_or_default();
        let contest = self.contest.clone().unwrap_or_default();
        let keywords = vec![("service", service.as_str()), ("contest", contest.as_str())];
        let keywords = HashMap::from_iter(keywords);
        let ref base = self.base_dir;
        self.testsuites.resolve_as_path(base, "", &keywords)
    }

    pub fn suite_paths(&self, target: &str) -> ConfigResult<SuiteFilePaths> {
        let dir = self.suite_dir()?;
        let ref exts = self.extensions_on_judging;
        Ok(SuiteFilePaths::new(&dir, target, exts))
    }

    /// Returns the path of the source file.
    pub fn src_path(&self, target: &str, lang_name: Option<&str>) -> ConfigResult<PathBuf> {
        let lang = self.lang_property(lang_name)?;
        Ok(lang.resolve_src(&self.base_dir, target)?)
    }

    /// Returns the `lang_id` of `lang_name` or a default language
    pub fn atcoder_lang_id(&self, lang_name: Option<&str>) -> ConfigResult<u32> {
        let lang = self.lang_property(lang_name)?;
        lang.atcoder_lang_id
            .ok_or_else(|| ConfigError::from(ConfigErrorKind::PropertyNotSet("atcoder_lang_id")))
    }

    /// Constructs arguments of compilation command for given or default
    /// language.
    pub fn construct_compilation_command(
        &self,
        target: &str,
        lang_name: Option<&str>,
    ) -> ConfigResult<Option<CompilationCommand>> {
        let lang = self.lang_property(lang_name)?;
        Ok(lang.construct_compilation_command(&self.base_dir, target)?)
    }

    /// Constructs arguments of execution command for given or default language.
    pub fn construct_solver(
        &self,
        target: &str,
        lang_name: Option<&str>,
    ) -> ConfigResult<JudgingCommand> {
        let lang = self.lang_property(lang_name)?;
        Ok(lang.construct_solver(&self.base_dir, target)?)
    }

    fn lang_property(&self, lang_name: Option<&str>) -> ConfigResult<&LangProperty> {
        let lang_name = lang_name.unwrap_or(&self.default_lang);
        self.languages
            .iter()
            .find(|lang| lang.name == lang_name)
            .ok_or_else(|| ConfigError::from(ConfigErrorKind::NoSuchLanguage(lang_name.to_owned())))
    }
}

/// Names of programming contest services.
#[derive(Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ServiceName {
    AtCoder,
    AtCoderBeta,
    HackerRank,
}

impl fmt::Display for ServiceName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ServiceName::AtCoder => write!(f, "atcoder"),
            ServiceName::AtCoderBeta => write!(f, "atcoderbeta"),
            ServiceName::HackerRank => write!(f, "hackerrank"),
        }
    }
}

impl FromStr for ServiceName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        return match s.to_lowercase().as_str() {
            "atcoder" => Ok(ServiceName::AtCoder),
            "atcoderbeta" => Ok(ServiceName::AtCoderBeta),
            "hackerrank" => Ok(ServiceName::HackerRank),
            _ => Err(()),
        };
    }
}

fn find_base() -> ConfigResult<(PathBuf, PathBuf)> {
    fn snowchain_yml_exists(dir: &Path) -> io::Result<bool> {
        for entry in fs::read_dir(dir)? {
            let path = entry?.path();
            if path.is_file() && path.file_name().unwrap() == "snowchains.yml" {
                return Ok(true);
            }
        }
        Ok(false)
    }

    let mut dir = env::current_dir()?;
    loop {
        if let Ok(true) = snowchain_yml_exists(&dir) {
            let mut path = dir.clone();
            path.push("snowchains.yml");
            return Ok((dir, path));
        } else if !dir.pop() {
            bail!(ConfigErrorKind::ConfigFileNotFound);
        }
    }
}

fn default_extensions() -> Vec<SuiteFileExtension> {
    use testsuite::SuiteFileExtension::{Json, Toml, Yaml, Yml};
    vec![Json, Toml, Yaml, Yml]
}

#[derive(Serialize, Deserialize)]
struct LangProperty {
    name: String,
    src: PathFormat,
    bin: Option<PathFormat>,
    compile: Option<PathFormat>,
    #[serde(default = "PathFormat::bin")]
    run: PathFormat,
    #[serde(default)]
    compilation_working_dir: InputPath,
    #[serde(default)]
    runtime_working_dir: InputPath,
    atcoder_lang_id: Option<u32>,
}

impl LangProperty {
    fn resolve_src(&self, base: &Path, target: &str) -> ConfigResult<PathBuf> {
        self.src.resolve_as_path(base, target, &HashMap::new())
    }

    fn construct_compilation_command(
        &self,
        base: &Path,
        target: &str,
    ) -> ConfigResult<Option<CompilationCommand>> {
        let working_dir = self.compilation_working_dir.resolve(base)?;
        let (src, bin) = self.resolve_src_and_bin(base, target)?;
        if let Some(comp) = self.compile.as_ref() {
            let command = comp.to_compilation_command(target, working_dir, Some(src), bin)?;
            Ok(Some(command))
        } else {
            Ok(None)
        }
    }

    fn construct_solver(&self, base: &Path, target: &str) -> ConfigResult<JudgingCommand> {
        let working_dir = self.runtime_working_dir.resolve(base)?;
        let (src, bin) = self.resolve_src_and_bin(base, target)?;
        let src = src.display().to_string();
        let bin = bin.map(|p| p.display().to_string()).unwrap_or_default();
        self.run.to_solver(target, working_dir, &src, &bin)
    }

    fn resolve_src_and_bin(
        &self,
        base: &Path,
        target: &str,
    ) -> ConfigResult<(PathBuf, Option<PathBuf>)> {
        let src = self.src.resolve_as_path(base, target, &HashMap::new())?;
        let bin = match self.bin {
            Some(ref bin) => Some(bin.resolve_as_path(base, target, &HashMap::new())?),
            None => None,
        };
        Ok((src, bin))
    }
}

#[derive(Serialize, Deserialize)]
struct InputPath(String);

impl Default for InputPath {
    fn default() -> Self {
        InputPath("".to_owned())
    }
}

impl InputPath {
    fn resolve(&self, base: &Path) -> io::Result<PathBuf> {
        if self.0.chars().next() == Some('~') {
            return util::path_under_home(&[&self.0.chars().skip(2).collect::<String>()]);
        }
        let path = PathBuf::from(&self.0);
        Ok(if path.is_absolute() {
            path
        } else {
            let mut pathbuf = PathBuf::from(base);
            pathbuf.push(path);
            pathbuf
        })
    }
}

#[derive(Serialize, Deserialize)]
struct PathFormat(String);

impl PathFormat {
    fn default_testsuites() -> Self {
        PathFormat("snowchains/$service/$contest/".to_owned())
    }

    fn bin() -> Self {
        PathFormat("$bin".to_owned())
    }

    fn resolve_as_path(
        &self,
        base: &Path,
        target: &str,
        keywords: &HashMap<&'static str, &str>,
    ) -> ConfigResult<PathBuf> {
        let path = self.format(&target, keywords)?;
        Ok(InputPath(path).resolve(base)?)
    }

    fn to_compilation_command(
        &self,
        target: &str,
        working_dir: PathBuf,
        src: Option<PathBuf>,
        bin: Option<PathBuf>,
    ) -> ConfigResult<CompilationCommand> {
        let src_s = src.as_ref()
            .map(|p| p.display().to_string())
            .unwrap_or_default();
        let bin_s = bin.as_ref()
            .map(|p| p.display().to_string())
            .unwrap_or_default();
        let src_and_bin = match (src, bin) {
            (Some(src), Some(bin)) => Some((src, bin)),
            _ => None,
        };
        let keywords = HashMap::from_iter(vec![("src", src_s.as_str()), ("bin", bin_s.as_str())]);
        let command = self.format(target, &keywords)?;
        Ok(CompilationCommand::new(command, working_dir, src_and_bin))
    }

    fn to_solver(
        &self,
        target: &str,
        working_dir: PathBuf,
        src: &str,
        bin: &str,
    ) -> ConfigResult<JudgingCommand> {
        let keywords = HashMap::from_iter(vec![("src", src), ("bin", bin)]);
        let command = self.format(target, &keywords)?;
        Ok(JudgingCommand::new(command, working_dir))
    }

    fn format(
        &self,
        target: &str,
        keywords: &HashMap<&'static str, &str>,
    ) -> PathFormatResult<String> {
        enum Token {
            Text(String),
            Var(String),
            Target(String),
        }

        impl Token {
            fn format(
                &self,
                whole: &str,
                target: &str,
                keywords: &HashMap<&'static str, &str>,
                f: &mut String,
            ) -> PathFormatResult<()> {
                fn trim_lr(s: &str) -> String {
                    lazy_static! {
                        static ref CENTOR: Regex = Regex::new(r"^\s*(\S*)\s*$").unwrap();
                    }
                    match CENTOR.captures(s) {
                        Some(cap) => cap[1].to_owned(),
                        None => s.to_owned(),
                    }
                }

                match *self {
                    Token::Text(ref s) => Ok(f.push_str(s)),
                    Token::Var(ref s) => match keywords.get(s.as_str()) {
                        Some(v) => Ok(f.push_str(v)),
                        None => {
                            let (whole, s) = (whole.to_owned(), s.to_owned());
                            let keywords = keywords.keys().cloned().collect();
                            Err(PathFormatError::NoSuchKeyword(whole, s, keywords))
                        }
                    },
                    Token::Target(ref s) => {
                        let s = trim_lr(s);
                        if s == "" {
                            Ok(f.push_str(target))
                        } else if ["c", "C"].contains(&s.as_str()) {
                            Ok(f.push_str(&target.camelize()))
                        } else {
                            let whole = whole.to_owned();
                            static EXPECTED_KWS: &'static [&'static str] = &["c", "C"];
                            Err(PathFormatError::NoSuchSpecifier(whole, s, EXPECTED_KWS))
                        }
                    }
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
                    State::Plain(ref mut s) => s.push(c),
                    State::Dollar(ref mut s) => s.push(c),
                    State::Brace(ref mut s) => s.push(c),
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

            fn end(self, whole: &str, tokens: &mut Vec<Token>) -> PathFormatResult<()> {
                match self {
                    State::Plain(s) => Ok(tokens.push(Token::Text(s))),
                    State::Dollar(s) => Ok(tokens.push(Token::Var(s))),
                    State::Brace(_) => Err(PathFormatError::Syntax(whole.to_owned())),
                }
            }
        }

        let syntax_error = || PathFormatError::Syntax(self.0.clone());

        let tokens = {
            let mut state = State::Plain("".to_owned());
            let mut tokens = vec![];
            for c in self.0.chars() {
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
            state.end(&self.0, &mut tokens)?;
            tokens
        };

        let mut formatted = "".to_owned();
        for token in tokens.into_iter() {
            token.format(&self.0, target, keywords, &mut formatted)?;
        }
        Ok(formatted)
    }
}

#[cfg(test)]
mod tests {
    use super::PathFormat;

    use std::collections::HashMap;
    use std::iter::FromIterator;

    #[test]
    fn test_pathformat_format() {
        let format = PathFormat("cc/{}.cc".to_owned());
        let keywords = HashMap::new();
        assert_eq!("cc/a.cc", format.format("a", &keywords).unwrap());
        let format = PathFormat("csharp/{C}/{C}.cs".to_owned());
        let keywords = HashMap::new();
        assert_eq!("csharp/A/A.cs", format.format("a", &keywords).unwrap());
        let format = PathFormat("gcc -o $bin $src".to_owned());
        let keywords = HashMap::from_iter(vec![("src", "SRC"), ("bin", "BIN")]);
        assert_eq!("gcc -o BIN SRC", format.format("", &keywords).unwrap());
        let format = PathFormat("{ c }/{c}/{C}".to_owned());
        let keywords = HashMap::new();
        assert_eq!("Name/Name/Name", format.format("name", &keywords).unwrap());
        let format = PathFormat("$foo/$bar/$baz".to_owned());
        let keywords = HashMap::from_iter(vec![("foo", "FOO"), ("bar", "BAR"), ("baz", "BAZ")]);
        assert_eq!("FOO/BAR/BAZ", format.format("", &keywords).unwrap());
        let format = PathFormat("$$$".to_owned());
        let keywords = HashMap::from_iter(vec![("", "AAA")]);
        assert_eq!("AAAAAAAAA", format.format("", &keywords).unwrap());

        let format = PathFormat("{}/{{}}".to_owned());
        assert!(format.format("", &HashMap::new()).is_err());
        let format = PathFormat("{}/{".to_owned());
        assert!(format.format("", &HashMap::new()).is_err());
        let format = PathFormat("{}/}".to_owned());
        assert!(format.format("", &HashMap::new()).is_err());
        let format = PathFormat("}/{}".to_owned());
        assert!(format.format("", &HashMap::new()).is_err());
        let format = PathFormat("{}/{aaa C}/{}".to_owned());
        assert!(format.format("", &HashMap::new()).is_err());
        let format = PathFormat("$unexistingkeyword".to_owned());
        assert!(format.format("", &HashMap::new()).is_err());
    }
}
