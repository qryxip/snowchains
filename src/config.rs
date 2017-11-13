use error::{ConfigError, ConfigErrorKind, ConfigResult};
use judge::{CompilationCommand, JudgingCommand};
use testsuite::SuiteFileExtension;
use util::{self, ToCamlCase};

use serde_yaml;
use std::{env, fs};
use std::borrow::Cow;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::str::FromStr;


/// Creates `snowchains.yml` in `dir`.
pub fn create_config_file(lang_name: &str, dir: &str) -> ConfigResult<()> {
    fn append_exe_if_windows(path: &'static str) -> Cow<'static, str> {
        if cfg!(target_os = "windows") {
            format!("{}.exe", path).into()
        } else {
            path.into()
        }
    }

    let csharp_or_mono = if cfg!(target_os = "windows") {
        LangProperty::new(
            "c#",
            "csharp/{C}/{C}.cs",
            Some("csharp/{C}/bin/Release/{C}.exe"),
            Some("csc /o+ /r:System.Numerics /out:$bin $src"),
            "$bin",
            "csharp/",
            "csharp/",
            Some(3006),
        )
    } else {
        LangProperty::new(
            "c#",
            "csharp/{C}/{C}.cs",
            Some("csharp/{C}/bin/Release/{C}.exe"),
            Some("mcs -o+ -r:System.Numerics -out:$bin $src"),
            "mono $bin",
            "csharp/",
            "csharp/",
            Some(3006),
        )
    };

    let config = Config {
        service: Some(ServiceName::AtCoderBeta),
        contest: Some("chokudai_s001".to_owned()),
        testsuites: InputPath("snowchains/".to_owned()),
        extension_on_downloading: SuiteFileExtension::Yml,
        extensions_on_judging: default_extensions(),
        default_lang: lang_name.to_owned(),
        languages: vec![
            LangProperty::new(
                "c",
                "c/{}.c",
                Some(append_exe_if_windows("c/build/{}")),
                Some("gcc -std=c11 -O2 -lm -o $bin $src"),
                "$bin",
                "c/",
                "c/",
                Some(3002)
            ),
            LangProperty::new(
                "c++",
                "cc/{}.cc",
                Some(append_exe_if_windows("cc/build/{}")),
                Some("g++ -std=c++14 -O2 -o $bin $src"),
                "$bin",
                "cc/",
                "cc/",
                Some(3003)
            ),
            LangProperty::new(
                "rust",
                "rust/src/bin/{}.rs",
                Some(append_exe_if_windows("rust/target/release/{}")),
                Some("rustc -O -o $bin $src"),
                "$bin",
                "rust/",
                "rust/",
                Some(3504)
            ),
            LangProperty::new(
                "haskell",
                "haskell/src/{C}.hs",
                Some(append_exe_if_windows("haskell/target/{}")),
                Some("stack ghc -- -O2 -o $bin $src"),
                "$bin",
                "haskell/",
                "haskell/",
                Some(3014)
            ),
            LangProperty::new(
                "java",
                "java/src/main/java/{C}.java",
                Some("java/build/classes/java/main/{C}.class"),
                Some("javac -d ./build/classes/java/main/ $src"),
                "java -classpath ./build/classes/java/main/ {C}",
                "java/",
                "java/",
                Some(3016)
            ),
            LangProperty::new(
                "scala",
                "scala/src/main/scala/{C}.scala",
                Some("scala/target/scala-2.12/classes/{C}.class"),
                Some("scalac -optimise -d ./target/scala-2.12/classes/ $src"),
                "scala -classpath ./target/scala-2.12/classes/ {C}",
                "scala/",
                "scala/",
                Some(3025)
            ),
            csharp_or_mono,
            LangProperty::new::<&'static str>(
                "python3",
                "python/{}.py",
                None,
                None,
                "python3 $src",
                "",
                "python/",
                Some(3023)
            ),
        ],
        base_dir: PathBuf::new(),
    };

    let config = serde_yaml::to_string(&config)?;
    let mut path = PathBuf::from(dir);
    path.push("snowchains.yml");
    Ok(util::create_file_and_dirs(&path)?.write_all(
        config.as_bytes(),
    )?)
}


/// Sets a property in `snowchains.yml`.
pub fn set_property(key: PropertyKey, value: &str) -> ConfigResult<()> {
    let mut config = Config::load_from_file()?;
    match key {
        PropertyKey::Service => config.service = Some(serde_yaml::from_str(value)?),
        PropertyKey::Contest => config.contest = Some(value.to_owned()),
        PropertyKey::TestSuites => config.testsuites = InputPath(value.to_owned()),
        PropertyKey::ExtensionOnDownloading => {
            if let Ok(extension) = SuiteFileExtension::from_str(value) {
                config.extension_on_downloading = extension;
            } else {
                bail!(ConfigErrorKind::UnsupportedExtension(value.to_owned()));
            }
        }
        PropertyKey::DefaultLang => config.default_lang = value.to_owned(),
    }
    let path = find_base()?.1;
    let mut file = util::create_file_and_dirs(&path)?;
    let config = serde_yaml::to_string(&config)?;
    file.write_all(config.as_bytes())?;
    Ok(println!("Saved to {}", path.display()))
}


/// Config data.
#[derive(Serialize, Deserialize)]
pub struct Config {
    service: Option<ServiceName>,
    contest: Option<String>,
    #[serde(default = "InputPath::default_test_suite_path")]
    testsuites: InputPath,
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

    /// Get `service`.
    pub fn service_name(&self) -> ConfigResult<ServiceName> {
        match self.service.clone() {
            Some(service) => Ok(service),
            None => bail!(ConfigErrorKind::PropertyNotSet("service")),
        }
    }

    /// Get `contest`.
    pub fn contest_name(&self) -> ConfigResult<String> {
        match self.contest.clone() {
            Some(contest) => Ok(contest),
            None => bail!(ConfigErrorKind::PropertyNotSet("contest")),
        }
    }

    /// Get the attribute `extension_on_downloading`.
    pub fn get_extension_on_downloading(&self) -> SuiteFileExtension {
        self.extension_on_downloading
    }

    /// Get the attribute `extensions_on_judging`.
    pub fn get_extensions_on_judging(&self) -> &[SuiteFileExtension] {
        &self.extensions_on_judging
    }

    /// Get the absolute path of the test suite files directory
    pub fn suite_dir(&self) -> ConfigResult<PathBuf> {
        Ok(self.testsuites.resolve(&self.base_dir)?)
    }

    /// Returns the path of the source file.
    pub fn src_path(&self, target: &str, lang_name: Option<&str>) -> ConfigResult<PathBuf> {
        let lang = self.lang_property(lang_name)?;
        Ok(lang.resolve_src(&self.base_dir, target)?)
    }

    /// Returns the `lang_id` of given or default language
    pub fn atcoder_lang_id(&self, lang_name: Option<&str>) -> ConfigResult<u32> {
        let lang = self.lang_property(lang_name)?;
        lang.atcoder_lang_id.ok_or_else(|| {
            ConfigError::from(ConfigErrorKind::PropertyNotSet("atcoder_lang_id"))
        })
    }

    /// Constructs arguments of compilation command for given or default language.
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
            .ok_or_else(|| {
                ConfigError::from(ConfigErrorKind::NoSuchLanguage(lang_name.to_owned()))
            })
    }
}


/// Property names of `snowchains.yml`.
pub enum PropertyKey {
    Service,
    Contest,
    TestSuites,
    ExtensionOnDownloading,
    DefaultLang,
}

impl FromStr for PropertyKey {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        let s = s.to_lowercase();
        return match &s {
            s if s == "service" => Ok(PropertyKey::Service),
            s if s == "contest" => Ok(PropertyKey::Contest),
            s if s == "testsuites" => Ok(PropertyKey::TestSuites),
            s if s == "extension_on_downloading" => Ok(PropertyKey::ExtensionOnDownloading),
            s if s == "default_lang" => Ok(PropertyKey::DefaultLang),
            _ => Err(()),
        };
    }
}


/// Names of programming contest services.
#[derive(Clone, Serialize, Deserialize)]
pub enum ServiceName {
    #[serde(rename = "atcoder")]
    AtCoder,
    #[serde(rename = "atcoder-beta")]
    AtCoderBeta,
    #[serde(rename = "hackerrank")]
    HackerRank,
}

impl FromStr for ServiceName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        let s = s.to_lowercase();
        return match &s {
            s if s == "atcoder" => Ok(ServiceName::AtCoder),
            s if s == "atcoder-beta" => Ok(ServiceName::AtCoderBeta),
            s if s == "hackerrank" => Ok(ServiceName::HackerRank),
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
    src: BraceFormat,
    bin: Option<BraceFormat>,
    compile: Option<BraceFormat>,
    #[serde(default = "BraceFormat::bin")]
    run: BraceFormat,
    #[serde(default)]
    compilation_working_dir: InputPath,
    #[serde(default)]
    runtime_working_dir: InputPath,
    atcoder_lang_id: Option<u32>,
}

impl LangProperty {
    fn new<S: Into<Cow<'static, str>>>(
        name: &'static str,
        src: &'static str,
        bin: Option<S>,
        compile: Option<&'static str>,
        run: &'static str,
        compilation_working_dir: &'static str,
        runtime_working_dir: &'static str,
        atcoder_lang_id: Option<u32>,
    ) -> Self {
        Self {
            name: name.to_owned(),
            src: BraceFormat(src.to_owned()),
            bin: bin.map(|bin| BraceFormat(bin.into().into_owned())),
            compile: compile.map(|comp| BraceFormat(comp.to_owned())),
            run: BraceFormat(run.to_owned()),
            compilation_working_dir: InputPath(compilation_working_dir.to_owned()),
            runtime_working_dir: InputPath(runtime_working_dir.to_owned()),
            atcoder_lang_id: atcoder_lang_id,
        }
    }

    fn resolve_src(&self, base: &Path, target: &str) -> io::Result<PathBuf> {
        self.src.resolve_as_path(base, target)
    }

    fn construct_compilation_command(
        &self,
        base: &Path,
        target: &str,
    ) -> ConfigResult<Option<CompilationCommand>> {
        let working_dir = self.compilation_working_dir.resolve(base)?;
        let (src, bin) = self.resolve_src_and_bin(base, target)?;
        Ok(self.compile.as_ref().map(|build| {
            build.to_compilation_command(target, working_dir, Some(src), bin)
        }))
    }

    fn construct_solver(&self, base: &Path, target: &str) -> ConfigResult<JudgingCommand> {
        let working_dir = self.runtime_working_dir.resolve(base)?;
        let (src, bin) = self.resolve_src_and_bin(base, target)?;
        let src = src.display().to_string();
        let bin = bin.map(|p| p.display().to_string()).unwrap_or_default();
        Ok(self.run.to_solver(target, working_dir, &src, &bin))
    }

    fn resolve_src_and_bin(
        &self,
        base: &Path,
        target: &str,
    ) -> ConfigResult<(PathBuf, Option<PathBuf>)> {
        let src = self.src.resolve_as_path(base, target)?;
        let bin = match self.bin {
            Some(ref bin) => Some(bin.resolve_as_path(base, target)?),
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
    fn default_test_suite_path() -> Self {
        InputPath("snowchains/".to_owned())
    }

    fn resolve(&self, base: &Path) -> io::Result<PathBuf> {
        if self.0.chars().next() == Some('~') {
            return util::path_under_home(&[&self.0.chars().skip(2).collect::<String>()]);
        }
        let path = PathBuf::from(&self.0);
        if path.is_absolute() {
            Ok(path)
        } else {
            let mut pathbuf = PathBuf::from(base);
            pathbuf.push(path);
            Ok(pathbuf)
        }
    }
}


#[derive(Serialize, Deserialize)]
struct BraceFormat(String);

impl BraceFormat {
    fn bin() -> Self {
        BraceFormat("$bin".to_owned())
    }

    fn resolve_as_path(&self, base: &Path, target: &str) -> io::Result<PathBuf> {
        InputPath(self.format(&target, "", "")).resolve(base)
    }

    fn to_compilation_command(
        &self,
        target: &str,
        working_dir: PathBuf,
        src: Option<PathBuf>,
        bin: Option<PathBuf>,
    ) -> CompilationCommand {
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
        let command = self.format(target, &src_s, &bin_s);
        CompilationCommand::new(command, working_dir, src_and_bin)
    }

    fn to_solver(
        &self,
        target: &str,
        working_dir: PathBuf,
        src: &str,
        bin: &str,
    ) -> JudgingCommand {
        let command = self.format(target, src, bin);
        JudgingCommand::new(command, working_dir)
    }

    fn format(&self, target: &str, src: &str, bin: &str) -> String {
        enum St {
            Nest(usize),
            Format(Option<char>),
            Var(String),
        }

        let (mut r, mut s) = (String::new(), St::Nest(0));
        macro_rules! close_format(($str: expr) => { {
            r += $str;
            St::Nest(0)
        } });
        macro_rules! close_var(($var: expr, $next_state: expr, $c: expr) => { {
            let v = $var.to_lowercase();
            r += match v.as_str() { "src" => src, "bin" => bin, _ => "" };
            if let Some(c) = $c { r.push(c); }
            $next_state
        } });
        macro_rules! read_one(($c: expr) => { {
            r.push($c);
            St::Nest(0)
        } });
        macro_rules! push_to_var(($var: expr, $c: expr) => { {
            let mut v = $var;
            if $c != '}' { v.push($c); }
            St::Var(v)
        } });
        for c in self.0.chars() {
            s = match (c, s) {
                ('{', St::Nest(0)) => St::Format(None),
                ('{', St::Nest(n)) => St::Nest(n + 1),
                ('}', St::Nest(1)) => close_format!(target),
                ('}', St::Nest(n)) if n > 1 => St::Nest(n - 1),
                ('$', St::Nest(0)) => St::Var("".to_owned()),
                (c, St::Nest(0)) => read_one!(c),
                ('{', St::Format(_)) => St::Nest(2),
                ('}', St::Format(Some('c'))) |
                ('}', St::Format(Some('C'))) => close_format!(&target.to_caml_case()),
                ('}', St::Format(_)) => close_format!(target),
                (c, St::Format(None)) if c != ' ' => St::Format(Some(c)),
                (c, St::Format(Some(_))) if c != ' ' => St::Nest(1),
                ('{', St::Var(v)) => close_var!(v, St::Format(None), None),
                (' ', St::Var(v)) => close_var!(v, St::Nest(0), Some(' ')),
                ('$', St::Var(v)) => close_var!(v, St::Var("".to_owned()), None),
                (c, St::Var(v)) => push_to_var!(v, c),
                (_, s) => s,
            };
        }
        if let St::Var(v) = s {
            close_var!(v, St::Nest(0), None);
        }
        r
    }
}


#[cfg(test)]
mod tests {
    use super::BraceFormat;


    #[test]
    fn test_braceformat_format() {
        let format = BraceFormat("cc/{}.cc".to_owned());
        assert_eq!("cc/a.cc", format.format("a", "", ""));
        let format = BraceFormat("csharp/{C}/{C}.cs".to_owned());
        assert_eq!("csharp/A/A.cs", format.format("a", "", ""));
        let format = BraceFormat("gcc -o $bin $src".to_owned());
        assert_eq!("gcc -o BIN SRC", format.format("", "SRC", "BIN"));
        let format = BraceFormat("{}/{{}}".to_owned());
        assert_eq!("name/name", format.format("name", "", ""));
        let format = BraceFormat("{}/{aaa C}/{}".to_owned());
        assert_eq!("name/name/name", format.format("name", "", ""));
        let format = BraceFormat("{}/{ C }/{C}".to_owned());
        assert_eq!("name/Name/Name", format.format("name", "", ""));
        let format = BraceFormat("{}/{".to_owned());
        assert_eq!("name/", format.format("name", "", ""));
        let format = BraceFormat("{}/}".to_owned());
        assert_eq!("name/}", format.format("name", "", ""));
        let format = BraceFormat("}/{}".to_owned());
        assert_eq!("}/name", format.format("name", "", ""));
    }
}
