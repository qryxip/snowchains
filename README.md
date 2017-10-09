# Snowchains

Tools for online programming contests.

Works on
- [x] Linux
- [x] Windows
- [ ] OS X (Probably works.)

## Instrallation

Install [Cargo](https://github.com/rust-lang/cargo) with
[rustup](https://github.com/rust-lang-nursery/rustup.rs),
add `~/.cargo/bin` to your `$PATH`, and

```console
$ cargo install --git https://github.com/wariuni/snowchains
```

## Usage

```console
$ snowchains init-config <language> ./
$ snowchains set service <service>     # "atcoder", "atcoder-beta", "hackerrank"
$ snowchains set contest <contest>     # e.g. "agc001"
$ snowchains download (--open-browser) # The username and password required when not yet logged-in
$ $EDITOR ./snowchains/<target>.yml    # Add more test cases
$ snowchains judge <target> (<language>)
$ snowchains submit <target> (<language>) (--open-browser) (--skip-judging) (--force)
```

## Config File (snowchains.yml)

```yaml
# Example
---
service: "atcoder-beta"   # optional
contest: "chokudai_s001"  # optional
testcases: "snowchains/"  # default: "./snowchains/"
testcase_extension: "yml" # default: "yml"
default_lang: "c++"

# test file: <testcases>/<target-name>.<testcase_extension>
# source:    <<src> % <target-name>>
# binary:    <<bin> % <target-name>>
# e.g.
# "cc/{}.cc" % "problem-a"          ⊦ <the directory which has snowchains.yml>/"cc/problem-a.cc"
# "csharp/{C}/{C}.cs" % "problem-a" ⊦ <the directory which has snowchains.yml>/"csharp/ProblemA/ProblemA.cs"
languages:
  -
    name: "c++"
    src: "cc/{}.cc"
    bin: "cc/build/{}"                         # optional
    compile: "g++ -std=c++14 -O2 -o $bin $src" # optional
    run: "$bin"                                # default: "$bin"
    compilation_working_dir: "cc/"             # default: ""
    runtime_working_dir: "cc/"                 # default: ""
    atcoder_lang_id: 3003                      # see the HTML source or open DevTools, and search by "option"
  -
    name: "rust"
    src: "rust/src/bin/{}.rs"
    bin: "rust/target/release/{}"
    compile: "rustc -O -o $bin $src"
    run: "$bin"
    compilation_working_dir: "rust/"
    runtime_working_dir: "rust/"
    atcoder_lang_id: 3504
  -
    name: "python3"
    src: "python/{}.py"
    bin: ~
    compile: ~
    run: "python3 $src"
    compilation_working_dir: ""
    runtime_working_dir: "python/" 
    atcoder_lang_id: 3023
  -
    name: "java"
    src: "java/src/main/java/{C}.java"
    bin: "java/build/classes/java/main/{C}.class"
    compile: "javac -d ./build/classes/java/main/ $src"
    run: "java -classpath ./build/classes/java/main/ {C}"
    compilation_working_dir: "java/"
    runtime_working_dir: "java/"
    atcoder_lang_id: 3016 # a main class name is replaced with "Main" in AtCoder
  -
    # Windows
    name: "c#"
    src: "csharp/{C}/{C}.cs"
    bin: "csharp/{C}/bin/Release/{C}.exe"
    compile: "csc /o+ /r:System.Numerics /out:$bin $src"
    run: "$bin"
    compilation_working_dir: "csharp/"
    runtime_working_dir: "csharp/"
    atcoder_lang_id: 3006
  -
    # *nix
    name: "c#"
    src: "csharp/{C}/{C}.cs"
    bin: "csharp/{C}/bin/Release/{C}.exe"
    compile: "mcs -o+ -r:System.Numerics -out:$bin $src"
    run: "mono ./%/bin/Release/%.exe"
    compilation_working_dir: "csharp/"
    runtime_working_dir: "csharp/"
    atcoder_lang_id: 3006
```

Or simply:

```yaml
---
service: "atcoder"
contest: "chokuda_s001"
default_lang: "c++"
languages:
  -
    name: "c++"
    src: "cc/{}.cc"
    bin: "cc/build/{}"
    compile "g++ -std=c++14 -O2 -o $bin $src"
    run: "$bin"
    compilation_working_dir: "cc/"
    runtime_working_dir: "cc/"
    atcoder_lang_id: 3003
```

## Test Cases

### Download

- [x] atcoder (http://{}/contest.atcoder.jp)
- [x] atcoder-beta (https://beta.atcoder.jp/contests/{})
- [x] hackerrank (https://www.hackerrank.com/contests/{})

```console
$ #snowchains login <service>
$ #snowchains participate <service> <contest>
$ snowchains download (--open-browser)
```

### Format

Here's exmaples for [Welcome to AtCoder](https://beta.atcoder.jp/contests/practice/tasks/practice_1).

#### YAML
```yaml
---
timelimit: 2000 # Optional

# Possible types of "expected" and "input":
# * Integer
# * Float
# * String (a '\n' is appended automatically if missing)
# * Array of [Integer|Float|String] (in TOML, arrays cannot contain different types of data)
cases:
  -
    expected: "6 test"
    input: "1\n2 3\ntest"
  -
    timelimit: 10 # Override "timelimit"
    expected: ['456 myonmyon']
    input: [72, '128 256', 'myonmyon']
  -
    input: [1000, "1000 1000", "ooooooooooooooooooooooooooooo"] # "expected" is optional
```

#### TOML

```toml
timelimit = 2000

[[cases]]
expected = "6 test"
input = "1\n2 3\ntest"

[[cases]]
timelimit = 10
expected = ['456 myonmyon']
input = ['72', '128 256', 'myonmyon']

[[cases]]
input = ["1000", "1000 1000", "ooooooooooooooooooooooooooooo"]
```

#### JSON

```json
{
  "timelimit": 2000,
  "cases": [
    {
      "expected": "6 test",
      "input": "1\n2 3\ntest"
    },
    {
      "timelimit": 10,
      "expected": ["456 myonmyon"],
      "input": [72, "128 256", "myonmyon"]
    },
    {
      "input": [1000, "1000 1000", "ooooooooooooooooooooooooooooo"]
    }
  ]
}
```

## Editor Integrations

### Rust (Cargo) + Emacs

```lisp
(require 'cargo)
(require 'term-run)

(defun my-rust-run ()
  (interactive)
  (let ((file-path (buffer-file-name)))
    (cond ((string-match (format "^.*/%s/src/bin/\\(.+\\)\\.rs$" my-rust--snowchains-crate) file-path)
           (let ((buffer (get-buffer "*snowchains*")))
             (when buffer
               (with-current-buffer buffer
                 (erase-buffer))))
           (let ((problem-name (match-string 1 file-path)))
             (term-run "snowchains" "*snowchains*" "judge" problem-name "rust")))
          ((string-match "^.*/src/bin/\\(.+\\)\\.rs$" file-path)
           (cargo-process-run-bin (match-string 1 file-path)))
          (t
           (cargo-process-run)))))

(defconst my-rust--snowchains-crate "contest/rust")
```
