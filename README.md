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
# source:    <<src> % <target-name (converted to CamlCase if <camelize_src>)>>
# binary:    <<bin> % <target-name (converted to CamlCase if <camelize_bin>)>>
languages:
  -
    name: "c"
    type: "build"
    camelize_src: false   # default: false
    camelize_bin: false   # default: false
    src: "c/%.c"
    bin: "c/build/%"
    working_dir: "c/"     # default: ""
    compile: "gcc -std=c11 -O2 -o build/% %.c"
    atcoder_lang_id: 3002 # see HTML source or open the inspector, and search by "option"
  -
    name: "c++"
    type: "build"
    camelize_src: false # default: false
    camelize_bin: false # default: false
    src: "cc/%.cc"
    bin: "cc/build/%"
    working_dir: "cc/"  # default: ""
    build: "ninja"      # optional
    atcoder_lang_id: 3003
  -
    name: "rust"
    type: "build"
    camelize_src: false
    camelize_bin: false
    src: "rust/src/bin/%.rs"
    bin: "rust/target/release/%"
    working_dir: "rust/"
    build: "cargo build --release" # or ["cargo", "build", "--release"]
    atcoder_lang_id: 3504
  -
    name: "haskell"
    type: "vm"
    camelize_src: true                # default: true
    camelize_bin: false               # default: true
    src: "haskell/src/%.hs"
    build_working_dir: "haskell/src/" # default: ""
    runtime_working_dir: "haskell/"   # default: ""
    build: "stack install"
    runtime: "stack run %"
    atcoder_lang_id: 3014
  -
    # Windows
    name: "c#"
    type: "build"
    camelize_src: true
    camelize_bin: true
    src: "csharp/%/%.cs"
    bin: "csharp/%/bin/Release/%.exe"
    working_dir: "csharp/"
    build: "MSBuild .\\csharp.sln /p:Configuration=Release"
    atcoder_lang_id: 3006
  -
    # *nix
    name: "mono"
    type: "vm"
    camelize_src: true # default: true
    camelize_bin: true # default: true
    src: "csharp/%/%.cs"
    build_working_dir: "csharp/"
    runtime_working_dir: "csharp/"
    build: "msbuild.exe ./csharp.sln /p:Configuration=Release"
    runtime: "mono ./%/bin/Release/%.exe"
    atcoder_lang_id: 3006
  -
    name: "java"
    type: "vm"
    camelize_src: true
    camelize_bin: true
    src: "java/src/main/java/%.java"
    build_working_dir: "java/"
    runtime_working_dir: "java/build/classes/java/main/"
    build: "gradle --daemon compileJava"
    runtime: "java %"
    atcoder_lang_id: 3016 # class names are replaced with "Main" in AtCoder
  -
    name: "scala"
    type: "vm"
    camelize_src: true
    camelize_bin: true
    src: "scala/src/main/%.scala"
    build_working_dir: "scala/"
    runtime_working_dir: "scala/target/scala-2.12/classes/"
    build: "sbt compile"
    runtime: "scala %"
    atcoder_lang_id: 3025
  -
    name: "python3"
    type: "script"
    camelize: false      # default: false
    src: "python/%.py"
    working_dir: "python/"
    runtime: "python3 %" # or shebang
    atcoder_lang_id: 3023
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
    type: "build"
    camelize_src: false
    camelize_bin: false
    src: "cc/%.cc"
    bin: "cc/build/%"
    working_dir: "cc/"
    build: "ninja"
    atcoder_lang_id: 3003
```

## Test Cases

### Download

- [x] atcoder (e.g. abcxxx, arcxxx, agcxxx, chokudai_sxxx)
- [x] atcoder-beta (〃)
- [x] hackerrank

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
