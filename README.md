# Snowchains

Tools for online programming contests.

Works on
- [x] Linux
- [x] Windows (If you are using MSYS2, use winpty when you login.)
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
$ cd <project-dir>
$ snowchains init-config <language> ./
$ snowchains set service <service>
$ snowchains set contest <contest>
$ snowchains download # The username and password required when not yet signed-in
$ $EDITOR <project>/snowchains/<target>.yml # Add more test cases
$ snowchains judge <target>
```

## Config File (snowchains.yml)

```yaml
# Example
---
service: "atcoder-beta"    # optional
contest: "agc001"          # optional
testcases: "./snowchains/" # default: "./snowchains/"
testcase_extension: "yml"  # default: "yml"
targets:
  # source file: <src>/<name>.<extension>
  # binary:      <bin>/<name (the first letter is capitalized if <capitalize>)>(.[class|exe])
  # test file:   <testcases>/<name>.<testcase_extension>
  -
    name: "a"
    project: "python3"
  -
    name: "b"
    project: "java"
  -
    name: "c"
    project: "rust"
  -
    name: "d"
    project: "c"
  -
    name: "e"
    project: "c++"
  -
    name: "f"
    project: "c++"
projects:
  -
    name: "c"
    type: "build"
    src: "./c/"
    bin: "./c/build/"
    extension: "c"
    build: "ninja"
  -
    name: "c++"
    type: "build"
    src: "./cc/"
    bin: "./cc/build/"
    extension: "cc"
    build: "ninja"
  -
    name: "rust"
    type: "build"
    src: "./rust/src/bin/"
    bin: "./rust/target/release/"
    extension: "rs"
    capitalize: false # default: false
    build: ["cargo", "build", "--release"] # optional
  -
    name: "java"
    type: "java"
    src: "./java/src/main/java/"
    bin: "./java/build/classes/java/main/"
    build: ["gradle", "build", "--daemon"] # also optional
  -
    name: "python3"
    type: "script"
    src: "./python/"
    extension: "py"
    runtime: "python3" # or shebang
```

Or simply:

```yaml
---
service: "atcoder"
contest: "agc001"
targets:
  -
    name: "a"
    project: "c++"
  -
    name: "b"
    project: "c++"
  -
    name: "c"
    project: "c++"
  -
    name: "d"
    project: "c++"
  -
    name: "e"
    project: "c++"
  -
    name: "f"
    project: "c++"
projects:
  -
    name: "c++"
    type: "build"
    src: "./"
    bin: "./build/"
    extension: "cc"
    build: "ninja"
```

## Test Cases

### Download

- [x] atcoder (abcxxx, arcxxx, agcxxx, chokudai_sxxx)
- [x] atcoder-beta (〃)

```console
$ #snowchains login <service>
$ #snowchains participate <service> <contest>
$ cd <wherever under the project>
$ snowchains download
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
             (term-run "snowchains" "*snowchains*" "judge" problem-name)))
          ((string-match "^.*/src/bin/\\(.+\\)\\.rs$" file-path)
           (cargo-process-run-bin (match-string 1 file-path)))
          (t
           (cargo-process-run)))))

(defconst my-rust--snowchains-crate "contest/rust")
```
