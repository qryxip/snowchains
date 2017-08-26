# Snowchains

Tools for online programming contests.

Works on
- [x] Linux
- [x] Windows (If you are using mintty, use winpty when you login.)
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
$ snowchains login <service>
$ snowchains participate <contest>
$ snowchains download <service> <contest> <directory-to-save-test-files> -e <extension>
$ $EDITOR <path-to-test-file> # Add more test cases
$ snowchains judge <path-to-test-file> <path-to-target> [args]...
```

### Rust (Cargo)

```console
$ $EDITOR <crate-root>/src/bin/<problem-name>.rs                  # target
$ $EDITOR <crate-root>/snowchains/<problem-name>.[toml|yaml|json] # test cases
$ snowchains judge-cargo snowchains/<problem-name>.[toml|yaml|json] <problem-name>
```

## Test cases

### Download

- [x] AtCoder (abcxxx, arcxxx, agcxxx, chokudai_sxxx)

```console
$ snowchains login <service>
$ snowchains participate <contest>
$ snowchains download <service> <contest> <some-directory>
```

### Format

Here's exmaples for [Welcome to AtCoder](http://practice.contest.atcoder.jp/tasks/practice_1).

#### TOML

```toml
timelimit = 2000 # Optional


# Possible types: 
# * Integer
# * Float
# * String (a '\n' is appended automatically if missing)
# * Array of [Integer|Float|String] (in TOML, arrays cannot contain different types of data)

[[cases]]
expected = "6 test"
input = "1\n2 3\ntest"

[[cases]]
expected = ['456 myonmyon']
input = ['72', '128 256', 'myonmyon']

[[cases]]
input = ["1000", "1000 1000", "ooooooooooooooooooooooooooooo"] # "expected" is optional
```

#### YAML
```yaml
timelimit: 2000

cases:
  - expected: "6 test"
    input: "1\n2 3\ntest"
  - expected: ['456 myonmyon']
    input: [72, '128 256', 'myonmyon']
  - input: [1000, "1000 1000", "ooooooooooooooooooooooooooooo"]
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
             (term-run "snowchains" "*snowchains*" "judge-cargo"
                       (format "%s/%s.%s" my-rust--snowchains-dir problem-name my-rust--snowchains-ext)
                       problem-name)))
          ((string-match "^.*/src/bin/\\(.+\\)\\.rs$" file-path)
           (cargo-process-run-bin (match-string 1 file-path)))
          (t
           (cargo-process-run)))))

(defconst my-rust--snowchains-crate "rust-contest")
(defconst my-rust--snowchains-dir "snowchains")
(defconst my-rust--snowchains-ext "toml")
```
