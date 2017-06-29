# Snowchains

Tools for online programming contests.

## Instrallation

Install [Cargo](https://github.com/rust-lang/cargo) with
[rustup](https://github.com/rust-lang-nursery/rustup.rs),
add `~/.cargo/bin` to your `$PATH`, and

```console
$ cargo install --git https://github.com/wariuni/snowchains
```

## Usage

### Rust (Cargo)

```console
$ $EDITOR <crate root>/src/bin/<problem name>.rs        # target
$ $EDITOR <crate root>/cases/<problem name>.[toml|json] # test cases
$ snowchains cargo judge <problem name> <problem name>.[toml|json]
```

## Test cases

Here's exmaples for [Welcome to AtCoder](http://practice.contest.atcoder.jp/tasks/practice_1).

### TOML

```toml
timeout = 2000


# Possible types: 
# * Integer
# * Float
# * String (a '\n' is appended automatically if missing)
# * Array of [Integer|Float|String] (in TOML, arrays cannot contain different types of data)

[[cases]]
expected = '6 test'
input = '1\n2 3\ntest'

[[cases]]
expected = ['456 myonmyon']
input = ['72', '128 256', 'myonmyon']
```

### JSON

```json
{
  "timeout": 2000,
  "cases": [
    {
      "expected": "6 test",
      "input": "1\n2 3\ntest"
    },
    {
      "expected": ["456 myonmyon"],
      "input": [72, "128 256", "myonmyon"]
    }
  ]
}
```

## Editor Integrations

### Rust (Cargo) + Emacs

```lisp
(require 'cargo)
(require 'term-run)

(defvar my-rust--snowchains-crate "rust-contest")
(defvar my-rust--snowchains-dir "snowchains")
(defvar my-rust--snowchains-ext "toml")

(defun my-rust-run ()
  (interactive)
  (let ((file-path (buffer-file-name)))
    (cond ((string-match (format "^.*/%s/src/bin/\\(.+\\)\\.rs$" my-rust--snowchains-crate) file-path)
           (let ((buffer (get-buffer "*snowchains*")))
             (when buffer
               (with-current-buffer buffer
                 (erase-buffer))))
           (let ((problem-name (match-string 1 file-path)))
             (term-run "snowchains" "*snowchains*" "cargo" "judge" problem-name
                       (format "%s/%s.%s" my-rust--snowchains-dir problem-name my-rust--snowchains-ext))))
          ((string-match "^.*/src/bin/\\(.+\\)\\.rs$" file-path)
           (cargo-process-run-bin (match-string 1 file-path)))
          (t
           (cargo-process-run)))))
```
