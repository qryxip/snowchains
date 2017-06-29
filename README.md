# Snowchains

Tools for online programming contests.

## Usage

### Rust

```
$ $EDITOR <crate root>/src/bin/<problem name>.rs        # target
$ $EDITOR <crate root>/cases/<problem name>.[toml|json] # test cases
$ snowchains cargo judge <problem name> <problem name>.[toml|json]
```

If you're writing Rust with Emacs, use a function like this:
```lisp
(require 'cargo)
(require 'term-run)

(defun my-rust-run ()
  (interactive)
  (let ((file-path (buffer-file-name)))
    (cond ((string-match ".*/rust-contest/src/bin/\\(.+\\)\\.rs" file-path)
           (let ((buffer (get-buffer "*snowchains*")))
             (when buffer
               (with-current-buffer buffer
                 (erase-buffer))))
           (let ((problem (match-string 1 file-path)))
             (term-run "snowchains" "*snowchains*" "cargo" "judge" problem (concat problem ".toml"))))
          ((string-match ".*/src/bin/\\(.+\\)\\.rs" file-path)
           (cargo-process-run-bin (match-string 1 file-path)))
          (t
           (cargo-process-run)))))
```

## Test cases

Exmaples for [practice contest in AtCoder](http://practice.contest.atcoder.jp/tasks/practice_1):

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
