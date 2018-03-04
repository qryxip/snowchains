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
$ snowchains init ./
$ snowchains switch <service> <contest>                  # e.g. ("atcoderbeta", "agc001")
$ snowchains download (--open-browser)                   # The username and password required when not yet logged-in
$ $EDITOR ./snowchains/<service>/<contest>/<target>.yaml # Add more test cases
$ snowchains judge <target> (<language>)
$ snowchains submit <target> (<language>) (--open-browser) (--skip-judging) (--force)
```

## Config File (snowchains.yaml)

```yaml
# Example
---
service: "atcoderbeta"                                 # optional
contest: "chokudai_s001"                               # optional
testsuites: "snowchains/$service/$contest/"            # default: 〃
extension_on_downloading: "yaml"                       # default: 〃
extensions_on_judging: ["json", "toml", "yaml", "yml"] # default: 〃

atcoder:
  default_language: "c++"
  variables:
    "cxx_flags": "-std=c++14 -O2 -Wall -Wextra"
    "rust_version": "1.15.1"

hackerrank:
  default_language: "c++"
  variables:
    "cxx_flags": "-std=c++14 -O2 -Wall -Wextra -lm"
    "rust_version": "1.21.0"

# test files: <testsuite>/<target-name>.<extension> for <extension> in each <extensions_on_judging>
# source:     <<src> % <target-name>>
# binary:     <<bin> % <target-name>>
# e.g.
# "cc/{}.cc" % "problem-a"          ⊦ <the directory which has snowchains.yaml>/cc/problem-a.cc
# "csharp/{C}/{C}.cs" % "problem-a" ⊦ <the directory which has snowchains.yaml>/csharp/ProblemA/ProblemA.cs
languages:
  - name: "c++"
    src: "cc/{}.cc"
    compile:                                 # optional
      bin: "cc/build/{}"
      command: "g++ $cxx_flags -o $bin $src"
      working_directory: "cc/"               # default: ""
    run:
      command: "$bin"                        # default: "$bin"
      working_directory: "cc/"               # default: ""
    language_ids:                            # optional
      atcoder: 3003
  - name: "rust"
    src: "rs/src/bin/{}.rs"
    compile:
      bin: "rs/target/release/{}"
      command: "rustc +$rust_version -o $bin $src"
      working_directory: "rs/"
    run:
      command: "$bin"
      working_directory: "rs/"
    language_ids:
      atcoder: 3504
  - name: "haskell"
    src: "hs/src/{C}.hs"
    compile:
      bin: "hs/target/{C}"
      command: "stack ghc -- -O2 -o $bin $src"
      working_directory: "hs/"
    run:
      command: "$bin"
      working_directory: "hs/"
    language_ids:
      atcoder: 3014
  - name: "python3"
    src: "py/{}.py"
    compile: ~
    run:
      command: "./venv/bin/python3 $src"
      working_directory: "py/"
    language_ids:
      atcoder: 3023
  - name: "java"
    src: "java/src/main/java/{C}.java"
    compile:
      bin: "java/build/classes/java/main/{C}.class"
      command: "javac -d ./build/classes/java/main/ $src"
      working_directory: "java/"
    run:
      command: "java -classpath ./build/classes/java/main/{C}"
      working_directory: "java/"
    language_ids:
      atcoder: 3016
  - # Windows
    name: "c#"
    src: "cs/{C}/{C}.cs"
    compile:
      bin: "cs/{C}/bin/Release/{C}.exe"
      command: "csc /o+ /r:System.Numerics /out:$bin $src"
      working_directory: "cs/"
    run:
      command: "$bin"
      working_directory: "cs/"
    language_ids:
      atcoder: 3006
  - # Unix
    name: "c#"
    src: "cs/{C}/{C}.cs"
    compile:
      bin: "cs/{C}/bin/Release/{C}.exe"
      command: "mcs -o+ -r:System.Numerics -out:$bin $src"
      working_directory: "cs/"
    run:
      command: "mono $bin"
      working_directory: "cs/"
    language_ids:
      atcoder: 3006
```

Or simply:

```yaml
---
service: "atcoderbeta"
contest: "chokudai_s001"
testsuites: "snowchains/$service/$contest/"
extension_on_downloading: "yaml"
extensions_on_judging: ["json", "toml", "yaml", "yml"]

atcoder:
  default_language: "c++"
  variables:
    "cxx_flags": "-std=c++14 -O2 -Wall -Wextra"

hackerrank:
  default_language: "c++"
  variables:
    "cxx_flags": "-std=c++14 -O2 -Wall -Wextra -lm"

languages:
  - name: "c++"
    src: "{}.cc"
    compile:
      bin: "build/{}"
      command: "g++ $cxx_flags -o $bin $src"
      working_directory: ""
    run:
      command: "$bin"
      working_directory: ""
    language_ids:
      atcoder: 3003
```

## Test Suite

### Download

- [x] atcoder (http://{}.contest.atcoder.jp)
- [x] atcoderbeta (https://beta.atcoder.jp/contests/{})
- [x] hackerrank (https://www.hackerrank.com/contests/{})

```console
$ #snowchains login <service>
$ #snowchains participate <service> <contest>
$ snowchains download (--open-browser)
```

### Format

Here are exmaples for [Task A of Welcome to AtCoder](https://beta.atcoder.jp/contests/practice/tasks/practice_1).

#### YAML

```yaml
---
type: "simple"  # "simple" or "interactive"
timelimit: 2000 # Optional

# Possible types of "expected" and "input":
# * Integer
# * Float
# * String (a '\n' is appended automatically if missing)
# * Array of [Integer|Float|String] (in TOML, arrays cannot contain different types of data)
cases:
  - expected: "6 test"
    input: "1\n2 3\ntest"
  - timelimit: 10 # Override "timelimit"
    expected: ['456 myonmyon']
    input: [72, '128 256', 'myonmyon']
  - input: [1000, "1000 1000", "ooooooooooooooooooooooooooooo"] # "expected" is optional
```

#### TOML

```toml
type = "simple"
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
  "type": "simple",
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

#### Interactive

Here is a exmaple for [Task B of Welcome to AtCoder](https://beta.atcoder.jp/contests/practice/tasks/practice_2).

```yaml
---
type: "interactive"
timelimit: 2000

cases:
  - tester: "../checkers/py/atcoder_practice_b_checker.py ABCDE"
  - tester: "../checkers/py/atcoder_practice_b_checker.py EDCBA"
  - tester: "../checkers/hs/src/AtcoderPracticeBChecker.hs ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  - tester: "../checkers/hs/src/AtcoderPracticeBChecker.hs ZYXWVUTSRQPONMLKJIHGFEDCBA"
```

```python
#!/usr/bin/env python3
import re
import sys


def main() -> None:
    bs = sys.argv[1]
    n = len(bs)
    q = 7 if n == 5 else 100

    def judge(a):
        if a == bs:
            sys.exit(0)
        else:
            print('Wrong answer', file=sys.stderr)
            sys.exit(1)

    def reply(c1, c2):
        print('<' if weight_by(c1) < weight_by(c2) else '>', flush=True)

    def weight_by(c):
        try:
            return bs.index(c)
        except ValueError:
            raise RuntimeError(f'No such ball: {c}')

    print(f'{n} {q}', flush=True)
    for _ in range(q):
        ts = re.split('[ \n]', sys.stdin.readline())
        if len(ts) == 4 and ts[0] == '?':
            reply(ts[1], ts[2])
        elif len(ts) == 3 and ts[0] == '!':
            judge(ts[1])
        else:
            raise RuntimeError('Invalid input')
    else:
        ts = re.split('[ \n]', sys.stdin.readline())
        if len(ts) == 3 and ts[0] == '!':
            judge(ts[1])
        raise RuntimeError(
            'Expected "! <answer>" (you have run out of queries)')


if __name__ == '__main__':
    main()
```

```haskell
#!/usr/bin/env runghc

{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad      (forM_)
import Data.List          (elemIndex)
import Data.Maybe         (fromMaybe)
import System.Environment (getArgs)
import System.Exit        (die, exitSuccess)
import Text.Printf        (printf)

main :: IO ()
main = do
  bs <- (!! 0) <$> getArgs
  let n                   = length bs
      q                   = if n == 5 then 7 else 100 :: Int
      reply c1 c2         = putStrLn $ if weightBy c1 < weightBy c2 then "<" else ">"
      judge a | a == bs   = exitSuccess
              | otherwise = die "Wrong answer"
      weightBy c          = fromMaybe (error (printf "No such ball: %c" c)) (c `elemIndex` bs)
  printf "%d %d\n" n q
  forM_ [1..q] $ \_ -> words <$> getLine >>= \case
    ["?", [c1], [c2]] -> reply c1 c2
    ["!", a]          -> judge a
    _                 -> error "Invalid format"
  words <$> getLine >>= \case
    ["!", a] -> judge a
    _        -> error "Expected \"! <answer>\" (You have run out of queries)"
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
             (term-run "snowchains" "*snowchains*" "submit" problem-name)))
          ((string-match "^.*/src/bin/\\(.+\\)\\.rs$" file-path)
           (cargo-process-run-bin (match-string 1 file-path)))
          (t
           (cargo-process-run)))))

(defun my-rust-test ()
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
           (cargo-process--start "Test Bin" (concat "cargo test --bin " (match-string 1 file-path))))
          (t
           (cargo-process-test)))))

(defconst my-rust--snowchains-crate "contest/rs")
```
