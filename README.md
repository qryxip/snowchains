# Snowchains

[![Build Status](https://travis-ci.org/wariuni/snowchains.svg?branch=master)](https://travis-ci.org/wariuni/snowchains)
[![Build status](https://ci.appveyor.com/api/projects/status/hfc4x704uufkb2sh/branch/master?svg=true)](https://ci.appveyor.com/project/wariuni/snowchains/branch/master)

Tools for online programming contests.

## Features

- Scrapes sample cases as YAML, TOML, or JSON
- Tests a source code with downloaded sample cases
- Submits a source code
- Downloads source codes you have submitted

|                         | Target                                       | "contest" attribute | Scrape samples  | Download system tests | Submit          |
| :---------------------- | :------------------------------------------- | :------------------ | :-------------: | :-------------------: | :-------------: |
| AtCoder Beta            | `beta.atcoder.jp/contests/{}`                | `.*`                | ✓               | ⨉                     | ✓               |
| HackerRank (Problems)   | `www.hackerrank.com/challenges/{}`           | `master`            | ✓               | ✓                     | Not implemented |
| HackerRank (Contests)   | `www.hackerrank.com/contests/{}/challenges/` | `(?!master)`        | ✓               | ✓                     | Not implemented |
| yukicoder (Problems)    | `yukicoder.me/problems/no/{}`                | `no`                | ✓               | ✓                     | ✓               |
| yukicoder (Contests)    | `yukicoder.me/contests/{}`                   | `(?!no)`            | ✓               | ✓                     | ✓               |

## Instrallation

Install [Cargo](https://github.com/rust-lang/cargo) with
[rustup](https://github.com/rust-lang-nursery/rustup.rs),
add `~/.cargo/bin` to your `$PATH`, and

```console
$ cargo [+stable] install --git https://github.com/wariuni/snowchains
```

To update:

```console
$ cargo [+stable] install-update -ag
```

## Usage

```console
$ snowchains --help
$ snowchains <i|init> ./
$ snowchains <w|switch> [-s <service>] [-c <contest>] [-l <language>]                       # e.g. "-s atcoder -c arc100 -l c++"
$ snowchains <d|download> [-b|--open-browser] [-s <service>] [-c <contest>] [-p <problems>] # Does not ask username and password unless they are needed
$ $EDITOR ./snowchains/<service>/<contest>/<problem>.yaml                                   # Add more test cases
$ snowchains <j|judge> [--force-compile] [-s <service>] [-c <contest>] [-l <language>] <problem>
$ snowchains <s|submit> [-b|--open-browser] [--force-compile] [-j|--skip-judging] [-d|--skip-checking-duplication] \
                        [-s <service>] [-c <contest>] [-l <language>] <problem>
```

## Config File (snowchains.yaml)

```yaml
# Example
---
service: atcoder # "atcoder", "hackerrank", "yukicoder", "other"
contest: arc100
language: c++    # Priorities: <command line argument>, `service._.language`, `language`

# console:
#   color: 256color # "8color", "16color", "256color"
#   cjk: true

session:
  timeout: 10
  cookies: ~/.local/share/snowchains/$service

shell: [$SHELL, -c] # Used if `languages._.[compile|run].command` is a single string.

testfiles:
  directory: snowchains/$service/$contest/
  forall: [json, toml, yaml, yml, zip]
  scrape: yaml
  zip:
    timelimit: 2000
    match: exact
    entries:
      # AtCoder
      - in:
          entry: /\Ain/([a-z0-9_\-]+)\.txt\z/
          match_group: 1
        out:
          entry: /\Aout/([a-z0-9_\-]+)\.txt\z/
          match_group: 1
        sort: [dictionary]
      # HackerRank
      - in:
          entry: /\Ainput/input([0-9]+)\.txt\z/
          match_group: 1
        out:
          entry: /\Aoutput/output([0-9]+)\.txt\z/
          match_group: 1
        sort: [number]
      # yukicoder
      - in:
          entry: /\Atest_in/([a-z0-9_]+)\.txt\z/
          match_group: 1
        out:
          entry: /\Atest_out/([a-z0-9_]+)\.txt\z/
          match_group: 1
        sort: [dictionary, number]

services:
  atcoder:
    # language: c++
    variables:
      rust_version: 1.15.1
      java_class: Main
  hackerrank:
    # language: c++
    variables:
      rust_version: 1.21.0
      java_class: Main
  yukicoder:
    # language: c++
    variables:
      rust_version: 1.28.0
      java_class: Main
  other:
    # language: c++
    variables:
      rust_version: stable

interactive:
  python3:
    src: py/{kebab}-tester.py
    run:
      command: python3 -- $src $*
      working_directory: py
  haskell:
    src: hs/src/{Pascal}Tester.hs
    compile:
      bin: hs/target/{Pascal}Tester
      command: [stack, ghc, --, -O2, -o, $bin, $src]
      working_directory: hs
    run:
      command: $bin $*
      working_directory: hs

# test files: <testsuite>/<problem>.[json|toml|yaml|yml|zip]
# source:     <<src> % <problem>>
# binary:     <<bin> % <problem>>
#
# Common:
#   "plain"                        => "plain";
#   "{}"          % "problem name" => "problem name"
#   "{lower}"     % "problem name" => "problem name"
#   "{UPPER}"     % "problem name" => "PROBLEM NAME"
#   "{kebab}"     % "problem name" => "problem-name"
#   "{snake}"     % "problem name" => "problem_name"
#   "{SCREAMING}" % "problem name" => "PROBLEM_NAME"
#   "{mixed}"     % "problem name" => "problemName"
#   "{Pascal}"    % "problem name" => "ProblemName"
#   "{Title}"     % "problem name" => "Problem Name"
#   "$ENVVAR"                      => "<value of ENVVAR>"
#   "$${{}}"                       => "${}"
# Path:
#   "", "."                                    => "./"
#   "relative", "./relative"                   => "./relative"
#   "/absolute"                                => "/absolute"
#   "cpp/{snake}.cpp"         % "problem name" => "./cpp/problem_name.cpp"
#   "cs/{Pascal}/{Pascal}.cs" % "problem name" => "./cs/ProblemName/ProblemName.cs"
# Command:
#   "$src" => "<path to the source file>"
#   "$bin" => "<path to the binary file>"
languages:
  c++:
    src: cpp/{kebab}.cpp     # source file to test and to submit
    compile:                 # optional
      bin: cpp/build/{kebab}
      command: [g++, -std=c++14, -Wall, -Wextra, -g, -fsanitize=undefined, -D_GLIBCXX_DEBUG, -o, $bin, $src]
      working_directory: cpp # default: "."
    run:
      command: [$bin]
      working_directory: cpp # default: "."
    language_ids:            # optional
      atcoder: 3003          # "C++14 (GCC x.x.x)"
      yukicoder: cpp14       # "C++14 (gcc x.x.x)"
  rust:
    src: rs$rust_version/src/bin/{kebab}.rs
    compile:
      bin: rs$rust_version/target/release/{kebab}
      command: [rustc, +$rust_version, -o, $bin, $src]
      working_directory: rs$rust_version
    run:
      command: [$bin]
      working_directory: rs$rust_version
    language_ids:
      atcoder: 3504
      yukicoder: rust
  haskell:
    src: hs/src/{Pascal}.hs
    compile:
      bin: hs/target/{Pascal}
      command: [stack, ghc, --, -O2, -o, $bin, $src]
      working_directory: hs
    run:
      command: [$bin]
      working_directory: hs
    language_ids:
      atcoder: 3014
      yukicoder: haskell
  python3:
    src: py/{kebab}.py
    run:
      command: [./venv/bin/python3, $src]
      working_directory: py
    language_ids:
      atcoder: 3023      # "Python3 (x.x.x)"
      yukicoder: python3 # "Python3 (x.x.x + numpy x.x.x)"
  java:
    src: java/src/main/java/{Pascal}.java
    compile:
      bin: java/build/classes/java/main/{Pascal}.class
      command: [javac, -d, ./build/classes/java/main, $src]
      working_directory: java
    run:
      command: [java, -classpath, ./build/classes/java/main, '{Pascal}']
      working_directory: java
    replace:
      regex: /^\s*public(\s+final)?\s+class\s+([A-Z][a-zA-Z0-9_]*).*$/
      regex_group: 2
      local: '{Pascal}'
      submit: $java_class
      all_matched: false
    language_ids:
      atcoder: 3016
      yukicoder: java8
  # c#:
  #   src: cs/{Pascal}/{Pascal}.cs
  #   compile:
  #     bin: cs/{Pascal}/bin/Release/{Pascal}.exe
  #     command: [csc, /o+, '/r:System.Numerics', '/out:$bin', $src]
  #     working_directory: cs
  #   run:
  #     command: [$bin]
  #     working_directory: cs
  #   language_ids:
  #     atcoder: 3006     # "C# (Mono x.x.x.x)"
  #     yukicoder: csharp # "C# (csc x.x.x.x)"
  c#:
    src: cs/{Pascal}/{Pascal}.cs
    compile:
      bin: cs/{Pascal}/bin/Release/{Pascal}.exe
      command: [mcs, -o+, '-r:System.Numerics', '-out:$bin', $src]
      working_directory: cs
    run:
      command: [mono, $bin]
      working_directory: cs
    language_ids:
      atcoder: 3006          # "C# (Mono x.x.x.x)"
      yukicoder: csharp_mono # "C#(mono) (mono x.x.x.x)"
```

## Test file

- [x] YAML
- [x] TOML
- [x] JSON

### Simple (one input, one output)

<https://beta.atcoder.jp/contests/practice/tasks/practice_1>

```yaml
---
type: simple    # "simple" or "interactive"
timelimit: 2000 # Optional
match: exact    # "exact" or "float"

# Possible types of "in" and "out":
# * Integer
# * Float
# * String (a '\n' is appended automatically if missing)
# * Array of [Integer|Float|String] (in TOML, arrays cannot contain different types of data)
cases:
  - in: |
      1
      2 3
      test
    out: 6 test
  - in: |
      72
      128 256
      myonmyon
    out: 456 myonmyon
  # "out" is optional
  - in: |
      1000
      1000 1000
      oooooooooooooo
```

<https://beta.atcoder.jp/contests/tricky/tasks/tricky_2>

```yaml
---
type: simple
timelimit: 2000
match:
  float:
    absolute_error: 1E-9
    relative_error: 1E-9

cases:
  - in: |
      3
      1 -3 2
      -10 30 -20
      100 -300 200
    out: |
      2 1.000 2.000
      2 1.000 2.000
      2 1.000 2.000
```

### Interactive

<https://beta.atcoder.jp/contests/practice/tasks/practice_2>

```yaml
---
type: interactive
timelimit: 2000
tester: python3

each_args:
  - [ABCDE]
  - [EDCBA]
  - [ABCDEFGHIJKLMNOPQRSTUVWXYZ]
  - [ZYXWVUTSRQPONMLKJIHGFEDCBA]
```

```python
#!/usr/bin/env python3
import re
import sys


def main() -> None:
    bs = sys.argv[1]
    n = len(bs)
    q = 7 if n == 5 else 100

    def reply(c1, c2):
        print('<' if bs.index(c1) < bs.index(c2) else '>', flush=True)

    def judge(a):
        if a == bs:
            sys.exit(0)
        else:
            print('wrong', file=sys.stderr)
            sys.exit(1)

    print(f'{n} {q}', flush=True)
    for _ in range(q):
        ts = re.split(r'[ \n]', sys.stdin.readline())
        if len(ts) == 4 and ts[0] == '?':
            reply(ts[1], ts[2])
        elif len(ts) == 3 and ts[0] == '!':
            judge(ts[1])
        else:
            raise RuntimeError('invalid')
    else:
        ts = re.split(r'[ \n]', sys.stdin.readline())
        if len(ts) == 3 and ts[0] == '!':
            judge(ts[1])
        raise RuntimeError('answer me')


if __name__ == '__main__':
    main()
```

```haskell
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad      (forM_)
import Data.List          (elemIndex)
import Data.Maybe         (fromMaybe)
import System.Environment (getArgs)
import System.Exit        (die, exitSuccess)
import System.IO          (hFlush, stdout)
import Text.Printf        (printf)

main :: IO ()
main = do
  bs <- (!! 0) <$> getArgs
  let n                   = length bs
      q                   = if n == 5 then 7 else 100 :: Int
      reply c1 c2         = putStrLn (if weightBy c1 < weightBy c2 then "<" else ">") >> hFlush stdout
      judge a | a == bs   = exitSuccess
              | otherwise = die "wrong"
      weightBy c          = fromMaybe (error "out of bounds") (c `elemIndex` bs)
  printf "%d %d\n" n q >> hFlush stdout
  forM_ [1..q] $ \_ -> words <$> getLine >>= \case
    ["?", [c1], [c2]] -> reply c1 c2
    ["!", a]          -> judge a
    _                 -> error "invalid"
  words <$> getLine >>= \case
    ["!", a] -> judge a
    _        -> error "answer me"
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
             (term-run "snowchains" "*snowchains*" "submit" problem-name "-l" "rust")))
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
             (term-run "snowchains" "*snowchains*" "judge" problem-name "-l" "rust")))
          ((string-match "^.*/src/bin/\\(.+\\)\\.rs$" file-path)
           (cargo-process--start "Test Bin" (concat "test --bin " (match-string 1 file-path))))
          (t
           (cargo-process-test)))))

(defconst my-rust--snowchains-crate "contest/rs")
```
