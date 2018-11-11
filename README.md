# Snowchains

[![Build Status](https://travis-ci.org/wariuni/snowchains.svg?branch=master)](https://travis-ci.org/wariuni/snowchains)
[![Build status](https://ci.appveyor.com/api/projects/status/hfc4x704uufkb2sh/branch/master?svg=true)](https://ci.appveyor.com/project/wariuni/snowchains/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/wariuni/snowchains/badge.svg?branch=master)](https://coveralls.io/github/wariuni/snowchains?branch=master)

Tools for online programming contests.

## Features

- Scrapes sample cases as YAML, TOML, or JSON
- Tests a source file with downloaded sample cases
- Submits a source file
- Downloads source file you have submitted

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

```
snowchains 0.23.0
Ryo Yamashita <wariuni@gmail.com>
Tools for online programming contests

USAGE:
    snowchains <i|init> [OPTIONS] [directory]
    snowchains <w|switch> [OPTIONS]
    snowchains <l|login> [OPTIONS] <service>
    snowchains <p|participate> [OPTIONS] <service> <contest>
    snowchains <d|download> [FLAGS] [OPTIONS]
    snowchains <r|restore> [OPTIONS]
    snowchains <j|judge> [FLAGS] [OPTIONS] <problem>
    snowchains <s|submit> [FLAGS] [OPTIONS] <problem>
    snowchains show num-cases [OPTIONS] <problem> <extension>
    snowchains show timelimit-millis [OPTIONS] <problem> <nth>
    snowchains show in [OPTIONS] <problem> <nth>
    snowchains show accepts [OPTIONS] <problem> <nth>
    snowchains modify timelimit [OPTIONS] <problem> <nth> [timelimit]
    snowchains modify append [OPTIONS] <problem> <extensioon> <input> [output]

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

SUBCOMMANDS:
    init           Creates a config file ("snowchains.yaml")
    switch         Changes attribute values of a config file
    login          Logges in to a service
    participate    Participates in a contest
    download       Downloads test cases
    restore        Downloads source files you have submitted
    judge          Tests a binary or script
    submit         Submits a source file
    show           Prints information
    modify         Modifies values in a config file or test files
    help           Prints this message or the help of the given subcommand(s)
```

```console
$ snowchains init ./
$ snowchains switch --service atcoder --contest practice --language c++
$ # snowchains login atcoder
$ # snowchains participate atcoder practice
$ snowchains download --open-browser           # does not ask your username and password unless they are needed
$ $EDITOR ./snowchains/atcoder/practice/a.yaml # add more test cases
$ $EDITOR ./cpp/a.cpp
$ # snowchains judge a
$ snowchains submit a --open-browser           # executes `judge` command before submitting
```

## Config File (snowchains.yaml)

```yaml
# Example
---
service: atcoder # "atcoder", "hackerrank", "yukicoder", "other"
contest: arc100
language: c++    # Priorities: <command line argument>, `service._.language`, `language`

console:
  cjk: false

session:
  timeout: 60s
  cookies: ~/.local/share/snowchains/$service

shell: [$SHELL, -c] # Used if `languages._.[compile|run].command` is a single string.

testfiles:
  jobs: 4
  path: snowchains/$service/$contest/{snake}.$extension
  forall: [json, toml, yaml, yml, zip]
  scrape: yaml
  zip:
    timelimit: 2000ms
    match: exact
    entries:
      # AtCoder
      - in:
          entry: /\Ain/([a-z0-9_\-]+)\.txt\z/
          match_group: 1
          crlf_to_lf: true
        out:
          entry: /\Aout/([a-z0-9_\-]+)\.txt\z/
          match_group: 1
          crlf_to_lf: true
        sort: [dictionary]
      # HackerRank
      - in:
          entry: /\Ainput/input([0-9]+)\.txt\z/
          match_group: 1
          crlf_to_lf: true
        out:
          entry: /\Aoutput/output([0-9]+)\.txt\z/
          match_group: 1
          crlf_to_lf: true
        sort: [number]
      # yukicoder
      - in:
          entry: /\Atest_in/([a-z0-9_]+)\.txt\z/
          match_group: 1
          crlf_to_lf: true
        out:
          entry: /\Atest_out/([a-z0-9_]+)\.txt\z/
          match_group: 1
          crlf_to_lf: true
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
    src: testers/py/test-{{kebab}}.py
    run:
      command: [./venv/bin/python3, $src, $1, $2, $3, $4, $5, $6, $7, $8, $9]
      working_directory: testers/py
      # crlf_to_lf: false
  haskell:
    src: testers/hs/app/Test{{Pascal}}.hs
    compile:
      bin: testers/hs/target/Test{{Pascal}}
      command: [stack, ghc, --, -O2, -o, $bin, $src]
      working_directory: testers/hs
    run:
      command: [$bin, $1, $2, $3, $4, $5, $6, $7, $8, $9]
      working_directory: testers/hs
      # crlf_to_lf: false

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
      # crlf_to_lf: false
    language_ids:            # optional
      atcoder: 3003          # "C++14 (GCC x.x.x)"
      yukicoder: cpp14       # "C++14 (gcc x.x.x)"
  rust:
    src: rs/src/bin/{kebab}.rs
    compile:
      bin: rs/target/release/{kebab}
      command: [rustc, +$rust_version, -o, $bin, $src]
      working_directory: rs
    run:
      command: [$bin]
      working_directory: rs
      # crlf_to_lf: false
    language_ids:
      atcoder: 3504
      yukicoder: rust
  haskell:
    src: hs/app/{Pascal}.hs
    compile:
      bin: hs/target/{Pascal}
      command: [stack, ghc, --, -O2, -o, $bin, $src]
      working_directory: hs
    run:
      command: [$bin]
      working_directory: hs
      # crlf_to_lf: false
    language_ids:
      atcoder: 3014
      yukicoder: haskell
  python3:
    src: py/{kebab}.py
    run:
      command: [./venv/bin/python3, $src]
      working_directory: py
      # crlf_to_lf: false
    language_ids:
      atcoder: 3023      # "Python3 (3.x.x)"
      yukicoder: python3 # "Python3 (3.x.x + numpy x.x.x)"
  java:
    src: java/src/main/java/{Pascal}.java
    compile:
      bin: java/build/classes/java/main/{Pascal}.class
      command: [javac, -d, ./build/classes/java/main, $src]
      working_directory: java
    run:
      command: [java, -classpath, ./build/classes/java/main, '{Pascal}']
      working_directory: java
      # crlf_to_lf: false
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
  #     crlf_to_lf: true
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
      # crlf_to_lf: false
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
type: simple      # "simple", "interactive", or "unsubmittable"
timelimit: 2000ms # optional
match: exact      # "any", "exact", or "float"

cases:
  - in: |
      1
      2 3
      test
    out: |
      6 test
  - in: |
      72
      128 256
      myonmyon
    out: |
      456 myonmyon
  # "out" is optional
  - in: |
      1000
      1000 1000
      oooooooooooooo
```

```toml
type = 'simple'
timelimit = '2000ms'
match = 'exact'

[[cases]]
in = '''
1
2 3
test
'''
out = '''
6 test
'''

[[cases]]
in = '''
72
128 256
myonmyon
'''
out = '''
456 myonmyon
'''

[[cases]]
in = '''
1000
1000 1000
oooooooooooooo
'''
```

<https://beta.atcoder.jp/contests/tricky/tasks/tricky_2>

```yaml
---
type: simple
timelimit: 2000ms
match:
  float:
    absolute_error: 1e-9
    relative_error: 1e-9

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

```toml
type = 'simple'
timelimit = '2000ms'

[match.float]
absolute_error = 1e-9
relative_error = 1e-9

[[cases]]
in = '''
3
1 -3 2
-10 30 -20
100 -300 200
'''
out = '''
2 1.000 2.000
2 1.000 2.000
2 1.000 2.000
'''
```

### Interactive

<https://beta.atcoder.jp/contests/practice/tasks/practice_2>

```yaml
---
type: interactive
timelimit: 2000ms
tester: python3

each_args:
  - [ABCDE]
  - [EDCBA]
  - [ABCDEFGHIJKLMNOPQRSTUVWXYZ]
  - [ZYXWVUTSRQPONMLKJIHGFEDCBA]
```

```python
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
