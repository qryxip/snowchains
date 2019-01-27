# Snowchains


[![Build Status](https://travis-ci.org/wariuni/snowchains.svg?branch=master)](https://travis-ci.org/wariuni/snowchains)
[![codecov](https://codecov.io/gh/wariuni/snowchains/branch/master/graph/badge.svg)](https://codecov.io/gh/wariuni/snowchains)

Tools for online programming contests.

## Features

- Scrapes sample cases as YAML, TOML, or JSON
- Tests a source file with downloaded sample cases
- Submits a source file
- Downloads source file you have submitted

|                         | Target                                       | "contest" attribute | Scrape samples  | Download system tests | Submit          |
| :---------------------- | :------------------------------------------- | :------------------ | :-------------: | :-------------------: | :-------------: |
| AtCoder                 | `atcoder.jp/contests/{}`                     | `.*`                | ✓               | ✓                     | ✓               |
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
    snowchains <w|switch|c|checkout> [OPTIONS]
    snowchains <l|login> [OPTIONS] <service>
    snowchains <p|participate> [OPTIONS] <service> <contest>
    snowchains <d|download> [FLAGS] [OPTIONS]
    snowchains <r|restore> [OPTIONS]
    snowchains <j|judge|t|test> [FLAGS] [OPTIONS] <problem>
    snowchains <s|submit> [FLAGS] [OPTIONS] <problem>
    snowchains show num-cases [OPTIONS] <problem> <extension>
    snowchains show timelimit-millis [OPTIONS] <problem> <nth>
    snowchains show in [OPTIONS] <problem> <nth>
    snowchains show accepts [OPTIONS] <problem> <nth>
    snowchains modify timelimit [OPTIONS] <problem> <nth> [timelimit]
    snowchains modify append [OPTIONS] <problem> <extensioon> <input> [output]
    snowchains modify match [OPTIONS] <problem> <extension> <match>

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
$ snowchains download --open                   # does not ask your username and password unless they are needed
$ $EDITOR ./snowchains/atcoder/practice/a.yaml # add more test cases
$ $EDITOR ./cpp/a.cpp
$ # snowchains judge a
$ snowchains submit a --open                   # executes `judge` command before submitting
```

## Examples

### Config File (snowchains.yaml)

```yaml
---
service: atcoder # "atcoder", "yukicoder", "other"
contest: arc100
language: c++

console:
  cjk: false
  # alt_width: 100

shell:
  bash: [/usr/bin/bash, -c, $command]
  # bash: ["C:/tools/msys64/usr/bin/bash.exe", -c, "PATH=/usr/bin:$$PATH; $command"]
  # bash: ["C:/msys64/usr/bin/bash.exe", -c, "PATH=/usr/bin:$$PATH; $command"]
  # bash: ["C:/Program Files/Git/usr/bin/bash.exe", -c, "PATH=/usr/bin:$$PATH; $command"]
  # ps: ["C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe", -Command, $command]
  # cmd: ["C:/Windows/System32/cmd.exe", /C, $command]

testfile_path: $service/$contest/tests/{snake}.$extension

session:
  timeout: 60s
  silent: false
  cookies: ~/.local/share/snowchains/$service
  dropbox: false
  # dropbox:
  #   auth: ~/.local/share/snowchains/dropbox.json
  download:
    extension: yaml
    text_file_dir: $service/$contest/tests/{{snake}}

judge:
  testfile_extensions: [json, toml, yaml, yml]
  # jobs: 4
  display_limit: 1KiB

env:
  atcoder:
    CXXFLAGS: -std=gnu++1y -I/usr/include/boost -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra
    RUST_VERSION: 1.15.1
  yukicoder:
    CXXFLAGS: -std=gnu++14 -lm -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra
    RUST_VERSION: 1.30.1
  other:
    CXXFLAGS: -std=gnu++17 -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra
    RUST_VERSION: stable

hooks:
  switch:
    - bash: |
        service="$(echo "$SNOWCHAINS_RESULT" | jq -r .new.service)"
        contest="$(echo "$SNOWCHAINS_RESULT" | jq -r .new.contest)"
        if [ ! -d "./$service/$contest/rs" ]; then
          mkdir -p "./$service/$contest" &&
          cargo new --lib --edition 2015 --name "$contest" "./$service/$contest/rs" &&
          mkdir "./$service/$contest/rs/src/bin" &&
          rm "./$service/$contest/rs/src/lib.rs"
        fi
  download:
    - bash: |
        if [ "$(echo "$SNOWCHAINS_RESULT" | jq -r .open_in_browser)" = true ]; then
          service="$(echo "$SNOWCHAINS_RESULT" | jq -r .service)"
          contest="$(echo "$SNOWCHAINS_RESULT" | jq -r .contest.slug)"
          echo "$SNOWCHAINS_RESULT" |
            jq -r --arg service "$service" --arg contest "$contest" '.problems | map("./" + $service + "/" + $contest + "/rs/src/bin/" + .name_kebab + ".rs") | join("\n")' |
            xargs -d \\n -I % -r cp "./templates/rs/src/bin/$service.rs" % &&
          echo "$SNOWCHAINS_RESULT" |
            jq -r --arg service "$service" --arg contest "$contest" '.problems | map(["./" + $service + "/" + $contest + "/rs/src/bin/" + .name_kebab + ".rs", .test_suite_path]) | flatten | join("\n")' |
            xargs -d \\n -r emacsclient -n
        fi

tester:
  src: testers/py/{kebab}.py
  run:
    command:
      bash: ./venv/bin/python3 "$SNOWCHAINS_SRC" $SNOWCHAINS_ARGS_JOINED
    # crlf_to_lf: false
  working_directory: testers/py
  # src: testers/hs/app/{Pascal}.hs
  # compile:
  #   bin: testers/hs/target/{Pascal}
  #   command: [stack, ghc, --, -O2, -o, $bin, $src]
  # run:
  #   command:
  #     bash: "$SNOWCHAINS_BIN" $SNOWCHAINS_ARGS_JOINED
  #   # crlf_to_lf: false
  # working_directory: testers/hs

languages:
  c++:
    src: $service/$contest/cpp/{kebab}.cpp     # source file to test and to submit
    compile:                                   # optional
      bin: $service/$contest/cpp/build/{kebab}
      command:
        bash: g++ $CXXFLAGS -o "$SNOWCHAINS_BIN" "$SNOWCHAINS_SRC"
    run:
      command: [$bin]
      # crlf_to_lf: false
    working_directory: $service/$contest/cpp # default: "."
    language_ids:                              # optional
      atcoder: 3003                            # "C++14 (GCC x.x.x)"
      yukicoder: cpp14                         # "C++14 (gcc x.x.x)"
  rust:
    src: $service/$contest/rs/src/bin/{kebab}.rs
    compile:
      bin: $service/$contest/rs/target/manually/{kebab}
      command: [rustc, +$RUST_VERSION, -o, $bin, $src]
    run:
      command: [$bin]
      # crlf_to_lf: false
    working_directory: $service/$contest/rs
    # language_ids:
    #   atcoder: 3504   # "Rust (x.x.x)"
    #   yukicoder: rust # "Rust (x.x.x)"
  haskell:
    src: $service/$contest/hs/app/{Pascal}.hs
    compile:
      bin: $service/$contest/hs/target/{Pascal}
      command: [stack, ghc, --, -O2, -o, $bin, $src]
    run:
      command: [$bin]
      # crlf_to_lf: false
    working_directory: $service/$contest/hs
    # language_ids:
    #   atcoder: 3014      # "Haskell (GHC x.x.x)"
    #   yukicoder: haskell # "Haskell (x.x.x)"
  python3:
    src: $service/$contest/py/{kebab}.py
    run:
      command: [../../../venvs/python3_$service/bin/python3, $src]
      # crlf_to_lf: false
    working_directory: $service/$contest/py
    language_ids:
      atcoder: 3023      # "Python3 (3.x.x)"
      yukicoder: python3 # "Python3 (3.x.x + numpy x.x.x + scipy x.x.x)"
  pypy3:
    src: $service/$contest/py/{kebab}.py
    run:
      command: [../../../venvs/pypy3_$service/bin/python3, $src]
    working_directory: $service/$contest/py
    language_ids:
      atcoder: 3510
      yukicoder: pypy3
  java:
    src: $service/$contest/java/src/main/java/{Pascal}.java
    transpile:
      transpiled: $service/$contest/java/build/replaced/{lower}/src/Main.java
      command:
        bash: cat "$SNOWCHAINS_SRC" | sed -r "s/class\s+$SNOWCHAINS_PROBLEM_PASCAL/class Main/g" > "$SNOWCHAINS_TRANSPILED"
        # ps: cat ${env:SNOWCHAINS_SRC} | % { $_ -replace "class\s+${env:SNOWCHAINS_PROBLEM_PASCAL}", "class Main" } | sc ${env:SNOWCHAINS_TRANSPILED}
    compile:
      bin: $service/$contest/java/build/replaced/{lower}/classes/Main.class
      command: [javac, -d, './build/replaced/{lower}/classes', $transpiled]
    run:
      command: [java, -classpath, './build/replaced/{lower}/classes', Main]
      # crlf_to_lf: false
    working_directory: $service/$contest/java
    language_ids:
      atcoder: 3016      # "Java8 (OpenJDK 1.8.x)"
      # yukicoder: java8 # "Java8 (openjdk 1.8.x.x)"
  # c#:
  #   src: $service/$contest/cs/{Pascal}/{Pascal}.cs
  #   compile:
  #     bin: $service/$contest/cs/{Pascal}/bin/Release/{Pascal}.exe
  #     command: [csc, /o+, '/r:System.Numerics', '/out:$bin', $src]
  #   run:
  #     command: [$bin]
  #     crlf_to_lf: true
  #   working_directory: $service/$contest/cs
  #   language_ids:
  #     # atcoder: 3006   # "C# (Mono x.x.x.x)"
  #     yukicoder: csharp # "C# (csc x.x.x.x)"
  c#:
    src: $service/$contest/cs/{Pascal}/{Pascal}.cs
    compile:
      bin: $service/$contest/cs/{Pascal}/bin/Release/{Pascal}.exe
      command: [mcs, -o+, '-r:System.Numerics', '-out:$bin', $src]
    run:
      command: [mono, $bin]
      # crlf_to_lf: false
    working_directory: $service/$contest/cs
    language_ids:
      # atcoder: 3006        # "C# (Mono x.x.x.x)"
      yukicoder: csharp_mono # "C#(mono) (mono x.x.x.x)"
```

### Test file

- [x] YAML
- [x] TOML
- [x] JSON

#### Batch (one input, one output)

<https://atcoder.jp/contests/practice/tasks/practice_1>

```yaml
---
type: batch       # "batch", "interactive", or "unsubmittable"
timelimit: 2000ms # optional
match: exact      # "any", "exact", or "float"

cases:
  - name: Sample 1
    in: |
      1
      2 3
      test
    out: |
      6 test
  - name: Sample 2
    in: |
      72
      128 256
      myonmyon
    out: |
      456 myonmyon
  # "name" and "out" are optional
  - in: |
      1000
      1000 1000
      oooooooooooooo
```

```toml
type = 'batch'
timelimit = '2000ms'
match = 'exact'

[[cases]]
name = 'Sample 1'
in = '''
1
2 3
test
'''
out = '''
6 test
'''

[[cases]]
name = 'Sample 2'
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

<https://atcoder.jp/contests/tricky/tasks/tricky_2>

```yaml
---
type: batch
timelimit: 2000ms
match:
  float:
    absolute_error: 1e-9
    relative_error: 1e-9

cases:
  - name "Sample 1"
    in: |
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
type = 'batch'
timelimit = '2000ms'

[match.float]
absolute_error = 1e-9
relative_error = 1e-9

[[cases]]
name = 'Sample 1'
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

#### Interactive

<https://atcoder.jp/contests/practice/tasks/practice_2>

```yaml
---
type: interactive
timelimit: 2000ms

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
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           RIO
import qualified RIO.ByteString     as B
import           RIO.List
import           RIO.List.Partial
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Printf

main :: IO ()
main = do
  RIO.hSetBuffering stdout LineBuffering
  bs <- (!! 0) <$> getArgs
  let n                   = length bs
      q                   = if n == 5 then 7 else 100 :: Int
      reply c1 c2         = B.putStr (if weight c1 < weight c2 then "<\n" else ">\n")
      judge a | a == bs   = exitSuccess
              | otherwise = die "wrong"
      weight c            = fromMaybe (error "out of bounds") (c `elemIndex` bs)
  printf "%d %d\n" n q
  forM_ [1..q] $ \_ -> words <$> getLine >>= \case
    ["?", [c1], [c2]] -> reply c1 c2
    ["!", a]          -> judge a
    _                 -> error "invalid"
```
