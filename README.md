# Snowchains

[![Build Status](https://img.shields.io/travis/qryxip/snowchains.svg?branch=master&label=windows%20%26%20macos%20%26%20linux)](https://travis-ci.org/qryxip/snowchains)
[![codecov](https://codecov.io/gh/qryxip/snowchains/branch/master/graph/badge.svg)](https://codecov.io/gh/qryxip/snowchains)

Tools for online programming contests.

## Features

- Scrapes sample cases as YAML, TOML, or JSON
- Tests a source file with downloaded sample cases
- Submits a source file
- Downloads source file you have submitted

|                         | Target                                       | `contest` field         | Scrape samples  | Download system tests | Submit          |
| :---------------------- | :------------------------------------------- | :---------------------: | :-------------: | :-------------------: | :-------------: |
| AtCoder                 | `atcoder.jp/contests/{}`                     | `.*`                    | ✓               | ✓                     | ✓               |
| Codeforces              | `codeforces.com/contest/{}`                  | unsigned 64-bit integer | ✓               | N/A                   | ✓               |
| yukicoder (Problems)    | `yukicoder.me/problems/no/{}`                | `no`                    | ✓               | ✓                     | ✓               |
| yukicoder (Contests)    | `yukicoder.me/contests/{}`                   | `(?!no)`                | ✓               | ✓                     | ✓               |

## Instrallation

### GitHub Releases

https://github.com/qryxip/snowchains/releases

### Crates.io

Install [Cargo](https://github.com/rust-lang/cargo) with
[rustup](https://github.com/rust-lang-nursery/rustup.rs),
add `~/.cargo/bin` to your `$PATH`, and

```console
$ cargo install snowchains
```

To update:

```console
$ cargo uninstall snowchains && cargo install snowchains
```

Or

```
$ cargo install cargo-update
$ cargo install-update snowchains
```

## Usage

```
snowchains 0.1.0
Ryo Yamashita <qryxip@gmail.com>
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
    snowchains list-langs [OPTIONS] [problem]
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
    init           Creates a config file ("snowchains.toml")
    switch         Modifies values in a config file
    login          Logges in to a service
    participate    Participates in a contest
    download       Downloads test cases
    restore        Downloads source files you have submitted
    judge          Tests a binary or script
    list-langs     List available languages
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
$ snowchains download --open                  # does not ask your username and password unless they are needed
$ $EDITOR ./snowchains/atcoder/practice/a.yml # add more test cases
$ $EDITOR ./cpp/a.cpp
$ # snowchains judge a
$ snowchains submit a --open                  # executes `judge` command before submitting
```

## Examples

### Config File (snowchains.toml)

```toml
service = "atcoder"
contest = "arc100"
language = "c++"

[console]
cjk = false
# alt_width = 100

[shell]
bash = ["/usr/bin/bash", "-c", "${command}"]
# bash = ["C:/tools/msys64/usr/bin/bash.exe", "-c", "PATH=/usr/bin:$$PATH; ${command}"]
# bash = ["C:/msys64/usr/bin/bash.exe", "-c", "PATH=/usr/bin:$$PATH; ${command}"]
# bash = ["C:/Program Files/Git/usr/bin/bash.exe", "-c", "PATH=/usr/bin:$$PATH; ${command}"]
# ps = ["C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe", "-Command", "${command}"]
# cmd = ["C:/Windows/System32/cmd.exe", "/C", "${command}"]

[testfiles]
path = "${service}/${snake_case(contest)}/tests/${snake_case(problem)}.${extension}"

[session]
timeout = "60s"
silent = false
robots = true
cookies = "~/.local/share/snowchains/${service}"
api_tokens = "~/.local/share/snowchains/api_tokens/${service}.json"
dropbox = false
# dropbox = { auth: "~/.local/share/snowchains/dropbox.json" }

[session.download]
extension = "yml"
text_file_dir = "${service}/${snake_case(contest)}/tests/${snake_case(problem)}"

[judge]
testfile_extensions = ["json", "toml", "yaml", "yml"]
# jobs = 4
display_limit = "1KiB"

[env.atcoder]
CXXFLAGS = "-std=gnu++1y -I/usr/include/boost -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra"
RUST_VERSION = "1.15.1"

[env.codeforces]
CXXFLAGS = "-std=gnu++17 -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra"
RUST_VERSION = "1.31.1"

[env.yukicoder]
CXXFLAGS = "-std=gnu++14 -lm -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra"
RUST_VERSION = "1.30.1"

[env.other]
CXXFLAGS = "-std=gnu++17 -g -fsanitize=undefined -D_GLIBCXX_DEBUG -Wall -Wextra"
RUST_VERSION = "stable"

[[hooks.switch]]
bash = '''
service="$(echo "$SNOWCHAINS_RESULT" | jq -r .new.service)"
contest="$(echo "$SNOWCHAINS_RESULT" | jq -r .new.contest_snake_case)"
if [ ! -d "./$service/$contest/rs" ]; then
  mkdir -p "./$service/$contest" &&
  cargo new --lib --edition 2015 --name "${service}_${contest}" "./$service/$contest/rs" &&
  mkdir "./$service/$contest/rs/src/bin" &&
  rm "./$service/$contest/rs/src/lib.rs"
fi
'''

[[hooks.download]]
bash = '''
if [ "$(echo "$SNOWCHAINS_RESULT" | jq -r .open_in_browser)" = true ]; then
  service="$(echo "$SNOWCHAINS_RESULT" | jq -r .service)"
  echo "$SNOWCHAINS_RESULT" |
    jq -r '
      . as $root
      | .problems
      | map("./" + $root.service + "/" + $root.contest.slug_snake_case + "/rs/src/bin/" + .name_kebab_case + ".rs")
      | join("\n")
    ' | xargs -d \\n -I % -r cp "./templates/rs/src/bin/$service.rs" % &&
  echo "$SNOWCHAINS_RESULT" |
    jq -r '
      . as $root
      | .problems
      | map(["./" + $root.service + "/" + $root.contest.slug_snake_case + "/rs/src/bin/" + .name_kebab_case + ".rs", .test_suite_path])
      | flatten
      | join("\n")
    ' | xargs -d \\n -r emacsclient -n
fi
'''

[tester]
src = "testers/py/${kebab_case(problem)}.py"
run = { bash = './venv/bin/python3 "$SNOWCHAINS_SRC" $SNOWCHAINS_ARGS_JOINED' }
working_directory = "testers/py"

# [tester]
# src = "testers/hs/app/${pascal_case(problem)}.hs"
# bin = "testers/hs/target/${pascal_case(problem)}"
# run = { bash = '"$SNOWCHAINS_BIN" "$SNOWCHAINS_SRC" $SNOWCHAINS_ARGS_JOINED' }
# working_directory = "testers/hs"

[languages.'c++']
src = "${service}/${snake_case(contest)}/cpp/${kebab_case(problem)}.cpp"
bin = "${service}/${snake_case(contest)}/cpp/build/${kebab_case(problem)}"
compile = { bash = 'g++ $CXXFLAGS -o "$SNOWCHAINS_BIN" "$SNOWCHAINS_SRC"' }
run = ["${bin}"]
working_directory = "${service}/${snake_case(contest)}/cpp"

[languages.'c++'.names]
atcoder = "C++14 (GCC 5.4.1)"
codeforces = "GNU G++17 7.3.0"
yukicoder = "C++17(1z） (gcc 8.2.0)"

[languages.rust]
src = "${service}/${snake_case(contest)}/rs/src/bin/${kebab_case(problem)}.rs"
bin = "${service}/${snake_case(contest)}/rs/target/manually/${kebab_case(problem)}"
compile = ["rustc", "+${env:RUST_VERSION}", "-o", "${bin}", "${src}"]
run = ["${bin}"]
working_directory = "${service}/${snake_case(contest)}/rs"

[languages.rust.names]
atcoder = "Rust (1.15.1)"
codeforces = "Rust 1.31.1"
yukicoder = "Rust (1.30.1)"

[languages.go]
src = "${service}/${snake_case(contest)}/go/${kebab_case(problem)}.go"
bin = "${service}/${snake_case(contest)}/go/${kebab_case(problem)}"
compile = ["go", "build", "-o", "${bin}", "${src}"]
run = ["${bin}"]
working_directory = "${service}/${snake_case(contest)}/go"

[languages.go.names]
atcoder = "Go (1.6)"
codeforces = "Go 1.11.4"
yukicoder = "Go (1.11.2)"

[languages.haskell]
src = "${service}/${snake_case(contest)}/hs/app/${pascal_case(problem)}.hs"
bin = "${service}/${snake_case(contest)}/hs/target/${pascal_case(problem)}"
compile = ["stack", "ghc", "--", "-O2", "-o", "${bin}", "${src}"]
run = ["${bin}"]
working_directory = "${service}/${snake_case(contest)}/hs"

[languages.haskell.names]
atcoder = "Haskell (GHC 7.10.3)"
codeforces = "Haskell GHC 7.8.3 (2014.2.0.0)"
yukicoder = "Haskell (8.6.2)"

[languages.bash]
src = "${service}/${snake_case(contest)}/bash/${kebab_case(problem)}.bash"
run = ["bash", "${src}"]
working_directory = "${service}/${snake_case(contest)}/bash"

[languages.bash.names]
atcoder = "Bash (GNU bash v4.3.11)"
yukicoder = "Bash (Bash 4.2.46)"

[languages.python3]
src = "${service}/${snake_case(contest)}/py/${kebab_case(problem)}.py"
run = ["../../../venvs/python3_${service}/bin/python3", "${src}"]
working_directory = "${service}/${snake_case(contest)}/py"

[languages.python3.names]
atcoder = "Python3 (3.4.3)"
codeforces = "Python 3.7.2"
yukicoder = "Python3 (3.7.1 + numpy 1.14.5 + scipy 1.1.0)"

[languages.pypy3]
src = "${service}/${snake_case(contest)}/py/${kebab_case(problem)}.py"
run = ["../../../venvs/pypy3_${service}/bin/python3", "${src}"]
working_directory = "${service}/${snake_case(contest)}/py"

[languages.pypy3.names]
atcoder = "PyPy3 (2.4.0)"
codeforces = "PyPy 3.5 (6.0.0)"
yukicoder = "PyPy3 (6.0.0)"

[languages.java]
src = "${service}/${snake_case(contest)}/java/src/main/java/${pascal_case(problem)}.java"
transpiled = "${service}/${snake_case(contest)}/java/build/replaced/${lower_case(pascal_case(problem))}/src/Main.java"
bin = "${service}/${snake_case(contest)}/java/build/replaced/${lower_case(pascal_case(problem))}/classes/Main.class"
transpile = { bash = 'cat "$SNOWCHAINS_SRC" | sed -r "s/class\s+$SNOWCHAINS_PROBLEM_PASCAL_CASE/class Main/g" > "$SNOWCHAINS_TRANSPILED"' }
compile = ["javac", "-d", "./build/replaced/${lower_case(pascal_case(problem))}/classes", "${transpiled}"]
run = ["java", "-classpath", "./build/replaced/${lower_case(pascal_case(problem))}/classes", "Main"]
working_directory = "${service}/${snake_case(contest)}/java"

[languages.java.names]
atcoder = "Java8 (OpenJDK 1.8.0)"
codeforces = "Java 1.8.0_162"
yukicoder = "Java8 (openjdk 1.8.0.191)"

[languages.scala]
src = "${service}/${snake_case(contest)}/scala/src/main/scala/${pascal_case(problem)}.scala"
transpiled = "${service}/${snake_case(contest)}/scala/target/replaced/${lower_case(pascal_case(problem))}/src/Main.scala"
bin = "${service}/${snake_case(contest)}/scala/target/replaced/${lower_case(pascal_case(problem))}/classes/Main.class"
transpile = { bash = 'cat "$SNOWCHAINS_SRC" | sed -r "s/object\s+$SNOWCHAINS_PROBLEM_PASCAL_CASE/object Main/g" > "$SNOWCHAINS_TRANSPILED"' }
compile = ["scalac", "-optimise", "-d", "./target/replaced/${lower_case(pascal_case(problem))}/classes", "${transpiled}"]
run = ["scala", "-classpath", "./target/replaced/${lower_case(pascal_case(problem))}/classes", "Main"]
working_directory = "${service}/${snake_case(contest)}/scala"

[languages.scala.names]
atcoder = "Scala (2.11.7)"
codeforces = "Scala 2.12.8"
yukicoder = "Scala(Beta) (2.12.7)"

[languages.'c#']
src = "${service}/${snake_case(contest)}/cs/${pascal_case(problem)}/${pascal_case(problem)}.cs"
bin = "${service}/${snake_case(contest)}/cs/${pascal_case(problem)}/bin/Release/${pascal_case(problem)}.exe"
compile = ["mcs", "-o+", "-r:System.Numerics", "-out:${bin}", "${src}"]
run = ["mono", "${bin}"]
working_directory = "${service}/${snake_case(contest)}/cs"

[languages.'c#'.names]
atcoder = "C# (Mono 4.6.2.0)"
codeforces = "C# Mono 5.18"
yukicoder = "C#(mono) (mono 5.16.0.187)"

# [languages.'c#']
# src = "${service}/${snake_case(contest)}/cs/${pascal_case(problem)}/${pascal_case(problem)}.cs"
# bin = "${service}/${snake_case(contest)}/cs/${pascal_case(problem)}/bin/Release/${pascal_case(problem)}.exe"
# compile = ["csc", "/o+", "/r:System.Numerics", "/out:${bin}", "${src}"]
# run = ["${bin}"]
# crlf_to_lf: true
# working_directory = "${service}/${snake_case(contest)}/cs"
#
# [languages.'c#'.names]
# atcoder = "C# (Mono 4.6.2.0)"
# codeforces = "C# Mono 5.18"
# yukicoder = "C# (csc 2.8.2.62916)"

[languages.text]
src = "${service}/${snake_case(contest)}/txt/${snake_case(problem)}.txt"
run = ["cat", "${src}"]
working_directory = "${service}/${snake_case(contest)}/txt"

[languages.text.names]
atcoder = "Text (cat)"
yukicoder = "Text (cat 8.22)"
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

## License

Dual-licensed under [MIT](https://opensource.org/licenses/MIT) or [Apache-2.0](http://www.apache.org/licenses/LICENSE-2.0).
