# Snowchains

[![CI](https://github.com/qryxip/snowchains/workflows/CI/badge.svg)](https://github.com/qryxip/snowchains/actions?workflow=CI)
[![Crates.io](https://img.shields.io/crates/v/snowchains.svg)](https://crates.io/crates/snowchains)
[![Crates.io](https://img.shields.io/crates/l/snowchains.svg)](https://crates.io/crates/snowchains)
<!-- [![codecov](https://codecov.io/gh/qryxip/snowchains/branch/master/graph/badge.svg)](https://codecov.io/gh/qryxip/snowchains/branch/master) -->
<!-- https://github.com/srijs/deps.rs/pull/33 -->
<!-- [![dependency status](https://deps.rs/repo/github/qryxip/snowchains/status.svg)](https://deps.rs/repo/github/qryxip/snowchains) -->

Tools for online programming contests.

## Features

- Scrapes sample cases as YAML
- Tests a source file with downloaded sample cases
- Submits a source file
- Downloads source file you have submitted

|            | Register to a contest | Get sample test cases | Get system test cases | Submit             | Get submissions | Watch submissions  |
| :--------: | :-------------------: | :-------------------: | :-------------------: | :----------------: | :-------------: | :----------------: |
| AtCoder    | :heavy_check_mark:    | :heavy_check_mark:    | :heavy_check_mark:    | :heavy_check_mark: | :x:             | :heavy_check_mark: |
| Codeforces | :x:                   | :heavy_check_mark:    | N/A                   | :heavy_check_mark: | :x:             | :x:                |
| yukicoder  | N/A                   | :heavy_check_mark:    | :heavy_check_mark:    | :heavy_check_mark: | :x:             | :x:                |

## Instrallation

### GitHub Releases

<https://github.com/qryxip/snowchains/releases>

### `cargo install` (Crates.io)

```console
$ cargo install snowchains
```

### `cargo install` (GitHub)

```console
$ cargo install --git https://github.com/qryxip/snowchains
```

## Usage

```console
$ snowchains -h
snowchains 0.4.0
Ryo Yamashita <qryxip@gmail.com>
Tools for online programming contests

USAGE:
    snowchains <SUBCOMMAND>

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

SUBCOMMANDS:
    init           Create a new config file [aliases: i]
    login          Logges in to a service [aliases: l]
    participate    Participates in a contest
    retrieve       Retrieves data [aliases: r]
    download       Alias for `retrieve testcases` [aliases: d]
    watch          Watches data [aliases: w]
    judge          Tests code [aliases: j, test, t]
    submit         Submits code [aliases: s]
    xtask          Runs a custom subcommand written in the config file [aliases: x]
    help           Prints this message or the help of the given subcommand(s)
$ snowchains r -h
snowchains-retrieve 0.4.0
Ryo Yamashita <qryxip@gmail.com>
Retrieves data

USAGE:
    snowchains retrieve <SUBCOMMAND>

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

SUBCOMMANDS:
    languages    Retrieves list of languages [aliases: l]
    testcases    Retrieves test cases [aliases: t]
    help         Prints this message or the help of the given subcommand(s)
$ snowchains w -h
snowchains-watch 0.4.0
Ryo Yamashita <qryxip@gmail.com>
Watches data

USAGE:
    snowchains watch <SUBCOMMAND>

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

SUBCOMMANDS:
    submissions    Watches your submissions [aliases: s]
    help           Prints this message or the help of the given subcommand(s)
```

```console
$ snowchains i .
Wrote `/home/ryo/src/local/a/snowchains.dhall`
$ snowchains x setup -h
usage: snowchains xtask setup [-h] [-p [PROBLEM [PROBLEM ...]]] service contest {cpp,rs} {code,vim,emacs}

positional arguments:
  service
  contest
  {cpp,rs}
  {code,vim,emacs}

optional arguments:
  -h, --help            show this help message and exit
  -p [PROBLEM [PROBLEM ...]], --problems [PROBLEM [PROBLEM ...]]
$ echo "$ATCODER_USERNAME\n$ATCODER_PASSWORD" | snowchains x setup atcoder practice cpp code
GET https://atcoder.jp/contests/practice/tasks ... 404 Not Found
GET https://atcoder.jp/contests/practice ... 200 OK
Username: Password: GET https://atcoder.jp/login ... 200 OK
POST https://atcoder.jp/login ... 302 Found
GET https://atcoder.jp/settings ... 200 OK
GET https://atcoder.jp/contests/practice ... 200 OK
GET https://atcoder.jp/contests/practice/tasks ... 200 OK
GET https://atcoder.jp/contests/practice/tasks_print ... 200 OK
A: Saved to /home/ryo/src/local/a/.snowchains/tests/atcoder/practice/a.yml (2 test cases)
B: Saved to /home/ryo/src/local/a/.snowchains/tests/atcoder/practice/b.yml (interactive problem)
Opening https://atcoder.jp/contests/practice/tasks/practice_1 ...
Opening https://atcoder.jp/contests/practice/tasks/practice_2 ...
```

![Screenshot](https://user-images.githubusercontent.com/14125495/88492107-56435300-cfe3-11ea-92fe-4ce950ebf1bc.png)

![Record](https://user-images.githubusercontent.com/14125495/88492159-b0dcaf00-cfe3-11ea-8daa-c1eb56d293d6.gif)

## License

Dual-licensed under [MIT](https://opensource.org/licenses/MIT) or [Apache-2.0](http://www.apache.org/licenses/LICENSE-2.0).
