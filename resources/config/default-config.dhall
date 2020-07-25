let List/index =
      https://prelude.dhall-lang.org/v17.0.0/List/index sha256:e657b55ecae4d899465c3032cb1a64c6aa6dc2aa3034204f3c15ce5c96c03e63

let Snowchains =
      https://raw.githubusercontent.com/qryxip/snowchains/rewrite-almost-everything/resources/config/schema/Snowchains/package.dhall sha256:6acc68ed8830bdabc6beb6bc39d4348886bdcf2d78a102b3f1c9338b07f84389

let Service/lowercase = Snowchains.Service/lowercase

let CaseConvertedText/kebabCase = Snowchains.CaseConvertedText/kebabCase

let Script/new = Snowchains.Script/new

let Command = Snowchains.Command

let Mode/lowercase = Snowchains.Mode/lowercase

let Target = Snowchains.Target

let Compile = Snowchains.Compile

let Language = Snowchains.Language

let Config = Snowchains.Config

let bash = Script/new "bash" "bash"

let python = Script/new "python" "py"

in    { detectServiceFromRelativePathSegments = List/index 0 Text
      , detectContestFromRelativePathSegments = List/index 1 Text
      , detectProblemFromRelativePathSegments = λ(_ : List Text) → None Text
      , detectLanguageFromRelativePathSegments = List/index 2 Text
      , languages =
          λ(target : Target) →
            let service = target.service

            let contest =
                  merge
                    { Some = CaseConvertedText/kebabCase, None = "problems" }
                    target.contest

            let problem = target.problem

            let mode = target.mode

            let cpp
                : Language
                = let problem = problem.kebabCase

                  let src =
                        "${Service/lowercase
                             service}/${contest}/cpp/${problem}.cpp"

                  let bin =
                        "${Service/lowercase
                             service}/${contest}/cpp/target/${problem}"

                  in  { src
                      , transpile = None Compile
                      , compile = Some
                        { command =
                            Command.Args
                              (   [ "g++", src, "-o", bin, "-Wall", "-Wextra" ]
                                # merge
                                    { Atcoder =
                                      [ "-std=gnu++17"
                                      , "-DONLINE_JUDGE"
                                      , "-I/usr/include/boost"
                                      ]
                                    , Codeforces = [ "-std=gnu++17" ]
                                    , Yukicoder =
                                      [ "-std=c++1z"
                                      , "-lm"
                                      , "-I/usr/include/boost"
                                      ]
                                    }
                                    service
                                # merge
                                    { Debug =
                                      [ "-g"
                                      , "-fsanitize=undefined"
                                      , "-D_GLIBCXX_DEBUG"
                                      ]
                                    , Release = [ "-O2" ]
                                    }
                                    mode
                              )
                        , output = bin
                        }
                      , run = Command.Args [ bin ]
                      , languageId =
                          merge
                            { Atcoder = Some "4003"
                            , Codeforces = Some "54"
                            , Yukicoder = Some "cpp17"
                            }
                            service
                      }

            let rs
                : Language
                = let problem = problem.kebabCase

                  let src =
                        "${Service/lowercase
                             service}/${contest}/rs/src/bin/${problem}.rs"

                  let bin =
                        "${Service/lowercase
                             service}/target/${Mode/lowercase
                                                 mode}/${contest}-${problem}"

                  in  { src
                      , transpile = None Compile
                      , compile = Some
                        { command =
                            Command.Args
                              (   [ "cargo"
                                  , "build"
                                  , "--manifest-path"
                                  , "./${Service/lowercase
                                           service}/${contest}/rs/Cargo.toml"
                                  , "--bin"
                                  , "${contest}-${problem}"
                                  ]
                                # merge
                                    { Debug = [] : List Text
                                    , Release = [ "--release" ]
                                    }
                                    mode
                              )
                        , output = bin
                        }
                      , run = Command.Args [ bin ]
                      , languageId =
                          merge
                            { Atcoder = Some "4050"
                            , Codeforces = Some "49"
                            , Yukicoder = Some "rust"
                            }
                            service
                      }

            let java
                : Language
                = let problem = problem.pascalCase

                  let src =
                        "${Service/lowercase
                             service}/${contest}/java/src/main/java/${problem}.java"

                  let buildDir =
                        "${Service/lowercase
                             target.service}/${contest}/java/build/classrenamed/${problem}"

                  let transpiled = "${buildDir}/Main.java"

                  let bin = "${buildDir}/Main.class"

                  in  { src
                      , transpile = Some
                        { command =
                            Command.Script
                              ( bash
                                  ''
                                  cat ${src} | sed -r 's/class\s+${problem}/class Main/g' > ${bin}
                                  ''
                              )
                        , output = transpiled
                        }
                      , compile = Some
                        { command =
                            Command.Args [ "javac", "-d", buildDir, transpiled ]
                        , output = bin
                        }
                      , run =
                          Command.Args
                            [ "java", "-classpath", buildDir, "Main" ]
                      , languageId =
                          merge
                            { Atcoder = Some "4052"
                            , Codeforces = Some "36"
                            , Yukicoder = Some "java8"
                            }
                            service
                      }

            let py
                : Language
                = let problem = problem.kebabCase

                  let src =
                        "${Service/lowercase
                             service}/${contest}/py/${problem}.py"

                  in  { src
                      , transpile = None Compile
                      , compile = None Compile
                      , run = Command.Args [ "python", src ]
                      , languageId =
                          merge
                            { Atcoder = Some "4050"
                            , Codeforces = Some "31"
                            , Yukicoder = Some "python3"
                            }
                            service
                      }

            in  toMap { cpp, rs, java, py }
      , xtask = toMap
          { setup =
              python
                ''
                import itertools
                import json
                import os
                import subprocess
                import sys
                import urllib.request
                import webbrowser
                from argparse import ArgumentParser
                from pathlib import Path
                from subprocess import DEVNULL, PIPE
                from typing import List, Optional, Iterable, Iterator, AnyStr


                def main() -> None:
                    parser = ArgumentParser(prog='snowchains xtask setup')
                    parser.add_argument('-p', '--problems', nargs='*', metavar='PROBLEM')
                    parser.add_argument('service')
                    parser.add_argument('contest')
                    parser.add_argument('language', choices=['cpp', 'rs'])
                    parser.add_argument('editor', choices=['code', 'vim', 'emacs'])
                    args = parser.parse_args()

                    problems: Optional[List[str]] = args.problems
                    service: str = args.service
                    contest: str = args.contest
                    language: str = args.language
                    editor: str = args.editor

                    output = json.loads(subprocess.run(
                        ['snowchains', 'r', 't', '--json', '-s', service, '-c', contest,
                        *(['-p', *problems] if problems else [])],
                        check=True, stdout=PIPE,
                    ).stdout.decode())

                    urls = []
                    problem_indexes = []
                    test_suite_paths = []

                    for problem in output['problems']:
                        urls.append(problem['url'])
                        problem_indexes.append(problem['index']['kebab'])
                        test_suite_paths.append(Path(problem['test_suite']['path']))

                    browser = webbrowser.get()
                    for url in urls:
                        print(f'Opening {url} ...', file=sys.stderr, flush=True)
                        browser.open(url, autoraise=False)

                    if language == 'cpp':
                        src_paths = cpp(service, contest, problem_indexes)
                        paths = interleave_longest(src_paths, test_suite_paths)
                    elif language == 'rs':
                        src_paths = rs(service, contest, problem_indexes)
                        paths = interleave_longest(src_paths, test_suite_paths)
                    else:
                        paths = []

                    if editor == 'code':
                        args = ['code', *paths, '-a', f'./{service}/{contest}/{language}']
                    elif editor == 'vim':
                        args = ['vim', '-p', *paths]
                    elif editor == 'emacs':
                        args = ['emacsclient', '-n', *paths]

                    subprocess.run(args, check=True)


                CPP_TEMPLATE = 'int main() { return 0; }\n'


                def cpp(service: str, contest: str, problem_indexes: List[str]) -> List[Path]:
                    dir_path = Path('.', service, contest, 'cpp')
                    src_paths = [dir_path.joinpath(f'{s}.cpp') for s in problem_indexes]

                    dir_path.mkdir(parents=True, exist_ok=True)

                    for src_path in src_paths:
                        with open(src_path, 'w') as file:
                            file.write(CPP_TEMPLATE)

                    return src_paths


                RS_TEMPLATE = 'use proconio::input;\n' \
                            '\n' \
                            'fn main() {\n' \
                            '    input! {\n' \
                            '        n: usize,\n' \
                            '    }\n' \
                            '}\n'

                ATCODER_RUST_VERSION = '1.42.0'
                CODEFORCES_RUST_VERSION = '1.42.0'
                YUKICODER_RUST_VERSION = '1.44.1'


                def rs(service: str, contest: str, problem_indexes: List[str]) -> List[Path]:
                    src_paths = [Path('.', service, contest, 'rs', 'src', 'bin', f'{s}.rs')
                                for s in problem_indexes]

                    if not (ws_existed := Path('.', service, 'Cargo.toml').exists()):
                        Path('.', service).mkdir(exist_ok=True)
                        with open(Path('.', service, 'Cargo.toml'), 'w') as file:
                            file.write('[workspace]\n')

                    if not Path('.', service, 'rust-toolchain').exists():
                        if service == 'atcoder':
                            version = ATCODER_RUST_VERSION
                        elif service == 'codeforces':
                            version = CODEFORCES_RUST_VERSION
                        elif service == 'yukicoder':
                            version = YUKICODER_RUST_VERSION
                        else:
                            version = 'stable'

                        with open(Path('.', service, 'rust-toolchain'), 'w') as file:
                            file.write(f'{version}\n')

                    if not Path('.', service, contest, 'rs').exists():
                        subprocess.run(
                            ['cargo', 'member', 'new', '--manifest-path',
                            Path('.', service, 'Cargo.toml'), '--vcs', 'none', '--name',
                            contest, Path('.', service, contest, 'rs')], check=True,
                        )

                        if service == 'atcoder':
                            subprocess.run(
                                ['cargo', 'add', '--manifest-path',
                                Path('.', service, contest, 'rs', 'Cargo.toml'),
                                'alga@=0.9.3', 'ascii@=1.0.0', 'bitset-fixed@=0.1.0',
                                'either@=1.5.3', 'fixedbitset@=0.2.0', 'getrandom@=0.1.14',
                                'im-rc@=14.3.0', 'indexmap@=1.3.2', 'itertools@=0.9.0',
                                'itertools-num@=0.1.3', 'lazy_static@=1.4.0', 'libm@=0.2.1',
                                'maplit@=1.0.2', 'nalgebra@=0.20.0', 'ndarray@=0.13.0',
                                'num@=0.2.1', 'num-bigint@=0.2.6', 'num-complex@=0.2.4',
                                'num-derive@=0.3.0', 'num-integer@=0.1.42',
                                'num-iter@=0.1.40', 'num-rational@=0.2.4',
                                'num-traits@=0.2.11', 'ordered-float@=1.0.2',
                                'permutohedron@=0.2.4', 'petgraph@=0.5.0', 'proconio@=0.3.6',
                                'rand@=0.7.3', 'rand_chacha@=0.2.2', 'rand_core@=0.5.1',
                                'rand_distr@=0.2.2', 'rand_hc@=0.2.0', 'rand_pcg@=0.2.1',
                                'regex@=1.3.6', 'rustc-hash@=1.1.0', 'smallvec@=1.2.0',
                                'superslice@=1.0.0', 'text_io@=0.1.8', 'whiteread@=0.5.0'],
                                check=True,
                            )

                        with open(Path('.', service, contest, 'rs', 'Cargo.toml'),
                                'a') as file:
                            for problem_index in problem_indexes:
                                file.write(f'\n'
                                        f'[[bin]]\n'
                                        f'name = {repr(f"{contest}-{problem_index}")}\n'
                                        f'path = {repr(f"src/bin/{problem_index}.rs")}\n')

                    if Path('.', service, contest, 'rs', 'src', 'main.rs').exists():
                        os.remove(Path('.', service, contest, 'rs', 'src', 'main.rs'))

                    Path('.', service, contest, 'rs', 'src', 'bin').mkdir(exist_ok=True)

                    for src_path in src_paths:
                        with open(src_path, 'w') as file:
                            file.write(RS_TEMPLATE)

                    if not ws_existed:
                        with urllib.request.urlopen('https://raw.githubusercontent.com/rust-l'
                                                    'ang-ja/atcoder-rust-base/ja-all-enabled-'
                                                    'update-the-crates/Cargo.lock') as res:
                            cargo_lock = res.read()

                        with open(Path('.', service, 'Cargo.lock'), 'wb') as file:
                            file.write(cargo_lock)

                        subprocess.run(
                            ['cargo', 'metadata', '--format-version', '1', '--manifest-path',
                            Path('.', service, 'Cargo.toml')], check=True, stdout=DEVNULL,
                        )

                    return src_paths


                def interleave_longest(*sss: Iterable[AnyStr]) -> Iterator[AnyStr]:
                    return (s for s
                            in itertools.chain.from_iterable(itertools.zip_longest(*sss))
                            if s is not None)


                if __name__ == '__main__':
                    main()
                ''
          }
      }
    : Config
