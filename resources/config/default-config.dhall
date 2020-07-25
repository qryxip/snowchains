let List/index =
      https://prelude.dhall-lang.org/v17.0.0/List/index sha256:e657b55ecae4d899465c3032cb1a64c6aa6dc2aa3034204f3c15ce5c96c03e63

let Snowchains =
      https://raw.githubusercontent.com/qryxip/snowchains/rewrite-almost-everything/resources/config/schema/Snowchains/package.dhall sha256:28a3f4b9125086c16d99389249a3dce4f8c1044a6a8e85b2fed410d61d1bdecf

let Service/lowercase = Snowchains.Service/lowercase

let CaseConvertedText/kebabCase = Snowchains.CaseConvertedText/kebabCase

let Script/new = Snowchains.Script/new

let Command = Snowchains.Command

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
                             service}/${contest}/rs/target/manual/${problem}"

                  in  { src
                      , transpile = None Compile
                      , compile = Some
                        { command =
                            let version =
                                  merge
                                    { Atcoder = "1.42.0"
                                    , Codeforces = "1.42.0"
                                    , Yukicoder = "1.44.1"
                                    }
                                    service

                            let optLevel =
                                  merge
                                    { Atcoder = "3"
                                    , Codeforces = "2"
                                    , Yukicoder = "2"
                                    }
                                    service

                            in  Command.Args
                                  [ "rustup"
                                  , "run"
                                  , version
                                  , "rustc"
                                  , "-C"
                                  , "opt-level=${optLevel}"
                                  , "-o"
                                  , bin
                                  , src
                                  ]
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
                import webbrowser
                from argparse import ArgumentParser
                from pathlib import Path
                from subprocess import PIPE
                from typing import List, Optional, Iterable, Iterator, AnyStr


                def main() -> None:
                    parser = ArgumentParser(prog='snowchains xtask setup')
                    parser.add_argument('-p', '--problems', nargs='*', metavar='PROBLEM')
                    parser.add_argument('service')
                    parser.add_argument('contest')
                    parser.add_argument('language')
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
                        check=True,
                        stdout=PIPE,
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

                    dir_path = Path(f'./{service}/{contest}/{language}')

                    if language == 'cpp':
                        src_paths = cpp(dir_path, problem_indexes)
                        paths = interleave_longest(src_paths, test_suite_paths)
                    elif language == 'rs':
                        src_paths = rs(dir_path, service, contest, problem_indexes)
                        paths = interleave_longest(src_paths, test_suite_paths)
                    else:
                        paths = []

                    if editor == 'code':
                        args = ['code', *paths, '-a', dir_path]
                    elif editor == 'vim':
                        args = ['vim', '-p', *paths]
                    elif editor == 'emacs':
                        args = ['emacsclient', '-n', *paths]

                    subprocess.run(args, check=True)


                CPP_TEMPLATE = 'int main() { return 0; }\n'


                def cpp(dir_path: Path, problem_indexes: List[str]) -> List[Path]:
                    src_paths = [dir_path.joinpath(f'{s}.cpp') for s in problem_indexes]

                    if not dir_path.is_dir():
                        dir_path.mkdir(parents=True)

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


                def rs(dir_path: Path, service: str, contest: str,
                       problem_indexes: List[str]) -> List[Path]:
                    src_paths = [dir_path.joinpath('src', 'bin', f'{s}.rs')
                                 for s in problem_indexes]

                    if not dir_path.is_dir():
                        subprocess.run(
                            ['cargo', 'member', 'new', '--vcs', 'none', '--name',
                             f'{service}-{contest}', dir_path],
                            check=True,
                        )

                    subprocess.run(
                        ['cargo', 'add', '--manifest-path', dir_path.joinpath('Cargo.toml'),
                         'proconio@0.3.6',
                         ],
                        check=True,
                    )

                    if dir_path.joinpath('src', 'main.rs').exists():
                        os.remove(dir_path.joinpath('src', 'main.rs'))

                    dir_path.joinpath('src', 'bin').mkdir(exist_ok=True)

                    for src_path in src_paths:
                        with open(src_path, 'w') as file:
                            file.write(RS_TEMPLATE)

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
