let List/index =
      https://prelude.dhall-lang.org/v17.0.0/List/index sha256:e657b55ecae4d899465c3032cb1a64c6aa6dc2aa3034204f3c15ce5c96c03e63

let Snowchains =
      /home/ryo/src/github.com/qryxip/snowchains/resources/config/schema/Snowchains/package.dhall

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

in    { xtask = toMap
          { custom-cmd =
              python
                ''
                from argparse import ArgumentParser


                def main() -> None:
                    parser = ArgumentParser(prog='snowchains xtask custom-cmd')
                    _ = parser.parse_args()


                if __name__ == '__main__':
                    main()
                ''
          }
      , detectServiceFromRelativePathSegments = List/index 0 Text
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
                            { Atcoder = Some "3006"
                            , Codeforces = Some "31"
                            , Yukicoder = Some "python3"
                            }
                            service
                      }

            in  toMap { cpp, rs, java, py }
      }
    : Config
