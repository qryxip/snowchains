let Snowchains = /home/ryo/src/local/snowchains_config/Snowchains.dhall

let Shell = Snowchains.Shell

let ShellCommand = Snowchains.ShellCommand

let Command = Snowchains.Command

let Compile = Snowchains.Compile

let Language = Snowchains.Language

let Service = Snowchains.Service

let Service/lowercase = Snowchains.Service/lowercase

let Service/uppercase = Snowchains.Service/uppercase

let Service/snakeCase = Snowchains.Service/snakeCase

let Service/kebabCase = Snowchains.Service/kebabCase

let Service/mixedCase = Snowchains.Service/mixedCase

let Service/pascalCase = Snowchains.Service/pascalCase

let CaseConvertedText = Snowchains.CaseConvertedText

let Mode = Snowchains.Mode

let Target = Snowchains.Target

let Config = Snowchains.Config

let Map =
      https://prelude.dhall-lang.org/v17.0.0/Map/Type sha256:210c7a9eba71efbb0f7a66b3dcf8b9d3976ffc2bc0e907aadfb6aa29c333e8ed

let List/concat =
      https://prelude.dhall-lang.org/v17.0.0/List/concat sha256:54e43278be13276e03bd1afa89e562e94a0a006377ebea7db14c7562b0de292b

let bash = { runner = "/bin/bash", extension = "sh" } : Shell

in  λ(target : Target) →
      let service = target.service

      let contest =
            merge
              { Some = λ(s : CaseConvertedText) → s.kebabCase
              , None = "problems"
              }
              target.contest

      let problem = target.problem

      let mode = target.mode

      let gPlusPlus17
          : Language
          = let problem = problem.kebabCase

            let src =
                  "${Service/lowercase service}/${contest}/cpp/${problem}.cpp"

            let bin =
                  "${Service/lowercase
                       service}/${contest}/cpp/target/${problem}"

            in  { src
                , transpile = None Compile
                , compile = Some
                  { command =
                      Command.Args
                        ( List/concat
                            Text
                            [ [ "g++" ]
                            , merge
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
                            , merge
                                { Debug =
                                  [ "-g"
                                  , "-fsanitize=undefined"
                                  , "-D_GLIBCXX_DEBUG"
                                  ]
                                , Release = [ "-O2" ]
                                }
                                mode
                            , [ "-Wall", "-Wextra", "-o", bin, src ]
                            ]
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

      let rust
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

      let java8
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
                      Command.Shell
                        { shell = bash
                        , code =
                            ''
                            cat ${src} | sed -r 's/class\s+${problem}/class Main/g' > ${bin}
                            ''
                        }
                  , output = transpiled
                  }
                , compile = Some
                  { command =
                      Command.Args [ "javac", "-d", buildDir, transpiled ]
                  , output = bin
                  }
                , run = Command.Args [ "java", "-classpath", buildDir, "Main" ]
                , languageId =
                    merge
                      { Atcoder = Some "4052"
                      , Codeforces = Some "36"
                      , Yukicoder = Some "java8"
                      }
                      service
                }

      let python
          : Language
          = let problem = problem.kebabCase

            let src = "${Service/lowercase service}/${contest}/py/${problem}.py"

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

      in    { customSubcommands = toMap
                { hage = Command.Shell { shell = bash, code = "#" } }
            , defaultLanguage = rust
            , languages = toMap
                { cpp = gPlusPlus17, rs = rust, java = java8, py = python }
            }
          : Config
