let Shell =
  { runner : Text
  , extension : Text
  }

let ShellCommand =
  { shell : Shell
  , code : Text
  }

let Command =
  < Args : List Text
  | Shell : ShellCommand
  >

let Compile =
  { command : Command
  , output : Text
  }

let Language =
  { src : Text
  , transpile : Optional Compile
  , compile : Optional Compile
  , run : Command
  , languageId : Optional Text
  }

let Service =
  < Atcoder
  | Codeforces
  | Yukicoder
  >

let Service/lowercase = λ(service : Service) →
  merge { Atcoder = "atcoder"
        , Codeforces = "codeforces"
        , Yukicoder = "yukicoder"
        }
        service

let Service/uppercase = λ(service : Service) →
  merge { Atcoder = "ATCODER"
        , Codeforces = "CODEFORCES"
        , Yukicoder = "YUKICODER"
        }
        service

let Service/snakeCase = λ(service : Service) →
  Service/lowercase service

let Service/kebabCase = λ(service : Service) →
  Service/lowercase service

let Service/mixedCase = λ(service : Service) →
  Service/lowercase service

let Service/pascalCase = λ(service : Service) →
  merge { Atcoder = "Atcoder"
        , Codeforces = "Codeforces"
        , Yukicoder = "Yukicoder"
        }
        service

let CaseConvertedText =
  -- https://github.com/dhall-lang/dhall-lang/issues/631
  { original : Text
  , lowercase : Text
  , uppercase : Text
  , snakeCase : Text
  , kebabCase : Text
  , mixedCase : Text
  , pascalCase : Text
  }

let Mode =
  < Debug
  | Release
  >

let Target =
  { service : Service
  , contest : Optional CaseConvertedText
  , problem : CaseConvertedText
  , mode : Mode
  }

let Config =
  let Map = https://prelude.dhall-lang.org/v17.0.0/Map/Type
            sha256:210c7a9eba71efbb0f7a66b3dcf8b9d3976ffc2bc0e907aadfb6aa29c333e8ed
  in { customSubcommands : Map Text Command
     , defaultLanguage : Language
     , languages : Map Text Language
     }

in { Shell = Shell
   , ShellCommand = ShellCommand
   , Command = Command
   , Compile = Compile
   , Language = Language
   , Service = Service
   , Service/lowercase = Service/lowercase
   , Service/uppercase = Service/uppercase
   , Service/snakeCase = Service/snakeCase
   , Service/kebabCase = Service/kebabCase
   , Service/mixedCase = Service/mixedCase
   , Service/pascalCase = Service/pascalCase
   , CaseConvertedText = CaseConvertedText
   , Mode = Mode
   , Target = Target
   , Config = Config
   }