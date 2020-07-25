let Map =
      https://prelude.dhall-lang.org/v17.0.0/Map/Type sha256:210c7a9eba71efbb0f7a66b3dcf8b9d3976ffc2bc0e907aadfb6aa29c333e8ed

let Service = < Atcoder | Codeforces | Yukicoder >

let Service/lowercase =
      λ(service : Service) →
        merge
          { Atcoder = "atcoder"
          , Codeforces = "codeforces"
          , Yukicoder = "yukicoder"
          }
          service

let Service/uppercase =
      λ(service : Service) →
        merge
          { Atcoder = "ATCODER"
          , Codeforces = "CODEFORCES"
          , Yukicoder = "YUKICODER"
          }
          service

let Service/snakeCase = Service/lowercase

let Service/kebabCase = Service/lowercase

let Service/mixedCase = Service/lowercase

let Service/pascalCase =
      λ(service : Service) →
        merge
          { Atcoder = "Atcoder"
          , Codeforces = "Codeforces"
          , Yukicoder = "Yukicoder"
          }
          service

let CaseConvertedText =
    -- https://github.com/dhall-lang/dhall-lang/issues/631
      { lowercase : Text
      , uppercase : Text
      , snakeCase : Text
      , kebabCase : Text
      , mixedCase : Text
      , pascalCase : Text
      }

let CaseConvertedText/lowercase = λ(s : CaseConvertedText) → s.lowercase

let CaseConvertedText/uppercase = λ(s : CaseConvertedText) → s.uppercase

let CaseConvertedText/snakeCase = λ(s : CaseConvertedText) → s.snakeCase

let CaseConvertedText/kebabCase = λ(s : CaseConvertedText) → s.kebabCase

let CaseConvertedText/mixedCase = λ(s : CaseConvertedText) → s.mixedCase

let CaseConvertedText/pascalCase = λ(s : CaseConvertedText) → s.pascalCase

let Script = { program : Text, extension : Text, content : Text }

let Script/new =
      λ(program : Text) →
      λ(extension : Text) →
      λ(content : Text) →
        { program, extension, content }

let Command = < Args : List Text | Script : Script >

let Mode = < Debug | Release >

let Target =
      { service : Service
      , contest : Optional CaseConvertedText
      , problem : CaseConvertedText
      , mode : Mode
      }

let Compile = { command : Command, output : Text }

let Language =
      { src : Text
      , transpile : Optional Compile
      , compile : Optional Compile
      , run : Command
      , languageId : Optional Text
      }

let Config =
      { detectServiceFromRelativePathSegments : List Text → Optional Text
      , detectContestFromRelativePathSegments : List Text → Optional Text
      , detectProblemFromRelativePathSegments : List Text → Optional Text
      , detectLanguageFromRelativePathSegments : List Text → Optional Text
      , languages : Target → Map Text Language
      , xtask : Map Text Script
      }

in  { Service
    , Service/lowercase
    , Service/uppercase
    , Service/snakeCase
    , Service/kebabCase
    , Service/mixedCase
    , Service/pascalCase
    , CaseConvertedText
    , CaseConvertedText/lowercase
    , CaseConvertedText/uppercase
    , CaseConvertedText/snakeCase
    , CaseConvertedText/kebabCase
    , CaseConvertedText/mixedCase
    , CaseConvertedText/pascalCase
    , Script
    , Script/new
    , Command
    , Target
    , Compile
    , Language
    , Config
    }
