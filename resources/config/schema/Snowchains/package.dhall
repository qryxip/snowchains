-- https://github.com/Nadrieril/dhall-rust/blob/3d9c0b12c6b34185e556071ee16401691bfd8e49/dhall/src/semantics/resolve/resolve.rs#L54-L59
let Map = λ(k : Type) → λ(v : Type) → List { mapKey : k, mapValue : v }

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

let Mode/lowercase =
      λ(m : Mode) → merge { Debug = "debug", Release = "release" } m

let Mode/uppercase =
      λ(m : Mode) → merge { Debug = "DEBUG", Release = "RELEASE" } m

let Mode/snakeCase = Mode/lowercase

let Mode/kebabCase = Mode/lowercase

let Mode/mixedCase = Mode/lowercase

let Mode/pascalCase =
      λ(m : Mode) → merge { Debug = "Debug", Release = "Release" } m

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
    , Mode
    , Mode/lowercase
    , Mode/uppercase
    , Mode/snakeCase
    , Mode/kebabCase
    , Mode/mixedCase
    , Mode/pascalCase
    , Target
    , Compile
    , Language
    , Config
    }
