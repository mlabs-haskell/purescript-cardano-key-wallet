{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
  with halogen-vdom.dependencies = [ "extra-dependency" ] # halogen-vdom.dependencies
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20230105/packages.dhall
        sha256:3e9fbc9ba03e9a1fcfd895f65e2d50ee2f5e86c4cd273f3d5c841b655a0e1bda

let additions =
      { aeson =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "argonaut-codecs"
          , "argonaut-core"
          , "arrays"
          , "bifunctors"
          , "const"
          , "control"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "integers"
          , "js-bigints"
          , "lists"
          , "maybe"
          , "mote"
          , "numbers"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "quickcheck"
          , "record"
          , "spec"
          , "strings"
          , "tuples"
          , "typelevel"
          , "typelevel-prelude"
          , "uint"
          , "untagged-union"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-aeson.git"
        , version = "v2.0.0"
        }
      , properties =
        { dependencies = [ "prelude", "console" ]
        , repo = "https://github.com/Risto-Stevcev/purescript-properties.git"
        , version = "v0.2.0"
        }
      , lattice =
        { dependencies = [ "prelude", "console", "properties" ]
        , repo = "https://github.com/Risto-Stevcev/purescript-lattice.git"
        , version = "v0.3.0"
        }
      , bytearrays =
        { dependencies =
          [ "aeson"
          , "aff"
          , "arraybuffer-types"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "maybe"
          , "newtype"
          , "prelude"
          , "quickcheck"
          , "quickcheck-laws"
          , "spec"
          , "strings"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-bytearrays"
        , version = "v1.0.0"
        }
      , cardano-message-signing =
        { dependencies =
          [ "bytearrays"
          , "cardano-types"
          , "console"
          , "effect"
          , "newtype"
          , "prelude"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-message-signing"
        , version = "v1.0.0"
        }
      , cardano-collateral-select =
        { dependencies =
          [ "arrays"
          , "cardano-types"
          , "console"
          , "effect"
          , "exceptions"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "newtype"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "tuples"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-collateral-select"
        , version = "v1.0.0"
        }
      , cardano-types =
        { dependencies =
          [ "aeson"
          , "aff"
          , "arraybuffer-types"
          , "arrays"
          , "bifunctors"
          , "bytearrays"
          , "cardano-plutus-data-schema"
          , "cardano-serialization-lib"
          , "control"
          , "datetime"
          , "effect"
          , "either"
          , "encoding"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "integers"
          , "js-bigints"
          , "lattice"
          , "lists"
          , "literals"
          , "maybe"
          , "monad-logger"
          , "mote"
          , "mote-testplan"
          , "newtype"
          , "nonempty"
          , "nullable"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "profunctor"
          , "profunctor-lenses"
          , "quickcheck"
          , "rationals"
          , "record"
          , "safe-coerce"
          , "spec"
          , "these"
          , "tuples"
          , "typelevel-prelude"
          , "uint"
          , "unfoldable"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-cardano-types"
        , version = "56877b43ea392ef6486e37d52e1c37d8c2b8c42d"
        }
      , cardano-serialization-lib =
        { dependencies =
          [ "aeson"
          , "aff"
          , "argonaut"
          , "bifunctors"
          , "bytearrays"
          , "effect"
          , "either"
          , "enums"
          , "maybe"
          , "nullable"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "profunctor"
          , "spec"
          , "transformers"
          , "tuples"
          , "unsafe-coerce"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-serialization-lib"
        , version = "v1.0.0"
        }
      , cardano-plutus-data-schema =
        { dependencies = [ "prelude" ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-plutus-data-schema"
        , version = "eb0bb78927c50c4bee364e932c9fa8cf94546191"
        }
      , js-bigints =
        { dependencies = [ "integers", "maybe", "prelude" ]
        , repo = "https://github.com/purescript-contrib/purescript-js-bigints"
        , version = "36a7d8ac75a7230043ae511f3145f9ed130954a9"
        }
      , mote-testplan =
        { dependencies =
          [ "aff"
          , "console"
          , "datetime"
          , "effect"
          , "foldable-traversable"
          , "maybe"
          , "mote"
          , "newtype"
          , "numbers"
          , "ordered-collections"
          , "prelude"
          , "spec"
          , "transformers"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-mote-testplan"
        , version = "d1c93e08ef09f5427b460f396b89da7255f8af52"
        }
      }

in  upstream // additions
