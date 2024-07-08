{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "purescript-cardano-key-wallet"
, dependencies =
  [ "aeson"
  , "aff"
  , "arrays"
  , "bytearrays"
  , "cardano-collateral-select"
  , "cardano-message-signing"
  , "cardano-types"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "maybe"
  , "mote"
  , "mote-testplan"
  , "newtype"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "quickcheck"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
