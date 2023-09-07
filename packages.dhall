let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.9-20230629/packages.dhall
        sha256:f91d36c7e4793fe4d7e042c57fef362ff3f9e9ba88454cd38686701e30bf545a

let additions =
      { foreign-generic =
        { dependencies =
          [ "arrays"
          , "assert"
          , "bifunctors"
          , "console"
          , "control"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "identity"
          , "lists"
          , "maybe"
          , "newtype"
          , "partial"
          , "prelude"
          , "record"
          , "strings"
          , "transformers"
          , "tuples"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/lumihq/purescript-foreign-generic.git"
        , version = "proxy-instances"
        }
      , promises =
        { dependencies =
          [ "console"
          , "datetime"
          , "effect"
          , "exceptions"
          , "functions"
          , "prelude"
          , "transformers"
          , "arrays"
          , "either"
          , "foldable-traversable"
          , "unfoldable"
          , "maybe"
          ]
        , repo = "https://github.com/clipperz/purescript-promises.git"
        , version = "a87a9b1cbab5446574a289a770c4f26bcd9dd21d"
        }
      }

in  (upstream // additions)
  with metadata.version = "v0.15.2"
