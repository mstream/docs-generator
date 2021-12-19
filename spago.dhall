{ name = "demo"
, dependencies =
  [ "aff"
  , "ansi"
  , "argonaut"
  , "argonaut-generic"
  , "arrays"
  , "codec"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "free"
  , "freer-free"
  , "language-cst-parser"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "node-child-process"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "nonempty"
  , "ordered-collections"
  , "parallel"
  , "posix-types"
  , "prelude"
  , "psci-support"
  , "string-parsers"
  , "strings"
  , "tidy-codegen"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
