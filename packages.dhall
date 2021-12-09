let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20210919/packages.dhall
        sha256:03516fdd4f6d1bd6c9eb5e63cf3af3037bc079459117ab93eb85b6eb46e258a7

let extras =
      { freer-free =
        { dependencies = [ "psci-support", "prelude", "record" ]
        , repo = "https://github.com/meeshkan/purescript-freer-free"
        , version = "v0.0.0"
        }
      }

in  upstream // extras
