module Test.Main where

import Prelude

import Test.Utils as Utils
import Effect (Effect)
import Data.Foldable (traverse_)
import Test.Programs.ReprintingCurrentTime as ReprintingCurrentTime

main ∷ Effect Unit
main = do
  traverse_
    ( \{ name, program } → Utils.generateSnapshot
        program
        ("test/outputs/" <> name <> ".ansi")
    )
    [ { name: "ReprintingCurrentTime"
      , program: ReprintingCurrentTime.program
      }
    ]
