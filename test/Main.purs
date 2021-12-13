module Test.Main where

import Prelude

import Test.Utils as Utils
import Effect (Effect)
import Data.Foldable (traverse_)
import Test.Programs.ReprintingCurrentTime as ReprintingCurrentTime

main ∷ Effect Unit
main = do
  Utils.generateSnapshots
    ReprintingCurrentTime.program
    "test/outputs/ReprintingCurrentTime"
