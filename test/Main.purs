module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Test.Programs.ReprintingCurrentTime as ReprintingCurrentTime
import Test.Utils as Utils

main ∷ Effect Unit
main = Aff.launchAff_ $
  Utils.generateSnapshots
    ReprintingCurrentTime.program
    "test/outputs/ReprintingCurrentTime"
