module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Test.Programs.JoiningFileNames as JoiningFileNames
import Test.Programs.ReprintingCurrentTime as ReprintingCurrentTime
import Test.Utils as Utils

main ∷ Effect Unit
main = Aff.launchAff_ $ do
  Utils.generateSnapshots
    JoiningFileNames.program
    "test/outputs/JoiningFileNames"
  Utils.generateSnapshots
    ReprintingCurrentTime.program
    "test/outputs/ReprintingCurrentTime"
