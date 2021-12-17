module Test.Main where

import Prelude

import Data.String.NonEmpty as NES
import Effect (Effect)
import Effect.Aff as Aff
import Test.Programs.JoiningFileNames as JoiningFileNames
import Test.Programs.ReprintingCurrentTime as ReprintingCurrentTime
import Test.Utils as Utils
import Type.Proxy (Proxy(Proxy))

main ∷ Effect Unit
main = Aff.launchAff_ $ do
  Utils.generateSnapshots
    JoiningFileNames.program
    (NES.nes (Proxy ∷ Proxy "JoiningFileNames"))
  Utils.generateSnapshots
    ReprintingCurrentTime.program
    (NES.nes (Proxy ∷ Proxy "ReprintingCurrentTime"))
