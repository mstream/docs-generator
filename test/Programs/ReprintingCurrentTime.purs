module Test.Programs.ReprintingCurrentTime (program) where

import Prelude

import Data.String as String
import Program (BashCommand(Date, Echo), Program, f)

program ∷ Program Unit
program = do
  f.comment "Obtaining the current time"
  output ← f.bash Date
  f.comment "Printing it back"
  void $ f.bash $ Echo $ "'the time was: "
    <> (String.trim output)
    <> "'"
