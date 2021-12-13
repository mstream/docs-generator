module Test.Programs.ReprintingCurrentTime (program) where

import Prelude

import Node.Encoding (Encoding(UTF8))
import Execution as Execution
import Output as Output
import Program (BashCommand(Date, Echo), Program, f)
import Node.FS.Sync as FS
import Effect (Effect)
import Data.String as String

program ∷ Program Unit
program = do
  f.comment "Obtaining the current time"
  output ← f.bash Date
  f.comment "Printing it back"
  void $ f.bash $ Echo $ "'the time was: "
    <> (String.trim output)
    <> "'"
