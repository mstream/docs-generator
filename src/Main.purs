module Main where

import Prelude
import Output (encode)
import Execution (run)
import Effect (Effect)
import Program (BashCommand(Date, Echo), Program, f)
import Effect.Class.Console (log)
import Data.String (trim)

main ∷ Effect Unit
main = do
  executionResult ← run program
  log $ encode executionResult

program ∷ Program Unit
program = do
  f.comment "Obtaining the current time"
  output ← f.bash Date
  f.comment "Printing it back"
  void $ f.bash $ Echo $ "'the time was: "
    <> (trim output)
    <> "'"
