module Main where

import Prelude
import Program (Program, f, run)
import Effect (Effect)
import Effect.Class.Console (log)

main ∷ Effect Unit
main = do
  steps ← run program
  log $ show steps

program ∷ Program Unit
program = do
  output ← f.command "date"
  void $ f.command $ "echo 'the time is: " <> output <> "'"
