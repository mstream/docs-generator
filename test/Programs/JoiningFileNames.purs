module Test.Programs.JoiningFileNames (program) where

import Prelude

import Data.String (Pattern(Pattern), Replacement(Replacement))
import Data.String as String
import Program (BashCommand(Echo, Ls), Program, f)

program ∷ Program Unit
program = do
  f.comment "Obtaining names of files in the current directory"
  output ← f.bash Ls
  f.comment "Printing names joined with a comma"
  void $ f.bash $ Echo $ "'names of files in the current directory: "
    <>
      ( String.replaceAll (Pattern "\n") (Replacement ", ")
          (String.trim output)
      )
    <> "'"
