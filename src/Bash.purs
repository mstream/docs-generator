module Bash
  ( Bash
  , newline
  , printCommand
  , printComment
  , shebang
  , stringify
  ) where

import Prelude

newtype Bash = Bash String

derive newtype instance Semigroup Bash
derive newtype instance Monoid Bash

printCommand ∷ String → Bash
printCommand = Bash

printComment ∷ String → Bash
printComment = printCommand <<< append "# "

newline ∷ Bash
newline = Bash "\n"

shebang ∷ Bash
shebang = Bash "#! /usr/bin/env bash"

stringify ∷ Bash → String
stringify (Bash s) = s
