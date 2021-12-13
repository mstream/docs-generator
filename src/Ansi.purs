module Ansi
  ( Ansi
  , newline
  , printInput
  , printOutput
  , printComment
  , toString
  ) where

import Prelude

import Ansi.Codes (Color(BrightBlack, BrightWhite))
import Ansi.Output (foreground, withGraphics)

newtype Ansi = Ansi String

derive newtype instance Semigroup Ansi
derive newtype instance Monoid Ansi

printInput ∷ String → Ansi
printInput = Ansi
  <<< append "> "
  <<< withGraphics (foreground BrightWhite)

printOutput ∷ String → Ansi
printOutput = Ansi

printComment ∷ String → Ansi
printComment = printInput
  <<< withGraphics (foreground BrightBlack)
  <<< append "# "

newline ∷ Ansi
newline = Ansi "\n"

toString ∷ Ansi → String
toString (Ansi s) = s
