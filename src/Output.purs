module Output
  ( class Codable
  , codec
  , decode
  , encode
  , printComment
  , printInput
  , printOutput
  ) where

import Prelude
import Ansi.Codes (Color(BrightBlack, BrightWhite, White))
import Ansi.Output (foreground, withGraphics)
import Data.Codec (BasicCodec)
import Data.Codec as Codec
import Data.Either (Either)

class Codable a b where
  codec ∷ BasicCodec (Either String) a b

encode ∷ ∀ a b m. Codable a b ⇒ b → a
encode = Codec.encode codec

decode ∷ ∀ a b m. Codable a b ⇒ a → Either String b
decode = Codec.decode codec

printInput ∷ String → String
printInput = append "> " <<< withGraphics (foreground BrightWhite)

printOutput ∷ String → String
printOutput = identity

printComment ∷ String → String
printComment = printInput
  <<< withGraphics (foreground BrightBlack)
  <<< append "# "
