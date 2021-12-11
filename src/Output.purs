module Output (class Codable, codec, decode, encode) where

import Prelude
import Data.Codec (BasicCodec)
import Data.Codec as Codec
import Data.Either (Either)

class Codable a b where
  codec ∷ BasicCodec (Either String) a b

encode ∷ ∀ a b m. Codable a b ⇒ b → a
encode = Codec.encode codec

decode ∷ ∀ a b m. Codable a b ⇒ a → Either String b
decode = Codec.decode codec
