module Output
  ( class Codable
  , codec
  , decode
  , decodeWithOptions
  , encode
  , encodeWithOptions
  ) where

import Prelude
import Data.Codec (BasicCodec)
import Data.Codec as Codec
import Data.Either (Either)

class Codable o a b | a → o, o → a where
  codec ∷ o → BasicCodec (Either String) a b

encode ∷ ∀ a b. Codable Unit a b ⇒ b → a
encode = Codec.encode (codec unit)

encodeWithOptions ∷ ∀ a b o. Codable o a b ⇒ o → b → a
encodeWithOptions options = Codec.encode (codec options)

decode ∷ ∀ a b m. Codable Unit a b ⇒ a → Either String b
decode = Codec.decode (codec unit)

decodeWithOptions ∷ ∀ a b m o. Codable o a b ⇒ o → a → Either String b
decodeWithOptions options = Codec.decode (codec options)
