module Program
  ( BashCommand(..)
  , Program
  , ProgramF(..)
  , f
  ) where

import Prelude
import Output (class Codable, encode)
import Data.Either (Either(Left))
import Data.Codec (basicCodec)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Freer.Free (Constructors, constructors)
import Data.Generic.Rep (class Generic)

data BashCommand
  = Date
  | Echo String

derive instance Generic BashCommand _

instance Codable String BashCommand where
  codec = basicCodec
    (const $ Left "parsing error")
    ( case _ of
        Date → "date"
        Echo s → "echo " <> s
    )

data ProgramF a
  = Bash BashCommand (String → a)
  | Comment String a

instance Codable String (ProgramF a) where
  codec = basicCodec
    (const $ Left "parsing error")
    ( case _ of
        Bash command _ → "bash -c \"" <> encode command <> "\""
        _ → mempty
    )

derive instance Functor ProgramF
derive instance Generic (ProgramF a) _

type Program = Free ProgramF

f ∷ Constructors ProgramF Program
f = constructors (liftF ∷ ProgramF ~> Program)
