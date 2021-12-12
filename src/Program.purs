module Program
  ( class Installable
  , class Versioned
  , BashCommand(..)
  , Program
  , ProgramF(..)
  , f
  , nixPackageNames
  , versionCommandsAndParsers
  ) where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Array as Array
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (anyChar, string)
import Text.Parsing.StringParser.Combinators (manyTill)
import Output (class Codable, encode)
import Data.String.CodeUnits (fromCharArray)
import Data.Either (Either(Left))
import Data.Tuple.Nested ((/\))
import Data.List as List
import Control.Plus (empty)
import Data.List (List)
import Data.Codec (basicCodec)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Freer.Free (Constructors, constructors)
import Data.Generic.Rep (class Generic)

class Installable a where
  nixPackageNames ∷ a → List String

class Versioned a where
  versionCommandsAndParsers
    ∷ a
    → Map String
        { versionCommand ∷ String, responseParser ∷ Parser String }

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

instance Installable BashCommand where
  nixPackageNames command =
    List.singleton "bash" <> case command of
      Date → empty
      Echo _ → empty

instance Versioned BashCommand where
  versionCommandsAndParsers command = Map.insert
    "bash"
    { versionCommand: "bash --version"
    , responseParser: bashVersionResponseParser
    }
    case command of
      Date → Map.empty
      Echo _ → Map.empty

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

bashVersionResponseParser ∷ Parser String
bashVersionResponseParser = do
  void $ string "GNU bash, version "
  fromCharArray <<< Array.fromFoldable <$> manyTill anyChar
    (string " ")
