module Program
  ( class Versioned
  , BashCommand(..)
  , Program
  , ProgramF(..)
  , f
  , versionCommandsAndParsers
  ) where

import Prelude

import Control.Monad.Free (Free, liftF)
import Control.Monad.Freer.Free (Constructors, constructors)
import Control.Plus (empty)
import Data.Array as Array
import Data.Codec as Codec
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.String.CodeUnits (fromCharArray)
import Data.String.NonEmpty as NES
import Data.Tuple.Nested ((/\))
import Nix (class Installable)
import Nix as Nix
import Output (class Serializable)
import Output as Output
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints (anyChar, string)
import Text.Parsing.StringParser.Combinators (manyTill)
import Type.Proxy (Proxy(Proxy))

class Versioned a where
  versionCommandsAndParsers
    ∷ a
    → Map String
        { versionCommand ∷ String, responseParser ∷ Parser String }

data BashCommand
  = Date
  | Echo String

derive instance Generic BashCommand _

instance Serializable Unit String BashCommand where
  serialize _ = Codec.encode $ Codec.basicCodec
    (const $ Left "parsing error")
    ( case _ of
        Date → "date"
        Echo s → "echo " <> s
    )

instance Installable BashCommand where
  packageNames command =
    (Set.singleton $ Nix.packageName $ NES.nes (Proxy ∷ Proxy "bash"))
      <>
        case command of
          Date → Set.empty
          Echo _ → Set.empty

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

instance Serializable Unit String (ProgramF a) where
  serialize _ = Codec.encode $ Codec.basicCodec
    (const $ Left "parsing error")
    ( case _ of
        Bash command _ → "bash -c \"" <> Output.serialize_ command <>
          "\""
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
  fromCharArray <<< Array.fromFoldable <$> manyTill
    anyChar
    (string " ")
