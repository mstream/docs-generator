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
import Data.Array as Array
import Data.Codec as Codec
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.String.CodeUnits (fromCharArray)
import Data.String.NonEmpty as NES
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
  | Ls

derive instance Generic BashCommand _

instance Serializable Unit String BashCommand where
  serialize _ = Codec.encode $ Codec.basicCodec
    (const $ Left "parsing error")
    ( case _ of
        Date → "date"
        Echo s → "echo " <> s
        Ls → "ls"
    )

instance Installable BashCommand where
  packageNames command =
    (Set.singleton $ Nix.packageName $ NES.nes (Proxy ∷ Proxy "bash"))
      <>
        case command of
          Date → Set.empty
          Echo _ → Set.empty
          Ls → Set.empty

instance Versioned BashCommand where
  versionCommandsAndParsers command = Map.insert
    "bash"
    { versionCommand: "bash --version"
    , responseParser: bashVersionResponseParser
    }
    case command of
      Date → Map.empty
      Echo _ → Map.empty
      Ls → Map.empty

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

lsVersionResponseParser ∷ Parser String
lsVersionResponseParser = do
  void $ string "GNU bash, version "
  fromCharArray <<< Array.fromFoldable <$> manyTill
    anyChar
    (string " ")
