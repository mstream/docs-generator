module Os
  ( Os(..)
  , uname
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Codec (BasicCodec)
import Data.Codec as Codec
import Data.Either (Either(Left, Right))
import Data.Either as Either
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.String.CodePoints as CodePoints
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Effect.Aff (Aff)
import Effect.Exception as Exception
import Output (class Deserializable, class Serializable)
import Output as Output
import Text.Parsing.StringParser as StringParser
import Text.Parsing.StringParser.CodePoints (noneOf, skipSpaces)
import Text.Parsing.StringParser.Combinators (many1)

newtype Os = Os { name ∷ NonEmptyString, release ∷ NonEmptyString }

derive instance Generic Os _

instance EncodeJson Os where
  encodeJson = genericEncodeJson

instance Deserializable Unit String Os where
  deserialize _ = Codec.decode stringCodec

instance Serializable Unit String Os where
  serialize _ = Codec.encode stringCodec

unameCommand ∷ String
unameCommand = "uname -o -r"

uname ∷ (String → Aff String) → Aff Os
uname executeCommand = do
  output ← executeCommand unameCommand
  Either.either
    (throwError <<< Exception.error)
    pure
    (Output.deserialize_ output)

stringCodec ∷ BasicCodec (Either String) String Os
stringCodec = Codec.basicCodec decodeString encodeString

decodeString ∷ String → String \/ Os
decodeString s =
  case StringParser.runParser parser s of
    Left parsingError → Left $
      StringParser.printParserError parsingError
    Right os → pure os
  where
  parser = do
    releaseChars ← many1 $ noneOf [ ' ', '\n' ]
    skipSpaces
    nameChars ← many1 $ noneOf [ '\n' ]
    pure $ Os
      { name: NES.fromFoldable1
          (CodePoints.codePointFromChar <$> nameChars)
      , release: NES.fromFoldable1
          (CodePoints.codePointFromChar <$> releaseChars)
      }

encodeString ∷ Os → String
encodeString (Os { name, release }) =
  (NES.toString name) <> " " <> (NES.toString release)
