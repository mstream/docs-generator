module Os
  ( Os(..)
  , make
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
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Exception as Exception
import Output (class Deserializable, class Serializable)
import Output as Output
import PureScript.CST.Types (Expr, Ident(Ident))
import Text.Parsing.StringParser as StringParser
import Text.Parsing.StringParser.CodePoints (noneOf, skipSpaces)
import Text.Parsing.StringParser.Combinators (many1)
import Tidy.Codegen as Codegen

newtype Os = Os { name ∷ NonEmptyString, release ∷ NonEmptyString }

derive instance Generic Os _

instance EncodeJson Os where
  encodeJson = genericEncodeJson

instance Deserializable Unit String Os where
  deserialize _ = Codec.decode stringCodec

instance Serializable Unit (Expr e) Os where
  serialize _ = Codec.encode purescriptExpressionCodec

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

purescriptExpressionCodec
  ∷ ∀ e. BasicCodec (Either String) (Expr e) Os
purescriptExpressionCodec = Codec.basicCodec
  (const $ Left "parsing error")
  ( \(Os { name, release }) → Codegen.exprApp
      (Codegen.exprIdent $ Ident "Os.make")
      [ Codegen.exprRecord
          [ "name" /\ (Output.serialize_ name)
          , "release" /\ (Output.serialize_ release)
          ]
      ]
  )

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

make ∷ { name ∷ NonEmptyString, release ∷ NonEmptyString } → Os
make = Os
