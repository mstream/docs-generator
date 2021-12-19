module Execution.Result.Step
  ( Step(..)
  , bashCommandExecution
  , commentCreation
  , map
  ) where

import Prelude

import Ansi (Ansi)
import Ansi as Ansi
import Data.Argonaut (class EncodeJson, Json)
import Data.Argonaut as Argonaut
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Codec (BasicCodec)
import Data.Codec as Codec
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Output (class Serializable)
import PureScript.CST.Types (Expr, Ident(Ident))
import Tidy.Codegen as Codegen

data Step
  = BashCommandExecution { input ∷ String, output ∷ String }
  | CommentCreation String

derive instance Generic Step _

instance Show Step where
  show = genericShow

instance EncodeJson Step where
  encodeJson = genericEncodeJson

instance Serializable Unit Ansi Step where
  serialize _ = Codec.encode ansiCodec

instance Serializable Unit (Expr e) Step where
  serialize _ = Codec.encode purescriptExpressionCodec

instance Serializable Unit Json Step where
  serialize _ = Codec.encode jsonCodec

instance Serializable Unit String Step where
  serialize _ = Codec.encode stringCodec

ansiCodec ∷ BasicCodec (Either String) Ansi Step
ansiCodec = Codec.basicCodec
  (const $ Left "parsing error")
  ( case _ of
      BashCommandExecution { input, output } →
        (Ansi.printInput input)
          <> Ansi.newline
          <> (Ansi.printOutput output)
      CommentCreation s → Ansi.printComment s
  )

jsonCodec ∷ BasicCodec (Either String) Json Step
jsonCodec = Codec.basicCodec
  (const $ Left "parsing error")
  Argonaut.encodeJson

stringCodec ∷ BasicCodec (Either String) String Step
stringCodec = Codec.basicCodec
  (const $ Left "parsing error")
  ( case _ of
      BashCommandExecution { input, output } →
        "> "
          <> input
          <> "\n"
          <> output
      CommentCreation s → "> # " <> s
  )

bashCommandExecution ∷ { input ∷ String, output ∷ String } → Step
bashCommandExecution = BashCommandExecution

commentCreation ∷ String → Step
commentCreation = CommentCreation

purescriptExpressionCodec
  ∷ ∀ e. BasicCodec (Either String) (Expr e) Step
purescriptExpressionCodec = Codec.basicCodec
  (const $ Left "parsing error")
  ( case _ of
      BashCommandExecution { input, output } →
        Codegen.exprApp
          (Codegen.exprIdent $ Ident "Step.bashCommandExecution")
          [ Codegen.exprRecord
              [ "input" /\ (Codegen.exprString input)
              , "output" /\ (Codegen.exprString output)
              ]
          ]
      CommentCreation s →
        Codegen.exprApp
          (Codegen.exprIdent $ Ident "Step.commentCreation")
          [ Codegen.exprString s ]
  )

map
  ∷ ∀ a
  . { bashCommandExecution ∷ { input ∷ String, output ∷ String } → a
    , commentCreation ∷ String → a
    }
  → Step
  → a
map { bashCommandExecution, commentCreation } = case _ of
  BashCommandExecution { input, output } →
    bashCommandExecution { input, output }
  CommentCreation s → commentCreation s
