module Execution.Result
  ( Result(..)
  , Step(..)
  , bashCommandExecution
  , commentCreation
  , make
  ) where

import Prelude

import Ansi (Ansi)
import Ansi as Ansi
import Data.Argonaut (class EncodeJson, Json)
import Data.Argonaut as Argonaut
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array as Array
import Data.Codec (BasicCodec)
import Data.Codec as Codec
import Data.Either (Either(Left))
import Data.Foldable (intercalate)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (Map)
import Data.Show.Generic (genericShow)
import Markdown (Markdown)
import Markdown as Markdown
import Os (Os)
import Output (class Serializable)
import Output as Output

newtype Result = Result
  { os ∷ Os, steps ∷ List Step, versions ∷ Map String String }

derive instance Generic Result _

instance EncodeJson Result where
  encodeJson = genericEncodeJson

instance Serializable Unit String Result where
  serialize _ = Codec.encode stringCodec

instance Serializable Unit Ansi Result where
  serialize _ = Codec.encode ansiCodec

instance Serializable Unit Json Result where
  serialize _ = Codec.encode jsonCodec

instance Serializable Unit Markdown Result where
  serialize _ = Codec.encode markdownCodec

data Step
  = BashCommandExecution { input ∷ String, output ∷ String }
  | CommentCreation String

derive instance Generic Step _

instance Show Step where
  show = genericShow

instance EncodeJson Step where
  encodeJson = genericEncodeJson

instance Serializable Unit String Step where
  serialize _ = Codec.encode $ Codec.basicCodec
    (const $ Left "parsing error")
    ( case _ of
        BashCommandExecution { input, output } →
          "> "
            <> input
            <> "\n"
            <> output
        CommentCreation s → "> # " <> s
    )

instance Serializable Unit Json Step where
  serialize _ = Codec.encode $ Codec.basicCodec
    (const $ Left "parsing error")
    Argonaut.encodeJson

instance Serializable Unit Ansi Step where
  serialize _ = Codec.encode $ Codec.basicCodec
    (const $ Left "parsing error")
    ( case _ of
        BashCommandExecution { input, output } →
          (Ansi.printInput input)
            <> Ansi.newline
            <> (Ansi.printOutput output)
        CommentCreation s → Ansi.printComment s
    )

ansiCodec ∷ BasicCodec (Either String) Ansi Result
ansiCodec = Codec.basicCodec
  (const $ Left "parsing error")
  ( \(Result { os, steps, versions }) →
      ( Ansi.printComment $
          "OS info: " <> (Output.serialize_ os) <> "\n"
      )
        <>
          (Ansi.printComment "\n")
        <>
          (Ansi.printComment "Program versions\n")
        <>
          ( foldMapWithIndex
              ( \name version → Ansi.printComment $
                  name <> ": " <> version <> "\n"
              )
              versions
          )
        <>
          Ansi.printComment "\n"
        <>
          ( intercalate
              Ansi.newline
              ( Output.serialize_ <$>
                  (Array.reverse $ Array.fromFoldable steps)
              )
          )
  )

jsonCodec ∷ BasicCodec (Either String) Json Result
jsonCodec = Codec.basicCodec
  (const $ Left "parsing error")
  Argonaut.encodeJson

markdownCodec ∷ BasicCodec (Either String) Markdown Result
markdownCodec = Codec.basicCodec
  (const $ Left "parsing error")
  (Markdown.codeBlock <<< Output.serialize_)

stringCodec ∷ BasicCodec (Either String) String Result
stringCodec = Codec.basicCodec
  (const $ Left "parsing error")
  ( \(Result { os, steps, versions }) →
      ("> # OS info: " <> (Output.serialize_ os) <> "\n")
        <>
          "\n"
        <>
          "> # Program versions\n"
        <>
          ( foldMapWithIndex
              ( \name version →
                  "> # " <> name <> ": " <> version <> "\n"
              )
              versions
          )
        <>
          "\n"
        <>
          ( intercalate
              ""
              ( Output.serialize_ <$>
                  (Array.reverse $ Array.fromFoldable steps)
              )
          )
  )

bashCommandExecution ∷ { input ∷ String, output ∷ String } → Step
bashCommandExecution = BashCommandExecution

commentCreation ∷ String → Step
commentCreation = CommentCreation

make
  ∷ { os ∷ Os, steps ∷ List Step, versions ∷ Map String String }
  → Result
make = Result
