module Execution.Result
  ( Result
  , Step
  , bashCommandExecution
  , commentCreation
  , make
  ) where

import Prelude

import Ansi (Ansi)
import Ansi as Ansi
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
import Output (class Serializable)
import Output as Output
import Text.Parsing.StringParser (Parser, printParserError, runParser)
import Type.Proxy (Proxy(Proxy))

newtype Result = Result
  { os ∷ String, steps ∷ List Step, versions ∷ Map String String }

instance Serializable Unit String Result where
  serialize _ = Codec.encode stringCodec

instance Serializable Unit Ansi Result where
  serialize _ = Codec.encode ansiCodec

instance Serializable Unit Markdown Result where
  serialize _ = Codec.encode markdownCodec

data Step
  = BashCommandExecution { input ∷ String, output ∷ String }
  | CommentCreation String

derive instance Generic Step _

instance Show Step where
  show = genericShow

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
      (Ansi.printComment $ "OS version: " <> os <> "\n")
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

markdownCodec ∷ BasicCodec (Either String) Markdown Result
markdownCodec = Codec.basicCodec
  (const $ Left "parsing error")
  (Markdown.codeBlock <<< Output.serialize_)

stringCodec ∷ BasicCodec (Either String) String Result
stringCodec = Codec.basicCodec
  (const $ Left "parsing error")
  ( \(Result { os, steps, versions }) →
      ("> # OS version: " <> os <> "\n")
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
  ∷ { os ∷ String, steps ∷ List Step, versions ∷ Map String String }
  → Result
make = Result
