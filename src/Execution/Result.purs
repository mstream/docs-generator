module Execution.Result (Result) where

import Prelude
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map (Map)
import Text.Parsing.StringParser (Parser, printParserError, runParser)
import Data.Foldable (intercalate)
import Data.List (List)
import Data.Array as Array
import Ansi (Ansi)
import Ansi as Ansi
import Markdown (Markdown)
import Markdown as Markdown
import Output
  ( class Codable
  , encode
  )
import Type.Proxy (Proxy(Proxy))
import Data.Either (Either(Left))
import Data.Codec (basicCodec)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype Result = Result
  { os ∷ String, steps ∷ List Step, versions ∷ Map String String }

instance Codable Unit String Result where
  codec _ = basicCodec
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
                ( encode <$>
                    (Array.reverse $ Array.fromFoldable steps)
                )
            )
    )

instance Codable Unit Ansi Result where
  codec _ = basicCodec
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
                ( encode <$>
                    (Array.reverse $ Array.fromFoldable steps)
                )
            )
    )

instance Codable Unit Markdown Result where
  codec _ = basicCodec
    (const $ Left "parsing error")
    (Markdown.codeBlock <<< encode)

data Step
  = BashCommandExecution { input ∷ String, output ∷ String }
  | CommentCreation String

derive instance Generic Step _

instance Show Step where
  show = genericShow

instance Codable Unit String Step where
  codec _ = basicCodec
    (const $ Left "parsing error")
    ( case _ of
        BashCommandExecution { input, output } →
          "> "
            <> input
            <> "\n"
            <> output
        CommentCreation s → "> # " <> s
    )

instance Codable Unit Ansi Step where
  codec _ = basicCodec
    (const $ Left "parsing error")
    ( case _ of
        BashCommandExecution { input, output } →
          (Ansi.printInput input)
            <> Ansi.newline
            <> (Ansi.printOutput output)
        CommentCreation s → Ansi.printComment s
    )
