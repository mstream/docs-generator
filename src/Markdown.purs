module Markdown (Markdown, codeBlock, stringify) where

import Prelude

import Data.Codec (BasicCodec)
import Data.Codec as Codec
import Data.Either (Either(Left))
import Output (class Serializable)

data Markdown =
  CodeBlock String

instance Serializable Unit String Markdown where
  serialize _ = Codec.encode codec

codec ∷ BasicCodec (Either String) String Markdown
codec = Codec.basicCodec
  (const $ Left "parsing error")
  ( case _ of
      CodeBlock s → "```\n" <> s <> "\n```"
  )

codeBlock ∷ String → Markdown
codeBlock = CodeBlock

stringify ∷ Markdown → String
stringify = Codec.encode codec
