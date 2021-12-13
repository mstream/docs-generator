module Markdown (Markdown, codeBlock, toString) where

import Prelude

import Data.Codec (basicCodec)
import Data.Either (Either(Left), either)
import Output (class Codable)
import Output as Output

data Markdown =
  CodeBlock String

instance Codable Unit String Markdown where
  codec _ = basicCodec
    (const $ Left "parsing error")
    ( case _ of
        CodeBlock s → "```\n" <> s <> "\n```"
    )

codeBlock ∷ String → Markdown
codeBlock = CodeBlock

toString ∷ Markdown → String
toString = Output.encode
