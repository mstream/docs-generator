module Test.Utils (generateSnapshots) where

import Prelude

import Ansi as Ansi
import Data.Argonaut as Argonaut
import Data.Foldable (traverse_)
import Effect.Aff (Aff)
import Execution as Execution
import Markdown as Markdown
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Output as Output
import Program (Program)

generateSnapshots ∷ Program Unit → FilePath → Aff Unit
generateSnapshots program filePathBase = do
  executionResult ← Execution.run program
  traverse_
    ( \{ contents, extension } → FS.writeTextFile
        UTF8
        (filePathBase <> "." <> extension)
        contents
    )
    [ { contents: Output.serialize_ executionResult
      , extension: "txt"
      }
    , { contents: Ansi.stringify $ Output.serialize_ executionResult
      , extension: "ansi"
      }
    , { contents: Argonaut.stringify $ Output.serialize_ executionResult
      , extension: "json"
      }
    , { contents: Markdown.stringify $ Output.serialize_ executionResult
      , extension: "md"
      }
    ]
