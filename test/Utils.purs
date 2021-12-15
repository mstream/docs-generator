module Test.Utils (generateSnapshots) where

import Prelude

import Ansi as Ansi
import Data.Foldable (traverse_)
import Effect (Effect)
import Execution as Execution
import Markdown as Markdown
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Output as Output
import Program (Program)

generateSnapshots ∷ Program Unit → FilePath → Effect Unit
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
    , { contents: Markdown.stringify $ Output.serialize_ executionResult
      , extension: "md"
      }
    ]
