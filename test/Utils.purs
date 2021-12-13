module Test.Utils (generateSnapshots) where

import Prelude
import Data.Foldable (traverse_)
import Program (Program)
import Execution (ExecutionResult)
import Execution as Execution
import Output as Output
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Effect (Effect)
import Output (class Codable)
import Ansi as Ansi
import Markdown as Markdown

generateSnapshots ∷ Program Unit → FilePath → Effect Unit
generateSnapshots program filePathBase = do
  executionResult ← Execution.run program
  traverse_
    ( \{ contents, extension } → FS.writeTextFile
        UTF8
        (filePathBase <> "." <> extension)
        contents
    )
    [ { contents: Output.encode executionResult
      , extension: "txt"
      }
    , { contents: Ansi.toString $ Output.encode executionResult
      , extension: "ansi"
      }
    , { contents: Markdown.toString $ Output.encode executionResult
      , extension: "md"
      }
    ]
