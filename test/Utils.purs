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
import Shell as Shell

generateSnapshots ∷ Program Unit → FilePath → Aff Unit
generateSnapshots program filePathBase = do
  executionResult ← Execution.run program
  traverse_
    ( \{ contents, extension, validate } → do
        let
          filePath = filePathBase <> "." <> extension
        saveSnapshot filePath contents
        validate filePath
    )
    [ { contents: Output.serialize_ executionResult
      , extension: "txt"
      , validate: const $ pure unit
      }
    , { contents: Ansi.stringify $ Output.serialize_ executionResult
      , extension: "ansi"
      , validate: const $ pure unit
      }
    , { contents: Argonaut.stringify $ Output.serialize_ executionResult
      , extension: "json"
      , validate: \filePath → void
          $ Shell.executeCommand
          $ "jq . " <> filePath
      }
    , { contents: Markdown.stringify $ Output.serialize_ executionResult
      , extension: "md"
      , validate: \filePath → void
          $ Shell.executeCommand
          $ "mdl " <> filePath
      }
    ]

saveSnapshot ∷ FilePath → String → Aff Unit
saveSnapshot filePath contents =
  FS.writeTextFile UTF8 filePath contents
