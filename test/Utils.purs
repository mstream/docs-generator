module Test.Utils (generateSnapshot) where

import Prelude
import Program (Program)
import Execution as Execution
import Output as Output
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Effect (Effect)
import Ansi as Ansi

generateSnapshot ∷ Program Unit → FilePath → Effect Unit
generateSnapshot program filePath = do
  executionResult ← Execution.run program
  FS.writeTextFile
    UTF8
    filePath
    (Ansi.toString $ Output.encode executionResult)
