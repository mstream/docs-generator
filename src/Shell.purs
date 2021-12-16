module Shell (executeCommand) where

import Prelude

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer as Buffer
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding(UTF8))

executeCommand ∷ String → Aff String
executeCommand input = liftEffect $ do
  outputBuffer ← ChildProcess.execSync
    input
    ChildProcess.defaultExecSyncOptions
  Buffer.toString UTF8 outputBuffer
