module Execution
  ( ExecutionResult
  , Step(..)
  , run
  ) where

import Prelude
import Control.Monad.Free (foldFree)
import Effect (Effect)
import Program (BashCommand, Program, ProgramF)
import Data.List (List, (:))
import Control.Monad.Trans.Class (lift)
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.State.Class (modify_)
import Node.Buffer (toString)
import Node.Encoding (Encoding(UTF8))
import Control.Monad.Freer.Free (interpreter)
import Data.Array as Array
import Output (class Codable, encode)
import Control.Plus (empty)
import Data.Either (Either(Left))
import Data.String (joinWith)
import Data.Codec (basicCodec)
import Data.Generic.Rep (class Generic)
import Data.Tuple.Nested ((/\))
import Data.Show.Generic (genericShow)

data Step
  = BashCommandExecution { input ∷ String, output ∷ String }
  | CommentCreation String

derive instance Generic Step _

instance Show Step where
  show = genericShow

instance Codable String Step where
  codec = basicCodec
    (const $ Left "parsing error")
    ( case _ of
        BashCommandExecution { input, output } →
          "> " <> input <> "\n" <> output
        CommentCreation s → "> # " <> s
    )

newtype ExecutionResult = ExecutionResult { steps ∷ List Step }

instance Semigroup ExecutionResult where
  append (ExecutionResult r1) (ExecutionResult r2) = ExecutionResult
    { steps: r1.steps <> r2.steps }

instance Monoid ExecutionResult where
  mempty = ExecutionResult { steps: empty }

instance Codable String ExecutionResult where
  codec = basicCodec
    (const $ Left "parsing error")
    ( \(ExecutionResult { steps }) →
        joinWith
          "\n"
          (encode <$> (Array.reverse $ Array.fromFoldable steps))
    )

interpret ∷ ProgramF ~> StateT ExecutionResult Effect
interpret = interpreter { bash, comment }

bash ∷ BashCommand → StateT ExecutionResult Effect String
bash command = do
  let
    input = encode command
  outputBuffer ← lift $
    execSync
      input
      defaultExecSyncOptions
  output ← lift $ toString UTF8 outputBuffer
  modify_ $ \(ExecutionResult result) → ExecutionResult $
    result
      { steps = BashCommandExecution { input, output } : result.steps }
  pure output

comment ∷ String → StateT ExecutionResult Effect Unit
comment s = do
  modify_ $ \(ExecutionResult result) → ExecutionResult $
    result { steps = CommentCreation s : result.steps }

run ∷ Program Unit → Effect ExecutionResult
run program = do
  _ /\ result ← runStateT (foldFree interpret program) mempty
  pure result
