module Execution
  ( run
  ) where

import Prelude

import Ansi (Ansi)
import Ansi as Ansi
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (foldFree)
import Control.Monad.Freer.Free (interpreter)
import Control.Monad.State.Class (gets, modify_)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Plus (empty)
import Data.Array as Array
import Data.Codec (basicCodec)
import Data.Either (Either(Left), either)
import Data.Foldable (intercalate, traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Show.Generic (genericShow)
import Data.String (joinWith, trim)
import Data.String.NonEmpty as NES
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Docker as Docker
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Execution.Result (Result, Step)
import Execution.Result as Result
import Markdown (Markdown)
import Markdown as Markdown
import Nix (PackageName)
import Nix as Nix
import Node.Buffer (toString)
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(UTF8))
import Output (class Serializable)
import Output as Output
import Program
  ( BashCommand
  , Program
  , ProgramF
  , versionCommandsAndParsers
  )
import Text.Parsing.StringParser (Parser, printParserError, runParser)
import Type.Proxy (Proxy(Proxy))

type State =
  { context ∷ Context
  , steps ∷ List Step
  , versions ∷ Map String String
  }

type Context = { execCommand ∷ String → Aff String }

interpret ∷ ProgramF ~> StateT State Aff
interpret = interpreter { bash, comment }

execShellCommand ∷ String → Aff String
execShellCommand input = liftEffect $ do
  outputBuffer ← execSync
    input
    defaultExecSyncOptions
  toString UTF8 outputBuffer

getCommandVersion
  ∷ (String → Aff String)
  → { versionCommand ∷ String, responseParser ∷ Parser String }
  → Aff String
getCommandVersion execCommand { versionCommand, responseParser } = do
  output ← execCommand versionCommand
  either
    (throwError <<< Exception.error <<< printParserError)
    pure
    (runParser responseParser output)

bash ∷ BashCommand → StateT State Aff String
bash command = do
  let
    input = Output.serialize_ command
  { execCommand } ← gets (_.context)

  lift $ traverse_
    (Nix.installPackage execCommand)
    (Nix.packageNames command)

  versions ← lift $ traverse
    (getCommandVersion execCommand)
    (versionCommandsAndParsers command)

  output ← lift $ execCommand input
  modify_ $ \state →
    state
      { steps =
          ( List.singleton $ Result.bashCommandExecution
              { input, output }
          )
            <> state.steps
      , versions = Map.union state.versions versions
      }
  pure output

comment ∷ String → StateT State Aff Unit
comment s = do
  modify_ $ \state →
    state
      { steps = (List.singleton $ Result.commentCreation s) <>
          state.steps
      }

run ∷ Program Unit → Aff Result
run program = do
  Docker.executeInContainer
    execShellCommand
    Docker.nixosNix
    containerProgram
  where
  containerProgram execCommand = do
    os ← trim <$> execCommand "uname -o -r"
    Nix.addChannel
      execCommand
      (Nix.channelName $ NES.nes (Proxy ∷ Proxy "nixpkgs"))
      (Nix.channelUrl Nix.nixos2105)
    Nix.updateChannels execCommand
    _ /\ { steps, versions } ← runStateT
      (foldFree interpret program)
      { context: { execCommand }, steps: empty, versions: Map.empty }
    pure $ Result.make { os, steps, versions }
