module Execution
  ( run
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Free (foldFree)
import Control.Monad.Freer.Free (interpreter)
import Control.Monad.State.Class (gets, modify_)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Plus (empty)
import Data.Either (either)
import Data.Foldable (traverse_)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.String.NonEmpty as NES
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Docker as Docker
import Effect.Aff (Aff)
import Effect.Exception as Exception
import Execution.Result (Result, Step)
import Execution.Result as Result
import Nix as Nix
import Os as Os
import Output as Output
import Program
  ( BashCommand
  , Program
  , ProgramF
  , versionCommandsAndParsers
  )
import Shell as Shell
import Text.Parsing.StringParser (Parser, printParserError, runParser)
import Type.Proxy (Proxy(Proxy))

type State =
  { context ∷ Context
  , steps ∷ List Step
  , versions ∷ Map String String
  }

type Context = { executeCommand ∷ String → Aff String }

interpret ∷ ProgramF ~> StateT State Aff
interpret = interpreter { bash, comment }

getCommandVersion
  ∷ (String → Aff String)
  → { versionCommand ∷ String, responseParser ∷ Parser String }
  → Aff String
getCommandVersion executeCommand { versionCommand, responseParser } = do
  output ← executeCommand versionCommand
  either
    (throwError <<< Exception.error <<< printParserError)
    pure
    (runParser responseParser output)

bash ∷ BashCommand → StateT State Aff String
bash command = do
  let
    input = Output.serialize_ command
  { executeCommand } ← gets (_.context)

  lift $ traverse_
    (Nix.installPackage executeCommand)
    (Nix.packageNames command)

  versions ← lift $ traverse
    (getCommandVersion executeCommand)
    (versionCommandsAndParsers command)

  output ← lift $ executeCommand input
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
    Shell.executeCommand
    Docker.nixosNix
    containerProgram
  where
  containerProgram executeCommand = do
    os ← Os.uname executeCommand
    Nix.addChannel
      executeCommand
      (Nix.channelName $ NES.nes (Proxy ∷ Proxy "nixpkgs"))
      (Nix.channelUrl Nix.nixos2105)
    Nix.updateChannels executeCommand
    _ /\ { steps, versions } ← runStateT
      (foldFree interpret program)
      { context: { executeCommand }, steps: empty, versions: Map.empty }
    pure $ Result.make { os, steps, versions }
