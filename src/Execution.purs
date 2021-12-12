module Execution
  ( ExecutionResult
  , Step(..)
  , run
  ) where

import Prelude
import Data.FoldableWithIndex (foldMapWithIndex)
import Control.Monad.Free (foldFree)
import Data.Map (Map)
import Data.Map as Map
import Text.Parsing.StringParser (Parser, printParserError, runParser)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Data.Foldable (traverse_)
import Data.Traversable (traverse)
import Program
  ( BashCommand
  , Program
  , ProgramF
  , nixPackageNames
  , versionCommandsAndParsers
  )
import Data.List (List)
import Data.List as List
import Control.Monad.Trans.Class (lift)
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.State.Class (gets, modify_)
import Node.Buffer (toString)
import Node.Encoding (Encoding(UTF8))
import Control.Monad.Freer.Free (interpreter)
import Data.Array as Array
import Output
  ( class Codable
  , encode
  , printComment
  , printInput
  , printOutput
  )
import Control.Plus (empty)
import Data.Either (Either(Left), either)
import Data.String (joinWith, trim)
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
          (printInput input) <> "\n" <> (printOutput output)
        CommentCreation s → printComment s
    )

newtype ExecutionResult = ExecutionResult
  { os ∷ String, steps ∷ List Step, versions ∷ Map String String }

instance Codable String ExecutionResult where
  codec = basicCodec
    (const $ Left "parsing error")
    ( \(ExecutionResult { os, steps, versions }) →
        (printComment $ "OS version: " <> os <> "\n")
          <>
            (printComment "\n")
          <>
            (printComment "Program versions\n")
          <>
            ( foldMapWithIndex
                ( \name version → printComment $
                    name <> ": " <> version <> "\n"
                )
                versions
            )
          <>
            printComment "\n"
          <>
            ( joinWith
                "\n"
                ( encode <$>
                    (Array.reverse $ Array.fromFoldable steps)
                )
            )
    )

type State =
  { context ∷ Context
  , steps ∷ List Step
  , versions ∷ Map String String
  }

type Context = { execCommand ∷ String → Effect String }

interpret ∷ ProgramF ~> StateT State Effect
interpret = interpreter { bash, comment }

execShellCommand ∷ String → Effect String
execShellCommand input = do
  outputBuffer ← execSync
    input
    defaultExecSyncOptions
  toString UTF8 outputBuffer

execDockerCommand ∷ String → String → Effect String
execDockerCommand dockerContainerId input =
  execShellCommand $ "docker exec " <> dockerContainerId <> " " <> input

installNixPackage ∷ (String → Effect String) → String → Effect Unit
installNixPackage execCommand packageName =
  void $ execCommand ("nix-env -i " <> packageName)

getCommandVersion
  ∷ (String → Effect String)
  → { versionCommand ∷ String, responseParser ∷ Parser String }
  → Effect String
getCommandVersion execCommand { versionCommand, responseParser } = do
  output ← execCommand versionCommand
  either
    (throwException <<< error <<< printParserError)
    pure
    (runParser responseParser output)

bash ∷ BashCommand → StateT State Effect String
bash command = do
  let
    input = encode command
  { execCommand } ← gets (_.context)

  lift $ traverse_
    (installNixPackage execCommand)
    (nixPackageNames command)

  versions ← lift $ traverse
    (getCommandVersion execCommand)
    (versionCommandsAndParsers command)

  output ← lift $ execCommand input
  modify_ $ \state →
    state
      { steps =
          (List.singleton $ BashCommandExecution { input, output })
            <> state.steps
      , versions = Map.union state.versions versions
      }
  pure output

comment ∷ String → StateT State Effect Unit
comment s = do
  modify_ $ \state →
    state
      { steps = (List.singleton $ CommentCreation s) <> state.steps }

run ∷ Program Unit → Effect ExecutionResult
run program = do
  dockerContainerId ← trim <$> execShellCommand
    "docker run -d nixos/nix:2.3.12 sleep 60"
  let
    execCommand = execDockerCommand dockerContainerId
  os ← trim <$> execCommand "uname -o -r"
  void $ execCommand
    "nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs"
  void $ execCommand
    "nix-channel --update"
  _ /\ { steps, versions } ← runStateT
    (foldFree interpret program)
    { context: { execCommand }, steps: empty, versions: Map.empty }
  pure $ ExecutionResult { os, steps, versions }
