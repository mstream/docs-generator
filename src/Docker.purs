module Docker
  ( ContainerId
  , Image
  , exec
  , executeInContainer
  , nixosNix
  , rm
  , run
  , stop
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Parallel (parSequence_)
import Data.Maybe as Maybe
import Data.Posix.Signal (Signal(SIGINT))
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Node.Process as Process
import Output (class Serializable)
import Output as Output
import Type.Proxy (Proxy(Proxy))

newtype ContainerId = ContainerId NonEmptyString

instance Serializable Unit String ContainerId where
  serialize _ (ContainerId nes) = NES.toString nes

newtype Image = Image NonEmptyString

nixosNix ∷ Image
nixosNix = Image $ NES.nes (Proxy ∷ Proxy "nixos/nix:2.3.12")

execCommand ∷ ContainerId → String → String
execCommand (ContainerId id) command =
  "docker exec " <> NES.toString id <> " " <> command

rmCommand ∷ ContainerId → String
rmCommand (ContainerId id) =
  "docker rm " <> NES.toString id

runCommand ∷ Image → String
runCommand (Image img) =
  "docker run --entrypoint /bin/sh -dit " <> NES.toString img

stopCommand ∷ ContainerId → String
stopCommand (ContainerId id) =
  "docker stop " <> NES.toString id

executeInContainer
  ∷ ∀ a
  . (String → Aff String)
  → Image
  → ((String → Aff String) → Aff a)
  → Aff a
executeInContainer executeCommand image program = do
  containerId ← run executeCommand image
  let
    cleanUp = cleanUpContainer executeCommand containerId
  parSequence_
    [ liftEffect $ Process.onBeforeExit (Aff.launchAff_ cleanUp)
    -- BUG: does not work
    , liftEffect $ Process.onSignal SIGINT (Aff.launchAff_ cleanUp)
    , logSandboxStart containerId
    ]
  result ← program $ exec executeCommand containerId
  pure result

cleanUpContainer ∷ (String → Aff String) → ContainerId → Aff Unit
cleanUpContainer executeCommand containerId = do
  logSandboxCleanUp containerId
  stop executeCommand containerId
  rm executeCommand containerId

exec ∷ (String → Aff String) → ContainerId → String → Aff String
exec executeCommand containerId command = executeCommand
  (execCommand containerId command)

rm ∷ (String → Aff String) → ContainerId → Aff Unit
rm executeCommand containerId = void $
  executeCommand (rmCommand containerId)

run
  ∷ (String → Aff String)
  → Image
  → Aff ContainerId
run executeCommand image = do
  output ← executeCommand (runCommand image)
  Maybe.maybe
    (throwError $ Exception.error "no container ID returned")
    (pure <<< ContainerId)
    (NES.fromString $ String.trim output)

stop ∷ (String → Aff String) → ContainerId → Aff Unit
stop executeCommand containerId = void $
  executeCommand (stopCommand containerId)

logSandboxStart ∷ ContainerId → Aff Unit
logSandboxStart containerId = Console.error $
  "sandbox runing inside container " <> (Output.serialize_ containerId)

logSandboxCleanUp ∷ ContainerId → Aff Unit
logSandboxCleanUp containerId = Console.error $
  "cleaning ip sandbox of container " <> (Output.serialize_ containerId)
