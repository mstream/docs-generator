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
import Data.String as String
import Data.Maybe as Maybe
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Type.Proxy (Proxy(Proxy))
import Data.Foldable (class Foldable)
import Data.Set (Set)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Effect.Exception (Error)
import Effect.Exception as Exception

newtype ContainerId = ContainerId NonEmptyString
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
  ∷ ∀ a m
  . MonadThrow Error m
  ⇒ (String → m String)
  → Image
  → ((String → m String) → m a)
  → m a
executeInContainer executeCommand image program = do
  containerId ← run executeCommand image
  result ← program $ exec executeCommand containerId
  stop executeCommand containerId
  rm executeCommand containerId
  pure result

exec ∷ ∀ f. (String → f String) → ContainerId → String → f String
exec executeCommand containerId command = executeCommand
  (execCommand containerId command)

rm ∷ ∀ f. Functor f ⇒ (String → f String) → ContainerId → f Unit
rm executeCommand containerId = void $ executeCommand
  (rmCommand containerId)

run
  ∷ ∀ m
  . MonadThrow Error m
  ⇒ (String → m String)
  → Image
  → m ContainerId
run executeCommand image = do
  output ← executeCommand (runCommand image)
  Maybe.maybe
    (throwError $ Exception.error "no container ID returned")
    (pure <<< ContainerId)
    (NES.fromString $ String.trim output)

stop ∷ ∀ f. Functor f ⇒ (String → f String) → ContainerId → f Unit
stop executeCommand containerId = void $ executeCommand
  (stopCommand containerId)
