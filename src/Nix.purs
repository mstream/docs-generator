module Nix
  ( class Installable
  , Channel
  , ChannelName
  , ChannelUrl
  , PackageName
  , addChannel
  , channelName
  , channelUrl
  , installPackage
  , nixos2105
  , packageName
  , packageNames
  , updateChannels
  ) where

import Prelude

import Data.Set (Set)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Type.Proxy (Proxy(Proxy))

class Installable a where
  packageNames ∷ a → Set PackageName

newtype Channel = Channel NonEmptyString
newtype ChannelName = ChannelName NonEmptyString
newtype ChannelUrl = ChannelUrl NonEmptyString
newtype PackageName = PackageName NonEmptyString

derive newtype instance Eq PackageName
derive newtype instance Ord PackageName

nixos2105 ∷ Channel
nixos2105 = Channel $ NES.nes (Proxy ∷ Proxy "nixos-21.05")

channelName ∷ NonEmptyString → ChannelName
channelName = ChannelName

channelUrl ∷ Channel → ChannelUrl
channelUrl (Channel channel) = ChannelUrl $
  NES.nes (Proxy ∷ Proxy "https://nixos.org/channels/") <> channel

addChannelCommand ∷ ChannelName → ChannelUrl → String
addChannelCommand (ChannelName name) (ChannelUrl url) =
  "nix-channel --add "
    <> (NES.toString url)
    <> " "
    <> (NES.toString name)

updateChannelsCommand ∷ String
updateChannelsCommand = "nix-channel --update"

installPackageCommand ∷ PackageName → String
installPackageCommand (PackageName pkg) =
  "nix-env -i " <> (NES.toString pkg)

packageName ∷ NonEmptyString → PackageName
packageName = PackageName

addChannel
  ∷ ∀ f
  . Functor f
  ⇒ (String → f String)
  → ChannelName
  → ChannelUrl
  → f Unit
addChannel execCommand channelName channelUrl = void $ execCommand $
  addChannelCommand channelName channelUrl

installPackage
  ∷ ∀ f. Functor f ⇒ (String → f String) → PackageName → f Unit
installPackage execCommand packageName = void $ execCommand $
  installPackageCommand packageName

updateChannels ∷ ∀ f. Functor f ⇒ (String → f String) → f Unit
updateChannels execCommand = void $ execCommand updateChannelsCommand
