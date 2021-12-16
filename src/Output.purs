module Output
  ( class Deserializable
  , class Serializable
  , deserialize
  , deserialize_
  , serialize
  , serialize_
  ) where

import Prelude

import Data.Either.Nested (type (\/))

class Serializable o a b | a → o where
  serialize ∷ o → b → a

class Deserializable o a b | a → o where
  deserialize ∷ o → a → String \/ b

serialize_ ∷ ∀ a b. Serializable Unit a b ⇒ b → a
serialize_ x = serialize unit x

deserialize_ ∷ ∀ a b. Deserializable Unit a b ⇒ a → String \/ b
deserialize_ x = deserialize unit x
