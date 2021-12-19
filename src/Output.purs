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
import Data.Foldable as Foldable
import Data.FoldableWithIndex as FoldableWithIndex
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.CST.Types
  ( Expr
  , Ident(Ident)
  , Operator(Operator)
  , Proper(Proper)
  )
import Tidy.Codegen as Codegen

class Serializable o a b | a → o where
  serialize ∷ o → b → a

class Deserializable o a b | a → o where
  deserialize ∷ o → a → String \/ b

serialize_ ∷ ∀ a b. Serializable Unit a b ⇒ b → a
serialize_ x = serialize unit x

deserialize_ ∷ ∀ a b. Deserializable Unit a b ⇒ a → String \/ b
deserialize_ x = deserialize unit x

instance Serializable Unit (Expr e) String where
  serialize _ = Codegen.exprString

instance Serializable Unit (Expr e) NonEmptyString where
  serialize _ nes = Codegen.exprApp (Codegen.exprIdent $ Ident "nes")
    [ Codegen.exprTyped
        (Codegen.exprCtor $ Proper "Proxy")
        ( Codegen.typeApp
            (Codegen.typeCtor $ Proper "Proxy")
            [ Codegen.typeString $ NES.toString nes ]
        )
    ]

instance
  ( Serializable Unit (Expr e) a
  , Serializable Unit (Expr e) b
  ) ⇒
  Serializable Unit (Expr e) (a /\ b) where
  serialize _ (x /\ y) = Codegen.exprOp
    (serialize_ x)
    [ Codegen.binaryOp (Operator "/\\") (serialize_ y) ]

instance
  ( Serializable Unit (Expr e) k
  , Serializable Unit (Expr e) v
  ) ⇒
  Serializable Unit (Expr e) (Map k v) where
  serialize _ kvs = Codegen.exprApp
    (Codegen.exprIdent $ Ident "Map.fromFoldable")
    [ Codegen.exprArray $ serialize_ <$> Map.toUnfoldable kvs ]

instance
  Serializable Unit String a ⇒
  Serializable Unit String (List a) where
  serialize _ = Foldable.foldMap $ \x →
    serialize_ x <> "\n"

instance
  ( Serializable Unit String k
  , Serializable Unit String v
  ) ⇒
  Serializable Unit String (Map k v) where
  serialize _ = FoldableWithIndex.foldMapWithIndex $ \k v →
    serialize_ k <> ": " <> serialize_ v <> "\n"

instance Serializable Unit String String where
  serialize _ = identity
