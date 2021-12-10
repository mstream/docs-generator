module Step (Step, step, toExpr) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Tuple.Nested ((/\))
import Data.Show.Generic (genericShow)
import PureScript.CST.Types (Declaration, Expr, Module)
import Tidy.Codegen
  ( exprApp
  , exprCtor
  , exprIdent
  , exprRecord
  , exprString
  )

newtype Step = Step { input ∷ String, output ∷ String }

derive instance Generic Step _

instance Show Step where
  show = genericShow

step ∷ { input ∷ String, output ∷ String } → Step
step { input, output } = Step { input, output }

toExpr ∷ ∀ e. Partial ⇒ Step → Expr e
toExpr (Step { input, output }) = exprApp
  (exprIdent "step")
  [ exprRecord
      [ "input" /\ exprString input
      , "output" /\ exprString output
      ]
  ]
