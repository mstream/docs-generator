module Main where

import Prelude
import Data.FoldableWithIndex (foldMapWithIndex)
import Program (Program, f, run)
import Step (Step, toExpr)
import Data.Array as Array
import Data.List (List)
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Map (Map, keys, singleton)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Tidy.Codegen
  ( declImport
  , declValue
  , exportValue
  , exprArray
  , importValue
  , module_
  , printModule
  )
import PureScript.CST.Types (Declaration, Expr, Module)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(Proxy))

main ∷ Effect Unit
main = do
  steps ← run program
  log $ printModule $ generateModule $ singleton
    (NES.nes (Proxy ∷ Proxy "demo"))
    steps

generateModule ∷ Map NonEmptyString (List Step) → Module Void
generateModule stepsByName = unsafePartial $ module_
  "CodegenExamples"
  ( exportValue <<< NES.toString <$>
      (Array.fromFoldable $ keys stepsByName)
  )
  [ declImport "Step" [ importValue "step" ] ]
  ( foldMapWithIndex (\name steps → [ stepsToDeclaration name steps ])
      stepsByName
  )

stepsToDeclaration
  ∷ ∀ e. Partial ⇒ NonEmptyString → List Step → Declaration e
stepsToDeclaration name steps = declValue
  (NES.toString name)
  []
  (exprArray $ toExpr <$> Array.fromFoldable steps)

program ∷ Program Unit
program = do
  output ← f.command "date"
  void $ f.command $ "echo 'the time is: " <> output <> "'"
