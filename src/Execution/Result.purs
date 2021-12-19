module Execution.Result
  ( Result(..)
  , make
  , os
  , steps
  , versions
  ) where

import Prelude

import Ansi (Ansi)
import Ansi as Ansi
import Data.Argonaut (class EncodeJson, Json)
import Data.Argonaut as Argonaut
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array as Array
import Data.Codec (BasicCodec)
import Data.Codec as Codec
import Data.Either (Either(Left))
import Data.Foldable as Foldable
import Data.FoldableWithIndex as FoldableWithIndex
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (Map)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Tuple.Nested ((/\))
import Execution.Result.Step (Step)
import Markdown (Markdown)
import Markdown as Markdown
import Os (Os)
import Output (class Serializable)
import Output as Output
import PureScript.CST.Types
  ( Expr
  , Ident(Ident)
  , Module
  , ModuleName(ModuleName)
  , Operator(Operator)
  , Proper(Proper)
  )
import Tidy.Codegen as Codegen

newtype Result = Result
  { os ∷ Os, steps ∷ List Step, versions ∷ Map String String }

derive instance Generic Result _

instance EncodeJson Result where
  encodeJson = genericEncodeJson

instance Serializable Unit Ansi Result where
  serialize _ = Codec.encode ansiCodec

instance Serializable Unit (Expr e) Result where
  serialize _ = Codec.encode purescriptExpressionCodec

instance Serializable Unit Json Result where
  serialize _ = Codec.encode jsonCodec

instance Serializable Unit Markdown Result where
  serialize _ = Codec.encode markdownCodec

instance Serializable NonEmptyString (Module Void) Result where
  serialize = Codec.encode <<< purescriptModuleCodec

instance Serializable Unit String Result where
  serialize _ = Codec.encode stringCodec

ansiCodec ∷ BasicCodec (Either String) Ansi Result
ansiCodec = Codec.basicCodec
  (const $ Left "parsing error")
  ( \(Result { os, steps, versions }) →
      ( Ansi.printComment $
          "OS info: " <> (Output.serialize_ os) <> "\n"
      )
        <>
          (Ansi.printComment "\n")
        <>
          (Ansi.printComment "Program versions\n")
        <>
          ( FoldableWithIndex.foldMapWithIndex
              ( \name version → Ansi.printComment $
                  name <> ": " <> version <> "\n"
              )
              versions
          )
        <>
          Ansi.printComment "\n"
        <>
          ( Foldable.intercalate
              Ansi.newline
              ( Output.serialize_ <$>
                  (Array.reverse $ Array.fromFoldable steps)
              )
          )
  )

jsonCodec ∷ BasicCodec (Either String) Json Result
jsonCodec = Codec.basicCodec
  (const $ Left "parsing error")
  Argonaut.encodeJson

markdownCodec ∷ BasicCodec (Either String) Markdown Result
markdownCodec = Codec.basicCodec
  (const $ Left "parsing error")
  (Markdown.codeBlock <<< Output.serialize_)

purescriptModuleCodec
  ∷ NonEmptyString → BasicCodec (Either String) (Module Void) Result

purescriptModuleCodec moduleName = Codec.basicCodec
  (const $ Left "parsing error")
  ( \result →
      let
        resultIdent = Ident "result"
      in
        Codegen.module_
          (ModuleName $ NES.toString moduleName)
          [ Codegen.exportValue resultIdent ]
          [ Codegen.declImportAs
              (ModuleName "Data.List")
              []
              (ModuleName "List")
          , Codegen.declImportAs
              (ModuleName "Data.Map")
              []
              (ModuleName "Map")
          , Codegen.declImport
              (ModuleName "Data.String.NonEmpty")
              [ Codegen.importValue $ Ident "nes" ]
          , Codegen.declImport
              (ModuleName "Execution.Result")
              [ Codegen.importType $ Proper "Result" ]
          , Codegen.declImportAs
              (ModuleName "Execution.Result")
              []
              (ModuleName "Result")
          , Codegen.declImportAs
              (ModuleName "Os")
              []
              (ModuleName "Os")
          , Codegen.declImport
              (ModuleName "Type.Proxy")
              [ Codegen.importTypeMembers
                  (Proper "Proxy")
                  [ Proper "Proxy" ]
              ]
          , Codegen.declImportAs
              (ModuleName "Execution.Result.Step")
              []
              (ModuleName "Step")
          , Codegen.declImport
              (ModuleName "Data.Tuple.Nested")
              [ Codegen.importOp $ Operator "/\\" ]
          ]
          [ Codegen.declSignature
              resultIdent
              (Codegen.typeCtor $ Proper "Result")
          , Codegen.declValue resultIdent
              []
              (Output.serialize_ result ∷ Expr _)
          ]
  )

purescriptExpressionCodec
  ∷ ∀ e. BasicCodec (Either String) (Expr e) Result
purescriptExpressionCodec = Codec.basicCodec
  (const $ Left "parsing error")
  ( \(Result { os, steps, versions }) → Codegen.exprApp
      (Codegen.exprIdent $ Ident "Result.make")
      [ Codegen.exprRecord
          [ "os" /\ (Output.serialize_ os ∷ Expr _)
          , "steps" /\
              ( Codegen.exprApp
                  (Codegen.exprIdent $ Ident "List.fromFoldable")
                  [ Codegen.exprArray
                      $ Array.reverse
                      $ Array.fromFoldable
                      $ Output.serialize_ <$> steps
                  ]
              )
          , "versions" /\ (Output.serialize_ versions)
          ]
      ]
  )

stringCodec ∷ BasicCodec (Either String) String Result
stringCodec = Codec.basicCodec
  (const $ Left "parsing error")
  ( \(Result { os, steps, versions }) →
      ("> # OS info: " <> (Output.serialize_ os) <> "\n")
        <>
          "\n"
        <>
          "> # Program versions\n"
        <>
          ( FoldableWithIndex.foldMapWithIndex
              ( \name version →
                  "> # " <> name <> ": " <> version <> "\n"
              )
              versions
          )
        <>
          "\n"
        <>
          ( Foldable.intercalate
              "\n"
              ( Output.serialize_ <$>
                  (Array.reverse $ Array.fromFoldable steps)
              )
          )
  )

make
  ∷ { os ∷ Os, steps ∷ List Step, versions ∷ Map String String }
  → Result
make = Result

os ∷ Result → Os
os (Result { os }) = os

steps ∷ Result → List Step
steps (Result { steps }) = steps

versions ∷ Result → Map String String
versions (Result { versions }) = versions
