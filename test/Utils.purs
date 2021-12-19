module Test.Utils (generateSnapshots) where

import Prelude

import Ansi as Ansi
import Data.Argonaut as Argonaut
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Foldable as Foldable
import Data.NonEmpty ((:|))
import Data.Semigroup.Foldable as Foldable1
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Effect.Aff (Aff)
import Execution as Execution
import Markdown as Markdown
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path
import Output as Output
import Program (Program)
import Shell as Shell
import Tidy.Codegen as Codegen
import Type.Proxy (Proxy(Proxy))

generateSnapshots
  ∷ Program Unit
  → NonEmptyString
  → Aff Unit
generateSnapshots program programName = do
  executionResult ← Execution.run program
  Foldable.traverse_
    ( \{ contents, extension, validate } → do
        let
          filePath = Path.concat
            [ "example"
            , "generated"
            , (NES.toString programName) <> "." <> extension
            ]
        saveSnapshot filePath contents
        validate filePath
    )
    [ { contents: Output.serialize_ executionResult
      , extension: "txt"
      , validate: const $ pure unit
      }
    , { contents: Ansi.stringify $ Output.serialize_ executionResult
      , extension: "ansi"
      , validate: const $ pure unit
      }
    , { contents: Argonaut.stringify $ Output.serialize_ executionResult
      , extension: "json"
      , validate: \filePath → void
          $ Shell.executeCommand
          $ jqCommand filePath
      }
    , { contents: Markdown.stringify $ Output.serialize_ executionResult
      , extension: "md"
      , validate: \filePath → void
          $ Shell.executeCommand
          $ mdlCommand filePath
      }
    , { contents: Codegen.printModule $
          Output.serialize
            ( ( Foldable1.foldMap1
                  (\prefix → NES.appendString prefix ".")
                  ( NEA.fromNonEmpty $
                      NES.nes (Proxy ∷ Proxy "Example") :|
                        [ NES.nes (Proxy ∷ Proxy "Results") ]
                  )
              ) <>
                programName
            )
            executionResult
      , extension: "purs"
      , validate: \filePath → void
          $ Shell.executeCommand
          $ spagoBuildCommand filePath
      }
    ]

saveSnapshot ∷ FilePath → String → Aff Unit
saveSnapshot filePath contents = FS.writeTextFile
  UTF8
  filePath
  contents

jqCommand ∷ FilePath → String
jqCommand = append "jq . "

mdlCommand ∷ FilePath → String
mdlCommand filePath = "mdl --rules "
  <>
    ( Foldable.intercalate
        ","
        [ "MD001"
        , "MD002"
        , "MD003"
        , "MD004"
        , "MD005"
        , "MD006"
        , "MD007"
        , "MD009"
        , "MD010"
        , "MD011"
        , "MD012"
        , "MD014"
        , "MD018"
        , "MD019"
        , "MD020"
        , "MD021"
        , "MD022"
        , "MD023"
        , "MD024"
        , "MD025"
        , "MD026"
        , "MD027"
        , "MD028"
        , "MD029"
        , "MD030"
        , "MD031"
        , "MD032"
        , "MD033"
        , "MD034"
        , "MD035"
        , "MD036"
        , "MD037"
        , "MD038"
        , "MD039"
        , "MD046"
        ]
    )
  <> " "
  <> filePath

spagoBuildCommand ∷ FilePath → String
spagoBuildCommand = append "spago build --path "
