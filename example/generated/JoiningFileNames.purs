module Example.Results.JoiningFileNames (result) where

import Data.List as List
import Data.Map as Map
import Data.String.NonEmpty (nes)
import Execution.Result (Result)
import Execution.Result as Result
import Os as Os
import Type.Proxy (Proxy(Proxy))
import Execution.Result.Step as Step
import Data.Tuple.Nested ((/\))

result :: Result
result = Result.make
  { os: Os.make
      { name: nes (Proxy :: Proxy "Linux"), release: nes (Proxy :: Proxy "5.10.47-linuxkit") }
  , steps: List.fromFoldable
      [ Step.commentCreation
          "Obtaining\x000020names\x000020of\x000020files\x000020in\x000020the\x000020current\x000020directory"
      , Step.bashCommandExecution
          { input: "ls"
          , output:
              "bin\ndev\netc\nhome\nlib\nmedia\nmnt\nnix\nopt\nproc\nroot\nrun\nsbin\nsrv\nsys\ntmp\nusr\nvar\n"
          }
      , Step.commentCreation "Printing\x000020names\x000020joined\x000020with\x000020a\x000020comma"
      , Step.bashCommandExecution
          { input:
              "echo\x000020'names\x000020of\x000020files\x000020in\x000020the\x000020current\x000020directory:\x000020bin,\x000020dev,\x000020etc,\x000020home,\x000020lib,\x000020media,\x000020mnt,\x000020nix,\x000020opt,\x000020proc,\x000020root,\x000020run,\x000020sbin,\x000020srv,\x000020sys,\x000020tmp,\x000020usr,\x000020var'"
          , output:
              "names\x000020of\x000020files\x000020in\x000020the\x000020current\x000020directory:\x000020bin,\x000020dev,\x000020etc,\x000020home,\x000020lib,\x000020media,\x000020mnt,\x000020nix,\x000020opt,\x000020proc,\x000020root,\x000020run,\x000020sbin,\x000020srv,\x000020sys,\x000020tmp,\x000020usr,\x000020var\n"
          }
      ]
  , versions: Map.fromFoldable [ "bash" /\ "5.1.4(1)-release" ]
  }
