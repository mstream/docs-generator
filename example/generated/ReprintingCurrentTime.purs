module Example.Results.ReprintingCurrentTime (result) where

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
      [ Step.commentCreation "Obtaining\x000020the\x000020current\x000020time"
      , Step.bashCommandExecution
          { input: "date"
          , output: "Sun\x000020Dec\x00002019\x00002013:40:19\x000020UTC\x0000202021\n"
          }
      , Step.commentCreation "Printing\x000020it\x000020back"
      , Step.bashCommandExecution
          { input:
              "echo\x000020'the\x000020time\x000020was:\x000020Sun\x000020Dec\x00002019\x00002013:40:19\x000020UTC\x0000202021'"
          , output:
              "the\x000020time\x000020was:\x000020Sun\x000020Dec\x00002019\x00002013:40:19\x000020UTC\x0000202021\n"
          }
      ]
  , versions: Map.fromFoldable [ "bash" /\ "5.1.4(1)-release" ]
  }
