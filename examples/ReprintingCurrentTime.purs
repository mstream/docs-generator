module ReprintingCurrentTime (result) where

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
      [ Step.bashCommandExecution
          { input:
              "echo\x000020'the\x000020time\x000020was:\x000020Fri\x000020Dec\x00002017\x00002015:00:23\x000020UTC\x0000202021'"
          , output:
              "the\x000020time\x000020was:\x000020Fri\x000020Dec\x00002017\x00002015:00:23\x000020UTC\x0000202021\n"
          }
      , Step.commentCreation "Printing\x000020it\x000020back"
      , Step.bashCommandExecution
          { input: "date"
          , output: "Fri\x000020Dec\x00002017\x00002015:00:23\x000020UTC\x0000202021\n"
          }
      , Step.commentCreation "Obtaining\x000020the\x000020current\x000020time"
      ]
  , versions: Map.fromFoldable [ "bash" /\ "5.1.4(1)-release" ]
  }
