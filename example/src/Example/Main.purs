module Example.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Example.Results.ReprintingCurrentTime as ReprintingCurrentTime
import Example.Terminal as Terminal
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.Storybook (Stories, runStorybook)

main ∷ Effect Unit
main = HA.runHalogenAff do
  HA.awaitBody >>= runStorybook
    { stories: examples
    , logo: Just $ HH.text "Demo Examples"
    }

examples ∷ Stories Aff
examples =
  Object.fromFoldable
    [ "" /\ index
    , "ReprintingCurrentTime" /\
        (Terminal.render ReprintingCurrentTime.result)
    ]
  where
  index = Hooks.component \_ _ → Hooks.pure do
    HH.div_
      [ HH.h1_
          [ HH.text "Program demos" ]
      ]
