module Example.Terminal (render) where

import Prelude

import Data.Array as Array
import Execution.Result (Result)
import Execution.Result as Result
import Execution.Result.Step as Step
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Output as Output
import Web.HTML.Common (ClassName(ClassName))

render ∷ ∀ i m o q. Result → H.Component q i o m
render result = Hooks.component \_ _ → Hooks.pure do
  HH.div_
    [ HH.h1_
        [ HH.text "Demo" ]
    , HH.h2_
        [ HH.text "OS" ]
    , HH.p_
        [ HH.text $ Output.serialize_ $ Result.os result
        ]
    , HH.h2_
        [ HH.text "Program versions" ]
    , HH.p_
        [ HH.text $ Output.serialize_ $ Result.versions result
        ]
    , HH.h2_
        [ HH.text "Output" ]
    , renderWindow $ Result.steps result
    ]
  where
  renderWindow steps =
    HH.div_
      [ HH.div
          [ HP.classes [ ClassName "fakeMenu" ] ]
          [ HH.div
              [ HP.classes
                  [ ClassName "fakeButtons"
                  , ClassName "fakeClose"
                  ]
              ]
              []
          , HH.div
              [ HP.classes
                  [ ClassName "fakeButtons"
                  , ClassName "fakeMinimize"
                  ]
              ]
              []
          , HH.div
              [ HP.classes
                  [ ClassName "fakeButtons"
                  , ClassName "fakeZoom"
                  ]
              ]
              []
          ]
      , HH.div
          [ HP.classes [ ClassName "fakeScreen" ] ]
          (Array.concat $ Array.fromFoldable $ renderStep <$> steps)
      ]
  renderStep = Step.map
    { bashCommandExecution: \{ input, output } →
        [ HH.p
            [ HP.classes [ ClassName "input" ] ]
            [ HH.text input ]
        , HH.p
            [ HP.classes [ ClassName "output" ] ]
            [ HH.text output ]
        ]
    , commentCreation: \s →
        [ HH.p
            [ HP.classes [ ClassName "comment" ] ]
            [ HH.text s ]
        ]
    }
