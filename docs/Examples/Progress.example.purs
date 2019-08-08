module Lumi.Components.Examples.Progress where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toNullable)
import Lumi.Components.Button as Button
import Lumi.Components.Column (column, column_)
import Lumi.Components.Input as Input
import Lumi.Components.LabeledField (labeledField, RequiredField(..))
import Lumi.Components.Progress (progressBar, progressCircle, progressDefaults)
import Lumi.Components.Row (row_)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Text (h2_)
import Lumi.Components.Example (example)
import React.Basic (Component, JSX, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)

component :: Component Unit
component = createComponent "ProgressExample"

data Action
  = Set Int
  | Increment
  | Reset

type State =
  { percent :: Int
  }

docs :: JSX
docs = unit # make component
  { initialState: { percent: 20 }
  , render: \self ->
      column_
        [ column
            { style: R.css { maxWidth: "500px", padding: "20px 0" }
            , children:
                [ labeledField
                    { label: R.text "Adjust progress:"
                    , value:
                        row_
                          [ Input.input Input.range
                              { value = show self.state.percent
                              , min = toNullable $ Just 0.0
                              , max = toNullable $ Just 100.0
                              , onChange = handler targetValue (send self <<< Set <<< fromMaybe 0 <<< flip bind fromString)
                              , style = R.css { maxWidth: "200px" }
                              }
                          , Button.button Button.primary
                              { title = "+10%"
                              , onPress = handler_ $ send self Increment
                              , size = Small
                              , style = R.css { marginLeft: "16px" }
                              }
                          , Button.button Button.primary
                              { title = "Reset"
                              , onPress = handler_ $ send self Reset
                              , size = Small
                              , style = R.css { marginLeft: "8px" }
                              }
                          ]
                    , validationError: Nothing
                    , required: Neither
                    , forceTopLabel: false
                    , style: R.css {}
                    }
                ]
            }

        , h2_ "Bar"
        , example
            $ progressBar progressDefaults { completed = self.state.percent }

        , h2_ "Circle"
        , example
            $ progressCircle progressDefaults { completed = self.state.percent }
        ]
  }
  where
    send self = case _ of
      Set percent ->
        self.setState _ { percent = percent }
      Increment ->
        self.setState \state -> state { percent = min 100 $ state.percent + 10 }
      Reset ->
        self.setState _ { percent = 0 }

