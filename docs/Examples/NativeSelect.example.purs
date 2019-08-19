module Lumi.Components.Examples.NativeSelect where

import Prelude

import Data.Maybe (fromMaybe)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.NativeSelect (defaults, nativeSelect)
import React.Basic (Component, JSX, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)

component :: Component Unit
component = createComponent "SelectExample"

docs :: JSX
docs = unit # make component
  { initialState
  , render
  }
  where
    opts =
      [ { label: "Select your car...", value: "" }
      , { label: "Volvo", value: "volvo" }
      , { label: "Saab", value: "saab" }
      , { label: "Mercedes", value: "mercedes" }
      , { label: "Audi", value: "audi" }
      ]

    initialState = { selectedOption: "" }

    render self =
      column_
        [ example
            $ nativeSelect defaults
                { options = opts
                , onChange = handler targetValue \value ->
                              self.setState _ { selectedOption = fromMaybe "" value }
                , value = self.state.selectedOption
                }

        , example
            $ nativeSelect defaults
                { options = opts
                , onChange = handler targetValue \value ->
                              self.setState _ { selectedOption = fromMaybe "" value }
                , value = self.state.selectedOption
                , style = R.css { textAlignLast: "right" }
                }
        ]
