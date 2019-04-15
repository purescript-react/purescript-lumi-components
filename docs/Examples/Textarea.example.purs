module Lumi.Components.Examples.Textarea where

import Prelude

import Data.Foldable (traverse_)
import Lumi.Components.Column (column, column_)
import Lumi.Components.Text (p_)
import Lumi.Components.Textarea (defaults, textarea)
import Lumi.Components.Example (example)
import React.Basic (Component, JSX, createComponent, make)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)

component :: Component Unit
component = createComponent "TextareaExample"

docs :: JSX
docs = unit # make component { initialState , render }
  where
    initialState =
      { example1: ""
      }

    render self =
      column_
        [ p_ "Empty"
        , example $
            column
              { style: css { alignSelf: "stretch" }
              , children:
                  [ textarea defaults
                      { lines = 3
                      , placeholder = "Enter text here..."
                      , value = self.state.example1
                      , onChange = handler targetValue $ traverse_ \value -> self.setState _ { example1 = value }
                      }
                  ]
              }
        ]
