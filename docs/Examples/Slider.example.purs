module Lumi.Components.Examples.Slider where

import Prelude

import Effect.Uncurried (mkEffectFn1)
import Lumi.Components.Column (column_)
import Lumi.Components.Text (p_)
import Lumi.Components.Slider (slider)
import Lumi.Components.Example (example)
import React.Basic (Component, JSX, createComponent, make)
import React.Basic.DOM as R

component :: Component Unit
component = createComponent "SliderExample"

docs :: JSX
docs = unit # make component
  { initialState
  , render
  }
  where
    initialState =
      { percent: 50
      }

    render self =
      column_
        [ p_ $ show self.state.percent <> "%"
        , example $
            slider
              { completed: self.state.percent
              , onChange: mkEffectFn1 \percent -> self.setState _ { percent = percent }
              , style: R.css { }
              }
        ]
