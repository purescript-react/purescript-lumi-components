module Lumi.Components.Examples.FixedPrecisionInput where

import Prelude

import Data.Either (Either(..))
import Data.Fixed (Fixed, P100)
import Effect.Uncurried (mkEffectFn2)
import Lumi.Components.Spacing (vspace, Space(S12))
import Lumi.Components.Column (column_)
import Lumi.Components.Text (h3_, subtext_)
import Lumi.Components.FixedPrecisionInput (defaults, fixedPrecisionInput)
import Lumi.Components.Example (example)
import React.Basic (Component, JSX, createComponent, make)

component :: Component Unit
component = createComponent "FixedPrecisionInputExample"

docs :: JSX
docs = unit # make component
  { initialState
  , render
  }
  where
    initialState =
      { example1: Left "" :: Either String (Fixed P100)
      , example2: Left "" :: Either String (Fixed P100)
      }

    render self =
      column_
        [ h3_ "Fixed P100"
        , example $
            fixedPrecisionInput defaults
              { onChange = mkEffectFn2 \value _ ->
                  self.setState _ { example1 = value }
              , value = self.state.example1
              }
        , subtext_ (show self.state.example1)

        , vspace S12
        , h3_ "Fixed P100 (forced invalid)"
        , example $
            fixedPrecisionInput defaults
              { onChange = mkEffectFn2 \value _ ->
                  self.setState _ { example2 = value }
              , value = self.state.example2
              }
        , subtext_ (show self.state.example2)
        ]
