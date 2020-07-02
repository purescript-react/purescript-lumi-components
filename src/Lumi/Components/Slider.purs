module Lumi.Components.Slider where

import Prelude

import Color (cssStringHSLA)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toNullable)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import Lumi.Components.Input as Input
import React.Basic.Classic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events as Events

type SliderProps =
  { completed :: Int
  , onChange :: EffectFn1 Int Unit
  , style :: R.CSS
  }

component :: Component SliderProps
component = createComponent "Slider"

slider :: SliderProps -> JSX
slider = makeStateless component render
  where
    render props =
      lumiSliderElement
        { style: props.style
        , children:
            [ Input.input Input.range
                { value = show props.completed
                , min = toNullable $ Just 0.0
                , max = toNullable $ Just 100.0
                , onChange = Events.handler targetValue \targetValue -> do
                    runEffectFn1 props.onChange $ fromMaybe 0 $ fromString =<< targetValue
                }
            , lumiValueBarElement
                { style: R.css { width: show props.completed <> "%" }
                }
            ]
        }

    lumiSliderElement = element $ unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-slider"
    lumiValueBarElement = element $ unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-value-bar"

styles :: JSS
styles = jss
  { "@global":
      { "lumi-slider":
          { position: "relative"
          , width: "100%"

          , "& input.lumi[type=\"range\"]":
              { position: "absolute"
              , left: "0px"
              , padding: "0px"
              , margin: "0px"

              , width: "100%"
              , height: "4px"
              }
          , "& lumi-value-bar":
              { position: "absolute"
              , padding: "0px"
              , margin: "0px"

              , backgroundColor: cssStringHSLA colors.primary
              , height: "4px"
              , pointerEvents: "none"
              }
          }
      }
  }
