module Lumi.Components.Loader where

import Prelude

import Color (cssStringHSLA)
import Data.Nullable (Nullable)
import JSS (JSS, jss)
import Lumi.Components.Color (ColorName, colors)
import React.Basic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, unsafeCreateDOMComponent)

type LoaderProps =
  { style :: CSS
  , color :: ColorName
  , bgColor :: ColorName
  , testId :: Nullable String
  }

component :: Component LoaderProps
component = createComponent "Loader"

loader :: LoaderProps -> JSX
loader = makeStateless component $ loaderElement <<< mapProps
  where
    loaderElement = element (unsafeCreateDOMComponent "lumi-loader")
    mapProps props =
      { style: props.style
      , "data-testid": props.testId
      , "data-color": props.color
      , "data-bg-color": props.bgColor
      }

styles :: JSS
styles = jss
  { "@global":
      { "lumi-loader":
          -- @TODO add the rest of our possible colors
          { "&[data-color=\"white\"]": spinnerMixin
              { radius: "3.8rem"
              , borderWidth: "0.5rem"
              , color: cssStringHSLA colors.white
              }
          , "&[data-color=\"black\"]": spinnerMixin
              { radius: "3.8rem"
              , borderWidth: "0.5rem"
              , color: cssStringHSLA colors.black
              }
          -- @TODO add the rest of our possible colors
          , "&[data-bg-color=\"primary\"]::after":
              { background: cssStringHSLA colors.primary
              }
          , "&[data-bg-color=\"white\"]::after":
              { background: cssStringHSLA colors.white
              }
          , "&[data-bg-color=\"black\"]::after":
              { background: cssStringHSLA colors.black
              }
          }
      , "@keyframes spin":
          { from: { transform: "rotate(0deg)" }
          , to: { transform: "rotate(360deg)" }
          }
      }
  }

spinnerMixin :: { radius :: String, borderWidth :: String, color :: String } -> JSS
spinnerMixin { radius, borderWidth, color } = jss
  { width: radius
  , height: radius
  , borderRadius: "50%"
  , background: "linear-gradient(to right, " <> color <> " 10%, rgba(255, 255, 255, 0) 42%)"
  , position: "relative"
  , animation: "spin 1s infinite linear"
  , animationName: "spin"
  , "&::before":
      { width: "50%"
      , height: "50%"
      , background: color
      , borderRadius: "100% 0 0 0"
      , position: "absolute"
      , top: "0"
      , left: "0"
      , content: "\"\""
      }
  , "&::after":
      { width: "75%"
      , height: "75%"
      , borderRadius: "50%"
      , content: "\"\""
      , margin: "auto"
      , position: "absolute"
      , top: "0"
      , left: "0"
      , bottom: "0"
      , right: "0"
      }
  }
