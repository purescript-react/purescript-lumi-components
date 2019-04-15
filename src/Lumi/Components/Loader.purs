module Lumi.Components.Loader where

import Prelude

import Color (cssStringHSLA)
import Data.Nullable (Nullable)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import React.Basic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, unsafeCreateDOMComponent)

type LoaderProps =
  { style :: CSS
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
      }

styles :: JSS
styles = jss
  { "@global":
      { "lumi-loader": spinnerMixin { radius: "3.8rem", borderWidth: "0.5rem" }
      , "@keyframes spin":
          { from: { transform: "rotate(0deg)" }
          , to: { transform: "rotate(360deg)" }
          }
      }
  }

spinnerMixin :: { radius :: String, borderWidth :: String } -> JSS
spinnerMixin { radius, borderWidth } = jss
  { boxSizing: "border-box"
  , content: ""
  , display: "inline-block"
  , height: radius
  , width: radius
  , border: [ borderWidth, "solid", cssStringHSLA colors.black1 ]
  , borderTopColor: cssStringHSLA colors.black4
  , borderRadius: "50%"
  , animation: "spin 1s infinite linear"
  , animationName: "spin"
  }
