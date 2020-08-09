module Lumi.Components.Loader where

import Prelude

import Color (cssStringHSLA)
import Data.Nullable (Nullable)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (Color, colors)
import React.Basic.Classic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, unsafeCreateDOMComponent)

type LoaderProps
  = { style :: CSS
    , testId :: Nullable String
    }

component :: Component LoaderProps
component = createComponent "Loader"

loader :: LoaderProps -> JSX
loader = makeStateless component $ loaderElement <<< mapProps
  where
  loaderElement = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-loader")

  mapProps props =
    { style: props.style
    , "data-testid": props.testId
    }

styles :: JSS
styles =
  jss
    { "@global":
      { "lumi-loader":
          spinnerMixin
            { color: colors.black1
            , highlightColor: colors.black4
            , radius: "38px"
            , borderWidth: "5px"
            }
      , "@keyframes spin":
        { from: { transform: "rotate(0deg)" }
        , to: { transform: "rotate(360deg)" }
        }
      }
    }

spinnerMixin ::
  { color :: Color
  , highlightColor :: Color
  , radius :: String
  , borderWidth :: String
  } ->
  JSS
spinnerMixin { color: c, highlightColor, radius, borderWidth } =
  jss
    { boxSizing: "border-box"
    , content: "\"\""
    , display: "inline-block"
    , height: radius
    , width: radius
    , border: [ borderWidth, "solid", cssStringHSLA c ]
    , borderTopColor: cssStringHSLA highlightColor
    , borderRadius: "50%"
    , animation: "spin 1s infinite linear"
    , animationName: "spin"
    }
