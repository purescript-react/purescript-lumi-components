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
      -- @TODO how to pass backgroundColor as prop down from LoaderProps?
      { "lumi-loader": spinnerMixin { radius: "3.8rem", borderWidth: "0.5rem" }
      , "@keyframes spin":
          { from: { transform: "rotate(0deg)" }
          , to: { transform: "rotate(360deg)" }
          }
      }
  }

spinnerMixin :: { radius :: String, borderWidth :: String } -> JSS
spinnerMixin { radius, borderWidth } = jss
  { width: radius
  , height: radius
  , borderRadius: "50%"
  , background: "linear-gradient(to right, " <> cssStringHSLA colors.white <> " 10%, rgba(255, 255, 255, 0) 42%)"
  , position: "relative"
  , animation: "spin 1s infinite linear"
  , animationName: "spin"
  , "&::before":
      { width: "50%"
      , height: "50%"
      , background: cssStringHSLA colors.white
      , borderRadius: "100% 0 0 0"
      , position: "absolute"
      , top: "0"
      , left: "0"
      , content: "\"\""
      }
  , "&::after":
      -- @TODO this should be passed down from the loader
      -- cannot use alpha/transparency because we need background in order to have the linear-gradient affect on the spinner
      -- this solid background will cut the "hole" out of the center
      { background: cssStringHSLA colors.accent1
      , width: "75%"
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
