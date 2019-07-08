module Lumi.Components.Loader where

import Prelude

import Color (cssStringHSLA, desaturate, lighten)
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
          { "&[data-color=\"primary\"]": loaderColorMixin colors.primary
          , "&[data-color=\"primary-1\"]": loaderColorMixin colors.primary1
          , "&[data-color=\"primary-2\"]": loaderColorMixin colors.primary2
          , "&[data-color=\"primary-3\"]": loaderColorMixin colors.primary3
          , "&[data-color=\"primary-4\"]": loaderColorMixin colors.primary4
          , "&[data-color=\"secondary\"]": loaderColorMixin colors.secondary
          , "&[data-color=\"white\"]": loaderColorMixin colors.white
          , "&[data-color=\"black\"]": loaderColorMixin colors.black
          , "&[data-color=\"black-1\"]": loaderColorMixin colors.black1
          , "&[data-color=\"black-2\"]": loaderColorMixin colors.black2
          , "&[data-color=\"black-3\"]": loaderColorMixin colors.black3
          , "&[data-color=\"black-4\"]": loaderColorMixin colors.black4
          , "&[data-color=\"black-5\"]": loaderColorMixin colors.black5
          , "&[data-color=\"black-6\"]": loaderColorMixin colors.black6
          , "&[data-color=\"black-7\"]": loaderColorMixin colors.black7
          , "&[data-color=\"black-8\"]": loaderColorMixin colors.black8
          , "&[data-color=\"accent-1\"]": loaderColorMixin colors.accent1
          , "&[data-color=\"accent-2\"]": loaderColorMixin colors.accent2
          , "&[data-color=\"accent-3\"]": loaderColorMixin colors.accent3
          , "&[data-color=\"accent-33\"]": loaderColorMixin colors.accent33
          -- @TODO add the rest of our possible colors
          , "&[data-bg-color=\"primary\"]::after": buttonColorHoverMixin colors.primary
          , "&[data-bg-color=\"primary-1\"]::after": buttonColorHoverMixin colors.primary1
          , "&[data-bg-color=\"primary-2\"]::after": buttonColorHoverMixin colors.primary2
          , "&[data-bg-color=\"primary-3\"]::after": buttonColorHoverMixin colors.primary3
          , "&[data-bg-color=\"primary-4\"]::after": buttonColorHoverMixin colors.primary4
          , "&[data-bg-color=\"secondary\"]::after": buttonColorHoverMixin colors.secondary
          , "&[data-bg-color=\"white\"]::after": loaderBgColorMixin colors.white
          , "&[data-bg-color=\"black\"]::after": loaderBgColorMixin colors.black
          , "&[data-bg-color=\"black-1\"]::after": loaderBgColorMixin colors.black1
          , "&[data-bg-color=\"black-2\"]::after": loaderBgColorMixin colors.black2
          , "&[data-bg-color=\"black-3\"]::after": loaderBgColorMixin colors.black3
          , "&[data-bg-color=\"black-4\"]::after": loaderBgColorMixin colors.black4
          , "&[data-bg-color=\"black-5\"]::after": loaderBgColorMixin colors.black5
          , "&[data-bg-color=\"black-6\"]::after": loaderBgColorMixin colors.black6
          , "&[data-bg-color=\"black-7\"]::after": loaderBgColorMixin colors.black7
          , "&[data-bg-color=\"black-8\"]::after": loaderBgColorMixin colors.black8
          , "&[data-bg-color=\"accent-1\"]::after": loaderBgColorMixin colors.accent1
          , "&[data-bg-color=\"accent-2\"]::after": loaderBgColorMixin colors.accent2
          , "&[data-bg-color=\"accent-3\"]::after": loaderBgColorMixin colors.accent3
          , "&[data-bg-color=\"accent-33\"]::after": loaderBgColorMixin colors.accent33
          }
      , "@keyframes spin":
          { from: { transform: "rotate(0deg)" }
          , to: { transform: "rotate(360deg)" }
          }
      }
  }
  where
    -- @TODO should not be a duplicate, can I import from Lumi.Components.Button?
    buttonColorHoverMixin value =
      { backgroundColor: cssStringHSLA $ lighten 0.4137 $ desaturate 0.1972 $ value
      }

    loaderColorMixin value = spinnerMixin
      { radius: "3.8rem"
      , borderWidth: "0.5rem"
      , color: cssStringHSLA value
      }

    loaderBgColorMixin value =
      { background: cssStringHSLA value
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
