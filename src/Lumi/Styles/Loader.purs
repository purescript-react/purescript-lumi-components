module Lumi.Styles.Loader where

import Prelude

import Lumi.Components.Color (Color)
import Lumi.Styles (Style, StyleModifier, StyleProperty, color, css, merge, px, str, style)
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Emotion (nested)

loader :: StyleModifier
loader =
  style \(LumiTheme { colors }) ->
    ( merge
        [ mkLoader
            { color: colors.black1
            , highlightColor: colors.black4
            , radius: px 38
            , borderWidth: px 5
            }
        , spin
        ]
    )

spin :: Style
spin =
  css
    { "@keyframes spin":
      nested
        $ css
            { from: nested $ css { transform: str "rotate(0deg)" }
            , to: nested $ css { transform: str "rotate(360deg)" }
            }
    }

mkLoader ::
  { color :: Color
  , highlightColor :: Color
  , radius :: StyleProperty
  , borderWidth :: StyleProperty
  } ->
  Style
mkLoader { color: c, highlightColor, radius, borderWidth } =
  css
    { boxSizing: str "border-box"
    , content: str "\"\""
    , display: str "inline-block"
    , height: radius
    , width: radius
    , borderWidth: borderWidth
    , borderStyle: str "solid"
    , borderColor: color c
    , borderTopColor: color highlightColor
    , borderRadius: str "50%"
    , animation: str "spin 1s infinite linear"
    , animationName: str "spin"
    }
