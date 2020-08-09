module Lumi.Styles.Loader where


import Lumi.Components.Color (Color)
import Lumi.Styles (Style, StyleModifier, StyleProperty, borderBox, color, css, inlineBlock, keyframes, percent, px, solid, str, style)
import Lumi.Styles.Theme (LumiTheme(..))

loader :: StyleModifier
loader =
  style \(LumiTheme { colors }) ->
    ( mkLoader
        { color: colors.black1
        , highlightColor: colors.black4
        , radius: px 38
        , borderWidth: px 5
        }
    )

spin :: StyleProperty
spin =
  keyframes
    { from: css { transform: str "rotate(0deg)" }
    , to: css { transform: str "rotate(360deg)" }
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
    { boxSizing: borderBox
    , content: str "\"\""
    , display: inlineBlock
    , height: radius
    , width: radius
    , borderWidth: borderWidth
    , borderStyle: solid
    , borderColor: color c
    , borderTopColor: color highlightColor
    , borderRadius: percent 50.0
    , animation: str "1s infinite linear"
    , animationName: spin
    }
