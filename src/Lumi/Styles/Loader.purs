module Lumi.Styles.Loader where


import Lumi.Styles (Style, StyleModifier, StyleProperty, borderBox, color, css, inlineBlock, keyframes, percent, px, solid, str, style)
import Lumi.Styles.Theme (LumiTheme(..))

loader :: StyleModifier
loader =
  style \(LumiTheme { colors }) ->
    ( mkLoader
        { color: color colors.black1
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
  { color :: StyleProperty
  , radius :: StyleProperty
  , borderWidth :: StyleProperty
  } ->
  Style
mkLoader { color: c, radius, borderWidth } =
  css
    { boxSizing: borderBox
    , content: str "\"\""
    , display: inlineBlock
    , height: radius
    , width: radius
    , borderWidth: borderWidth
    , borderStyle: solid
    , borderColor: c
    , borderTopColor: str "transparent"
    , borderRadius: percent 50.0
    , animation: str "1s infinite linear"
    , animationName: spin
    }
