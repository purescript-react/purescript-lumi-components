module Lumi.Styles.Loader where

import Prelude
import Lumi.Styles (Style, StyleModifier, color, css, merge, str, styleModifier)
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Emotion (nested)

loader :: StyleModifier
loader =
  styleModifier \theme ->
    ( merge
        [ mkLoader theme { radius: "38px", borderWidth: "5px" }
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

mkLoader :: LumiTheme -> { radius :: String, borderWidth :: String } -> Style
mkLoader (LumiTheme { colors }) { radius, borderWidth } =
  css
    { boxSizing: str "border-box"
    , content: str "\"\""
    , display: str "inline-block"
    , height: str radius
    , width: str radius
    , borderWidth: str borderWidth
    , borderStyle: str "solid"
    , borderColor: color colors.black1
    , borderTopColor: color colors.black4
    , borderRadius: str "50%"
    , animation: str "spin 1s infinite linear"
    , animationName: str "spin"
    }
