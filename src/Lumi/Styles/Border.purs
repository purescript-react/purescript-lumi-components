module Lumi.Styles.Border where

import Prelude

import Color (cssStringHSLA)
import Lumi.Components.Color (colors)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion (class IsStyleProperty, Style, color, css, int, merge, nested, none, prop, str)


data Border
  = BorderRound
  | BorderTopBottom
  | BorderSquare

border :: LumiTheme -> Border -> Style
border theme b =
  merge
    [ css
        { borderWidth: int 1
        , borderColor: color theme.colors.black4
        , borderStyle: str "solid"
        , padding: str "8px 16px"
        }
    , case b of
        BorderRound ->
          css
            { borderRadius: int 4
            }
        BorderTopBottom ->
          css
            { borderLeft: none
            , borderRight: none
            , borderRadius: int 0
            , paddingLeft: str "0"
            , paddingRight: str "0"
            }
        BorderSquare ->
          css
            { borderRadius: int 0
            }
    ]
