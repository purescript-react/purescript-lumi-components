module Lumi.Styles.Slat where

import Lumi.Styles.Box (FlexAlign(..), align, justify, row)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion (Style, css, merge, str, unset)

slat :: LumiTheme -> Style
slat theme =
  merge
    [ row
    , align Center
    , justify SpaceBetween
    , css
      { flex: str "0 0 content"
      , color: unset
      , backgroundColor: unset
      , textDecoration: unset
      }
    ]
