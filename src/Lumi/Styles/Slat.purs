module Lumi.Styles.Slat where


import Lumi.Styles.Box (row)
import Lumi.Styles.Border (Border, border)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion (Style, merge)

slat :: LumiTheme -> Border -> Style
slat theme b =
  merge
    [ row
    , border theme b
    ]
