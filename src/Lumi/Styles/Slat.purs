module Lumi.Styles.Slat where


import Data.Monoid (guard)
import Lumi.Styles.Border (Border, border)
import Lumi.Styles.Box (FlexAlign(..), align, focusable, interactive, justify, row)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion (Style, merge)

slat :: LumiTheme -> Border -> Boolean -> Style
slat theme b isInteractive =
  merge
    [ row
    , align Center
    , justify SpaceBetween
    , border theme b isInteractive
    , guard isInteractive interactive
    , guard isInteractive focusable theme
    ]
