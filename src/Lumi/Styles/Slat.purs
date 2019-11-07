module Lumi.Styles.Slat where

import Prelude

import Color (cssStringHSLA)
import Lumi.Styles.Box (row)
import Lumi.Styles.Border (Border(..), border)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion (class IsStyleProperty, Style, css, merge, nested, prop, str)

slat :: LumiTheme -> Border -> Style
slat theme b =
  merge
    [ row
    , border theme b
    ]
