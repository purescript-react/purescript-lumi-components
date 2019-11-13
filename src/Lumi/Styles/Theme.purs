module Lumi.Styles.Theme where

import Lumi.Components.Color (Color, ColorMap, ColorName)

type LumiTheme
  = { colors :: ColorMap Color
    , colorNames :: ColorMap ColorName
    }
