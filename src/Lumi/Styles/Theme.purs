module Lumi.Styles.Theme where

import Data.Newtype (class Newtype)
import Lumi.Components.Color (Color, ColorMap, ColorName, colorNames, colors)

newtype LumiTheme
  = LumiTheme
      { colors :: ColorMap Color
      , colorNames :: ColorMap ColorName
      }

derive instance newtypeLumiTheme :: Newtype LumiTheme _

defaultTheme :: LumiTheme
defaultTheme = LumiTheme { colors, colorNames }
