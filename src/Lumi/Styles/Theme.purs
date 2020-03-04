module Lumi.Styles.Theme where

import Data.Newtype (class Newtype)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components.Color (Color, ColorMap, ColorName, colorNames, colors)
import React.Basic (ReactContext, createContext)
import React.Basic.Hooks (Hook, UseContext, useContext)

newtype LumiTheme
  = LumiTheme
  { colors :: ColorMap Color
  , colorNames :: ColorMap ColorName
  }

derive instance newtypeLumiTheme :: Newtype LumiTheme _

defaultTheme :: LumiTheme
defaultTheme = LumiTheme { colors, colorNames }

lumiThemeContext :: ReactContext LumiTheme
lumiThemeContext =
  unsafePerformEffect do
    createContext defaultTheme

useTheme :: Hook (UseContext LumiTheme) LumiTheme
useTheme = useContext lumiThemeContext
