module Lumi.Components.Color
  ( Color
  , ColorName
  , ColorMap
  , colors
  , colorNames
  ) where

import Prelude

import Color (rgba)
import Color as C
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Record.Extra (mapRecord)

type Color = C.Color

newtype ColorName = ColorName String

derive instance newtypeColorName :: Newtype ColorName _

type ColorMap a =
  { black :: a
  , black1 :: a
  , black2 :: a
  , black3 :: a
  , black4 :: a
  , black5 :: a
  , black6 :: a
  , black7 :: a
  , black8 :: a
  , primary :: a
  , primary1 :: a
  , primary2 :: a
  , primary3 :: a
  , primary4 :: a
  , secondary :: a
  , accent1 :: a
  , accent2 :: a
  , accent3 :: a
  , accent33 :: a
  , white :: a
  , transparent :: a
  }

colors :: ColorMap Color
colors = mapRecord (fromMaybe transparent <<< C.fromHexString)
  { black: "#232220"
  , black1: "#91908d"
  , black2: "#bebcb9"
  , black3: "#dddddc"
  , black4: "#e7e6e5"
  , black5: "#f5f4f2"
  , black6: "#f7f6f4"
  , black7: "#f9f8f7"
  , black8: "#fbfaf9"
  , primary: "#0044e4"
  , primary1: "#a6bef6"
  , primary2: "#bfd0f8"
  , primary3: "#d9e3fb"
  , primary4: "#edf2fd"
  , secondary: "#91908d"
  , accent1: "#49b860"
  , accent2: "#ffa502"
  , accent3: "#f1500d"
  , accent33: "#fde5db"
  , white: "#ffffff"
  , transparent: "transparent"
  }
  where
    transparent = rgba 0 0 0 0.0

colorNames :: ColorMap ColorName
colorNames =
  { black: ColorName "black"
  , black1: ColorName "black-1"
  , black2: ColorName "black-2"
  , black3: ColorName "black-3"
  , black4: ColorName "black-4"
  , black5: ColorName "black-5"
  , black6: ColorName "black-6"
  , black7: ColorName "black-7"
  , black8: ColorName "black-8"
  , primary: ColorName "primary"
  , primary1: ColorName "primary-1"
  , primary2: ColorName "primary-2"
  , primary3: ColorName "primary-3"
  , primary4: ColorName "primary-4"
  , secondary: ColorName "secondary"
  , accent1: ColorName "accent-1"
  , accent2: ColorName "accent-2"
  , accent3: ColorName "accent-3"
  , accent33: ColorName "accent-3-3"
  , white: ColorName "white"
  , transparent: ColorName "transparent"
  }

