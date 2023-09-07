module Lumi.Components.Color
  ( Color
  , ColorName
  , ColorMap
  , colors
  , colorNames
  , shade
  ) where

import Prelude

import Color (darken, desaturate, lighten, rgb, rgba)
import Color as C
import Data.Newtype (class Newtype)

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
  , accent11 :: a
  , accent12 :: a
  , accent13 :: a
  , accent2 :: a
  , accent21 :: a
  , accent22 :: a
  , accent23 :: a
  , accent3 :: a
  , accent31 :: a
  , accent32 :: a
  , accent33 :: a
  , white :: a
  , transparent :: a
  }

colors :: ColorMap Color
colors =
  { black: rgb 0x23 0x22 0x20
  , black1: rgb 0x91 0x90 0x8d
  , black2: rgb 0xbe 0xbc 0xb9
  , black3: rgb 0xdd 0xdd 0xdc
  , black4: rgb 0xe7 0xe6 0xe5
  , black5: rgb 0xf5 0xf4 0xf2
  , black6: rgb 0xf7 0xf6 0xf4
  , black7: rgb 0xf9 0xf8 0xf7
  , black8: rgb 0xfb 0xfa 0xf9
  , primary: rgb 0 150 212
  , primary1: rgb 151 221 243
  , primary2: rgb 181 230 246
  , primary3: rgb 211 241 250
  , primary4: rgb 235 247 253
  , secondary: rgb 0x91 0x90 0x8d
  , accent1: rgb 0x49 0xb8 0x60
  , accent11: rgb 0x78 0xdd 0x8d
  , accent12: rgb 0xbc 0xee 0xc6
  , accent13: rgb 0xe4 0xf5 0xe7
  , accent2: rgb 0xff 0xa5 0x02
  , accent21: rgb 0xff 0xb9 0x67
  , accent22: rgb 0xff 0xdc 0xb3
  , accent23: rgb 0xff 0xee 0xd9
  , accent3: rgb 0xf1 0x50 0x0d
  , accent31: rgb 0xf7 0x96 0x6d
  , accent32: rgb 0xfb 0xca 0xb6
  , accent33: rgb 0xfd 0xe5 0xdb
  , white: rgb 0xff 0xff 0xff
  , transparent: rgba 0 0 0 0.0
  }

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
  , accent11: ColorName "accent-1-1"
  , accent12: ColorName "accent-1-2"
  , accent13: ColorName "accent-1-3"
  , accent2: ColorName "accent-2"
  , accent21: ColorName "accent-2-1"
  , accent22: ColorName "accent-2-2"
  , accent23: ColorName "accent-2-3"
  , accent3: ColorName "accent-3"
  , accent31: ColorName "accent-3-1"
  , accent32: ColorName "accent-3-2"
  , accent33: ColorName "accent-3-3"
  , white: ColorName "white"
  , transparent: ColorName "transparent"
  }

shade ::
  { hue :: Color, white :: Color, black :: Color } ->
  { black :: Color
  , grey1 :: Color
  , grey2 :: Color
  , hue :: Color
  , hueDarker :: Color
  , hueDarkest :: Color
  , hueDisabled :: Color
  , white :: Color
  }
shade { hue, white, black } =
  let
    hueDarker = darken 0.1 hue
    hueDarkest = darken 0.15 hue
    hueDisabled = lighten 0.4137 $ desaturate 0.1972 hue
    grey1 = lighten 0.7 black
    grey2 = lighten 0.78 black
  in
    { hue, hueDarker, hueDarkest, hueDisabled, grey1, grey2, white, black }
