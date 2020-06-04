module Lumi.Components.Color
  ( Color
  , ColorName
  , ColorMap
  , colors
  , colorNames
  ) where

import Color (rgb, rgba)
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
  , accent2 :: a
  , accent23 :: a
  , accent3 :: a
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
  , primary: rgb 0x00 0x44 0xe4
  , primary1: rgb 0xa6 0xbe 0xf6
  , primary2: rgb 0xbf 0xd0 0xf8
  , primary3: rgb 0xd9 0xe3 0xfb
  , primary4: rgb 0xed 0xf2 0xfd
  , secondary: rgb 0x91 0x90 0x8d
  , accent1: rgb 0x49 0xb8 0x60
  , accent2: rgb 0xff 0xa5 0x02
  , accent23: rgb 0xff 0xee 0xd9
  , accent3: rgb 0xf1 0x50 0x0d
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
  , accent2: ColorName "accent-2"
  , accent23: ColorName "accent-2-3"
  , accent3: ColorName "accent-3"
  , accent33: ColorName "accent-3-3"
  , white: ColorName "white"
  , transparent: ColorName "transparent"
  }

