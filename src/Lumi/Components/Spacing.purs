module Lumi.Components.Spacing
  ( Space(..)
  , hspace
  , hspaceWithStyle
  , vspace
  , toPixels
  ) where

import Prelude

import React.Basic.Classic (JSX)
import React.Basic.DOM as R
import React.Basic.Emotion (class IsStyleProperty, str)

data Space
  = S4 | S8 | S12 | S16 | S24 | S32 | S40 | S48 | S56
  | S64 | S72 | S80 | S88 | S96 | S104 | S112

hspaceWithStyle :: R.CSS -> Space -> JSX
hspaceWithStyle sty size =
  R.div
    { style: sty <> R.css
        { paddingLeft: toPixels size
        }
    }

hspace :: Space -> JSX
hspace = hspaceWithStyle mempty

vspace :: Space -> JSX
vspace size =
  R.div
    { style: R.css
        { paddingTop: toPixels size
        }
    }

toPixels :: Space -> String
toPixels S4 = "4px"
toPixels S8 = "8px"
toPixels S12 = "12px"
toPixels S16 = "16px"
toPixels S24 = "24px"
toPixels S32 = "32px"
toPixels S40 = "40px"
toPixels S48 = "48px"
toPixels S56 = "56px"
toPixels S64 = "64px"
toPixels S72 = "72px"
toPixels S80 = "80px"
toPixels S88 = "88px"
toPixels S96 = "96px"
toPixels S104 = "104px"
toPixels S112 = "112px"

instance isStylePropertySpace :: IsStyleProperty Space where
  prop = str <<< toPixels
