module Lumi.Styles.Slat
  ( slat
  , module Border
  ) where

import Prelude

import Lumi.Styles (StyleModifier, styleModifier_)
import Lumi.Styles.Box (FlexAlign(..), _align, _justify, _row)
import Lumi.Styles.Border (border)
import Lumi.Styles.Border hiding (border) as Border
import React.Basic.Emotion (css, str, unset)

slat :: StyleModifier
slat =
  border
    >>> _row
    >>> _align Center
    >>> _justify SpaceBetween
    >>> styleModifier_
        ( css
            { label: str "slat"
            , flex: str "0 0 content"
            , color: unset
            , backgroundColor: unset
            , textDecoration: unset
            }
        )
