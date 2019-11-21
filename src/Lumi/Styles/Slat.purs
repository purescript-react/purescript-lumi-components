module Lumi.Styles.Slat where

import Prelude

import Lumi.Styles (StyleModifier, styleModifier_)
import Lumi.Styles.Box (FlexAlign(..), align, justify, row)
import React.Basic.Emotion (css, str, unset)

slat :: forall props. StyleModifier props
slat =
  row
    >>> align Center
    >>> justify SpaceBetween
    >>> styleModifier_
        ( css
            { flex: str "0 0 content"
            , color: unset
            , backgroundColor: unset
            , textDecoration: unset
            }
        )
