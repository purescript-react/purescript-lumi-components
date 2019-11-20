module Lumi.Styles.Slat where

import Prelude

import Lumi.Components (PropsModifier)
import Lumi.Styles (styleModifier_)
import Lumi.Styles.Box (FlexAlign(..), align, justify, row)
import React.Basic.Emotion (css, str, unset)

slat :: forall props. PropsModifier props
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
