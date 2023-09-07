module Lumi.Styles.Slat
  ( slat
  , slatColumn
  , module Border
  ) where

import Prelude

import Lumi.Components.Spacing (Space(S16))
import Lumi.Styles (StyleModifier, style_)
import Lumi.Styles.Box (FlexAlign(..), _align, _justify, _row)
import Lumi.Styles.Border (border)
import Lumi.Styles.Border hiding (border) as Border
import React.Basic.Emotion (css, nested, prop, str, unset)

slat :: StyleModifier
slat =
  border
    <<< _row
    <<< _align Center
    <<< _justify SpaceBetween
    <<< style_
        ( css
            { label: str "slat"
            , flex: str "0 0 content"
            , color: unset
            , backgroundColor: unset
            , textDecoration: unset
            }
        )

slatColumn :: Int -> StyleModifier
slatColumn flexGrow =
  style_
    $ css
    $ { flexGrow: str (show flexGrow)
      , "&:not(:first-child)":
        nested
          $ css
              { marginLeft: prop S16
              , alignItems: prop End
              }

      }
