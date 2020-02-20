module Lumi.Styles.Slat
  ( slat
  , _interactiveBg
  , module Border
  ) where

import Prelude

import Lumi.Components (PropsModifier)
import Lumi.Styles (styleModifier, styleModifier_)
import Lumi.Styles.Box (FlexAlign(..), _align, _interactive, _justify, _row)
import Lumi.Styles.Box as Box
import Lumi.Styles.Border (border)
import Lumi.Styles.Border hiding (border) as Border
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Emotion (color, css, str, nested, unset)

slat :: forall props. PropsModifier props
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

_interactiveBg :: forall props. PropsModifier props
_interactiveBg =
  Box._interactive
    >>> Box._focusable
    >>> styleModifier \(LumiTheme theme) ->
        css
          { "&:hover":
            nested
              $ css
                  { backgroundColor: color theme.colors.primary4
                  }
          }
