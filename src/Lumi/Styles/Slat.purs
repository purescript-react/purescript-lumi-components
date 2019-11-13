module Lumi.Styles.Slat where


import Data.Monoid (guard)
import Lumi.Styles.Border (Border, border)
import Lumi.Styles.Box (FlexAlign(..), align, focusable, interactive, justify, row)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion (Style, css, merge, str, unset)

slat ::
  LumiTheme ->
  { border :: Border
  , isInteractive :: Boolean
  , isList :: Boolean
  } ->
  Style
slat theme { border: b, isInteractive, isList } =
  merge
    [ row
    , align Center
    , justify SpaceBetween
    , css
      { flex: str "0 0 content"
      , color: unset
      , backgroundColor: unset
      , textDecoration: unset
      }
    , border theme { border: b, isInteractive, isList }
    , guard isInteractive interactive
    , guard isInteractive focusable theme
    ]
