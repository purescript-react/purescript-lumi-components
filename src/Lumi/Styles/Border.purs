module Lumi.Styles.Border where


import Prelude

import Data.Monoid (guard)
import Lumi.Components.Spacing (Space(..))
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion (Style, color, css, int, merge, nested, none, prop, str)


data Border
  = BorderRound
  | BorderTopBottom
  | BorderSquare

border :: LumiTheme -> Border -> Boolean -> Style
border theme b isInteractive =
  let
    borderWidth = 1
  in
    merge
      [ css
          { borderWidth: int borderWidth
          , borderColor: color theme.colors.black4
          , borderStyle: str "solid"
          , padding: str "8px 16px"
          }
      , case b of
          BorderRound ->
            css
              { borderRadius: int 4
              , "&:not(:first-child)": nested $ css
                { marginTop: prop S8
                }
              }
          BorderTopBottom ->
            css
              { borderLeft: none
              , borderRight: none
              , borderRadius: int 0
              , paddingLeft: int 0
              , paddingRight: int 0
              , "&:not(:first-child)": nested $ css
                { marginTop: int (-borderWidth)
                , ":not(:hover)": nested $ css
                  { borderTopColor: color theme.colors.transparent
                  }
                }
              }
          BorderSquare ->
            css
              { borderRadius: int 0
              , "&:not(:first-child)": nested $ css
                { marginTop: prop S8
                }
              }
      , guard isInteractive $ css
        { "&:hover": nested $ css
          { borderColor: color theme.colors.black2
          }
        }
      ]
