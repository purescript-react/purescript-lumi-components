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

border ::
  LumiTheme ->
  { border :: Border
  , isInteractive :: Boolean
  , isList :: Boolean
  } ->
  Style
border theme { border: b, isInteractive, isList } =
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
            merge
              [ css
                { borderRadius: int 4
                }
              , guard isList $ css
                { "&:not(:first-child)": nested $ css
                  { marginTop: prop S8
                  }
                }
              ]
          BorderTopBottom ->
            merge
              [ css
                { borderLeft: none
                , borderRight: none
                , borderRadius: int 0
                , paddingLeft: int 0
                , paddingRight: int 0
                }
              , guard isList css
                { "&:not(:first-child)": nested $ css
                  { marginTop: int (-borderWidth)
                  , ":not(:hover)": nested $ css
                    { borderTopColor: color theme.colors.transparent
                    }
                  }
                }
              ]
          BorderSquare ->
            merge
              [ css
                { borderRadius: int 0
                }
              , guard isList $ css
                { "&:not(:first-child)": nested $ css
                  { marginTop: prop S8
                  }
                }
              ]
      , guard isInteractive $ css
        { "&:hover": nested $ css
          { borderColor: color theme.colors.black2
          }
        }
      ]
