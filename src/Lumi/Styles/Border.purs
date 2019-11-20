module Lumi.Styles.Border where

import Prelude

import Lumi.Components (PropsModifier)
import Lumi.Components.Spacing (Space(..))
import Lumi.Styles (styleModifier, styleModifier_)
import Lumi.Styles.Box as Box
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Emotion (color, css, int, nested, none, prop, str)

border :: forall props. PropsModifier props
border =
  styleModifier \(LumiTheme theme) ->
    css
      { borderWidth: int 1
      , borderColor: color theme.colors.black4
      , borderStyle: str "solid"
      , padding: str "8px 16px"
      }

round :: forall props. PropsModifier props
round =
  border
    >>> styleModifier_ (css { borderRadius: int 4 })

topBottom :: forall props. PropsModifier props
topBottom =
  border
    >>> styleModifier_
        ( css
            { borderLeft: none
            , borderRight: none
            , borderRadius: int 0
            , paddingLeft: int 0
            , paddingRight: int 0
            }
        )

interactive :: forall props. PropsModifier props
interactive =
  border
    >>> Box.interactive
    >>> Box.focusable
    >>> styleModifier \(LumiTheme theme) ->
        css
          { "&:hover":
            nested
              $ css
                  { borderColor: color theme.colors.black2
                  }
          }

listSpaced :: forall props. PropsModifier props
listSpaced =
  styleModifier \(LumiTheme theme) ->
    css
      { "&:not(:first-child)":
        nested
          $ css
              { marginTop: prop S8
              }
      }

listCompact :: forall props. PropsModifier props
listCompact =
  styleModifier \(LumiTheme theme) ->
    css
      { "&:first-child":
        nested
          $ css
              { borderTopColor: color theme.colors.transparent
              }
      , "&:last-child":
        nested
          $ css
              { borderBottomColor: color theme.colors.transparent
              }
      , "&:not(:first-child)":
        nested
          $ css
              { marginTop: int (-1)
              , ":not(:hover)":
                nested
                  $ css
                      { borderTopColor: color theme.colors.transparent
                      }
              }
      }
