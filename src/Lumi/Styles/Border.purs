module Lumi.Styles.Border where

import Prelude

import Lumi.Components.Spacing (Space(..))
import Lumi.Styles (StyleModifier, px2, solid, style, style_)
import Lumi.Styles.Box (box)
import Lumi.Styles.Box as Box
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Emotion (color, css, px, nested, none, prop, str)

border :: StyleModifier
border =
  box
    <<< style \(LumiTheme theme) ->
      css
        { label: str "border"
        , borderWidth: px 1
        , borderColor: color theme.colors.black4
        , borderStyle: solid
        , padding: px2 8 16
        }

_round :: StyleModifier
_round =
  style_
    $ css
      { borderRadius: px 4
      }

_topBottom :: StyleModifier
_topBottom =
  style_
        ( css
            { borderLeft: none
            , borderRight: none
            , borderRadius: px 0
            , paddingLeft: px 0
            , paddingRight: px 0
            }
        )

_interactive :: StyleModifier
_interactive =
  Box._interactive
    <<< Box._focusable
    <<< style \(LumiTheme theme) ->
        css
          { "&:hover":
            nested
              $ css
                  { borderColor: color theme.colors.black2
                  }
          }

_listSpaced :: StyleModifier
_listSpaced =
  style \(LumiTheme theme) ->
    css
      { "&:not(:first-child)":
        nested
          $ css
              { marginTop: prop S8
              }
      }

_listCompact :: StyleModifier
_listCompact =
  style \(LumiTheme theme) ->
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
              { marginTop: px (-1)
              , ":not(:hover)":
                nested
                  $ css
                      { borderTopColor: color theme.colors.transparent
                      }
              }
      }
