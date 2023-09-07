module Lumi.Styles.Box where

import Prelude

import Color (cssStringHSLA)
import Lumi.Styles (StyleModifier, auto, baseline, borderBox, center, column, flex, flexEnd, flexStart, manipulation, none, pointer, px, row, spaceAround, spaceBetween, spaceEvenly, stretch, style, style_, wrap)
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Emotion (class IsStyleProperty, css, nested, prop, str)

box :: StyleModifier
box =
  style_
    $ css
        { label: str "box"
        , display: flex
        , flexDirection: column
        , boxSizing: borderBox
        , minHeight: px 0
        , minWidth: auto
        , flex: str "0 0 auto"
        , margin: px 0
        , padding: px 0
        }

_row :: StyleModifier
_row =
  style_
    $ css { flexDirection: row }

_column :: StyleModifier
_column =
  style_
    $ css { flexDirection: column }

_wrap :: StyleModifier
_wrap =
  style_
    $ css { flexWrap: wrap }

_flex :: StyleModifier
_flex =
  style_
    $ css { flex: str "1" }

data FlexAlign
  = Start
  | End
  | Center
  | Stretch
  | Baseline
  | SpaceAround
  | SpaceBetween
  | SpaceEvenly

instance isStylePropertyFlexAlign :: IsStyleProperty FlexAlign where
  prop a =
    case a of
      Start -> flexStart
      End -> flexEnd
      Center -> center
      Stretch -> stretch
      Baseline -> baseline
      SpaceAround -> spaceAround
      SpaceBetween -> spaceBetween
      SpaceEvenly -> spaceEvenly

_justify :: FlexAlign -> StyleModifier
_justify a = style_ $ css { justifyContent: prop a }

_align :: FlexAlign -> StyleModifier
_align a = style_ $ css { alignItems: prop a }

_alignSelf :: FlexAlign -> StyleModifier
_alignSelf a = style_ $ css { alignSelf: prop a }

_interactive :: StyleModifier
_interactive =
  style_
    $ css
    $ { touchAction: manipulation
      , userSelect: none
      , cursor: pointer
      }

_focusable :: StyleModifier
_focusable =
  style \(LumiTheme theme) ->
    css
      { "&:focus-within, &:active":
        nested
          $ css
              { outline: str "0"
              , boxShadow: str ("0 0 0 3px " <> cssStringHSLA theme.colors.primary3)
              }
      , "&::-moz-focus-inner":
        nested
          $ css
              { border: str "0"
              }
      }
