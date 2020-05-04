module Lumi.Styles.Box where

import Prelude
import Color (cssStringHSLA)
import Lumi.Styles (StyleModifier, int, style, style_)
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Emotion (class IsStyleProperty, css, nested, prop, str)

box :: StyleModifier
box =
  style_
    $ css
        { label: str "box"
        , display: str "flex"
        , flexDirection: str "column"
        , boxSizing: str "border-box"
        , minHeight: int 0
        , minWidth: str "min-content"
        , flex: str "0 0 auto"
        , margin: int 0
        , padding: int 0
        }

_row :: StyleModifier
_row =
  style_
    $ css { flexDirection: str "row" }

_column :: StyleModifier
_column =
  style_
    $ css { flexDirection: str "column" }

_wrap :: StyleModifier
_wrap =
  style_
    $ css { flexWrap: str "wrap" }

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
    str case a of
      Start -> "flex-start"
      End -> "flex-end"
      Center -> "center"
      Stretch -> "stretch"
      Baseline -> "baseline"
      SpaceAround -> "space-around"
      SpaceBetween -> "space-between"
      SpaceEvenly -> "space-evenly"

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
    $ { touchAction: str "manipulation"
      , userSelect: str "none"
      , cursor: str "pointer"
      }

_focusable :: StyleModifier
_focusable =
  style \(LumiTheme theme) ->
    css
      { "&:focus, &:active":
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
