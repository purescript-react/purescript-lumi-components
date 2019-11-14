module Lumi.Styles.Box where

import Prelude

import Color (cssStringHSLA)
import Lumi.Styles (StyleModifier, styleModifier, styleModifier_)
import Lumi.Styles as Styles
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Emotion (class IsStyleProperty, css, nested, prop, str)

box :: StyleModifier
box =
  styleModifier_
  $ css
  $ { display: str "flex"
    , flexDirection: str "column"
    , boxSizing: str "border-box"
    , minHeight: str "0"
    , minWidth: str "min-content"
    , flex: str "0 0 auto"
    }

column :: StyleModifier
column = box

row :: StyleModifier
row = Styles.do
  box
  styleModifier_ $ css { flexDirection: str "row" }

wrap :: StyleModifier
wrap =
  styleModifier_
  $ css
  $ { flexWrap: str "wrap"
    }

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

justify :: FlexAlign -> StyleModifier
justify a = styleModifier_ $ css { justifyContent: prop a }

align :: FlexAlign -> StyleModifier
align a = styleModifier_ $ css { alignItems: prop a }

alignSelf :: FlexAlign -> StyleModifier
alignSelf a = styleModifier_ $ css { alignSelf: prop a }

interactive :: StyleModifier
interactive =
  styleModifier_
  $ css
  $ { touchAction: str "manipulation"
    , userSelect: str "none"
    , cursor: str "pointer"
    }

focusable :: StyleModifier
focusable =
  styleModifier \(LumiTheme theme) ->
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
