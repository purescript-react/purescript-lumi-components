module Lumi.Styles.Box where

import Prelude

import Color (cssStringHSLA)
import Lumi.Styles (StyleModifier, styleModifier, styleModifier_)
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Emotion (class IsStyleProperty, css, nested, prop, str)

box :: forall props. StyleModifier props
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

column :: forall props. StyleModifier props
column = box

row :: forall props. StyleModifier props
row =
  box >>>
  styleModifier_ (css { flexDirection: str "row" })

wrap :: forall props. StyleModifier props
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

justify :: forall props. FlexAlign -> StyleModifier props
justify a = styleModifier_ $ css { justifyContent: prop a }

align :: forall props. FlexAlign -> StyleModifier props
align a = styleModifier_ $ css { alignItems: prop a }

alignSelf :: forall props. FlexAlign -> StyleModifier props
alignSelf a = styleModifier_ $ css { alignSelf: prop a }

interactive :: forall props. StyleModifier props
interactive =
  styleModifier_
  $ css
  $ { touchAction: str "manipulation"
    , userSelect: str "none"
    , cursor: str "pointer"
    }

focusable :: forall props. StyleModifier props
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
