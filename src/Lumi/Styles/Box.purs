module Lumi.Styles.Box where

import Prelude

import Color (cssStringHSLA)
import Lumi.Components (PropsModifier)
import Lumi.Styles (styleModifier, styleModifier_)
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Emotion (class IsStyleProperty, css, nested, prop, str)

box :: forall props. PropsModifier props
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

column :: forall props. PropsModifier props
column = box

row :: forall props. PropsModifier props
row =
  box >>>
  styleModifier_ (css { flexDirection: str "row" })

wrap :: forall props. PropsModifier props
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

justify :: forall props. FlexAlign -> PropsModifier props
justify a = styleModifier_ $ css { justifyContent: prop a }

align :: forall props. FlexAlign -> PropsModifier props
align a = styleModifier_ $ css { alignItems: prop a }

alignSelf :: forall props. FlexAlign -> PropsModifier props
alignSelf a = styleModifier_ $ css { alignSelf: prop a }

interactive :: forall props. PropsModifier props
interactive =
  styleModifier_
  $ css
  $ { touchAction: str "manipulation"
    , userSelect: str "none"
    , cursor: str "pointer"
    }

focusable :: forall props. PropsModifier props
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
