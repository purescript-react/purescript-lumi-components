module Lumi.Styles.Border where

import Prelude

import Lumi.Styles (StyleModifier, styleModifier, styleModifier_)
import Lumi.Styles as Styles
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Emotion (color, css, int, nested, none, str)

border :: StyleModifier
border =
  styleModifier \(LumiTheme theme) ->
    css
      { borderWidth: int 1
      , borderColor: color theme.colors.black4
      , borderStyle: str "solid"
      , padding: str "8px 16px"
      }

round :: StyleModifier
round = Styles.do
  border
  styleModifier_ $ css { borderRadius: int 4 }

topBottom :: StyleModifier
topBottom = Styles.do
  border
  styleModifier_
    $ css
    $ { borderLeft: none
      , borderRight: none
      , borderRadius: int 0
      , paddingLeft: int 0
      , paddingRight: int 0
      }

interactive :: StyleModifier
interactive = Styles.do
  border
  styleModifier \(LumiTheme theme) ->
    css
      { "&:hover": nested $ css
        { borderColor: color theme.colors.black2
        }
      }
