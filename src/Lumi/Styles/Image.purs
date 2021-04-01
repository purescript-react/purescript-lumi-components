module Lumi.Styles.Image where

import Prelude

import Lumi.Components.Spacing (Space(..))
import Lumi.Styles (StyleModifier, style, style_)
import Lumi.Styles.Box (box)
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Emotion (color, css, percent, prop, str)

imageThumb :: StyleModifier
imageThumb =
  box
    <<< _medium
    <<< style \(LumiTheme theme) ->
      css
        { boxSizing: str "border-box"
        , overflow: str "hidden"
        , display: str "flex"
        , border: str "1px solid"
        , borderColor: color theme.colors.black4
        }

_round :: StyleModifier
_round =
  style_
    $ css
      { borderRadius: percent 50.0
      }

_small :: StyleModifier
_small =
  style_ $ css
      { width: prop S24
      , height: prop S24
      }

_medium :: StyleModifier
_medium =
  style_ $ css
      { width: prop S32 -- 30px
      , height: prop S32 -- 30px
      }

_large :: StyleModifier
_large =
  style_ $ css
      { width: prop S40 -- 36px
      , height: prop S40 -- 36px
      }

_extraLarge :: StyleModifier
_extraLarge =
  style_ $ css
      { width: prop S112 -- 140px
      , height: prop S112 -- 140px
      }