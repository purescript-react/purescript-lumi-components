module Lumi.Styles.Clip where

import Prelude

import Lumi.Styles (StyleModifier, style)
import Lumi.Styles.Border (_round, border)
import Lumi.Styles.Box (FlexAlign(..), _justify, _row)
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Emotion (color, css)

clip :: StyleModifier
clip =
  border
    <<< _round
    <<< _row
    <<< _justify SpaceBetween
    <<< style \(LumiTheme { colors }) ->
        css
          { borderColor: color colors.black5
          , backgroundColor: color colors.black5
          }
