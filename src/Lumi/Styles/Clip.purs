module Lumi.Styles.Clip where

import Prelude
import Lumi.Components (PropsModifier)
import Lumi.Styles (styleModifier)
import Lumi.Styles.Border (_round, border)
import Lumi.Styles.Box (FlexAlign(..), _justify, _row)
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Emotion (color, css)

clip :: forall props. PropsModifier props
clip =
  border
    <<< _round
    <<< _row
    <<< _justify SpaceBetween
    <<< styleModifier \(LumiTheme { colors }) ->
        css
          { borderColor: color colors.black5
          , backgroundColor: color colors.black5
          }
