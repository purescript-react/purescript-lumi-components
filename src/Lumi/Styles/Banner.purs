module Lumi.Styles.Banner where

import Prelude

import Data.Foldable (fold)
import Lumi.Components (PropsModifier)
import Lumi.Styles (color, css, str, styleModifier)
import Lumi.Styles.Border (_round) as S
import Lumi.Styles.Box (FlexAlign(..), _align, _row, box) as S
import Lumi.Styles.Responsive (desktopQuery)
import Lumi.Styles.Theme (LumiTheme(..))

banner :: forall props. PropsModifier props
banner =
  S.box
  >>> S._row
  >>> S._align S.Center
  >>> S._round
  >>> styleModifier \(LumiTheme { colors }) ->
        fold
          [ css
              { backgroundColor: color colors.black4
              , color: color colors.black
              , padding: str "16px"
              }
          , desktopQuery $ css
              { padding: str "16px 24px"
              }
          ]
