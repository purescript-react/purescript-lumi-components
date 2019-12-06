module Lumi.Styles.Banner where

import Prelude

import Data.Foldable (fold)
import Lumi.Styles (StyleModifier, color, css, str, style)
import Lumi.Styles.Border (_round) as S
import Lumi.Styles.Box (FlexAlign(..), _align, _row, box) as S
import Lumi.Styles.Responsive (desktopQuery, onDesktop)
import Lumi.Styles.Theme (LumiTheme(..))

banner :: StyleModifier
banner =
  S.box
  <<< S._row
  <<< S._align S.Baseline
  <<< onDesktop (S._align S.Center)
  <<< S._round
  <<< style \(LumiTheme { colors }) ->
        fold
          [ css
              { backgroundColor: color colors.black4
              , color: color colors.black
              , padding: str "12px 16px"
              }
          , desktopQuery $ css
              { padding: str "12px 24px"
              }
          ]
