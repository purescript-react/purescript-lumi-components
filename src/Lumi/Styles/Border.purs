module Lumi.Styles.Border where

import Prelude

import Lumi.Components (StyleModifier, styleModifier)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion (color, css, int, nested, none, str)

border :: LumiTheme -> StyleModifier
border theme =
  styleModifier
  $ css
  $ { borderWidth: int 1
    , borderColor: color theme.colors.black4
    , borderStyle: str "solid"
    , padding: str "8px 16px"
    }

round :: LumiTheme -> StyleModifier
round theme =
  border theme >>>
  ( styleModifier
    $ css { borderRadius: int 4 }
  )

topBottom :: LumiTheme -> StyleModifier
topBottom theme =
  border theme >>>
  ( styleModifier
    $ css
    $ { borderLeft: none
      , borderRight: none
      , borderRadius: int 0
      , paddingLeft: int 0
      , paddingRight: int 0
      }
  )

interactive :: LumiTheme -> StyleModifier
interactive theme =
  border theme >>>
  ( styleModifier
    $ css
    $ { "&:hover": nested $ css
        { borderColor: color theme.colors.black2
        }
      }
  )
