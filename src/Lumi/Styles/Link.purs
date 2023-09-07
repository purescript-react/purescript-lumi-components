module Lumi.Styles.Link where

import Prelude

import Lumi.Styles (StyleModifier, color, nested, none, pointer, style, underline)
import Lumi.Styles.Box (box)
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Emotion (css)

link :: StyleModifier
link =
  box
    <<< style \(LumiTheme theme) ->
        css
          { color: color theme.colors.primary
          , textDecoration: none
          , "&:visited":
            nested
              $ css
                  { color: color theme.colors.primary
                  , textDecoration: none
                  }
          , "&:hover":
            nested
              $ css
                  { cursor: pointer
                  , textDecoration: underline
                  }
          }

secondary :: StyleModifier
secondary = style \(LumiTheme theme) ->
  css
    { color: color theme.colors.black
    , textDecoration: none
    , "&:visited":
      nested
        $ css
            { color: color theme.colors.black
            , textDecoration: none
            }
    , "&:hover":
      nested
        $ css
            { cursor: pointer
            , textDecoration: underline
            }
    }
