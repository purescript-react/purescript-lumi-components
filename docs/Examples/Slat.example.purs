module Lumi.Components.Examples.Slat where

import Prelude

import Data.Array (intercalate, replicate)
import Data.Maybe (Maybe(..))
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components ((%))
import Lumi.Components.Color (colors)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components2.Box (mkBox)
import Lumi.Components2.Slat (mkSlat)
import Lumi.Styles.Border (Border(..))
import Lumi.Styles.Theme (LumiTheme)
import React.Basic (JSX, createContext, fragment)
import React.Basic.DOM as R
import Web.HTML (window)
import Web.HTML.Window (alert)

theme :: LumiTheme
theme = { colors }

docs :: JSX
docs =
  unsafePerformEffect do
    t <- createContext theme
    box <- mkBox t
    slat <- mkSlat t
    pure $ column_
      $ intercalate [ vspace S16 ]
          [ [ example
                $ fragment
                $ replicate 3
                $ slat
                % _
                    { content =
                      [ box % _ { content = [ R.text "one" ] }
                      , box % _ { content = [ R.text "two" ] }
                      ]
                    }
            , example
                $ fragment
                $ replicate 3
                $ slat
                % _
                    { border = BorderSquare
                    , content = [ R.text "one", R.text "two" ]
                    , onClick = Just $ window >>= alert "click!"
                    }
            , example
                $ fragment
                $ replicate 9
                $ slat
                % _
                    { border = BorderTopBottom
                    , content = [ R.text "one", R.text "two" ]
                    , onClick = Just $ window >>= alert "click!"
                    }
            ]
          ]
