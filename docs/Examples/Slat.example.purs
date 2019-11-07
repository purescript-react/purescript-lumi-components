module Lumi.Components.Examples.Slat where

import Prelude

import Data.Array (intercalate)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components ((%))
import Lumi.Components.Color (colors)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Icon (IconType(..), icon)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Spacing (Space(..), hspace, vspace)
import Lumi.Components.Text (h2_, h4_)
import Lumi.Components2.Button (linkStyle, mkButton, primary, secondary)
import Lumi.Components2.Slat (mkSlat)
import Lumi.Styles.Box (box)
import Lumi.Styles.Border (Border(..))
import Lumi.Styles.Button (ButtonState(..))
import Lumi.Styles.Theme (LumiTheme)
import React.Basic (JSX, createContext)
import React.Basic.DOM as R
import React.Basic.Emotion as E

theme :: LumiTheme
theme = { colors }

docs :: JSX
docs = unsafePerformEffect do
  t <- createContext theme
  slat <- mkSlat t
  pure $ column_ $
    intercalate [ vspace S16 ]
      [ [ example
            $ slat % _
              { content =
                  [ E.element box R.div' { children: [ R.text "one" ], className: "" }
                  , E.element box R.div' { children: [ R.text "two" ], className: "" }
                  ]
              }

        , example
              $ slat % _ { border = BorderSquare, content = [ R.text "one", R.text "two" ] }
        ]
      ]
