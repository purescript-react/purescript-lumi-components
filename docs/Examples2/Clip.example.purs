module Lumi.Components2.Examples.Clip where

import Prelude
import Lumi.Components (lumiElement)
import Lumi.Components.Example (example)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Text (p_)
import Lumi.Components2.Box (box)
import Lumi.Components2.Clip (clip)
import Lumi.Styles (styleModifier_)
import Lumi.Styles as E
import React.Basic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  lumiElement box
    _
      { content =
        [ p_ "..."
        , vspace S24
        , example
            $ lumiElement clip
            $ _ { content = [ R.text "someone@email.com" ] }
        ]
      }
