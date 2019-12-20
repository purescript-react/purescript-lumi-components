module Lumi.Components2.Examples.Clip where

import Prelude
import Lumi.Components (lumiElement)
import Lumi.Components.Example (example)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Text (body_, p_)
import Lumi.Components2.Box (box)
import Lumi.Components2.Clip (clip)
import React.Basic (JSX)

docs :: JSX
docs =
  lumiElement box
    _
      { content =
        [ p_ "..."
        , vspace S24
        , example
            $ lumiElement clip
            $ _ { content = [ body_ "someone@email.com" ] }
        ]
      }
