module Lumi.Components2.Examples.Clip where

import Prelude
import Lumi.Components.Example (example)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Text (body_, p_)
import Lumi.Components2.Box (box)
import Lumi.Components2.Clip (clip)
import React.Basic (JSX)

docs :: JSX
docs =
  box
    _
      { content =
        [ p_ "The Clip component wraps the provided content with a grey border and a \"Copy\" button, which copies the text content into the system clipboard."
        , p_ "If clipboard access is not allowed or not supported the text will be left highlighted, allowing the user to press ctrl+c manually. Only the plain text content is copied, not the HTML."
        , vspace S24
        , example
            $ clip
            $ _ { content = [ body_ "someone@email.com" ] }
        , p_ "The Clip behavior is also available as a React hook."
        ]
      }
