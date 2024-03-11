module Lumi.Components2.Examples.Clip where

import Prelude

import Lumi.Components (($$$))
import Lumi.Components.Example (example)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components2.Text as T
import Lumi.Components2.Box (box)
import Lumi.Components2.Clip (clip)
import Lumi.Styles as S
import React.Basic.Classic (JSX)

docs :: JSX
docs =
  box
    _
      { content =
        [ T.paragraph_ $$$ "The Clip component wraps the provided content with a grey border and a \"Copy\" button, which copies the text content into the system clipboard."
        , T.paragraph_ $$$ "If clipboard access is not allowed or not supported the text will be left highlighted, allowing the user to press ctrl+c manually. Only the plain text content is copied, not the HTML."
        , vspace S24
        , example
            $ clip
            $ _ { content =
                    [ T.paragraph
                      $ S.style_ (S.css { marginBottom: S.px 0 })
                      $ T.truncate
                      $$$ [ T.text $$$ "someone@email.com" ]
                    ]
                }
        ]
      }
