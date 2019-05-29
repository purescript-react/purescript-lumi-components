module Lumi.Components.Examples.Responsive where

import Prelude

import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Responsive (desktop_, mobile_)
import Lumi.Components.Text (body_, h2_)
import React.Basic (JSX)

docs :: JSX
docs =
  column_ $
    [ h2_ "Desktop and mobile"
    , example $
        column_
          [ mobile_
              [ body_ "Mobile: this text only is only visible on mobile-sized screens."
              ]
          , desktop_
              [ body_ "Desktop: this text only is only visible on desktop-sized screens."
              ]
          ]
    ]
