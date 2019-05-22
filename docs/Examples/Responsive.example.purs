module Lumi.Components.Examples.Responsive where

import Prelude

import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Responsive (desktop, mobile)
import Lumi.Components.Text (body_, h2_)
import React.Basic (JSX)

docs :: JSX
docs =
  column_ $
    [ h2_ "Desktop and mobile"
    , example $
        column_
          [ mobile $ body_ "Mobile: this text only is only visible on mobile-sized screens."
          , desktop $ body_ "Desktop: this text only is only visible on desktop-sized screens."
          ]
    ]
