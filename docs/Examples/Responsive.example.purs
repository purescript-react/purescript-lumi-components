module Lumi.Components.Examples.Responsive where

import Prelude
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Responsive (desktop, mobile, phone)
import Lumi.Components.Text (body_, h2_)
import React.Basic (JSX)

docs :: JSX
docs =
  column_
    $ [ h2_ "Desktop and mobile"
      , example
          $ column_
              [ mobile \_ ->
                  body_ "Mobile: this text only is only rendered on mobile-sized screens."
              , desktop \_ ->
                  body_ "Desktop: this text only is only rendered on desktop-sized screens."
              , phone \_ ->
                  body_ "Phone: this text only is only rendered on phone-sized screens."
              ]
      ]
