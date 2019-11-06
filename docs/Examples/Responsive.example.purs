module Lumi.Components.Examples.Responsive where

import Prelude

import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Responsive (desktop, mobile, phone, withMobile)
import Lumi.Components.Text (body_, p_)
import React.Basic (JSX)

docs :: JSX
docs =
  column_
    $ [ p_ "The following component renders different values at each of these sizes: desktop, mobile, and phone"
      , p_ "Note that phone sized screens still report themselves as \"mobile\" as well."
      , example
          $ column_
              [ withMobile \isMobile ->
                  body_
                    if isMobile then
                      "Mobile: this text renders differently on mobile-sized screens."
                    else
                      "Not mobile: this text renders differently on mobile-sized screens."
              , mobile \_ ->
                  body_ "Mobile: this text is only rendered on mobile-sized screens."
              , desktop \_ ->
                  body_ "Desktop: this text is only rendered on desktop-sized screens."
              , phone \_ ->
                  body_ "Phone: this text is only rendered on phone-sized screens."
              ]
      ]
