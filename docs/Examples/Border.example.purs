module Lumi.Components.Examples.Border where

import Prelude

import Lumi.Components.Border (border)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import React.Basic (Component, JSX, createComponent, makeStateless)
import React.Basic.DOM (css)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ example
        $ border
          [ R.div
              { children: [ R.text "border element" ]
              , style: R.css { padding: "12px" }
              }
          ]
    ]
