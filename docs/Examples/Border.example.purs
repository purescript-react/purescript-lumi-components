module Lumi.Components.Examples.Border where

import Prelude

import Lumi.Components.Border (border)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import React.Basic (Component, JSX, createComponent, makeStateless)
import React.Basic.DOM (css)
import React.Basic.DOM as R

component :: Component Unit
component = createComponent "BorderExample"

docs :: JSX
docs = unit # makeStateless component render
  where
    render _ =
      column_
        [ example
            $ border
                { children:
                    [ R.div
                      { children: R.text "bordered element"
                      , style: R.css { padding: "12px" }
                      }
                    ]
                }
        ]
