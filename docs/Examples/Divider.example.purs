module Lumi.Components.Examples.Divider where

import Prelude

import Lumi.Components.Column (column_)
import Lumi.Components.Divider (divider_)
import Lumi.Components.Example (example)
import React.Basic (Component, JSX, createComponent, makeStateless)
import React.Basic.DOM (css)
import React.Basic.DOM as R

component :: Component Unit
component = createComponent "DividerExample"

docs :: JSX
docs = unit # makeStateless component render
  where
    render _ =
      column_
        [ example
            $ R.div
                { style: css { minWidth: 300 }
                , children: [ divider_ ]
                }
        ]
