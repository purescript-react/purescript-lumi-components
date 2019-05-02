module Lumi.Components.Examples.Border where

import Prelude

import Data.Maybe (Maybe(..))
import Lumi.Components.Border (border, border_)
import Lumi.Components.Column (column, column_)
import Lumi.Components.Example (example)
import Lumi.Components.Spacing (Space(..))
import Lumi.Components.Text (h2_)
import React.Basic (Component, JSX, createComponent, makeStateless)
import React.Basic.DOM (css)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ h2_ "Rounded borders"
    , example
        $ border_ $
            column
              { children: [ R.text "bordered element" ]
              , style: R.css { padding: "12px" }
              }
    , h2_ "Square borders"
    , example
        $ border
          { children:
              column
                { children: [ R.text "bordered element" ]
                , style: R.css { padding: "12px" }
                }
          , roundedBorders: false
          , style: R.css {}
          }
    ]
