module Lumi.Components.Examples.Border where

import Prelude

import Lumi.Components.Border (borderBottom, borderRound, borderSquare, borderSquareBottom, borderSquareTop, borderTop)
import Lumi.Components.Column (column, column_)
import Lumi.Components.Example (example)
import Lumi.Components.Text (h2_)
import React.Basic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ h2_ "Rounded borders"
    , example
        $ borderRound $
            column
              { children: [ R.text "bordered element" ]
              , style: R.css {}
              }
    , h2_ "Square borders"
    , example
        $ borderSquare $
            column
              { children: [ R.text "bordered element" ]
              , style: R.css {}
              }
    , h2_ "Square Top borders"
    , example
        $ borderSquareTop $
            column
              { children: [ R.text "bordered element" ]
              , style: R.css {}
              }
    , h2_ "Square Bottom borders"
    , example
        $ borderSquareBottom $
            column
              { children: [ R.text "bordered element" ]
              , style: R.css {}
              }
    , h2_ "Top borders"
    , example
        $ borderTop $
            column
              { children: [ R.text "bordered element" ]
              , style: R.css {}
              }
    , h2_ "Bottom borders"
    , example
        $ borderBottom $
            column
              { children: [ R.text "bordered element" ]
              , style: R.css {}
              }
    ]
