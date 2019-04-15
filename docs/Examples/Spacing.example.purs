module Lumi.Components.Examples.Spacing where

import Prelude

import Color (cssStringHSLA)
import Lumi.Components.Color (colors)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Row (row)
import Lumi.Components.Spacing (Space(..), hspace, vspace)
import Lumi.Components.Text (body_)
import React.Basic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ spacingExample "4px" S4
    , spacingExample "8px" S8
    , spacingExample "12px" S12
    , spacingExample "16px" S16
    , spacingExample "24px" S24
    , spacingExample "32px" S32
    , spacingExample "48px" S48
    , spacingExample "56px" S56
    , spacingExample "64px" S64
    , spacingExample "72px" S72
    , spacingExample "80px" S80
    , spacingExample "88px" S88
    , spacingExample "96px" S96
    , spacingExample "104px" S104
    , spacingExample "112px" S112
    ]
  where
    spacingExample text size =
      example $
        row
          { style: R.css { alignItems: "center" }
          , children:
              [ body_ text
              , hspace S12
              , row
                  { style: R.css { backgroundColor: cssStringHSLA colors.black3 }
                  , children:
                      [ R.div_ [ vspace S64 ]
                      , hspace size
                      ]
                  }
              ]
          }
