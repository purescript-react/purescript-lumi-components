module Lumi.Components.Examples.Details where

import Prelude

import Lumi.Components.Border (borderBottom, borderTop)
import Lumi.Components.Column (column_)
import Lumi.Components.Details (defaults, details)
import Lumi.Components.Divider (divider_)
import Lumi.Components.Example (example)
import Lumi.Components.Icon as Icon
import Lumi.Components.Row (row)
import Lumi.Components.Spacing (Space(..), hspace, vspace)
import Lumi.Components.Text (body_, h2_, subsectionHeader_)
import React.Basic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ h2_ "Bare/defaults"
    , example $
        details defaults
          { expanded = body_ "Here's all the info"
          }

    , h2_ "Custom arrow"
    , example $
        details defaults
          { summary = iconSummary $ subsectionHeader_ "Details"
          , expanded = body_ "Here's all the info"
          }

    , h2_ "Accordion"
    , example $
        borderTop $ borderBottom $
          details defaults
            { summary = iconSummary $ subsectionHeader_ "Details"
            , expanded =
                column_
                  [ vspace S8
                  , divider_
                  , vspace S8
                  , body_ "Here's all the info"
                  ]
            }
    ]
  where
    iconSummary child =
      row
        { style: R.css { alignItems: "center" }
        , children: [ Icon.icon_ Icon.ArrowRight, hspace S8, child ]
        }
