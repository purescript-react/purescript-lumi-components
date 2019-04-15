module Lumi.Components.Examples.Svg where

import Prelude

import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Svg (userSvg, clientSvg)
import Lumi.Components.Text (h2_)
import React.Basic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ h2_ "Client"
    , svgExample clientSvg

    , h2_ "User"
    , svgExample userSvg
    ]
  where
    svgExample svg =
      example $ R.div
        { style: R.css { maxHeight: 200, maxWidth: 200, height: "100%", width: "100%" }
        , children: [ svg ]
        }

