module Lumi.Components.Examples.Svg where

import Lumi.Components.Column (column_)
import Lumi.Components.Text (h2_)
import Lumi.Components.Svg (userSvg, clientSvg)
import Lumi.Components.Example (example)
import React.Basic (JSX)

docs :: JSX
docs =
  column_
    [ h2_ "Client"
    , example clientSvg

    , h2_ "User"
    , example userSvg
    ]
