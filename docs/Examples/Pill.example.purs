module Lumi.Components.Examples.Pill where

import Prelude

import Lumi.Components.Column (column_)
import Lumi.Components.Pill (pill_)
import Lumi.Components.Example (example)
import React.Basic (JSX)
import Lumi.Components.Status (Status(..))

docs :: JSX
docs =
  column_
    [ example $
        pill_ Active "Active"

    , example $
        pill_ Warning "Warning"

    , example $
        pill_ Error "Error"

    , example $
        pill_ Unknown "Unknown"
    ]
