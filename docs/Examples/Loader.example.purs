module Lumi.Components.Examples.Loader where

import Prelude

import Color (cssStringHSLA)
import Data.Nullable (null)
import Lumi.Components.Color (colors)
import Lumi.Components.Column (column, column_)
import Lumi.Components.Loader (loader)
import Lumi.Components.Example (example)
import React.Basic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ example $
        column {
          children: [ loader { style: R.css {}, testId: null } ]
          , style: R.css { backgroundColor: cssStringHSLA colors.accent1 }
        }
    ]
