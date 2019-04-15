module Lumi.Components.Examples.Loader where

import Prelude

import Data.Nullable (null)
import Lumi.Components.Column (column_)
import Lumi.Components.Loader (loader)
import Lumi.Components.Example (example)
import React.Basic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ example $
        loader { style: R.css {}, testId: null }
    ]
