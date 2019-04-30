module Lumi.Components.Border where

import Prelude

import Color (cssStringHSLA)
import Lumi.Components.Color (colors)
import Lumi.Components.Column (column)
import React.Basic (JSX)
import React.Basic.DOM as R

border :: Array JSX -> JSX
border children =
  column
    { style: R.css
        { border: "1px solid " <> cssStringHSLA colors.black3
        , borderRadius: "5px"
        }
    , children
    }
