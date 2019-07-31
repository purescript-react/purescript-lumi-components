module Lumi.Components.Examples.Badge where

import Lumi.Components.Badge (badge, badge_)
import Lumi.Components.Color (colors)
import Lumi.Components.Column (column_)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Text (nbsp)
import React.Basic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ badge_ "1"
    , vspace S8

    , badge_ "2"
    , vspace S8

    , badge_ "3"
    , vspace S8

    , badge_ "Hello!"
    , vspace S8

    , badge
        { background: colors.primary
        , color: colors.white
        , style: R.css {}
        , text: "1"
        }
    , vspace S8

    , badge
        { background: colors.primary
        , color: colors.white
        , style: R.css {}
        , text: nbsp
        }
    ]
