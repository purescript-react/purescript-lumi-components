module Lumi.Components.Examples.Column where

import Prelude

import Lumi.Components.Button (button, primary)
import Lumi.Components.Column (column_, responsiveColumn_)
import Lumi.Components.Text (h2_, p_)
import Lumi.Components.Example (example)
import React.Basic (JSX)

docs :: JSX
docs =
  column_
    [ example
        $ column_
            [ button primary { title = "Button" }
            , button primary { title = "Button" }
            , button primary { title = "Button" }
            ]

    , h2_ "Responsive column"
    , p_ "* Resize the window to see how the component responds."
    , example
        $ responsiveColumn_
            [ button primary { title = "Button" }
            , button primary { title = "Button" }
            , button primary { title = "Button" }
            ]
    ]
