module Lumi.Components.Examples.Row where

import Prelude

import Lumi.Components.Button (button, primary)
import Lumi.Components.Column (column_)
import Lumi.Components.Row (responsiveRow_, row_)
import Lumi.Components.Text (h2_, p_)
import Lumi.Components.Example (example)
import React.Basic (JSX)

docs :: JSX
docs =
  column_
    [ example
        $ row_
            [ button primary { title = "Button" }
            , button primary { title = "Button" }
            , button primary { title = "Button" }
            ]

    , h2_ "Responsive row"
    , p_ "* Resize the window to see how the component responds."
    , example
        $ responsiveRow_
            [ button primary { title = "Button" }
            , button primary { title = "Button" }
            , button primary { title = "Button" }
            ]
    ]
