module Lumi.Components.Examples.Card where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect.Console (log)
import Lumi.Components.Card (card, defaults)
import Lumi.Components.Column (column_)
import Lumi.Components.Row (row_)
import Lumi.Components.Spacing (Space(..), hspace)
import Lumi.Components.Text (h2_)
import Lumi.Components.Example (example)
import React.Basic (JSX)
import React.Basic.DOM as R
import Web.HTML.History (URL(..))

docs :: JSX
docs =
  row_
    [ column_
        [ h2_ "Default state"
        , example $
            card defaults
              { title = "Poly Mailers"
              , subtitle = "14.50\" × 19.00\""
              , href = Just $ URL "https://www.lumi.com/"
              , onNavigate = log <<< un URL
              , children =
                  [ R.img
                      { src: "https://s3.amazonaws.com/lumi-flapjack-production/mockups/4985252ac5ba4c79e2ecbc6d3438e4ca.jpg"
                      }
                  ]
              }
        ]

    , hspace S32

    , column_
        [ h2_ "Selected state"
        , example $
            card defaults
              { title = "Poly Mailers"
              , subtitle = "14.50\" × 19.00\""
              , selected = true
              , children =
                  [ R.img
                      { src: "https://s3.amazonaws.com/lumi-flapjack-production/mockups/4985252ac5ba4c79e2ecbc6d3438e4ca.jpg"
                      }
                  ]
              }
        ]
    ]
