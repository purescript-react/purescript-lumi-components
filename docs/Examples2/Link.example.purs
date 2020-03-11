module Lumi.Components2.Examples.Link where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Console (log)
import Effect.Uncurried (mkEffectFn1)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Text (body_)
import Lumi.Components2.Link (link)
import React.Basic (JSX)
import React.Basic.DOM as R
import Web.HTML.History (URL(..))

docs :: JSX
docs =
  column_
    [ example
        $ R.div
            { onClick: mkEffectFn1 \_ -> log "Propagated"
            , style: R.css { display: "flex", flexDirection: "column" }
            , children:
              [ link
                  _
                    { href = URL "#/link"
                    , navigate = pure $ log "link clicked"
                    , content = [ body_ "Click here" ]
                    }
              , link
                  _
                    { href = URL "#/input"
                    , target = Just "_blank"
                    , content = [ body_ "This should open in a new tab" ]
                    }
              ]
            }
    ]
