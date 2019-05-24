module Lumi.Components.Details where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Icon (icon_)
import Lumi.Components.Text (body_)
import React.Basic.DOM as R
import React.Basic.Hooks (JSX, component, element)

type DetailsProps =
  { summary :: JSX
  , expanded :: JSX
  , defaultOpen :: Boolean
  }

details :: DetailsProps -> JSX
details = element $ unsafePerformEffect do
  component "Details" \props ->
    pure $ R.details
      { className: "lumi"
      , children:
          [ R.summary
              { className: "lumi"
              , children: [ props.summary ]
              }
          , props.expanded
          ]
      }

defaults :: DetailsProps
defaults =
  { summary: body_ "Details"
  , expanded: mempty
  , defaultOpen: false
  }

styles :: JSS
styles = jss
  { "@global":
      { "details.lumi":
          { "& > summary.lumi":
              { listStyle: "none"

              , "&:focus":
                  { outline: "none"
                  }
              }
          }
      }
  }
