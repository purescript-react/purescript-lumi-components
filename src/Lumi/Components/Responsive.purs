module Lumi.Components.Responsive where

import Prelude

import JSS (JSS, jss)
import React.Basic (JSX, element)
import React.Basic.DOM (CSS, css, unsafeCreateDOMComponent)

type ResponsiveProps =
  { style :: CSS
  , children :: Array JSX
  }

mobile :: ResponsiveProps -> JSX
mobile = element (unsafeCreateDOMComponent "lumi-mobile")

mobile_ :: Array JSX -> JSX
mobile_ = mobile <<< { style: css {}, children: _ }

desktop :: ResponsiveProps -> JSX
desktop = element (unsafeCreateDOMComponent "lumi-desktop")

desktop_ :: Array JSX -> JSX
desktop_ = desktop <<< { style: css {}, children: _ }

styles :: JSS
styles = jss
  { "@global":
      { "@media (max-width: 860px)":
          { "lumi-desktop":
              { "display": "none !important"
              }
          }
      , "@media (min-width: 861px)":
          { "lumi-mobile":
              { "display": "none !important"
              }
          }
      }
  }
