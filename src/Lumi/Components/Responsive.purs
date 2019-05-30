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
      { "lumi-desktop":
          { "display": "flex"
          , "flex-shrink": "0"
          , "@media (max-width: 860px)":
              { "display": "none !important"
              }
          }
      , "lumi-mobile":
          { "display": "flex"
          , "flex-shrink": "0"
          , "@media (min-width: 861px)":
              { "display": "none !important"
              }
          }
      }
  }
