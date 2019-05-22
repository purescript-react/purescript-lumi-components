module Lumi.Components.Responsive where

import Prelude

import JSS (JSS, jss)
import React.Basic (JSX, element)
import React.Basic.DOM (unsafeCreateDOMComponent)

mobile :: JSX -> JSX
mobile = lumiMobileElement <<< { children: _ }
  where
    lumiMobileElement = element (unsafeCreateDOMComponent "lumi-mobile")

desktop :: JSX -> JSX
desktop = lumiDesktopElement <<< { children: _ }
  where
    lumiDesktopElement = element (unsafeCreateDOMComponent "lumi-desktop")

styles :: JSS
styles = jss
  { "@global":
      { "@media (max-width: 860px)":
          { "lumi-desktop":
              { "display": "none"
              }
          }
      , "@media (min-width: 861px)":
          { "lumi-mobile":
              { "display": "none"
              }
          }
      }
  }
