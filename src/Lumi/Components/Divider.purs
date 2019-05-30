module Lumi.Components.Divider where

import Prelude

import Color (cssStringHSLA)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import React.Basic (Component, JSX, createComponent, makeStateless)
import React.Basic.DOM as R

type DividerProps =
  { style :: R.CSS
  }

component :: Component DividerProps
component = createComponent "Divider"

divider :: DividerProps -> JSX
divider = makeStateless component $ R.hr <<< mapProps
  where
    mapProps props =
      { className: "lumi"
      , style: props.style
      }

divider_ :: JSX
divider_ = divider { style: R.css {} }

styles :: JSS
styles = jss
  { "@global":
      { "hr.lumi":
          { height: "1px"
          , color: cssStringHSLA colors.black4
          , background: cssStringHSLA colors.black4
          , fontSize: "0"
          , border: "0"
          , flexShrink: "0"
          }
      }
  }
