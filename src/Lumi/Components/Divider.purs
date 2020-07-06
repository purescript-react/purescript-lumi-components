module Lumi.Components.Divider where

import Prelude

import Color (cssStringHSLA)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import React.Basic.Classic (Component, JSX, createComponent, element, makeStateless)
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

flexDivider :: DividerProps -> JSX
flexDivider = \{ style } -> lumiFlexDividerElement { style }
  where
    lumiFlexDividerElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-flex-divider")

flexDivider_ :: JSX
flexDivider_ = flexDivider { style: R.css {} }

styles :: JSS
styles = jss
  { "@global":
      { "hr.lumi":
          { height: "1px"
          , alignSelf: "stretch"
          , color: cssStringHSLA colors.black4
          , background: cssStringHSLA colors.black4
          , fontSize: "0"
          , border: "0"
          , flexShrink: "0"
          , flexBasis: "1px"
          }

      , "lumi-flex-divider":
          { alignSelf: "stretch"
          , color: cssStringHSLA colors.black4
          , background: cssStringHSLA colors.black4
          , fontSize: "0"
          , border: "0"
          , flexShrink: "0"
          , flexBasis: "1px"
          }
      }
  }
