module Lumi.Components.Border where

import Prelude

import Color (cssStringHSLA)
import Lumi.Components.Color (colors)
import JSS (JSS, jss)
import React.Basic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, unsafeCreateDOMComponent)
import React.Basic.DOM as R

component :: Component BorderProps
component = createComponent "Border"

type BorderProps =
  { children :: JSX
  , roundedBorders :: Boolean
  , style :: CSS
  }

border :: BorderProps -> JSX
border = makeStateless component render
  where
    lumiBorderElement = element (unsafeCreateDOMComponent "lumi-border")

    render props =
      lumiBorderElement
        { "data-rounded-borders": props.roundedBorders
        , children: props.children
        }

border_ :: JSX -> JSX
border_ children =
  border
    { children
    , style: R.css {}
    , roundedBorders: true
    }

styles :: JSS
styles = jss
  { "@global":
      { "lumi-border":
          { border: "1px solid " <> cssStringHSLA colors.black3
          , overflow: "hidden"
          , "&[data-rounded-borders=true]":
              { borderRadius: "5px" }
          }
      }
  }
