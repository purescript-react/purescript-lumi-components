module Lumi.Components.ButtonGroup where

import Prelude

import JSS (JSS, jss)
import Lumi.Components.ZIndex (ziButtonGroup)
import React.Basic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (unsafeCreateDOMComponent, CSS)

type ButtonGroupProps =
  { children :: Array JSX
  , style :: CSS
  , joined :: Boolean
  }

component :: Component ButtonGroupProps
component = createComponent "ButtonGroup"

buttonGroup :: ButtonGroupProps -> JSX
buttonGroup = makeStateless component render
  where
    render props =
      buttonGroupElement
        { "data-joined": props.joined
        , style: props.style
        , children: map renderChild props.children
        }
      where
        renderChild child =
          buttonGroupChildElement { children: [ child ] }

    buttonGroupElement = element (unsafeCreateDOMComponent "lumi-button-group")
    buttonGroupChildElement = element (unsafeCreateDOMComponent "lumi-button-group-child")

styles :: JSS
styles = jss
  { "@global":
      { "lumi-button-group":
          { height: "100%"
          , display: "flex"
          , flexFlow: "row"

          , "&[data-joined=false] > lumi-button-group-child":
              { marginRight: "10px"
              , "&:last-child": { marginRight: "0" }
              }

          , "&[data-joined=true] > lumi-button-group-child":
              { "&:not(:last-child)":
                  { marginRight: "-1px"
                  , borderTopRightRadius: "0"
                  , borderBottomRightRadius: "0"
                  }
              , "&:not(:first-child)":
                  { borderTopLeftRadius: "0"
                  , borderBottomLeftRadius: "0"
                  }
              , "&:focus": { zIndex: ziButtonGroup }
              , "&:hover": { zIndex: ziButtonGroup }
              }
          }
      }
  }
