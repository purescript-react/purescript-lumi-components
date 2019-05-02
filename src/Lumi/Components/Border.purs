module Lumi.Components.Border where

import Prelude

import Color (cssStringHSLA)
import Lumi.Components.Color (colors)
import JSS (JSS, jss)
import React.Basic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (unsafeCreateDOMComponent)

data BorderStyle
  = Top
  | Bottom
  | Round
  | Square

toStyle :: BorderStyle -> String
toStyle Top = "top"
toStyle Bottom = "bottom"
toStyle Round = "round"
toStyle Square = "square"

component :: Component BorderProps
component = createComponent "Border"

type BorderProps =
  { children :: JSX
  , borderStyle :: BorderStyle
  }

border :: BorderProps -> JSX
border = makeStateless component render
  where
    lumiBorderElement = element (unsafeCreateDOMComponent "lumi-border")

    render props =
      lumiBorderElement
        { "data-border-style": toStyle props.borderStyle
        , children: props.children
        }

borderSquare :: JSX -> JSX
borderSquare children =
  border
    { children
    , borderStyle: Square
    }

borderRound :: JSX -> JSX
borderRound children =
  border
    { children
    , borderStyle: Round
    }

borderTop :: JSX -> JSX
borderTop children =
  border
    { children
    , borderStyle: Top
    }

borderBottom :: JSX -> JSX
borderBottom children =
  border
    { children
    , borderStyle: Bottom
    }


styles :: JSS
styles = jss
  { "@global":
      { "lumi-border":
          { border: "1px solid " <> cssStringHSLA colors.black3
          , overflow: "hidden"
          , "&[data-border-style='round']":
              { borderRadius: "5px" }
          , "&[data-border-style='square']":
              { borderRadius: "0px" }
          , "&[data-border-style='top']":
              { borderTopRightRadius: "5px"
              , borderTopLeftRadius: "5px"
              , borderBottomRightRadius: "0px"
              , borderBottomLeftRadius: "0px"
              }
          , "&[data-border-style='bottom']":
              { borderTopRightRadius: "0px"
              , borderTopLeftRadius: "0px"
              , borderBottomRightRadius: "5px"
              , borderBottomLeftRadius: "5px"
              }
          }
      }
  }
