module Lumi.Components.Border where

import Prelude

import Color (toHexString)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import React.Basic.Classic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (unsafeCreateDOMComponent)

data BorderStyle
  = SquareTop
  | SquareBottom
  | Round
  | Square
  | Top
  | Bottom

toStyle :: BorderStyle -> String
toStyle SquareTop = "square-top"
toStyle SquareBottom = "square-bottom"
toStyle Round = "round"
toStyle Square = "square"
toStyle Top = "top"
toStyle Bottom = "bottom"

component :: Component BorderProps
component = createComponent "Border"

type BorderProps =
  { children :: JSX
  , borderStyle :: BorderStyle
  }

border :: BorderProps -> JSX
border = makeStateless component render
  where
    lumiBorderElement = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-border")

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

borderSquareTop :: JSX -> JSX
borderSquareTop children =
  border
    { children
    , borderStyle: SquareTop
    }

borderSquareBottom :: JSX -> JSX
borderSquareBottom children =
  border
    { children
    , borderStyle: SquareBottom
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
          { borderColor: toHexString colors.black3
          , borderStyle: "solid"
          , borderWidth: "1px"
          , padding: "16px"
          , overflow: "hidden"
          , "&[data-border-style='round']":
              { borderRadius: "5px"
              }
          , "&[data-border-style='square']":
              { borderRadius: "0px"
              }
          , "&[data-border-style='square-bottom']":
              { borderTopRightRadius: "5px"
              , borderTopLeftRadius: "5px"
              , borderBottomRightRadius: "0px"
              , borderBottomLeftRadius: "0px"
              }
          , "&[data-border-style='square-top']":
              { borderTopRightRadius: "0px"
              , borderTopLeftRadius: "0px"
              , borderBottomRightRadius: "5px"
              , borderBottomLeftRadius: "5px"
              }
          , "&[data-border-style='top']":
              { border: "inherit"
              , borderTop: "1px solid " <> toHexString colors.black3
              , paddingLeft: "0px"
              , paddingRight: "0px"
              }
          , "&[data-border-style='bottom']":
              { border: "inherit"
              , borderBottom: "1px solid " <> toHexString colors.black3
              , paddingLeft: "0px"
              , paddingRight: "0px"
              }
          }
      }
  }
