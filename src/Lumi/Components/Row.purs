module Lumi.Components.Row where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import React.Basic.Classic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, css, unsafeCreateDOMComponent)

component :: Component RowProps
component = createComponent "Row"

type RowProps =
  { children :: Array JSX
  , style :: CSS
  }

row :: RowProps -> JSX
row = makeStateless component lumiRowElement
  where
    lumiRowElement = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-row")

row_ :: Array JSX -> JSX
row_ children = row { children, style: css {} }

responsiveRow :: RowProps -> JSX
responsiveRow = makeStateless component lumiResponsiveRowElement
  where
    lumiResponsiveRowElement = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-responsive-row")

responsiveRow_ :: Array JSX -> JSX
responsiveRow_ children = responsiveRow { children, style: css {} }

styles :: JSS
styles = jss
  { "@global":
      { "lumi-row":
          { boxSizing: "border-box"
          , display: "flex"
          , flexDirection: "row"
          }

      , "lumi-responsive-row":
          { boxSizing: "border-box"
          , display: "flex"
          , flexDirection: "row"

          , "@media (max-width: 860px)":
              { flexDirection: "column"
              }
          }
      }
  }
