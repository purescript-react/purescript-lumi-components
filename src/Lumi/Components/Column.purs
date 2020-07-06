module Lumi.Components.Column where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import React.Basic.Classic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, css, unsafeCreateDOMComponent)

component :: Component ColumnProps
component = createComponent "Column"

type ColumnProps =
  { children :: Array JSX
  , style :: CSS
  }

column :: ColumnProps -> JSX
column = makeStateless component lumiColumnElement
  where
    lumiColumnElement = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-column")

column_ :: Array JSX -> JSX
column_ children = column { children, style: css {} }

columnSelfStretch :: Array JSX -> JSX
columnSelfStretch children = column
  { style: css { alignSelf: "stretch" }
  , children
  }

responsiveColumn :: ColumnProps -> JSX
responsiveColumn = makeStateless component lumiResponsiveColumnElement
  where
    lumiResponsiveColumnElement = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-responsive-column")

responsiveColumn_ :: Array JSX -> JSX
responsiveColumn_ children = responsiveColumn { children, style: css {} }

styles :: JSS
styles = jss
  { "@global":
      { "lumi-column":
          { boxSizing: "border-box"
          , display: "flex"
          , flexDirection: "column"
          }

      , "lumi-responsive-column":
          { boxSizing: "border-box"
          , display: "flex"
          , flexDirection: "column"

          , "@media (max-width: 860px)":
              { flexDirection: "row"
              }
          }
      }
  }
