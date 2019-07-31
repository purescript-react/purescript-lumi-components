module Lumi.Components.Examples.Divider where

import Prelude

import Lumi.Components.Column (column, column_)
import Lumi.Components.Divider (divider_, flexDivider_)
import Lumi.Components.Example (example)
import Lumi.Components.Row (row)
import Lumi.Components.Text as T
import React.Basic (Component, JSX, createComponent, makeStateless)
import React.Basic.DOM as R

component :: Component Unit
component = createComponent "DividerExample"

docs :: JSX
docs = unit # makeStateless component render
  where
    item t =
      T.text T.body
        { style = R.css { margin: "12px" }
        , children = [ R.text t ]
        }

    render _ =
      column_
        [ T.text T.p
            { children =
                [ R.code_ [ R.text "hr" ]
                , R.text " based divider:"
                ]
            }
        , example $
            column
              { style: R.css
                  { minWidth: 300
                  , alignItems: "center"
                  }
              , children:
                  [ item "Lorem"
                  , divider_
                  , item "ipsum"
                  ]
              }

       ,  T.p_ "Flexbox based divider:"
        , example $
            row
              { style: R.css
                  { minHeight: 200
                  , alignItems: "center"
                  }
              , children:
                  [ item "Lorem"
                  , flexDivider_
                  , column_
                      [ item "ipsum"
                      , flexDivider_
                      , item "dolor"
                      ]
                  ]
              }
        ]
