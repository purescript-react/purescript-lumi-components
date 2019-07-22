module Lumi.Components.KeyValueList where

import Prelude

import Data.Nullable (notNull)
import JSS (JSS, jss)
import Lumi.Components.Color (colorNames)
import Lumi.Components.List as List
import Lumi.Components.Row (row)
import Lumi.Components.Text as T
import React.Basic (JSX, element)
import React.Basic.DOM as R


keyValueList
  :: { rightAligned :: Boolean
     , rows ::
         Array
           { label :: String
           , value :: JSX
           }
     , borders :: Boolean
     }
  -> JSX
keyValueList { rightAligned, rows, borders } =
  let
    lumiKeyValueListElement = element (R.unsafeCreateDOMComponent "lumi-key-value-list")
    lumiKeyValueListLabelElement = element (R.unsafeCreateDOMComponent "lumi-key-value-list-label")
    lumiKeyValueListValueElement = element (R.unsafeCreateDOMComponent "lumi-key-value-list-value")

    toRows r =
      r <#> \{ label, value } ->
        [ row
            { style: R.css
                { alignItems: "center"
                , justifyContent: "space-between"
                , width: "100%"
                }
            , children:
                [ lumiKeyValueListLabelElement
                    { children:
                        [ T.text T.body
                            { style = R.css {}
                            , color = notNull colorNames.black1
                            , children = [ R.text label ]
                            }
                        ]
                    , style: R.css {}
                    }
                , lumiKeyValueListValueElement
                    { children: [ value ]
                    , style: R.css
                        { justifyContent: if rightAligned then "flex-end" else "flex-start"
                        }
                    }
                ]
            }
        ]
  in
    lumiKeyValueListElement
      { children:
          [ if borders
              then List.list List.compactList
                { rows = toRows rows
                }
              else List.borderlessList List.compactList
                { rows = toRows rows
                }
          ]
      , style: R.css { width: "100%" }
      }

styles :: JSS
styles = jss
  { "@global":
      { "lumi-key-value-list":
          { "& lumi-key-value-list-label":
              { "flex": "3 5 0%"
              , "padding": "8px 0"
              }
          , "& lumi-key-value-list-value":
              { "display": "flex"
              , "flexFlow": "row"
              , "alignItems": "center"
              , "flex": "7 7 0%"
              , "flexWrap": "wrap"
              }
          , "@media (max-width: 860px)":
              { "width": "100%"
              , "& lumi-key-value-list-label":
                  { "flex": "initial"
                  }
              , "& lumi-key-value-list-value":
                  { "flex": "initial"
                  }
              }
          }
      }
  }
