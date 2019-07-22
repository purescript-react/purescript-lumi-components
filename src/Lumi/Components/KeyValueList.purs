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

    toRows r =
      r <#> \{ label, value } ->
        [ row
            { style: R.css
                { alignItems: "center"
                }
            , children:
                [ T.text T.body
                    { style = R.css
                        { flex: "3 5 0%"
                        , padding: "8px 0"
                        }
                    , color = notNull colorNames.black1
                    , children = [ R.text label ]
                    }
                , row
                    { style: R.css
                        { alignItems: "center"
                        , flex: "7 7 0%"
                        , flexWrap: "wrap"
                        , justifyContent: if rightAligned then "flex-end" else "flex-start"
                        }
                    , children: [ value ]
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
          {
          }
      }
  }
