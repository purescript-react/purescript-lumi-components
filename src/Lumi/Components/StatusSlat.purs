module Lumi.Components.StatusSlat where

-- TODO remove this in favor of Lumi.Common.Components.StatusSlat or vice-versa.
-- We probably don't need both

import Prelude

import Color (cssStringHSLA)
import Data.Nullable (Nullable, toNullable, toMaybe)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import Lumi.Components.Column (column)
import Lumi.Components.Row (row)
import Lumi.Components.Status (Status)
import Lumi.Components.Text (subtext_, title_)
import React.Basic.Classic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (css, unsafeCreateDOMComponent)

type StatusCellProps =
  { data :: Array
    { label :: String
    , content :: String
    , status :: Nullable Status
    }
  }

component :: Component StatusCellProps
component = createComponent "StatusSlat"

statusSlat :: StatusCellProps -> JSX
statusSlat = makeStateless component $ lumiStatusSlat <<< mapProps
  where
    mapProps props =
      { className: "lumi"
      , children:
          row
            { style: css
              { height: "100%" }
            , children: map toStatusCell props.data
            }
      }

    toStatusCell { label, content, status } =
      lumiStatusSlatCell
        { "data-status": toNullable (map show (toMaybe status))
        , children:
            column
              { style: css
                { height: "100%"
                , justifyContent: "center"
                }
              , children:
                [ subtext_ label
                , title_ content
                ]
              }
        }

    lumiStatusSlat = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-status-slat")
    lumiStatusSlatCell = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-status-slat-cell")

styles :: JSS
styles = jss
  { "@global":
      { "lumi-status-slat":
          { border: [ "1px", "solid", cssStringHSLA colors.black4 ]
          , width: "100%"
          , height: "64px"
          , display: "inline-block"
          , borderRadius: "5px"
          , "& lumi-status-slat-cell":
              { borderRight: [ "1px", "solid", cssStringHSLA colors.black4 ]
              , height: "100%"
              , minWidth: "calc(25% - (2 * 16px))"
              , display: "inline-block"
              , padding: "0 16px"
              , whiteSpace: "nowrap"
              , overflow: "hidden"
              , textOverflow: "ellipsis"
              , "&:last-child":
                  { borderRight: "none"
                  }
              , "&[data-status=\"active\"]":
                  { color: cssStringHSLA colors.accent1
                  }
              , "&[data-status=\"warning\"]":
                  { color: cssStringHSLA colors.accent2
                  }
              , "&[data-status=\"error\"]":
                  { color: cssStringHSLA colors.accent3
                  }
              , "&[data-status=\"unknown\"]":
                  { color: cssStringHSLA colors.black1
                  }
              }
          }



      }
  }
