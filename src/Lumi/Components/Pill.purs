module Lumi.Components.Pill where

import Prelude

import Color (cssStringHSLA)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import Lumi.Components.Status (Status)
import React.Basic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, css, unsafeCreateDOMComponent)

type PillProps =
  { status :: Status
  , style :: CSS
  , testId :: Nullable String
  , title :: String
  }

component :: Component PillProps
component = createComponent "Pill"

pill :: PillProps -> JSX
pill = makeStateless component $ lumiPillElement <<< mapProps
  where
    lumiPillElement = element (unsafeCreateDOMComponent "lumi-pill")
    mapProps props =
      { "data-status": show props.status
      , "data-testid": props.testId
      , children: props.title
      , style: props.style
      }

pill_ :: Status -> String -> JSX
pill_ status title = pill
  { status
  , style: css {}
  , testId: toNullable Nothing
  , title
  }

styles :: JSS
styles = jss
  { "@global":
      { "lumi-pill":
          { padding: "2px 8px"
          , borderRadius: "20px"
          , fontSize: "13px"
          , border: "1px solid"
          , whiteSpace: "nowrap"

          , "&[data-status=\"active\"]":
              { color: cssStringHSLA colors.accent1
              , borderColor: cssStringHSLA colors.accent1
              }

          , "&[data-status=\"warning\"]":
              { color: cssStringHSLA colors.accent2
              , borderColor: cssStringHSLA colors.accent2
              }

          , "&[data-status=\"error\"]":
              { color: cssStringHSLA colors.accent3
              , borderColor: cssStringHSLA colors.accent3
              }

          , "&[data-status=\"unknown\"]":
              { color: cssStringHSLA colors.black1
              , borderColor: cssStringHSLA colors.black1
              }
          }
      }
  }
