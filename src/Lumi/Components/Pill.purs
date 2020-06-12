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
              { color: cssStringHSLA colors.black
              , borderColor: cssStringHSLA colors.accent12
              , backgroundColor: cssStringHSLA colors.accent12
              }

          , "&[data-status=\"warning\"]":
              { color: cssStringHSLA colors.black
              , borderColor: cssStringHSLA colors.accent22
              , backgroundColor: cssStringHSLA colors.accent22
              }

          , "&[data-status=\"error\"]":
              { color: cssStringHSLA colors.black
              , borderColor: cssStringHSLA colors.accent32
              , backgroundColor: cssStringHSLA colors.accent32
              }

          , "&[data-status=\"unknown\"]":
              { color: cssStringHSLA colors.black
              , borderColor: cssStringHSLA colors.black4
              , backgroundColor: cssStringHSLA colors.black4
              }
          }
      }
  }
