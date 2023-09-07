module Lumi.Components.Pill where

import Prelude

import Color (cssStringHSLA)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (Color, colors)
import Lumi.Components.Status (Status)
import Lumi.Components.Text as T
import Lumi.Styles as S
import React.Basic.Classic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, css, unsafeCreateDOMComponent)
import React.Basic.DOM as R

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
    lumiPillElement = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-pill")
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

type SegmentedPillProps =
  { content :: Array PillSegmentProps
  , style :: CSS
  }

type PillSegmentProps =
  { fgColor :: Color
  , bgColor :: Color
  , text :: String
  }

segmentedComponent :: Component SegmentedPillProps
segmentedComponent = createComponent "SegmentedPill"

-- | like a pill, but made up of various segments of (potentially) different colors
segmentedPill :: SegmentedPillProps -> JSX
segmentedPill = makeStateless segmentedComponent $ pillElement <<< mapProps
  where
    pillElement = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-segmented-pill")
    mapProps props =
      { children: flip map props.content $ \seg ->
          T.text T.span { children = [ R.text seg.text ]
                        , style = css
                            { color: S.color seg.fgColor
                            , backgroundColor: S.color seg.bgColor
                            }
                        , title = toNullable (Just seg.text)
                        }
        , style: props.style <> case Array.head props.content of
            Just x -> css {
             color: S.color x.bgColor
            }
            _ -> css {}

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
      , "lumi-segmented-pill":
          { padding: "0"
          , overflow: "hidden"
          , borderRadius: "20px"
          , fontSize: "13px"
          , border: "1px solid"
          , whiteSpace: "nowrap"
          , "& span":
              { paddingTop: 0
              , paddingBottom: 0
              , paddingLeft: "0.5rem"
              , paddingRight: "0.25rem"
              , display: "inline-block"
              , maxWidth: "10rem"
              , textOverflow: "ellipsis"
              , overflow: "hidden"
              , whiteSpace: "nowrap"
              , verticalAlign: "top"
              }
          }
      }
  }
