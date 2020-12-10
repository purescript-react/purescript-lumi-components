module Lumi.Components.Progress where

import Prelude

import Color (cssStringHSLA)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components.Color (colors)
import React.Basic.Classic (Component, JSX, createComponent, displayNameFromComponent, element, makeStateless)
import React.Basic.DOM as R
import React.Basic.DOM.SVG as RS

type ProgressProps =
  { total :: Int
  , completed :: Int
  , style :: R.CSS
  , testId :: Maybe String
  }

progressDefaults :: ProgressProps
progressDefaults =
  { total: 100
  , completed: 0
  , style: R.css {}
  , testId: Nothing
  }

progressBar :: ProgressProps -> JSX
progressBar = makeProgressComponent (createComponent "ProgressBar") \{ total, completed } ->
  RS.svg
    { xmlns: "http://www.w3.org/2000/svg"
    , viewBox: "0 0 120 6"
    , preserveAspectRatio: "none"
    , height: "6"
    , width: "100%"
    , children:
        [ backgroundPath
            { fill
            , fillRule
            , strokeWidth
            , strokeMiterlimit
            , d
            , stroke: cssStringHSLA colors.black3
            }
        , meterPath
            { className: "meter"
            , fill
            , fillRule
            , strokeWidth
            , strokeMiterlimit
            , d
            , stroke: cssStringHSLA colors.primary
            , pathLength: total
            , strokeDasharray: total
            , strokeDashoffset: completed
            , style: progressAnimationStyles
            }
        ]
    }
  where
    fill = "none"
    fillRule = "nonzero"
    strokeWidth = "6"
    strokeMiterlimit = "round"
    d = "M0,3 L120,3"
    backgroundPath = RS.path
    meterPath = RS.path

progressCircle :: ProgressProps -> JSX
progressCircle = makeProgressComponent (createComponent "ProgressCircle") \{ total, completed } ->
  RS.svg
    { xmlns: "http://www.w3.org/2000/svg"
    , height: "24"
    , width: "24"
    , style: R.css { transform: "rotate(-90deg)" }
    , children:
        [ backgroundPath
            { fill
            , fillRule
            , strokeWidth
            , strokeMiterlimit
            , cx
            , cy
            , r
            , stroke: cssStringHSLA colors.black3
            }
        , meterPath
            { className: "meter"
            , fill
            , fillRule
            , strokeWidth
            , strokeMiterlimit
            , cx
            , cy
            , r
            , stroke: cssStringHSLA colors.primary
            , pathLength: total
            , strokeDasharray: total
            , strokeDashoffset: completed
            , style: progressAnimationStyles
            }
        ]
    }
  where
    fill = "none"
    fillRule = "evenodd"
    strokeWidth = "3"
    strokeMiterlimit = "round"
    cx = "12"
    cy = "12"
    r = "10"
    backgroundPath = RS.circle
    meterPath = RS.circle

makeProgressComponent :: Component ProgressProps -> ({ total :: String, completed :: String } -> JSX) -> ProgressProps -> JSX
makeProgressComponent component renderShape = makeStateless component render
  where
    render props =
      let
        total = toNumber (max 1 props.total)
        completed = min total (toNumber (max 0 props.completed))
        totalString = show total
        completedString = show (total - (total * (completed / total)))
      in
        lumiProgress
          { "data-testid": toNullable props.testId
          , "data-variant": displayNameFromComponent component
          , role: "progressbar"
          , "aria-valuenow": show completed
          , "aria-valuemin": "0"
          , "aria-valuemax": totalString
          , style: R.mergeStyles [ R.css { display: "flex" }, props.style ]
          , children: renderShape { total: totalString, completed: completedString }
          }

    lumiProgress = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-progress")

progressAnimationStyles :: R.CSS
progressAnimationStyles = R.css { willChange: "auto", transition: "stroke-dashoffset 400ms ease-in-out" }
