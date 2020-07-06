module Lumi.Components.Tooltip where

import Prelude

import Color (cssStringHSLA)
import Data.Nullable (Nullable)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import Lumi.Components.Size (Size)
import Lumi.Components.ZIndex (ziTooltip)
import React.Basic.Classic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, unsafeCreateDOMComponent)

type TooltipProps =
  { variant :: String
  , style :: CSS
  , text :: JSX
  , content :: JSX
  , size :: Nullable Size
  }

component :: Component TooltipProps
component = createComponent "Tooltip"

tooltip :: TooltipProps -> JSX
tooltip = makeStateless component render
  where
    lumiTooltipElement = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-tooltip")
    lumiTooltipTextElement = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-tooltip-text")

    render props =
      lumiTooltipElement
        { style: props.style
        , "data-size": show props.size
        , "data-variant": props.variant
        , children:
            [ lumiTooltipTextElement
                { children: [ props.text ] }
            , props.content
            ]
        }

styles :: JSS
styles = jss
  { "@global":
      { "lumi-tooltip":
          { position: "relative"
          , cursor: "pointer"
          , "& lumi-tooltip-text":
              { visibility: "hidden"
              , opacity: "0"
              , maxWidth: "200px"
              , backgroundColor: cssStringHSLA colors.black
              , color: cssStringHSLA colors.white
              , borderRadius: "3px"
              , padding: "7px"
              , position: "absolute"
              , zIndex: ziTooltip
              , transition: "opacity 0.3s"
              -- , -webkit-backdrop-filter: blur(1px);
              , backdropFilter: "blur(1px)"
              , fontSize: "13px"
              }
          , "&:not([data-size=\"large\"]) lumi-tooltip-text":
              { maxWidth: "200px"
              , whiteSpace: "nowrap"
              }
          , "&[data-size=\"large\"] lumi-tooltip-text":
              { width: "200px"
              }
          , "&[data-variant=\"basic\"] lumi-tooltip-text":
              { bottom: "125%"
              , left: "50%"
              , transform: "translateX(-50%)"
              }
          , "&:not([data-variant=\"basic\"]) lumi-tooltip-text::after":
              { content: "''"
              , position: "absolute"
              , borderWidth: "5px"
              , borderStyle: "solid"
              }
          , "&[data-variant=\"top\"] lumi-tooltip-text":
              { bottom: "125%"
              , left: "50%"
              , transform: "translateX(-50%)"
              , "&::after":
                  { top: "100%"
                  , left: "50%"
                  , marginLeft: "-5px"
                  , borderColor: [ cssStringHSLA colors.black, "transparent", "transparent", "transparent" ]
                  }
              }
          , "&[data-variant=\"right\"] lumi-tooltip-text":
              { top: "-5px"
              , left: "110%"
              , bottom: "auto"
              , "&::after":
                  { top: "50%"
                  , right: "100%"
                  , marginTop: "-5px"
                  , borderColor: [ "transparent", cssStringHSLA colors.black, "transparent", "transparent" ]
                  }
              }
          , "&[data-variant=\"left\"] lumi-tooltip-text":
              { top: "-5px"
              , right: "110%"
              , bottom: "auto"
              , "&::after":
                  { top: "50%"
                  , left: "100%"
                  , marginTop: "-5px"
                  , borderColor: [ "transparent", "transparent", "transparent", cssStringHSLA colors.black ]
                  }
              }
          , "&[data-variant=\"bottom\"] lumi-tooltip-text":
              { top: "125%"
              , left: "50%"
              , transform: "translateX(-50%)"
              , "&::after":
                  { bottom: "100%"
                  , left: "50%"
                  , marginLeft: "-5px"
                  , borderColor: [ "transparent", "transparent", cssStringHSLA colors.black, "transparent" ]
                  }
              }
          , "&:hover lumi-tooltip-text":
              { visibility: "visible"
              , opacity: "1"
              }
          }
      }
  }
