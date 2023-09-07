module Lumi.Components.Tooltip where

import Prelude

import Data.Maybe (Maybe(..))
import Color (cssStringHSLA)
import Data.Nullable as Nullable
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Icon as Icon
import Lumi.Components.Spacing (Space(..), hspace)
import Lumi.Components.Size (Size(..))
import Lumi.Components2.Box as Box
import Lumi.Components.Color (colors)
import Lumi.Components.ZIndex (ziTooltip)
import Lumi.Styles.Box as Box.Styles
import React.Basic.Classic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (CSS)
import React.Basic.DOM as R
import Record as Record
import Type.Proxy (Proxy(..))

type TooltipProps = Record (BaseTooltipProps ( content :: JSX ))

toTooltipProps :: JSX -> { | BaseTooltipProps () } -> TooltipProps
toTooltipProps content = Record.insert (Proxy :: _ "content") content

type BaseTooltipProps r =
  ( variant :: String
  , style :: CSS
  , text :: JSX
  , size :: Nullable.Nullable Size
  | r
  )

component :: Component TooltipProps
component = createComponent "Tooltip"

-- | Lumi's tooltip component.
-- |
-- | Tooltip positioning can get wonky depending upon the content size and
-- | likely other factors. If the variant-driven defaults aren't sufficient,
-- | a variant's absolute positioning may be overridden via CSS variables.
-- |
-- | The CSS variables available for overriding currently are:
-- |
-- | ```
-- | --lumi-tooltip-text-top
-- |   Default: "-5px"
-- |   Supported variants: "left", "right"
-- | ```
-- |
-- | You may override these variables using the `style` field of `TooltipProps`:
-- |
-- | ```
-- | tooltip
-- |   { variant: "left"
-- |   , style:
-- |       css
-- |         { "--lumi-tooltip-text-top": "-18px"
-- |         -- ...
-- |         }
-- |   -- ...
-- |   }
-- | ```
tooltip :: TooltipProps -> JSX
tooltip = makeStateless component render
  where
    lumiTooltipElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-tooltip")
    lumiTooltipTextElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-tooltip-text")

    render props =
      lumiTooltipElement
        { style: props.style
        , "data-size": show props.size
        , "data-variant": props.variant
        , children:
            [ lumiTooltipTextElement { children: [ props.text ] }
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
              , left: "var(--lumi-tooltip-text-left, 50%)"
              , transform: "translateX(-50%)"
              , "&::after":
                  { top: "100%"
                  , marginLeft: "-5px"
                  }
              }
          , "&[data-variant=\"right\"] lumi-tooltip-text":
              { top: "var(--lumi-tooltip-text-top, -5px)"
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
              { top: "var(--lumi-tooltip-text-top, -5px)"
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

data IconType = Warning | Info

withTooltip :: JSX -> String -> JSX
withTooltip content tip = withTooltip' content tip (R.css { textAlign: "center"}) Info

withTooltip' :: JSX -> String -> R.CSS -> IconType -> JSX
withTooltip' content tip style icon =
  tooltip
    { variant: "top"
    , style
    , text: R.text tip
    , content:
        Box.box
          $ Box.Styles._row
          $ Box.Styles._align Box.Styles.Center
          $ _ { content=
            [ content
            , hspace S4
            , case icon of
              Info ->
                Icon.icon
                  { type_: Icon.Info
                  , style: R.css { color: cssStringHSLA colors.primary }
                  }
              Warning ->
                R.img { src: "/images/alert-triangle.svg" }
            ] }
    , size: Nullable.toNullable $ Just Large
    }
