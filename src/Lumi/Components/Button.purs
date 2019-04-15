module Lumi.Components.Button where

import Prelude

import Color (cssStringHSLA, darken, lighten)
import Data.Array as Array
import Data.Char (fromCharCode)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.String (null)
import Data.String.CodeUnits (fromCharArray)
import Effect.Uncurried (mkEffectFn1)
import Foreign (isNull, isUndefined, unsafeToForeign)
import JSS (JSS, jss)
import Lumi.Components.Color (ColorName, colorNames, colors)
import Lumi.Components.Icon (IconType, icon)
import Lumi.Components.Loader (spinnerMixin)
import Lumi.Components.Size (Size(..))
import React.Basic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, css, unsafeCreateDOMComponent)
import React.Basic.DOM as R
import React.Basic.Events (EventHandler)

type CommonButtonProps rest =
  { accessibilityLabel :: Nullable String
  , color :: Nullable ColorName
  , disabled :: Boolean
  , onPress :: EventHandler
  , size :: Size
  , style :: CSS
  , testId :: Nullable String
  , title :: String
  , type :: String
  , loading :: Boolean
  | rest
  }

type ButtonProps = CommonButtonProps ()

component :: Component ButtonProps
component = createComponent "Button"

button :: ButtonProps -> JSX
button = makeStateless component render
  where
    lumiButtonElement = element (unsafeCreateDOMComponent "button")
    lumiButtonLinkElement = element (unsafeCreateDOMComponent "a")

    render props =
      if props.type == "link"
        then lumiButtonLinkElement
          { "aria-label": props.accessibilityLabel
          , children: children
          , className: "lumi"
          , "data-testid": props.testId
          , onClick: props.onPress
          , style: props.style
          , role: "button"
          }
        else
          lumiButtonElement
            { "aria-label": props.accessibilityLabel
            , children: children
            , className: "lumi"
            , "data-color": props.color
            , "data-size": show props.size
            , "data-testid": props.testId
            , disabled: props.disabled
            , onClick: props.onPress
            , style: props.style
            , type: props.type
            , "data-loading": props.loading
            }
      where
        children =
          if not null props.title && not (isNull || isUndefined) (unsafeToForeign props.title)
            then props.title
            else invisibleSpace -- preserves button size

defaults :: ButtonProps
defaults =
  { accessibilityLabel: toNullable Nothing
  , color: toNullable Nothing
  , disabled: false
  , onPress: mkEffectFn1 (const $ pure unit)
  , size: Medium
  , style: css {}
  , testId: toNullable Nothing
  , title: invisibleSpace
  , type: ""
  , loading: false
  }

primary :: ButtonProps
primary = defaults
  { color = toNullable $ Just $ colorNames.primary
  }

secondary :: ButtonProps
secondary = defaults
  { color = toNullable $ Just $ colorNames.secondary
  }

linkStyle :: ButtonProps
linkStyle = defaults
  { type = "link"
  }

invisibleSpace :: String
invisibleSpace = fromCharArray $ Array.catMaybes [ fromCharCode 0x2063 ]

type IconButtonProps = CommonButtonProps
  ( iconLeft :: Maybe IconType
  , iconRight :: Maybe IconType
  )

iconComponent :: Component IconButtonProps
iconComponent = createComponent "IconButton"

iconButton :: IconButtonProps -> JSX
iconButton = makeStateless iconComponent render
  where
    lumiButtonElement = element (unsafeCreateDOMComponent "button")
    render props =
      lumiButtonElement
        { "aria-label": props.accessibilityLabel
        , children: children
        , className: "lumi icon-button"
        , "data-color": props.color
        , "data-size": show props.size
        , "data-testid": props.testId
        , disabled: props.disabled
        , onClick: props.onPress
        , style: props.style
        , type: props.type
        , "data-loading": props.loading
        }
      where
        children =
          fold $ Array.catMaybes
            [ props.iconLeft <#> \iconLeft ->
                icon
                  { type_: iconLeft
                  , style: R.css { marginRight: "8px", fontSize: "11px" }
                  }
            , Just $ R.text props.title
            , props.iconRight <#> \iconRight ->
                icon
                  { type_: iconRight
                  , style: R.css { marginLeft: "8px", fontSize: "11px" }
                  }
            ]

iconButtonDefaults :: IconButtonProps
iconButtonDefaults =
  { accessibilityLabel: toNullable Nothing
  , color: toNullable Nothing
  , disabled: false
  , onPress: mkEffectFn1 (const $ pure unit)
  , size: Medium
  , style: css {}
  , testId: toNullable Nothing
  , title: invisibleSpace
  , type: ""
  , loading: false
  , iconLeft: Nothing
  , iconRight: Nothing
  }

styles :: JSS
styles = jss
  { "@global":
      { "button.lumi":
          { extend: buttonDefaults
          , "&:hover": { backgroundColor: cssStringHSLA $ darken 0.1 colors.primary }
          , "&:active": { backgroundColor: cssStringHSLA $ darken 0.15 colors.primary }
          , "&:disabled, &[data-loading=\"true\"]":
              { backgroundColor: cssStringHSLA colors.primary2
              , cursor: "default"
              }
          , "&:focus":
              { outline: 0
              , boxShadow: [ "0", "0", "0", "0.3rem", cssStringHSLA colors.primary3 ]
              }
          , "@media (min-width: 860px)":
              { padding: "6px 16px"
              , fontSize: "14px"
              , lineHeight: "20px"
              , height: "32px"
              , "&[data-size=\"small\"]":
                  { fontSize: "12px"
                  , lineHeight: "16px"
                  , height: "28px"
                  }
              , "&[data-size=\"large\"]":
                  { fontSize: "15px"
                  , lineHeight: "24px"
                  , padding: "12px 24px"
                  , height: "48px"
                  }
              , "&[data-size=\"extra-large\"]":
                  { fontSize: "20px"
                  , lineHeight: "32px"
                  , padding: "16px 32px"
                  , height: "64px"
                  }
              }
          , "&[data-color=\"secondary\"]":
              { extend: buttonSecondary
              , backgroundColor: cssStringHSLA colors.transparent
              , "&:hover":
                  { color: cssStringHSLA colors.primary
                  , borderColor: cssStringHSLA colors.primary
                  }
              , "&:disabled, &[data-loading=\"true\"]":
                  { color: cssStringHSLA colors.black2
                  , borderColor: cssStringHSLA colors.black3
                  }
              }
          , "&[data-loading=\"true\"]":
              { "&:after": spinnerMixin { radius: "16px", borderWidth: "2px" }
              , "@media (min-width: $break-point-mobile)":
                  { "&[data-size=\"small\"]":
                      { "&:after": spinnerMixin { radius: "12px", borderWidth: "2px" }
                      }
                  , "&[data-size=\"large\"]":
                      { "&:after": spinnerMixin { radius: "24px", borderWidth: "3px" }
                      }
                  , "&[data-size=\"extra-large\"]":
                      { "&:after": spinnerMixin { radius: "34px", borderWidth: "4px" }
                      }
                  }
              }

          , "&[data-color=\"black\"]": buttonColorHoverMixin colors.black
          , "&[data-color=\"black-1\"]": buttonColorHoverMixin colors.black1
          , "&[data-color=\"black-2\"]": buttonColorHoverMixin colors.black2
          , "&[data-color=\"black-3\"]": buttonColorHoverMixin colors.black3
          , "&[data-color=\"black-4\"]": buttonColorHoverMixin colors.black4
          , "&[data-color=\"black-5\"]": buttonColorHoverMixin colors.black5
          , "&[data-color=\"black-6\"]": buttonColorHoverMixin colors.black6
          , "&[data-color=\"black-7\"]": buttonColorHoverMixin colors.black7
          , "&[data-color=\"black-8\"]": buttonColorHoverMixin colors.black8
          , "&[data-color=\"primary\"]": buttonColorHoverMixin colors.primary
          , "&[data-color=\"primary-1\"]": buttonColorHoverMixin colors.primary1
          , "&[data-color=\"primary-2\"]": buttonColorHoverMixin colors.primary2
          , "&[data-color=\"primary-3\"]": buttonColorHoverMixin colors.primary3
          , "&[data-color=\"primary-4\"]": buttonColorHoverMixin colors.primary4
          , "&[data-color=\"accent-1\"]": buttonColorHoverMixin colors.accent1
          , "&[data-color=\"accent-2\"]": buttonColorHoverMixin colors.accent2
          , "&[data-color=\"accent-3\"]": buttonColorHoverMixin colors.accent3
          , "&[data-color=\"accent-3-3\"]": buttonColorHoverMixin colors.accent33
          , "&[data-color=\"white\"]": buttonColorHoverMixin colors.white
          , "&[data-color=\"finished\"]": buttonColorHoverMixin colors.primary
          , "&[data-color=\"active\"]": buttonColorHoverMixin colors.accent1
          , "&[data-color=\"warning\"]": buttonColorHoverMixin colors.accent2
          , "&[data-color=\"error\"]": buttonColorHoverMixin colors.accent3
          }
      }
  }
  where
    buttonDefaults =
      { touchAction: "manipulation"
      , userSelect: "none"
      , boxSizing: "border-box"
      , display: "flex"
      , flexFlow: "row nowrap"
      , alignItems: "center"
      , justifyContent: "center"
      , minWidth: "70px"
      , padding: "10px 20px"
      , fontSize: "14px"
      , lineHeight: "20px"
      , height: "40px"

      , border: "none"
      , borderRadius: "3px"
      , cursor: "pointer"
      , color: cssStringHSLA colors.white
      , backgroundColor: cssStringHSLA colors.primary
      , appearance: "none"
      }

    buttonSecondary =
      { backgroundColor: cssStringHSLA colors.transparent
      , color: cssStringHSLA colors.black
      , border: [ "1px", "solid", cssStringHSLA colors.black3 ]
      }

    buttonColorHoverMixin value =
      { backgroundColor: cssStringHSLA value
      , "&:hover": { backgroundColor: cssStringHSLA $ darken 0.1 value }
      , "&:active": { backgroundColor: cssStringHSLA $ darken 0.15 value }
      , "&:disabled, &[data-loading=\"true\"]":
          { backgroundColor: cssStringHSLA $ lighten 0.25 value
          }
      }
