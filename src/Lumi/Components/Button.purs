module Lumi.Components.Button
  ( module Lumi.Components.Button
  , module Lumi.Components2.Button
  ) where

import Prelude

import Color (cssStringHSLA, darken, desaturate, lighten)
import Data.Array as Array
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.String (null)
import Effect.Uncurried (mkEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (isNull, isUndefined, unsafeToForeign)
import JSS (JSS, jss)
import Lumi.Components.Color (ColorName, colorNames, colors)
import Lumi.Components.Icon (IconType, icon)
import Lumi.Components.Loader (spinnerMixin)
import Lumi.Components.Size (Size(..))
import Lumi.Components2.Button (ButtonState(..))
import React.Basic.Classic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, css, unsafeCreateDOMComponent)
import React.Basic.DOM as R
import React.Basic.Events (EventHandler)

type CommonButtonProps rest =
  { accessibilityLabel :: Nullable String
  , color :: Nullable ColorName
  , onPress :: EventHandler
  , size :: Size
  , style :: CSS
  , testId :: Nullable String
  , title :: String
  , type :: String
  , buttonState :: ButtonState
  | rest
  }

type ButtonProps = CommonButtonProps ()

component :: Component ButtonProps
component = createComponent "Button"

button :: ButtonProps -> JSX
button = makeStateless component render
  where
    lumiButtonElement = element (unsafePerformEffect $ unsafeCreateDOMComponent "button")
    lumiButtonLinkElement = element (unsafePerformEffect $ unsafeCreateDOMComponent "a")

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
            , disabled:
                case props.buttonState of
                  Enabled -> false
                  Disabled -> true
                  Loading -> false
            , "data-loading":
                case props.buttonState of
                  Enabled -> false
                  Disabled -> false
                  Loading -> true
            , onClick:
                case props.buttonState of
                  Enabled -> props.onPress
                  Disabled -> mkEffectFn1 mempty
                  Loading -> mkEffectFn1 mempty
            , style: props.style
            , type: props.type
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
  , onPress: mkEffectFn1 (const $ pure unit)
  , size: Medium
  , style: css {}
  , testId: toNullable Nothing
  , title: invisibleSpace
  , type: ""
  , buttonState: Enabled
  }

primary :: ButtonProps
primary = defaults
  { color = toNullable $ Just colorNames.primary
  }

secondary :: ButtonProps
secondary = defaults
  { color = toNullable $ Just colorNames.secondary
  }

linkStyle :: ButtonProps
linkStyle = defaults
  { type = "link"
  }

invisibleSpace :: String
invisibleSpace = "\x2063"

type IconButtonProps = CommonButtonProps
  ( iconLeft :: Maybe IconType
  , iconRight :: Maybe IconType
  )

iconComponent :: Component IconButtonProps
iconComponent = createComponent "IconButton"

iconButton :: IconButtonProps -> JSX
iconButton = makeStateless iconComponent render
  where
    lumiButtonElement = element (unsafePerformEffect $ unsafeCreateDOMComponent "button")
    render props =
      lumiButtonElement
        { "aria-label": props.accessibilityLabel
        , children: children
        , className: "lumi icon-button"
        , "data-color": props.color
        , "data-size": show props.size
        , "data-testid": props.testId
        , onClick: props.onPress
        , style: props.style
        , type: props.type
        , disabled:
            case props.buttonState of
              Enabled -> false
              Disabled -> true
              Loading -> true
        , "data-loading":
            case props.buttonState of
              Enabled -> false
              Disabled -> true
              Loading -> true
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
  , onPress: mkEffectFn1 (const $ pure unit)
  , size: Medium
  , style: css {}
  , testId: toNullable Nothing
  , title: invisibleSpace
  , type: ""
  , iconLeft: Nothing
  , iconRight: Nothing
  , buttonState: Enabled
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
              , boxShadow: [ "0", "0", "0", "3px", cssStringHSLA colors.primary3 ]
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
              , backgroundColor: cssStringHSLA colors.white
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
              { "&:after":
                  spinnerMixin
                    { color: colors.white
                    , highlightColor: colors.transparent
                    , radius: "16px"
                    , borderWidth: "2px"
                    }
              , "@media (min-width: $break-point-mobile)":
                  { "&[data-size=\"small\"]":
                      { "&:after":
                          spinnerMixin
                            { color: colors.white
                            , highlightColor: colors.transparent
                            , radius: "12px"
                            , borderWidth: "2px"
                            }
                      }
                  , "&[data-size=\"large\"]":
                      { "&:after":
                          spinnerMixin
                            { color: colors.white
                            , highlightColor: colors.transparent
                            , radius: "24px"
                            , borderWidth: "3px"
                            }
                      }
                  , "&[data-size=\"extra-large\"]":
                      { "&:after":
                          spinnerMixin
                            { color: colors.white
                            , highlightColor: colors.transparent
                            , radius: "34px"
                            , borderWidth: "4px"
                            }
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
          , "&[data-color=\"accent-1-1\"]": buttonColorHoverMixin colors.accent11
          , "&[data-color=\"accent-1-2\"]": buttonColorHoverMixin colors.accent12
          , "&[data-color=\"accent-1-3\"]": buttonColorHoverMixin colors.accent13
          , "&[data-color=\"accent-2\"]": buttonColorHoverMixin colors.accent2
          , "&[data-color=\"accent-2-1\"]": buttonColorHoverMixin colors.accent21
          , "&[data-color=\"accent-2-2\"]": buttonColorHoverMixin colors.accent22
          , "&[data-color=\"accent-2-3\"]": buttonColorHoverMixin colors.accent23
          , "&[data-color=\"accent-3\"]": buttonColorHoverMixin colors.accent3
          , "&[data-color=\"accent-3-1\"]": buttonColorHoverMixin colors.accent31
          , "&[data-color=\"accent-3-2\"]": buttonColorHoverMixin colors.accent32
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
      { backgroundColor: cssStringHSLA colors.white
      , color: cssStringHSLA colors.black
      , border: [ "1px", "solid", cssStringHSLA colors.black3 ]
      }

    buttonColorHoverMixin value =
      { backgroundColor: cssStringHSLA value
      , "&:hover": { backgroundColor: cssStringHSLA $ darken 0.1 value }
      , "&:active": { backgroundColor: cssStringHSLA $ darken 0.15 value }
      , "&:disabled, &[data-loading=\"true\"]":
          { backgroundColor: cssStringHSLA $ lighten 0.4137 $ desaturate 0.1972 $ value
          }
      }
