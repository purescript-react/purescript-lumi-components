module Lumi.Components.NativeSelect where

import Prelude

import Color (cssStringHSLA)
import Data.Foldable (find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toNullable)
import Effect.Uncurried (mkEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, important, jss)
import Lumi.Components.Color (colors)
import Lumi.Components.Input (lumiInputDisabledStyles, lumiInputFocusInvalidStyles, lumiInputFocusStyles, lumiInputHoverStyles, lumiInputInvalidStyles, lumiInputStyles)
import React.Basic.Classic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, css)
import React.Basic.DOM as R
import React.Basic.Events (EventHandler)

type NativeSelectProps =
  { disabled :: Boolean
  , name :: String
  , onChange :: EventHandler
  , options ::
      Array
        { label :: String
        , value :: String
        }
  , optionStyle :: CSS
  , placeholder :: String
  , required :: Boolean
  , style :: CSS
  , testId :: Nullable String
  , value :: String
  }

component :: Component NativeSelectProps
component = createComponent "NativeSelect"

nativeSelect :: NativeSelectProps -> JSX
nativeSelect = makeStateless component $ lumiSelectElement <<< mapProps
  where
    lumiSelectElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "select")
    lumiOptionElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "option")
    mapProps props =
      { "data-testid": props.testId
      , children: props.options <#> \{ label, value } ->
          lumiOptionElement
            { children: label
            , key: value
            , disabled: value == ""
            , value
            }
      , className: "lumi"
      , disabled: props.disabled
      , name: props.name
      , onChange: props.onChange
      , placeholder: props.placeholder
      , required: props.required
      , style: props.style
      , value: props.value
      , title:
        fromMaybe props.placeholder do
          selected <- props.options # find \o -> o.value == props.value
          pure selected.label
      }

defaults :: NativeSelectProps
defaults =
  { disabled: false
  , name: ""
  , onChange: mkEffectFn1 \_ -> pure unit
  , options: []
  , optionStyle: css {}
  , placeholder: ""
  , required: false
  , style: css {}
  , testId: toNullable Nothing
  , value: ""
  }

styles :: JSS
styles = jss
  { "@global":
      { "select.lumi":
          { appearance: "none"
          , backgroundImage: "url(\"data:image/svg+xml;charset=utf8,%3C?xml version='1.0' encoding='UTF-8'?%3E%3Csvg width='11px' height='5px' viewBox='0 0 11 5' version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'%3E%3C!-- Generator: Sketch 49.1 (51147) - http://www.bohemiancoding.com/sketch --%3E%3Ctitle%3ESlice 1%3C/title%3E%3Cdesc%3ECreated with Sketch.%3C/desc%3E%3Cdefs%3E%3Cpath d='M5.417,3.519 C5.797,3.187 6.307,2.733 6.912,2.185 L6.974,2.129 C7.70584693,1.46645784 8.43485361,0.800785071 9.161,0.132 C9.29247374,0.0108869595 9.47857352,-0.0308857001 9.64919735,0.0224173781 C9.81982119,0.0757204563 9.94904726,0.216001266 9.98819736,0.390417384 C10.0273475,0.564833501 9.97047374,0.746886966 9.839,0.868 C9.11068658,1.53896706 8.37934578,2.20664054 7.645,2.871 L7.583,2.927 C5.376,4.922 5.287,5 5,5 C4.713,5 4.624,4.922 2.417,2.927 L2.355,2.871 C1.62081041,2.20646869 0.889470368,1.5387959 0.161,0.868 C-0.0422407879,0.68077547 -0.0552245302,0.364240788 0.132,0.161 C0.31922453,-0.0422407879 0.635759212,-0.0552245302 0.839,0.132 C1.5649901,0.800955473 2.29399753,1.46662893 3.026,2.129 L3.088,2.185 C3.693,2.733 4.203,3.187 4.583,3.519 C4.75,3.665 4.89,3.785 5,3.877 C5.11,3.785 5.25,3.665 5.417,3.519 Z' id='path-1'%3E%3C/path%3E%3C/defs%3E%3Cg id='Page-1' stroke='none' stroke-width='1' fill='none' fill-rule='evenodd'%3E%3Cg id='arrow-down'%3E%3Cg id='a-link' fill='%2342413F' fill-rule='nonzero'%3E%3Cpath d='M5.417,3.519 C5.797,3.187 6.307,2.733 6.912,2.185 L6.974,2.129 C7.70584693,1.46645784 8.43485361,0.800785071 9.161,0.132 C9.29247374,0.0108869595 9.47857352,-0.0308857001 9.64919735,0.0224173781 C9.81982119,0.0757204563 9.94904726,0.216001266 9.98819736,0.390417384 C10.0273475,0.564833501 9.97047374,0.746886966 9.839,0.868 C9.11068658,1.53896706 8.37934578,2.20664054 7.645,2.871 L7.583,2.927 C5.376,4.922 5.287,5 5,5 C4.713,5 4.624,4.922 2.417,2.927 L2.355,2.871 C1.62081041,2.20646869 0.889470368,1.5387959 0.161,0.868 C-0.0422407879,0.68077547 -0.0552245302,0.364240788 0.132,0.161 C0.31922453,-0.0422407879 0.635759212,-0.0552245302 0.839,0.132 C1.5649901,0.800955473 2.29399753,1.46662893 3.026,2.129 L3.088,2.185 C3.693,2.733 4.203,3.187 4.583,3.519 C4.75,3.665 4.89,3.785 5,3.877 C5.11,3.785 5.25,3.665 5.417,3.519 Z' id='a'%3E%3C/path%3E%3C/g%3E%3Cg id='Clipped'%3E%3Cmask id='mask-2' fill='white'%3E%3Cuse xlink:href='%23path-1'%3E%3C/use%3E%3C/mask%3E%3Cg id='a'%3E%3C/g%3E%3Cg id='Group' mask='url(%23mask-2)' fill='%23292827' fill-rule='nonzero'%3E%3Cg transform='translate(-5.000000, -8.000000)' id='Shape'%3E%3Cpolygon points='0 0 20 0 20 20 0 20'%3E%3C/polygon%3E%3C/g%3E%3C/g%3E%3C/g%3E%3C/g%3E%3C/g%3E%3C/svg%3E\")"
          , backgroundRepeat: "no-repeat"
          , backgroundPositionY: "center"
          , backgroundPositionX: "calc(100% - 10px)"
          , backgroundSize: "10px 5px"
          , cursor: "pointer"
          , display: "inline-block"
          , textOverflow: "ellipsis"
          , "&:-moz-focusring":
              { color: "transparent"
              , textShadow: "0 0 0 " <> cssStringHSLA colors.black
              }
          , "& option":
              { fontWeight: "normal"
              , display: "block"
              , whiteSpace: "pre"
              , minHeight: "12px"
              , padding: "0 2px 1px"
              , "&:hover, &:active":
                  { backgroundColor: important $ cssStringHSLA colors.primary
                  }
              }

          , extend: lumiInputStyles
          , padding: "8px 24px 8px 7px"
          , "@media (min-width: 860px)":
              { padding: "4px 24px 4px 7px"
              }
          , "&:hover": lumiInputHoverStyles
          , "&:invalid": lumiInputInvalidStyles
          , "&:focus":
              { extend: lumiInputFocusStyles
              , "&:invalid": lumiInputFocusInvalidStyles
              }
          , "&:disabled": lumiInputDisabledStyles
          }
      }
  }
