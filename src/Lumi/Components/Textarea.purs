module Lumi.Components.Textarea where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Effect.Uncurried (mkEffectFn1)
import JSS (JSS, jss)
import Lumi.Components.Input (lumiInputDisabledStyles, lumiInputFocusInvalidStyles, lumiInputFocusStyles, lumiInputHoverStyles, lumiInputInvalidStyles, lumiInputPlaceholderStyles, lumiInputStyles)
import React.Basic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, css, unsafeCreateDOMComponent)
import React.Basic.Events (EventHandler)

type TextareaProps
  = { autoComplete :: String
    , disabled :: Boolean
    , lines :: Int
    , maxLength :: Nullable Number
    , minLength :: Nullable Number
    , name :: String
    , onBlur :: Nullable EventHandler
    , onChange :: EventHandler
    , onFocus :: Nullable EventHandler
    , placeholder :: String
    , readOnly :: Boolean
    , required :: Boolean
    , resizable :: Boolean
    , style :: CSS
    , testId :: Nullable String
    , value :: String
    }

component :: Component TextareaProps
component = createComponent "Textarea"

textarea :: TextareaProps -> JSX
textarea = makeStateless component render
  where
  render props =
    element (unsafeCreateDOMComponent "textarea")
      { "data-testid": props.testId
      , "data-disable-resize": not props.resizable
      , autoComplete: props.autoComplete
      , className: "lumi"
      , disabled: props.disabled
      , maxLength: props.maxLength
      , minLength: props.minLength
      , name: props.name
      , onBlur: props.onBlur
      , onChange: props.onChange
      , onFocus: props.onFocus
      , placeholder: props.placeholder
      , required: props.required
      , readOnly: props.readOnly
      , rows: props.lines
      , style: props.style
      , value: props.value
      }

defaults :: TextareaProps
defaults =
  { autoComplete: "on"
  , disabled: false
  , lines: 2
  , maxLength: toNullable Nothing
  , minLength: toNullable Nothing
  , name: ""
  , onBlur: toNullable Nothing
  , onChange: mkEffectFn1 $ pure <<< const unit
  , onFocus: toNullable Nothing
  , placeholder: ""
  , required: false
  , readOnly: false
  , resizable: true
  , style: css {}
  , testId: toNullable Nothing
  , value: ""
  }

textareaPlaceholderText_ :: String -> TextareaProps
textareaPlaceholderText_ s =
  defaults { placeholder = s }

styles :: JSS
styles =
  jss
    { "@global":
      { "textarea.lumi":
        { fontFamily: "inherit"
        , fontSize: "14px"
        , lineHeight: "20px"
        , touchAction: "manipulation"
        , boxSizing: "border-box"
        , resize: "vertical"
        , extend: lumiInputStyles
        , minHeight: "40px"
        , height: "unset"
        , "@media (min-width: 860px)":
            { minHeight: "32px"
            , height: "unset"
            }
        , "&:hover": lumiInputHoverStyles
        , "&:invalid": lumiInputInvalidStyles
        , "&:focus":
          { extend: lumiInputFocusStyles
          , "&:invalid": lumiInputFocusInvalidStyles
          }
        , "&:disabled": lumiInputDisabledStyles
        , "&::-webkit-input-placeholder": lumiInputPlaceholderStyles
        , "&::-moz-placeholder": lumiInputPlaceholderStyles
        , "&:-ms-input-placeholder": lumiInputPlaceholderStyles
        , "&::placeholder": lumiInputPlaceholderStyles
        , "&[data-disable-resize=\"true\"]": { resize: "none" }
        }
      }
    }
