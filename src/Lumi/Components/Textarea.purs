module Lumi.Components.Textarea where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Effect.Uncurried (mkEffectFn1)
import Foreign.Object (fromHomogeneous)
import JSS (JSS, jss)
import Lumi.Components.Input (lumiInputDisabledStyles, lumiInputFocusInvalidStyles, lumiInputFocusStyles, lumiInputHoverStyles, lumiInputInvalidStyles, lumiInputPlaceholderStyles, lumiInputStyles)
import React.Basic.Classic (Component, JSX, createComponent, makeStateless)
import React.Basic.DOM (CSS, css)
import React.Basic.DOM as R
import React.Basic.Events (EventHandler)
import Unsafe.Coerce (unsafeCoerce)

type TextareaProps
  = { autoComplete :: String
    , disabled :: Boolean
    , lines :: Int
    , maxLength :: Nullable Int
    , minLength :: Nullable Int
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
    R.textarea
      { _data: fromHomogeneous
          { testid: unsafeNullable props.testId
          , "disable-resize": show (not props.resizable)
          }
      , autoComplete: props.autoComplete
      , className: "lumi"
      , disabled: props.disabled
      , maxLength: unsafeNullable props.maxLength
      , minLength: unsafeNullable props.minLength
      , name: props.name
      , onBlur: unsafeNullable props.onBlur
      , onChange: props.onChange
      , onFocus: unsafeNullable props.onFocus
      , placeholder: props.placeholder
      , required: props.required
      , readOnly: props.readOnly
      , rows: props.lines
      , style: props.style
      , value: props.value
      }

  unsafeNullable :: forall a. Nullable a -> a
  unsafeNullable = unsafeCoerce

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
